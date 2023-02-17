library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(readr)
library(ggplot2)
library(hrbrthemes)
library(mapview)
library(patchwork)
library(scales)
library(ggnewscale)
options(scipen = 999)


ks <- function (x) { scales::label_number(accuracy = 1,
                                          scale = 1/1000,
                                          suffix = "k",
                                          big.mark = ",")(x)
  
}


# atencai aqyu
acess <- rbind(
  read_rds("../../data/thesis/output_access/acess_forpadrao.rds"),
  read_rds("../../data/thesis/output_access/acess_forcorrigidocm.rds"),
  read_rds("../../data/thesis/output_access/acess_forcorrigidoce.rds")
)

# abrir limits
limits <- geobr::read_municipality(2304400)


# open regions
regions <- st_read("../../data/thesis/macrozonas/AREA Macrozonas Fortaleza (2017) (Lara)/Macrozonas.shp")
regions <- st_read("../../data/thesis/macrozonas/AREA Regiões de Fortaleza (2017) (Lara)/Regiões de Fortaleza.shp")
mapview(regions)

# abrir maptles
maptile <- read_rds("../../data/thesis/maptile_crop_mapbox_for_2017.rds")

theme_mapa <- function(base_size) {
  
  theme_void(base_family="Roboto Condensed") %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(1.0,"line"),
      legend.key.height = unit(0.1,"cm"),
      # legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.7)),
      legend.text=element_text(size=unit(7, "cm")),
      # legend.title=element_blank(),
      plot.title = element_text(hjust = 0.5, vjust = 2, size = 12)
      
      
    )
}


# linhas HM
gtfs <- gtfstools::read_gtfs("../../otp/thesis/graphs/forpadrao/gtfs_for_metrofor_2021-01.zip")
# gtfs stop times needs ordering
gtfs$stop_times <- setorder(gtfs$stop_times, trip_id, stop_sequence)
linhas_hm <- gtfs %>%
  gtfstools::get_trip_geometry(file = "stop_times") %>%
  left_join(gtfs$trips %>% select(trip_id, route_id), by = "trip_id") %>%
  left_join(gtfs$routes %>% select(route_id, route_long_name), by = "route_id") %>%
  count(route_long_name)



acess <- acess %>%
  mutate(city = factor(city, levels = c("forpadrao", "forcorrigidocm", "forcorrigidoce")))


# map with the regions ------------------------------------------------------------------------
map_regions <- ggplot() +
  geom_raster(data = maptile, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = st_transform(regions, 3857), aes(fill = REGION), lwd = 0.5, color = "black", alpha = 0.6)+
  geom_sf_text(data = st_transform(regions, 3857), aes(label = REGION))+
  labs(
    fill = ""
    # title = var
  )+
  # facet_wrap(~city)+
  theme_mapa()+
  guides(fill = "none")


ggsave(filename = "A1/figures/regions.png",
       plot = map_regions,
       width = 16, height = 16,
       units = "cm")


# acesso diff - figura 1  -------------------------------------------------------------------


acess_wide <- acess %>%
  filter(city %in% c("forpadrao", "forcorrigidocm")) %>%
  select(origin, city, CMAET60, CMATT60) %>%
  pivot_longer(cols = matches("CMAET60|CMATT60"), names_to =  "ind", values_to = "valor") %>%
  spread(city, valor) %>%
  # calculate abs diffs
  mutate(dif_abs = forcorrigidocm - forpadrao,
         dif_rel = ((forcorrigidocm - forpadrao)/forpadrao),
         dif_log = log(forcorrigidocm/forpadrao)) %>%
  mutate(dif_rel_tc = ifelse(dif_rel > 0.4, 0.4,
                             ifelse(dif_rel < -0.4, -0.4, dif_rel))) %>%
  mutate(dif_log_tc = ifelse(dif_log > 0.4, 0.4,
                             ifelse(dif_log < -0.4, -0.4, dif_log))) %>%
  st_sf(crs = 4326) %>%
  filter(!is.na(dif_abs)) %>%
  st_sf(crs = 4326)



# mapview(acess_wide, zcol = "dif_log_tc", alpha = 0.2)
# mapview(acess_wide, zcol = "dif_log_tc", col.regions = RColorBrewer::brewer.pal(10, "RdBu"), col = NULL, alpha = 0.1)


median(acess_wide$dif_log, na.rm = TRUE)
summary(subset(acess_wide, ind == "CMATT60")$dif_log, na.rm = TRUE)



limits_ind <- acess_wide %>%
  st_set_geometry(NULL) %>%
  group_by(ind) %>%
  summarise(dif_abs = max(abs(dif_abs), na.rm = TRUE),
            dif_log_tc = max(abs(dif_log_tc), na.rm = TRUE),
            dif_rel_tc = max(abs(dif_rel_tc), na.rm = TRUE)
  ) %>% setDT()



max_plot1 <- max(acess %>% filter(city %in% c("forpadrao", "forcorrigidocm")) %>% pull(CMATT60))

acess_jobs1a <- ggplot(acess %>% filter(city %in% c("forpadrao")) %>% st_transform(3857))+
  geom_raster(data = maptile, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(aes(fill = CMATT60), color = NA)+
  geom_sf(data = st_transform(limits, 3857), fill = NA)+
  geom_sf(data = st_transform(linhas_hm, 3857), linewidth = 0.3, linetype=3)+
  scale_fill_viridis_c(option = "inferno",
                       label = ks,
                       limits = c(0, max_plot1)
  )+
  labs(
    fill = ""
    # title = var
  )+
  # facet_wrap(~city)+
  theme_mapa()

acess_jobs1b <- ggplot(acess %>% filter(city %in% c("forcorrigidocm")) %>% st_transform(3857))+
  geom_raster(data = maptile, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(aes(fill = CMATT60), color = NA)+
  geom_sf(data = st_transform(limits, 3857), fill = NA)+
  geom_sf(data = st_transform(linhas_hm, 3857), linewidth = 0.3, linetype=3)+
  # geom_sf(data = st_transform(regions, 3857), lwd = 0.5, fill = NA, color = "red")+
  scale_fill_viridis_c(option = "inferno",
                       label = ks,
                       limits = c(0, max_plot1)
  )+
  labs(
    fill = ""
    # title = var
  )+
  # facet_wrap(~city)+
  theme_mapa()



map_acess_dif_jobs_c1 <- acess_wide %>% st_transform(3857) %>%
  filter(ind == "CMATT60") %>%
  ggplot()+
  geom_raster(data = maptile, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(aes(fill = dif_rel_tc), color = NA)+
  geom_sf(data = st_transform(limits, 3857), fill = NA)+
  geom_sf(data = st_transform(linhas_hm, 3857), aes(linetype = ""), linewidth = 0.35)+
  geom_sf(data = st_transform(regions, 3857), aes(color = ""), lwd = 0.07, fill = NA)+
  scale_linetype_manual(values = c(4, 4, 4)) +
  scale_color_manual(values = c("red")) +
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-0.4, 0.4),
                       breaks = c(-0.4, -0.2, 0, 0.2, 0.4),
                       labels = c("<-40%", "-20%", 0, "20%", "40%>")
  ) +
  labs(
    fill = "",
    color = "Regions",
    linetype = "High capacity lines"
    # title = var
  )+
  theme_mapa()+
  guides(
    color = "none",
    linetype = "none"
  )

# boxplot_acess_dif_jobs_c1 <- acess_wide %>%
#   filter(ind == "CMATT60") %>%
#   ggplot()+
#   geom_boxplot(aes(x = "", y = dif_log))+
#   geom_hline(yintercept = 0)+
#   scale_y_continuous(labels = label_percent())+
#   labs(
#     x = ""
#     # title = var
#   )+
#   theme_ipsum_es(grid = "X")+
#   theme(axis.text.y = element_blank())+
#   coord_flip(ylim = c(-0.4, 0.4))


map_acess_dif_c1 <- 
  (acess_jobs1a + acess_jobs1b)  /map_acess_dif_jobs_c1+
  plot_layout(heights = c(1, 2), widths = c(2, 1))


map_acess_dif_c1[[1]] <- map_acess_dif_c1[[1]] + 
  plot_layout(tag_level = 'new', guides = 'collect')
map_acess_dif_c1 <- map_acess_dif_c1 + plot_annotation(tag_levels = c('A', '1')) &
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'))


ggsave(filename = "A1/figures/2-map_acess_c1.png",
       plot = map_acess_dif_c1,
       width = 16, height = 16,
       units = "cm")

# acesso diff - figura 2  -------------------------------------------------------------------


acess_wide <- acess %>%
  filter(city %in% c("forcorrigidocm", "forcorrigidoce")) %>%
  select(origin, city, CMAET60, CMATT60) %>%
  pivot_longer(cols = matches("CMAET60|CMATT60"), names_to =  "ind", values_to = "valor") %>%
  spread(city, valor) %>%
  # calculate abs diffs
  mutate(dif_abs = forcorrigidoce - forcorrigidocm,
         dif_rel = ((forcorrigidoce - forcorrigidocm)/forcorrigidocm),
         dif_log = log(forcorrigidoce/forcorrigidocm)) %>%
  mutate(dif_rel_tc = ifelse(dif_rel > 0.6, 0.6,
                             ifelse(dif_rel < -0.6, -0.6, dif_rel))) %>%
  mutate(dif_log_tc = ifelse(dif_log > 0.8, 0.8,
                             ifelse(dif_log < -0.8, -0.8, dif_log))) %>%
  st_sf(crs = 4326) %>%
  filter(!is.na(dif_abs)) %>%
  st_sf(crs = 4326)


# mapview(acess_wide, zcol = "dif_log_tc", alpha = 0.2)
# mapview(acess_wide, zcol = "dif_log_tc", col.regions = RColorBrewer::brewer.pal(10, "RdBu"), col = NULL, alpha = 0.1)


median(acess_wide$dif_log, na.rm = TRUE)
summary(subset(acess_wide, ind == "CMATT60")$dif_log, na.rm = TRUE)



limits_ind <- acess_wide %>%
  st_set_geometry(NULL) %>%
  group_by(ind) %>%
  summarise(dif_abs = max(abs(dif_abs), na.rm = TRUE),
            dif_rel_tc = max(abs(dif_rel_tc), na.rm = TRUE),
            dif_log_tc = max(abs(dif_log_tc), na.rm = TRUE)) %>% setDT()


max_plot2 <- max(acess %>% filter(city %in% c("forcorrigidocm", "forcorrigidoce")) %>% pull(CMATT60))

acess_jobs2a <- ggplot(acess %>% filter(city %in% c("forcorrigidocm")) %>% st_transform(3857))+
  geom_raster(data = maptile, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(aes(fill = CMATT60), color = NA)+
  geom_sf(data = st_transform(limits, 3857), fill = NA)+
  geom_sf(data = st_transform(linhas_hm, 3857), linewidth = 0.3, linetype=3)+
  scale_fill_viridis_c(option = "inferno",
                       label = ks,
                       limits = c(0, max_plot2)
  )+
  labs(
    fill = ""
    # title = var
  )+
  # facet_wrap(~city)+
  theme_mapa()

acess_jobs2b <- ggplot(acess %>% filter(city %in% c("forcorrigidoce")) %>% st_transform(3857))+
  geom_raster(data = maptile, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(aes(fill = CMATT60), color = NA)+
  geom_sf(data = limits, fill = NA)+
  geom_sf(data = st_transform(linhas_hm, 3857), linewidth = 0.3, linetype=3)+
  scale_fill_viridis_c(option = "inferno",
                       label = ks,
                       limits = c(0, max_plot2)
  )+
  labs(
    fill = ""
    # title = var
  )+
  # facet_wrap(~city)+
  theme_mapa()



map_acess_dif_jobs_c2 <- acess_wide %>% st_transform(3857) %>%
  filter(ind == "CMATT60") %>%
  ggplot()+
  geom_raster(data = maptile, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(aes(fill = dif_rel_tc), color = NA)+
  geom_sf(data = limits, fill = NA)+
  geom_sf(data = st_transform(linhas_hm, 3857), aes(linetype = ""), linewidth = 0.35)+
  geom_sf(data = st_transform(regions, 3857), aes(color = ""), lwd = 0.07, fill = NA)+
  scale_linetype_manual(values = c(4, 4, 4)) +
  scale_color_manual(values = c("grey15")) +
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-0.6, 0.6),
                       breaks = c(-0.6, -0.3, 0, 0.3, 0.6),
                       labels = c("<-60%", "-30%", 0, "30%", "60%>")
  ) +
  labs(
    fill = ""
    # title = var
  )+
  theme_mapa() +
  guides(
    color = "none",
    linetype = "none"
  )


map_acess_dif_c2 <- 
  (acess_jobs2a + acess_jobs2b)  /map_acess_dif_jobs_c2+
  plot_layout(heights = c(1, 2), widths = c(2, 1))


map_acess_dif_c2[[1]] <- map_acess_dif_c2[[1]] + 
  plot_layout(tag_level = 'new', guides = 'collect')
map_acess_dif_c2 <- map_acess_dif_c2 + plot_annotation(tag_levels = c('A', '1')) &
  theme(legend.position = "bottom",
        legend.key.width= unit(1, 'cm'))


ggsave(filename = "A1/figures/2-map_acess_c2.png",
       plot = map_acess_dif_c2,
       width = 16, height = 16,
       units = "cm")
