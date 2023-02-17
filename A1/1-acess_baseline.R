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



# acesso baseline - figura1 -------------------------------------------------------------------

acess_wide <- acess %>%
  filter(city %in% c("forcorrigidocm")) %>%
  select(origin, city, CMAET60, CMATT60) %>%
  # pivot_longer(cols = matches("CMAET60|CMATT60"), names_to =  "ind", values_to = "valor") %>%
  # spread(city, valor) %>%
  st_sf(crs = 4326)


acess_jobs <- ggplot(acess_wide)+
  geom_sf(aes(fill = CMATT60), color = NA)+
  geom_sf(data = limits, fill = NA)+
  geom_sf(data = linhas_hm, size = 0.2)+
  scale_fill_viridis_c(option = "inferno",
                       label = ks
  )+
  labs(
    fill = ""
    # title = var
  )+
  theme_mapa()


boxplot_jobs <- acess_wide %>%
  ggplot()+
  geom_boxplot(aes(x = "", y = CMATT60))+
  scale_y_continuous(labels = ks)+
  labs(
    x = ""
    # title = var
  )+
  theme_ipsum_es(grid = "X")+
  theme(axis.text.y = element_blank())+
  coord_flip()


acess_schools <- ggplot(acess_wide)+
  geom_sf(aes(fill = CMAET60), color = NA)+
  geom_sf(data = limits, fill = NA)+
  geom_sf(data = linhas_hm, size = 0.2)+
  scale_fill_viridis_c(option = "inferno", label = ks)+
  labs(
    fill = ""
    # title = var
  )+
  theme_mapa()


map_acess_baseline <- acess_jobs + boxplot_jobs + plot_layout(heights = c(5, 1))
# map_acess_baseline <- acess_jobs + acess_schools

ggsave(filename = "A1/figures/1-map_acess_baseline.png",
       plot = map_acess_baseline,
       width = 16, height = 12,
       units = "cm")






# acesso un - figura 2  -------------------------------------------------------------------

acess_wide <- acess %>%
  filter(city %in% c("forpadrao", "forcorrigidocm")) %>%
  select(origin, city, CMAUN) %>%
  pivot_longer(cols = matches("CMAUN"), names_to =  "ind", values_to = "valor") %>%
  spread(city, valor) %>%
  # calculate abs diffs
  mutate(dif_abs = forcorrigidocm - forpadrao,
         dif_log = log(forcorrigidocm/forpadrao)) %>%
  mutate(dif_log_tc = ifelse(dif_log > 0.2, 0.2,
                             ifelse(dif_log < -0.2, -0.2, dif_log))) %>%
  st_sf(crs = 4326) %>%
  filter(!is.na(dif_abs)) %>%
  st_sf(crs = 4326)

# mapview(acess_wide, zcol = "dif_abs", alpha = 0.2)

limits_ind <- acess_wide %>%
  st_set_geometry(NULL) %>%
  group_by(ind) %>%
  summarise(dif_abs = max(abs(dif_abs), na.rm = TRUE),
            dif_log = max(abs(dif_log), na.rm = TRUE),
            dif_log_tc = max(abs(dif_log_tc), na.rm = TRUE)) %>% setDT()



map_acess_dif_un_c1 <- acess_wide %>%
  filter(ind == "CMAUN") %>%
  ggplot()+
  geom_sf(aes(fill = dif_log_tc), color = NA)+
  geom_sf(data = limits, fill = NA)+
  geom_sf(data = linhas_hm, size = 0.2)+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*limits_ind[ind == "CMAUN"]$dif_log_tc,
                       # breaks = c(-0.5, 0, 0.5),
                       labels = label_percent()
  ) +
  labs(
    fill = ""
    # title = var
  )+
  theme_mapa()

boxplot_acess_dif_un_c1 <- acess_wide %>%
  filter(ind == "CMAUN") %>%
  ggplot()+
  geom_boxplot(aes(x = "", y = dif_log))+
  geom_hline(yintercept = 0)+
  scale_y_continuous(labels = label_percent())+
  labs(
    x = ""
    # title = var
  )+
  theme_ipsum_es(grid = "X")+
  theme(axis.text.y = element_blank())+
  coord_flip(ylim = c(-0.4, 0.4))





acess_dif_un_c1 <- 
  map_acess_dif_un_c1 + boxplot_acess_dif_un_c1 +
  plot_layout(heights = c(6, 1))

ggsave(filename = "A1/figures/2-map_acess_dif_un_c1.png",
       plot = acess_dif_un_c1,
       width = 16, height = 12,
       units = "cm")


# acesso diff - figura 2  -------------------------------------------------------------------

acess_wide <- acess %>%
  filter(city %in% c("forpadrao", "forcorrigidocm")) %>%
  select(origin, city, CMAET60, CMATT60) %>%
  pivot_longer(cols = matches("CMAET60|CMATT60"), names_to =  "ind", values_to = "valor") %>%
  spread(city, valor) %>%
  # calculate abs diffs
  mutate(dif_abs = forcorrigidocm - forpadrao,
         dif_log = log(forcorrigidocm/forpadrao)) %>%
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
            dif_log_tc = max(abs(dif_log_tc), na.rm = TRUE)) %>% setDT()



map_acess_dif_jobs_c1 <- acess_wide %>%
  filter(ind == "CMATT60") %>%
  ggplot()+
  geom_sf(aes(fill = dif_log_tc), color = NA)+
  geom_sf(data = limits, fill = NA)+
  geom_sf(data = linhas_hm, size = 0.2)+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*limits_ind[ind == "CMATT60"]$dif_log_tc,
                       # breaks = c(-0.5, 0, 0.5),
                       labels = label_percent()
  ) +
  labs(
    fill = ""
    # title = var
  )+
  theme_mapa()

boxplot_acess_dif_jobs_c1 <- acess_wide %>%
  filter(ind == "CMATT60") %>%
  ggplot()+
  geom_boxplot(aes(x = "", y = dif_log))+
  geom_hline(yintercept = 0)+
  scale_y_continuous(labels = label_percent())+
  labs(
    x = ""
    # title = var
  )+
  theme_ipsum_es(grid = "X")+
  theme(axis.text.y = element_blank())+
  coord_flip(ylim = c(-0.4, 0.4))


map_acess_dif_schools_c1 <- acess_wide %>%
  filter(ind == "CMAET60") %>%
  ggplot()+
  geom_sf(aes(fill = dif_log_tc), color = NA)+
  geom_sf(data = limits, fill = NA)+
  geom_sf(data = linhas_hm, size = 0.2)+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*limits_ind[ind == "CMAET60"]$dif_log_tc,
                       # breaks = c(-0.5, 0, 0.5),
                       labels = label_percent()
  ) +
  labs(
    fill = ""
    # title = var
  )+
  theme_mapa()




boxplot_acess_dif_schools_c1 <- acess_wide %>%
  filter(ind == "CMAET60") %>%
  ggplot()+
  geom_boxplot(aes(x = 1, y = dif_log))+
  geom_hline(yintercept = 0)+
  scale_y_continuous(labels = label_percent(), limits = c(-0.4, 0.4))+
  labs(
    x = ""
    # title = var
  )+
  theme_ipsum_es(grid = "X")+
  theme(axis.text.y = element_blank())+
  coord_flip(c(-0.4, 0.4))



map_acess_dif_c1 <- 
  map_acess_dif_jobs_c1 + boxplot_acess_dif_jobs_c1 + 
  # map_acess_dif_jobs_c1 + map_acess_dif_schools_c1 +boxplot_acess_dif_jobs_c1  +  boxplot_acess_dif_schools_c1 + 
  plot_layout(heights = c(5, 1))

ggsave(filename = "A1/figures/2-map_acess_dif_c1.png",
       plot = map_acess_dif_c1,
       width = 16, height = 12,
       units = "cm")




# acesso diff - figura 3  -------------------------------------------------------------------

acess_wide <- acess %>%
  filter(city %in% c("forcorrigidocm", "forcorrigidoce")) %>%
  select(origin, city, CMAET60, CMATT60) %>%
  pivot_longer(cols = matches("CMAET60|CMATT60"), names_to =  "ind", values_to = "valor") %>%
  spread(city, valor) %>%
  # calculate abs diffs
  mutate(dif_abs = forcorrigidoce - forcorrigidocm,
         dif_log = log(forcorrigidoce/forcorrigidocm)) %>%
  mutate(dif_log_tc = ifelse(dif_log > 0.8, 0.8,
                             ifelse(dif_log < -0.8, -0.8, dif_log))) %>%
  st_sf(crs = 4326) %>%
  filter(!is.na(dif_abs)) %>%
  st_sf(crs = 4326)

mapview(acess_wide, zcol = "dif_abs")


summary(subset(acess_wide, ind == "CMATT60")$dif_log, na.rm = TRUE)
summary(subset(acess_wide, ind == "CMATT60")$dif_abs, na.rm = TRUE)

limits_ind <- acess_wide %>%
  st_set_geometry(NULL) %>%
  group_by(ind) %>%
  summarise(dif_abs = max(abs(dif_abs), na.rm = TRUE),
            dif_log_tc = max(abs(dif_log_tc), na.rm = TRUE)) %>% setDT()



map_acess_dif_jobs_c2 <- acess_wide %>%
  filter(ind == "CMATT60") %>%
  ggplot()+
  geom_sf(aes(fill = dif_log_tc), color = NA)+
  geom_sf(data = limits, fill = NA)+
  geom_sf(data = linhas_hm, size = 0.2)+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*limits_ind[ind == "CMATT60"]$dif_log_tc,
                       # limits = c(-1,1)*limits_ind[ind == "CMATT60"]$dif_abs,
                       labels = label_percent()
  ) +
  labs(
    fill = ""
    # title = var
  )+
  theme_mapa()

boxplot_acess_dif_jobs_c2 <- acess_wide %>%
  filter(ind == "CMATT60") %>%
  ggplot()+
  geom_boxplot(aes(x = "", y = dif_log_tc))+
  geom_hline(yintercept = 0)+
  scale_y_continuous(labels = label_percent())+
  labs(
    x = ""
    # title = var
  )+
  theme_ipsum_es(grid = "X")+
  theme(axis.text.y = element_blank())+
  coord_flip(ylim = c(-0.8, 0))


map_acess_dif_schools_c2 <- acess_wide %>%
  filter(ind == "CMAET60") %>%
  ggplot()+
  geom_sf(aes(fill = dif_log_tc), color = NA)+
  geom_sf(data = limits, fill = NA)+
  # geom_sf(data = linhas_hm, size = 0.2)+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*limits_ind[ind == "CMAET60"]$dif_log_tc,
                       # breaks = c(-0.5, 0, 0.5),
                       labels = label_percent()
  ) +
  labs(
    fill = ""
    # title = var
  )+
  theme_mapa()




boxplot_acess_dif_schools_c2 <- acess_wide %>%
  filter(ind == "CMAET60") %>%
  ggplot()+
  geom_boxplot(aes(x = 1, y = dif_log_tc))+
  geom_hline(yintercept = 0)+
  scale_y_continuous(labels = label_percent(), limits = c(-0.8, 0))+
  labs(
    x = ""
    # title = var
  )+
  theme_ipsum_es(grid = "X")+
  theme(axis.text.y = element_blank())+
  coord_flip(c(-0.8, 0.8))



map_acess_dif_c2 <- 
  map_acess_dif_jobs_c2 + boxplot_acess_dif_jobs_c2 + 
  # map_acess_dif_jobs_c2 + map_acess_dif_schools_c2 +boxplot_acess_dif_jobs_c2  +  boxplot_acess_dif_schools_c2 + 
  plot_layout(heights = c(5, 1))

ggsave(filename = "A1/figures/2-map_acess_dif_c2.png",
       plot = map_acess_dif_c2,
       width = 16, height = 12,
       units = "cm")
