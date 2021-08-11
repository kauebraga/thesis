library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(readr)
library(ggplot2)
library(hrbrthemes)
library(mapview)
options(scipen = 999)


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


# comparacao PR x P50 -------------------------------------------------------------------------



acess_dif_wide <- acess %>%
  filter(city %in% c("forpadrao", "forcorrigidocm")) %>%
  gather("ind", "valor", CMATT45:FCATT90) %>%
  spread(city, valor) %>%
  # calculate abs diffs
  mutate(dif_abs = forcorrigidocm - forpadrao,
         dif_log = log(forcorrigidocm/forpadrao)) %>%
  mutate(dif_log_tc = ifelse(dif_log > 0.4, 0.4,
                             ifelse(dif_log < -0.4, -0.4, dif_log))) %>%
  st_sf(crs = 4326) %>%
  filter(!is.na(dif_abs))



# library(mapview)
# a <- acess_dif_wide %>% filter(stringr::str_detect(ind, "CMATT"))
# mapview(a, zcol = "dif_abs", col.regions = RColorBrewer::brewer.pal(10, "RdBu"), col = NULL)

# limits for each indicator
limits_ind <- acess_dif_wide %>%
  st_set_geometry(NULL) %>%
  group_by(ind) %>%
  summarise(dif_abs = abs(min(dif_abs, na.rm = TRUE)),
            dif_log_tc = abs(min(dif_log_tc, na.rm = TRUE))) %>% setDT()


# var <- "CMATT60"

# function to maps
fazer_mapa <- function(var, tipo) {
  
  # get limits
  limits_scale <- limits_ind[ind == var] %>% pull({{tipo}})
    
    acess_dif_wide %>%
    # filter(stringr::str_detect(ind, "TT")) %>%
    filter(stringr::str_detect(ind, var)) %>%
    ggplot()+
    geom_sf(aes(fill = {{tipo}}), color = NA)+
    geom_sf(data = limits, fill = NA)+
    scale_fill_distiller(palette = "RdBu", direction = 1,
                         limits = c(-1,1)*limits_scale
                         # breaks = c(-0.5, 0, 0.5),
                         # labels = c("-50%", "0", "+50%"))+
    ) +
    theme_mapa()
}


a <- lapply(c("CMATT45", "FCATT45",
              "CMATT60", "FCATT60",
              "CMATT90", "FCATT90"),
            fazer_mapa, tipo = dif_abs)

maps_abs <- purrr::reduce(a, `+`) + plot_layout(ncol = 2)

b <- lapply(c("CMATT45", "FCATT45",
              "CMATT60", "FCATT60",
              "CMATT90", "FCATT90"),
            fazer_mapa, tipo = dif_log_tc)

maps_log <- purrr::reduce(b, `+`) + plot_layout(ncol = 2)

library(patchwork)
# boxplot.stats(for_comparacao_tt$dif_log)$stats[2:4]
# comparacao espacial
map_difabs_TT_CMA <- acess_dif_wide %>%
  # filter(stringr::str_detect(ind, "TT")) %>%
  filter(stringr::str_detect(ind, "CMATT60")) %>%
  ggplot()+
  geom_sf(aes(fill = dif_abs), color = NA)+
  geom_sf(data = limits, fill = NA)+
  # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, )+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*max(limits_cma_abs)
                       # breaks = c(-0.5, 0, 0.5),
                       # labels = c("-50%", "0", "+50%")
  )+
  labs(fill = "Diferença de oportunidades\n acessíveis")+
  theme_mapa()



map_difabs_TT_FCA <- acess_dif_wide %>%
  # filter(stringr::str_detect(ind, "TT")) %>%
  filter(stringr::str_detect(ind, "FCATT60")) %>%
  ggplot()+
  geom_sf(aes(fill = dif_abs), color = NA)+
  geom_sf(data = limits, fill = NA)+
  # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, )+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*max(limits_tsfca_abs)
                       # breaks = c(-0.5, 0, 0.5),
                       # labels = c("-50%", "0", "+50%")
  )+
  labs(fill = "Diferença de oportunidades\n acessíveis")+
  theme_mapa()

map_difabs_TT_CMA + map_difabs_TT_FCA

# mapview breaks
# https://stackoverflow.com/questions/55485100/mapview-legend-scaling
# mapview(acess_dif_wide, zcol = "dif_abs", col.regions = RColorBrewer::brewer.pal(10, "RdBu"), col = NULL)




# diferenca relativa

map_difrel_TT_CMA <- acess_dif_wide %>%
  # filter(stringr::str_detect(ind, "TT")) %>%
  filter(stringr::str_detect(ind, "CMATT60")) %>%
  ggplot()+
  geom_sf(aes(fill = dif_log_tc), color = NA)+
  geom_sf(data = limits, fill = NA)+
  # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, )+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*max(limits_cma_rel)
                       # breaks = c(-0.5, 0, 0.5),
                       # labels = c("-50%", "0", "+50%")
  )+
  labs(fill = "Diferença relativa de oportunidades\n acessíveis")+
  theme_mapa()



map_difrel_TT_FCA <- acess_dif_wide %>%
  # filter(stringr::str_detect(ind, "TT")) %>%
  filter(stringr::str_detect(ind, "TSFCATT")) %>%
  ggplot()+
  geom_sf(aes(fill = dif_log_tc), color = NA)+
  geom_sf(data = limits, fill = NA)+
  # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, )+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*max(limits_tsfca_rel)
                       # breaks = c(-0.5, 0, 0.5),
                       # labels = c("-50%", "0", "+50%")
  )+
  labs(fill = "Diferença de oportunidades\n acessíveis")+
  theme_mapa()

map_difrel_TT_CMA + map_difrel_TT_FCA

# diference between the cma and tsfca
dif_CMA_TSFCA <- acess_dif_wide %>%
  select(origin, ind, dif_log_tc) %>%
  filter(stringr::str_detect(ind, "CMATT|TSFCATT")) %>%
  spread(key = ind,
         value = dif_log_tc,
         sep = "_"
  ) %>%
  mutate(dif = abs(ind_TSFCATT65  - ind_CMATT65)) %>%
  mutate(dif_tc = ifelse(dif > 0.2, 0.2, dif))

boxplot(dif_CMA_TSFCA$dif)
mapview(dif_CMA_TSFCA, zcol = "dif_tc")

mean(dif_CMA_TSFCA$ind_CMATT65, na.rm = TRUE)
mean(dif_CMA_TSFCA$ind_TSFCATT65, na.rm = TRUE)

acess_dif_wide %>%
  filter(stringr::str_detect(ind, "CMATT|TSFCATT")) %>%
  ggplot()+
  geom_boxplot(aes(x = ind, y = dif_log))

ggplot()+
  geom_sf(data = dif_CMA_TSFCA, aes(fill = dif_tc), color = NA)+
  geom_sf(data = limits, fill = NA)+
  # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, )+
  scale_fill_viridis_c(option = "inferno")+
  labs(fill = "Vermelho: A dif do CMA eh maior")+
  theme_mapa()









# comparacao P50 x P85 -------------------------------------------------------------------------



acess_dif_wide <- acess %>%
  filter(city %in% c("forcorrigidocm", "forcorrigidoce")) %>%
  gather("ind", "valor", CMATT45:FCATT90) %>%
  spread(city, valor) %>%
  # calculate abs diffs
  mutate(dif_abs = forcorrigidoce - forcorrigidocm,
         dif_log = log(forcorrigidoce/forcorrigidocm)) %>%
  filter(!is.na(dif_abs)) %>%
  filter(!is.infinite(dif_log)) %>%
  filter(!is.na(dif_log)) %>%
  mutate(dif_log_tc = ifelse(dif_log > 0.5, 0.5,
                             ifelse(dif_log < -0.5, -0.5, dif_log))) %>%
  st_sf(crs = 4326)



# library(mapview)
# a <- acess_dif_wide %>% filter(stringr::str_detect(ind, "CMATT"))
# mapview(a, zcol = "dif_abs", col.regions = RColorBrewer::brewer.pal(10, "RdBu"), col = NULL)

# limits for each indicator
limits_cma_abs <- acess_dif_wide %>%
  filter(stringr::str_detect(ind, "CMATT")) %>%
  pull(dif_abs) %>% abs() %>% max()

limits_tsfca_abs <- acess_dif_wide %>%
  filter(stringr::str_detect(ind, "^FCATT")) %>%
  pull(dif_abs) %>% abs() %>% max() 

limits_cma_rel <- acess_dif_wide %>%
  filter(stringr::str_detect(ind, "CMATT")) %>%
  pull(dif_log_tc) %>% abs() %>% max()

limits_tsfca_rel <- acess_dif_wide %>%
  filter(stringr::str_detect(ind, "^FCATT")) %>%
  pull(dif_log_tc) %>% abs() %>% max()



library(patchwork)
# boxplot.stats(for_comparacao_tt$dif_log)$stats[2:4]
# comparacao espacial
map_difabs_TT_CMA <- acess_dif_wide %>%
  # filter(stringr::str_detect(ind, "TT")) %>%
  filter(stringr::str_detect(ind, "CMATT60")) %>%
  ggplot()+
  geom_sf(aes(fill = dif_abs), color = NA)+
  geom_sf(data = limits, fill = NA)+
  # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, )+
  scale_fill_distiller(palette = "OrRd", direction = -1)+
  labs(fill = "Diferença de oportunidades\n acessíveis")+
  theme_mapa()

summary(subset(acess_dif_wide, ind == "CMATT65")$dif_abs)
summary(subset(acess_dif_wide, ind == "FCATT65")$dif_abs)

map_difabs_TT_FCA <- acess_dif_wide %>%
  # filter(stringr::str_detect(ind, "TT")) %>%
  filter(stringr::str_detect(ind, "^FCATT45")) %>%
  ggplot()+
  geom_sf(aes(fill = dif_abs), color = NA)+
  geom_sf(data = limits, fill = NA)+
  # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, )+
  # scale_fill_distiller(palette = "RdBu", direction = 1)+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*max(limits_tsfca_abs)
                       # breaks = c(-0.5, 0, 0.5),
                       # labels = c("-50%", "0", "+50%")
  )+
  labs(fill = "Diferença de oportunidades\n acessíveis")+
  theme_mapa()

map_difabs_TT_CMA + map_difabs_TT_FCA

# mapview breaks
# https://stackoverflow.com/questions/55485100/mapview-legend-scaling
# mapview(acess_dif_wide, zcol = "dif_abs", col.regions = RColorBrewer::brewer.pal(10, "RdBu"), col = NULL)

# diferenca relativa -----------------------------

map_difrel_TT_CMA <- acess_dif_wide %>%
  # filter(stringr::str_detect(ind, "TT")) %>%
  filter(stringr::str_detect(ind, "CMATT")) %>%
  ggplot()+
  geom_sf(aes(fill = dif_log_tc), color = NA)+
  geom_sf(data = limits, fill = NA)+
  # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, )+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*max(limits_cma_rel)
                       # breaks = c(-0.5, 0, 0.5),
                       # labels = c("-50%", "0", "+50%")
  )+
  labs(fill = "Diferença relativa de oportunidades\n acessíveis")+
  theme_mapa()



map_difrel_TT_FCA <- acess_dif_wide %>%
  # filter(stringr::str_detect(ind, "TT")) %>%
  filter(stringr::str_detect(ind, "^FCATT")) %>%
  ggplot()+
  geom_sf(aes(fill = dif_log), color = NA)+
  geom_sf(data = limits, fill = NA)+
  # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, )+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*max(limits_tsfca_rel)
                       # breaks = c(-0.5, 0, 0.5),
                       # labels = c("-50%", "0", "+50%")
  )+
  labs(fill = "Diferença de oportunidades\n acessíveis")+
  theme_mapa()


acess_dif_wide %>%
  # filter(stringr::str_detect(ind, "TT")) %>%
  filter(stringr::str_detect(ind, "^FCATT")) %>%
  mapview(zcol = "dif_log_tc", col.regions = RColorBrewer::brewer.pal(10, "RdBu"), col = NULL)

map_difrel_TT_CMA + map_difrel_TT_FCA

# diference between the cma and tsfca
dif_CMA_TSFCA <- acess_dif_wide %>%
  select(origin, ind, dif_log_tc) %>%
  filter(stringr::str_detect(ind, "CMATT|TSFCATT")) %>%
  spread(key = ind,
         value = dif_log_tc,
         sep = "_"
  ) %>%
  mutate(dif = abs(ind_TSFCATT65  - ind_CMATT65)) %>%
  mutate(dif_tc = ifelse(dif > 0.2, 0.2, dif))

boxplot(dif_CMA_TSFCA$dif)
mapview(dif_CMA_TSFCA, zcol = "dif_tc")

mean(dif_CMA_TSFCA$ind_CMATT65, na.rm = TRUE)
mean(dif_CMA_TSFCA$ind_TSFCATT65, na.rm = TRUE)

acess_dif_wide %>%
  filter(stringr::str_detect(ind, "CMATT|TSFCATT")) %>%
  ggplot()+
  geom_boxplot(aes(x = ind, y = dif_log))

ggplot()+
  geom_sf(data = dif_CMA_TSFCA, aes(fill = dif_tc), color = NA)+
  geom_sf(data = limits, fill = NA)+
  # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, )+
  scale_fill_viridis_c(option = "inferno")+
  labs(fill = "Vermelho: A dif do CMA eh maior")+
  theme_mapa()