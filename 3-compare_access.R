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
  gather("ind", "valor", CMAET50:TSFCAET50) %>%
  spread(city, valor) %>%
  # calculate abs diffs
  mutate(dif_abs = forcorrigidocm - forpadrao,
         dif_log = log(forcorrigidocm/forpadrao)) %>%
  mutate(dif_log_tc = ifelse(dif_log > 0.5, 0.5,
                             ifelse(dif_log < -0.5, -0.5, dif_log))) %>%
  st_sf(crs = 4326) %>%
  filter(!is.na(dif_abs))



# library(mapview)
# a <- acess_dif_wide %>% filter(stringr::str_detect(ind, "CMATT"))
# mapview(a, zcol = "dif_abs", col.regions = RColorBrewer::brewer.pal(10, "RdBu"), col = NULL)

# limits for each indicator
limits_cma_abs <- acess_dif_wide %>%
  filter(stringr::str_detect(ind, "CMATT")) %>%
  pull(abs(dif_abs))

limits_tsfca_abs <- acess_dif_wide %>%
  filter(stringr::str_detect(ind, "TSFCATT")) %>%
  pull(abs(dif_abs))




library(patchwork)
# boxplot.stats(for_comparacao_tt$dif_log)$stats[2:4]
# comparacao espacial
map_difabs_TT_CMA <- acess_dif_wide %>%
  # filter(stringr::str_detect(ind, "TT")) %>%
  filter(stringr::str_detect(ind, "CMATT")) %>%
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
  filter(stringr::str_detect(ind, "TSFCATT")) %>%
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


for_comparacao_tt %>%
  # mutate(corta = cut(dif_log_tc, 
  #                    breaks = seq(-0.5, 0.5, by = 0.1), 
  #                    labels = c("<0.5", "-0.4", "-0.3", "-0.2", "-0.1", "0", "0.1", "0.2", "0.3", "0.4", ">0.5"))) %>%
  ggplot()+
  # geom_histogram(aes(dif_log_tc), binwidth = 0.1)+
  geom_jitter(aes(x = 1, y = dif_log, color = dif_log), alpha = 1)+
  geom_boxplot(aes(x = 1, y = dif_log), alpha = 0.1)+
  scale_color_distiller(palette = "RdBu", direction = 1,
                        limits = c(-1,1)*max(abs(for_comparacao_tt$dif_log_tc)))+
  scale_y_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5),
                     labels = c("-150%", "-100%", "-50%", "0", "50%", "100%", "150%"))+
  # scale_fill_distiller(palette = "RdBu", direction = 1)+
  #                      # limits = c(-1,1)*max(abs(for_comparacao_tt$dif_log_tc)),
  #                      breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
  #                      labels = c("<-0.5", "-0.25", "0", "0.25", ">0.5")
  #                      )+
  # scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5), labels = c("<-0.5", "-0.25", "0", "0.25", ">0.5"))+
  theme_ipsum_rc(grid = "Y")+
  theme(plot.margin = unit(c(1, 1, 1, 1), "mm"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") +
  plot_layout(widths = c(2, 1))

ggsave("figure/5-comparacao_gtfs_tt_65.png", dpi = 300, units = "cm", width = 16, height = 10)
