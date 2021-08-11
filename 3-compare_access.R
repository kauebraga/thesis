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
  summarise(dif_abs = max(abs(dif_abs), na.rm = TRUE),
            dif_log_tc = max(abs(dif_log_tc), na.rm = TRUE)) %>% setDT()


# var <- "CMATT60"; tipo <- "dif_abs

# function to maps
fazer_mapa <- function(var, tipo) {
  
  # get limits
  limits_scale <- limits_ind[ind == var] %>% pull(!!rlang::sym(tipo))
  # print(limits_scale)
  # limits_ind[ind == var] %>% pull{tipo)
  
  labelss <- if (grepl("CMA", var) & tipo == "dif_abs") ks else label_number(accuracy = 0.01)
  
  
  acess_dif_wide %>%
    # filter(stringr::str_detect(ind, "TT")) %>%
    filter(stringr::str_detect(ind, var)) %>%
    ggplot()+
    geom_sf(aes(fill = !!rlang::sym(tipo)), color = NA)+
    geom_sf(data = limits, fill = NA)+
    scale_fill_distiller(palette = "RdBu", direction = 1,
                         limits = c(-1,1)*limits_scale,
                         # breaks = c(-0.5, 0, 0.5),
                         labels = labelss
    ) +
    labs(
      fill = ""
      # title = var
      )+
    theme_mapa()
}

# apply function
a <- lapply(c(
  # "CMATT45", "FCATT45",
  "CMATT60", "FCATT60",
  "CMATT90", "FCATT90"),
  fazer_mapa, tipo = "dif_abs")

maps_abs <- purrr::reduce(a, `+`) + plot_layout(ncol = 2)

ggsave(filename = "figures/anpet_2021/map_comp_abs_PRP50.png",
       plot = maps_abs,
       units = "cm",
       width = 16,
       height = 14)

b <- lapply(c(
  # "CMATT45", "FCATT45",
  "CMATT60", "FCATT60",
  "CMATT90", "FCATT90"),
  fazer_mapa, tipo = "dif_log_tc")

maps_log <- purrr::reduce(b, `+`) + plot_layout(ncol = 2)

ggsave(filename = "figures/anpet_2021/map_comp_log_PRP50.png",
       plot = maps_log,
       units = "cm",
       width = 16,
       height = 14)


# mapview breaks
# https://stackoverflow.com/questions/55485100/mapview-legend-scaling
# mapview(acess_dif_wide, zcol = "dif_abs", col.regions = RColorBrewer::brewer.pal(10, "RdBu"), col = NULL)







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
  mutate(dif_log_tc = ifelse(dif_log > 0.8, 0.8,
                             ifelse(dif_log < -0.8, -0.8, dif_log))) %>%
  st_sf(crs = 4326)



# acess_dif_wide %>% filter(ind == "FCATT45") %>% filter(dif_abs > 0.126)

# library(mapview)
# a <- acess_dif_wide %>% filter(stringr::str_detect(ind, "CMATT"))
# mapview(a, zcol = "dif_abs", col.regions = RColorBrewer::brewer.pal(10, "RdBu"), col = NULL)

# limits for each indicator
limits_ind <- acess_dif_wide %>%
  st_set_geometry(NULL) %>%
  group_by(ind) %>%
  summarise(dif_abs = max(abs(dif_abs), na.rm = TRUE),
            dif_log_tc = max(abs(dif_log_tc), na.rm = TRUE)) %>% setDT()


# var <- "CMATT60"; tipo <- "dif_abs

# function to maps
fazer_mapa <- function(var, tipo) {
  
  # get limits
  limits_scale <- limits_ind[ind == var] %>% pull(!!rlang::sym(tipo))
  # print(limits_scale)
  # limits_ind[ind == var] %>% pull{tipo)
  
  labelss <- if (grepl("CMA", var) & tipo == "dif_abs") ks else label_number(accuracy = 0.01)
  
  
  acess_dif_wide %>%
    # filter(stringr::str_detect(ind, "TT")) %>%
    filter(stringr::str_detect(ind, var)) %>%
    ggplot()+
    geom_sf(aes(fill = !!rlang::sym(tipo)), color = NA)+
    geom_sf(data = limits, fill = NA)+
    scale_fill_distiller(palette = "RdBu", direction = 1,
                         limits = c(-1,1)*limits_scale,
                         # breaks = c(-0.5, 0, 0.5),
                         labels = labelss
    ) +
    labs(
      fill = ""
      # title = var
    )+
    theme_mapa()
}

# apply function
a <- lapply(c(
  # "CMATT45", "FCATT45",
  "CMATT60", "FCATT60",
  "CMATT90", "FCATT90"),
  fazer_mapa, tipo = "dif_abs")

maps_abs <- purrr::reduce(a, `+`) + plot_layout(ncol = 2)

ggsave(filename = "figures/anpet_2021/map_comp_abs_P50P85.png",
       plot = maps_abs,
       units = "cm",
       width = 16,
       height = 14)

b <- lapply(c(
  # "CMATT45", "FCATT45",
  "CMATT60", "FCATT60",
  "CMATT90", "FCATT90"),
  fazer_mapa, tipo = "dif_log_tc")

maps_log <- purrr::reduce(b, `+`) + plot_layout(ncol = 2)

ggsave(filename = "figures/anpet_2021/map_comp_log_P50P85.png",
       plot = maps_log,
       units = "cm",
       width = 16,
       height = 14)
