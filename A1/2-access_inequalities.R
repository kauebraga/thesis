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
library(rlang)
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

# scenario <- "C1"
# scenario <- "C2"

caracterize_scenario <- function(scenario) {
  
  scenario1 <- if (scenario == "C1") c("forpadrao", "forcorrigidocm") else c("forcorrigidocm", "forcorrigidoce")
  
  
  acess_wide <- acess %>%
    st_set_geometry(NULL) %>%
    filter(city %in% scenario1) %>%
    select(origin, city, CMATT60, CMAET60) %>%
    gather("ind", "valor", CMATT60:CMAET60) %>%
    spread(city, valor) %>%
    # calculate abs diffs
    mutate(dif_abs = !! sym(scenario1[2]) - !! sym(scenario1[1]),
           dif_rel = ((!! sym(scenario1[2]) - !! sym(scenario1[1]))/!! sym(scenario1[1])),
           dif_log = log( !! sym(scenario1[2])  / !! sym(scenario1[1])  )) %>%
    mutate(dif_log_tc = ifelse(dif_log < -2, -2, dif_log)) %>%
    mutate(dif_log_tc = ifelse(dif_log_tc > 2, 2, dif_log_tc))
  
  
  
  # bring socioeconomic info
  # abrir oportunidades com hexagonos
  hexagonos_sf <- aopdata::read_landuse(city = "for") %>%
    # select vars
    select(id_hex, P001, R003)
  
  
  # bring vars to access
  acess_wide <- acess_wide %>%
    left_join(hexagonos_sf, by = c("origin" = "id_hex"))
  
  # select indc
  acess_wide_ind <- acess_wide %>%
    filter(grepl(pattern = "CMATT60", x = ind)) %>%
    # filter(grepl(pattern = "CMA(ET|TT)60", x = ind)) %>%
    mutate(R003 = as.factor(R003)) %>%
    mutate(P001 = ifelse(is.na(P001), 0, P001)) %>%
    filter(!is.na(R003))
  
  # calcular palma
  acess_palma <- acess_wide_ind %>%
    filter(R003 %in% c(1, 2, 3, 4, 10)) %>%
    mutate(classe = ifelse(R003 %in% c(1, 2, 3, 4), "pobre", "rico")) %>%
    group_by(ind, classe) %>%
    summarise(acess_medio1 = weighted.mean(!! sym(scenario1[1]) , P001),
              acess_medio2 = weighted.mean(!! sym(scenario1[2]) , P001)) %>%
    ungroup() %>%
    pivot_longer(cols = acess_medio1:acess_medio2, names_to = "cenario", values_to = "value") %>%
    pivot_wider(names_from = "classe", values_from = c(value)) %>%
    mutate(palma = rico/pobre)
  
  # table(acess_wide_ind$ind)
  # table(acess_wide_ind$R003)
  
  # acess_wide_ind %>% group_by(ind, R003) %>% summarise(mean = mean(dif_log, na.rm = TRUE))
  
  # acess_wide_ind <- acess_wide_ind %>%
  #   mutate(ind = factor(ind, levels = c("CMATT60", "CMAET60")))
  
  # facet_labels <- c(
  #   'CMATT60'="Jobs",
  #   'CMAET60'="School enrollments"
  # )
  
  boxplot_income <- ggplot()+
    geom_boxplot(data = acess_wide_ind, aes(x = R003, y = dif_log_tc, weight = P001, color = R003), outlier.size = 0.5, lwd = 0.3)+
    geom_hline(yintercept = 0)+
    # facet_wrap( ~ ind, ncol  = 1, labeller = as_labeller(facet_labels))+
    scale_colour_brewer(palette = "RdBu", labels=c('D1 \nPoorest', paste0('D', c(2:9)), 'D10 \nRichest'), name='Income\n decile')+
    scale_y_continuous(labels = scales::label_percent()) +
    hrbrthemes::theme_ipsum()+
    theme(panel.spacing = unit(0.1, "lines"),
          legend.position = "bottom",
          plot.margin=unit(c(2,0,0,0),"mm"))+
    labs(x = "",
         y = "Accessibility difference")+
    guides(colour = guide_legend(nrow = 1))+
    coord_cartesian(ylim = if (scenario == "C1")  c(-0.5, 0.5) else c(-0.8, 0))
  
  # save
  ggsave(plot = boxplot_income, filename = sprintf("A1/figures/boxplot_income_%s.png", scenario),
         width = 18, height = 12, units = "cm")
  
  
  # # scatterplot
  # scatterplot_income <- ggplot()+
  #   geom_point(data = acess_wide_ind, aes(x = !! sym(scenario1[1]), y = !! sym(scenario1[2]),
  #                                         size = P001, color = R003), alpha = 0.2)+
  #   geom_abline(slope = 1, intercept = 0)+
  #   scale_colour_brewer(palette = "RdBu", labels=c('D1 Poorest', paste0('D', c(2:9)), 'D10 Richest'), name='Income\n decile')+
  #   scale_size_continuous(guide = "none")+
  #   scale_x_continuous(labels = ks)+
  #   scale_y_continuous(labels = ks)+
  #   facet_wrap( ~ ind, ncol  = 1, scales = "free")+
  #   hrbrthemes::theme_ipsum()+
  #   labs(x = "")+
  #   theme(legend.position = "bottom",
  #         plot.margin=unit(c(2,0,0,0),"mm")) +
  #   guides(colour = guide_legend(nrow = 1))
  # 
  # # save
  # ggsave(plot = scatterplot_income, filename = sprintf("A1/figures/scatterplot_income_%s.png", scenario),
  #        width = 17, height = 15, units = "cm")
  
  
  
  
}


caracterize_scenario("C1")
caracterize_scenario("C2")
