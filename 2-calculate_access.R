library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(readr)
options(scipen = 999)

# gtfs_sigla <- "forpadrao"
# gtfs_sigla <- "forcorrigidocm"
# gtfs_sigla <- "forcorrigidoce"

calculate_access <- function(gtfs_sigla) {
  
  
  ttmatrix <- read_rds(sprintf("../../data/thesis/output_ttmatrix/ttmatrix_%s.rds", gtfs_sigla))
  
  ttmatrix <- ttmatrix %>% select(origin = fromId, destination = toId, tt_median = travel_time) %>% setDT()
  ttmatrix[, city := gtfs_sigla]
  
  # Pegar arquivo com os hexagonos com as atividades
  dir_hex <- sprintf("../../data/dissertacao/hex_agregados/hex_agregados_09.rds")
  
  # abrir oportunidades com hexagonos
  hexagonos_sf <- read_rds(dir_hex) %>%
    ungroup()
  
  # so populacao e renda
  hexagonos_pop <- hexagonos_sf %>%
    st_set_geometry(NULL) %>%
    select(id_hex, pop_total, renda_total) %>%
    setDT()
  
  # outras variaveis
  hexagonos_vars <- hexagonos_sf %>%
    st_set_geometry(NULL) %>%
    select(-pop_total, -renda_total) %>%
    setDT()
  
  # Juntar as variaveis de uso do solo com os tempos de viagem
  # Trazer a populacao e renda (juncao pela ORIGEM!)
  ttmatrix <- ttmatrix[hexagonos_pop, on = c("origin" = "id_hex"),  
                       c('pop_total', 'renda_total') := list(i.pop_total, i.renda_total)]
  
  
  # Trazer as demais variaveis (juncao pelo DESTINO!)
  ttmatrix <- ttmatrix[hexagonos_vars, on = c("destination" = "id_hex"),  
                       c('empregos_total', 'mat_total') := 
                         list(i.empregos_total, i.mat_total)]
  
  ttmatrix[, opp := 1]
  
  # calculate cumulative access -----------------------
  acess_cum <- ttmatrix[, 
                        .(
                          CMAUN = sum( opp[which( tt_median <= 70)], na.rm=T),
                          # CMA_ET_15 = sum( mat_total[which( tt_median <= 15)], na.rm=T)
                          # , CMA_ET_30 = sum( mat_total[which( tt_median <= 30)], na.rm=T)
                          CMAET45 = sum( mat_total[which( tt_median <= 45)], na.rm=T)
                          # CMAET50 = sum( mat_total[which( tt_median <= 50)], na.rm=T)
                          , CMAET60 = sum( mat_total[which( tt_median <= 60)], na.rm=T)
                          
                          # , CMA_TT_15 = sum( empregos_total[which( tt_median <= 15)], na.rm=T)
                          # , CMA_TT_30 = sum( empregos_total[which( tt_median <= 30)], na.rm=T)
                          , CMATT45 = sum( empregos_total[which( tt_median <= 45)], na.rm=T)
                          # , CMA_TT_60 = sum( empregos_total[which( tt_median <= 60)], na.rm=T)
                          , CMATT60 = sum( empregos_total[which( tt_median <= 60)], na.rm=T)
                          , CMATT75 = sum( empregos_total[which( tt_median <= 75)], na.rm=T)
                        ),
                        by=.(city, origin) ]
  
  
  
  
  
  
  
  # calculate 2SFCA --------------------
  # filter only hex with jobs / enrollments
  ttmatrix_jobs <- ttmatrix[empregos_total > 0]
  ttmatrix_schools <- ttmatrix[mat_total > 0]
  
  # calculate impedance
  ttmatrix_jobs[,':='(impedance_45 = fifelse(tt_median <= 45, 1, 0))]
  ttmatrix_jobs[,':='(impedance_60 = fifelse(tt_median <= 60, 1, 0))]
  ttmatrix_jobs[,':='(impedance_75 = fifelse(tt_median <= 75, 1, 0))]
  # ttmatrix_schools[,':='(impedance_50 = fifelse(tt_median <= 50, 1, 0))]
  
  # # calculate weights i (normalized impedance by origin)
  # ttmatrix_jobs[, wi := impedance_45/sum(impedance_45), by=origin]
  # ttmatrix_jobs[, wi := impedance_60/sum(impedance_60), by=origin]
  # ttmatrix_jobs[, wi := impedance_90/sum(impedance_90), by=origin]
  # ttmatrix_schools[, wi := impedance_50/sum(impedance_50), by=origin]
  # 
  # # calculate weights j (normalized impedance by destination)
  # ttmatrix_jobs[, wj := impedance_45/sum(impedance_45), by=destination]
  # ttmatrix_jobs[, wj := impedance_60/sum(impedance_60), by=destination]
  # ttmatrix_jobs[, wj := impedance_90/sum(impedance_90), by=destination]
  # ttmatrix_schools[, wj := impedance_50/sum(impedance_50), by=destination]
  
  # calculate pop served
  ttmatrix_jobs[, pop_served_45 := sum(pop_total * impedance_45, na.rm = TRUE), by= .(destination)]
  ttmatrix_jobs[, pop_served_60 := sum(pop_total * impedance_60, na.rm = TRUE), by= .(destination)]
  ttmatrix_jobs[, pop_served_75 := sum(pop_total * impedance_75, na.rm = TRUE), by= .(destination)]
  # ttmatrix_schools[, pop_served_50 := sum(pop_total * impedance_50, na.rm = TRUE), by= .(destination)]
  
  # for bfca
  # ttmatrix_jobs[, pop_served_65b := sum(pop_total * wi, na.rm = TRUE), by= .(destination)]
  # ttmatrix_schools[, pop_served_50b := sum(pop_total * wi, na.rm = TRUE), by= .(destination)]
  
  
  
  # calculate ppr
  ttmatrix_jobs[, ppr_45 := empregos_total[1] / pop_served_45, by = destination]
  ttmatrix_jobs[, ppr_60 := empregos_total[1] / pop_served_60, by = destination]
  ttmatrix_jobs[, ppr_75 := empregos_total[1] / pop_served_75, by = destination]
  # ttmatrix_schools[, ppr_50 := mat_total[1] / pop_served_50, by = destination]
  
  # ttmatrix_jobs[, ppr_65b := empregos_total[1] / pop_served_65b, by = destination]
  # ttmatrix_schools[, ppr_50b := mat_total[1] / pop_served_50b, by = destination]
  
  
  
  # calculate 2sfca
  acess_2sfca_jobs <- ttmatrix_jobs[, .(FCATT45 = sum(ppr_45 * impedance_45, na.rm = TRUE),
                                        FCATT60 = sum(ppr_60 * impedance_60, na.rm = TRUE),
                                        FCATT75 = sum(ppr_75 * impedance_75, na.rm = TRUE)
                                        # BFCATT65 = sum(ppr_65b * wj, na.rm = TRUE)
                                        ),
                                    by = .(city, origin)]
  
  # acess_2sfca_schools <- ttmatrix_schools[, .(FCAET50 = sum(ppr_50 * impedance_50, na.rm = TRUE),
  #                                             BFCAET50 = sum(ppr_50b * wj, na.rm = TRUE)),
  #                                         by = .(city, origin)]
  
  # bind both bfca
  acess_2sfca <- acess_2sfca_jobs
  # acess_2sfca <- full_join(acess_2sfca_jobs, acess_2sfca_schools,
  #                          by = c("city", "origin"),
  #                          suffix = c("TT", "ET"))
  
  # a <- acess_sf %>% filter(!is.infinite(TSFCA60)) %>% 
  # mutate(quant = cut(TSFCA45, breaks = quantile(TSFCA60, probs = seq(0, 1, 0.1))))
  # mutate(quant = cut(TSFCA45, breaks = seq(1, 1.4, 0.005)))
  
  # mapview::mapview(a, zcol = "TSFCA45")
  # mapview::mapview(a, zcol = "quant")
  
  
  # bind all access
  acess <- full_join(acess_cum, acess_2sfca,
                     by = c("city", "origin"))
  
  acess_sf <- merge(acess, setDT(hexagonos_sf)[, .(id_hex, geometry)],
                    by.x = "origin",
                    by.y = "id_hex",
                    all.x = TRUE) %>%
    # Transformar para sf
    st_sf()
  
  # Salvar
  path_out <- sprintf("../../data/thesis/output_access/acess_%s.rds", gtfs_sigla)
  write_rds(acess_sf, path_out)
  
  # mapview::mapview(acess_sf, zcol = "CMATT65", color = NULL)
  # acess_sf %>% filter(!is.infinite(FCATT65)) %>%
  #   filter(FCATT65 < 0.6) %>%
  # mapview::mapview(zcol = "FCATT65", color = NULL)
  
}



calculate_access("forpadrao")
calculate_access("forcorrigidocm")
calculate_access("forcorrigidoce")




# testes para alguns hex ----------------------------------------------------------------------
library(mapview)

ttmatrix <- rbind(read_rds(sprintf("../../data/thesis/output_ttmatrix/ttmatrix_%s.rds", "forcorrigidocm")) %>% 
                    mutate(cenario = "P50"),
                  read_rds(sprintf("../../data/thesis/output_ttmatrix/ttmatrix_%s.rds", "forcorrigidoce"))  
                  %>% mutate(cenario = "P85"))

# abrir oportunidades com hexagonos
hexagonos_sf <- read_rds("../../data/dissertacao/hex_agregados/hex_agregados_09.rds") %>%
  select(id_hex, empregos_total)
ungroup()

mapview(hexagonos_sf)


# pegar hexagonos de teste
# hex_teste <- "89801045c73ffff"
# hex_teste <- "89801041907ffff" # lado leste
# hex_teste <- "89801045c4fffff" # lado oeste

ttmatrix_teste <- ttmatrix %>% filter(fromId == hex_teste) %>% filter(travel_time < 60)
ttmatrix_teste <- ttmatrix_teste %>% left_join(hexagonos_sf, by = c("toId" = "id_hex")) %>% st_sf()

ttmatrix_teste %>% st_set_geometry(NULL) %>%
  group_by(cenario) %>% 
  summarise(empregos_total = sum(empregos_total, na.rm = TRUE)) %>%
  pivot_wider(names_from = "cenario", values_from = "empregos_total") %>%
  mutate(dif = log(P85/P50))

mapview(ttmatrix_teste, zcol = "cenario", alpha.regions = 0.3)
