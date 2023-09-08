library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(readr)
library(accessibility)
options(scipen = 999)

# gtfs_sigla <- "forpadrao"
# gtfs_sigla <- "forcorrigidocm"
# gtfs_sigla <- "forcorrigidoce"

calculate_access <- function(gtfs_sigla) {
  
  
  ttmatrix <- read_rds(sprintf("data/output_ttmatrix/ttmatrix_%s.rds", gtfs_sigla))
  
  ttmatrix <- ttmatrix %>% select(from_id = fromId, to_id = toId, travel_time) %>% setDT()
  
  
  # Pegar arquivo com os hexagonos com as atividades
  dir_hex <- sprintf("data/hex_agregados/hex_agregados_09.rds")
  
  # abrir oportunidades com hexagonos
  hexagonos_sf <- read_rds(dir_hex) %>%
    ungroup()
  
  hexagonos <- hexagonos_sf %>%
    st_set_geometry(NULL) %>%
    select(id = id_hex, pop_total, empregos_total)
  

  

# calculate cumulative ------------------------------------------------------------------------

  
  acess_cma <- accessibility::cumulative_cutoff(
    travel_matrix = ttmatrix,
    land_use_data = hexagonos,
    opportunity = "empregos_total",
    travel_cost = "travel_time",
    cutoff = c(45, 60, 75)
  )
  
  
  acess_cma[, city := gtfs_sigla]
  acess_cma <- acess_cma %>% rename(access_cum = empregos_total)
  
  
  
  # calculate 2SFCA cumulative --------------------
  
  
  acess_2sfca_cma <- floating_catchment_area(
    travel_matrix = ttmatrix,
    land_use_data = hexagonos,
    opportunity = "empregos_total",
    travel_cost = "travel_time",
    demand = "pop_total",
    method  = "2sfca",
    decay_function = decay_binary(c(45, 60, 75))
  )
  
  # acess_2sfca_cma[, ind := "2sfca_cum"]
  acess_2sfca_cma <- acess_2sfca_cma %>% rename("access_2sfca_cma" = empregos_total)
  
  # calculate 2SFCA for gravitational function --------
  
  
  acess_2sfca_grav <- floating_catchment_area(
    travel_matrix = ttmatrix,
    land_use_data = hexagonos,
    opportunity = "empregos_total",
    travel_cost = "travel_time",
    demand = "pop_total",
    method  = "2sfca",
    decay_function = decay_exponential(c(0.05, 0.1, 0.15))
    
  )
  
  acess_2sfca_grav[, decay_function_arg := ifelse(decay_function_arg == 0.05, 45,
                                                  ifelse(decay_function_arg == 0.1, 60, 
                                                         ifelse(decay_function_arg == 0.15, 75, decay_function_arg)))]
  # acess_2sfca_grav[, ind := "2sfca_grav"]
  acess_2sfca_grav <- acess_2sfca_grav %>% rename("access_2sfca_grav" = empregos_total)
  
  # bind all access
  acess <- full_join(acess_cma, acess_2sfca_cma,
                     by = c("id", "travel_time" = "decay_function_arg"))
  
  acess <- full_join(acess, acess_2sfca_grav,
                     by = c("id", "travel_time" = "decay_function_arg"))
  
  acess_sf <- left_join(acess, hexagonos_sf %>% select(id = id_hex)) %>%
    st_sf(crs = 4326)
  
  # Salvar
  path_out <- sprintf("data/A2/output_access/acess_%s.rds", gtfs_sigla)
  write_rds(acess_sf, path_out)
  
  
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
