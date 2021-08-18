
# get ttime isocrhone

# id_hex <- c("8980104e0d3ffff", "8980104e46bffff")
# comparison = c("forcorrigidocm", "forcorrigidoce")
id_hex <- "8980104199bffff"
tt <- 60

get_ttime_isocrhone <- function(id_hex, comparison = c("forpadrao", "forcorrigidocm"), tt) {
  
  # open matrixs
  ttmatrix1 <- read_rds(sprintf("../../data/thesis/output_ttmatrix/ttmatrix_%s.rds", comparison[1])) %>%
    filter(fromId == id_hex, travel_time <= tt) %>% mutate(tipo = comparison[1])
  ttmatrix2 <- read_rds(sprintf("../../data/thesis/output_ttmatrix/ttmatrix_%s.rds", comparison[2])) %>%
    filter(fromId == id_hex, travel_time <= tt) %>% mutate(tipo = comparison[2])
  
  # extract ttmatrix from desired hex
  # bring hex
  hex <- read_rds(sprintf("../../data/dissertacao/hex_agregados/hex_agregados_09.rds")) %>%
    select(id_hex)
  
  ttmatrix_sf <- rbind(left_join(ttmatrix1, hex, by = c("toId" = "id_hex")),
                       left_join(ttmatrix2, hex, by = c("toId" = "id_hex"))) %>%
    st_sf()
  
  mapview(ttmatrix_sf, zcol = "tipo")
  
  
}



