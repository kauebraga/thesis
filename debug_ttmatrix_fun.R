
# get ttime isocrhone

# id_hex <- c("8980104e0d3ffff", "8980104e46bffff")
# comparison = c("forcorrigidocm", "forcorrigidoce")
id_hex <- "898010451c7ffff"
id_hex <- "8980107b28bffff"
tt <- 60

get_ttime_isocrhone <- function(id_hex, comparison = c("forpadrao", "forcorrigidocm"), tt) {
  
  # open matrixs
  ttmatrix1 <- read_rds(sprintf("../../data/thesis/output_ttmatrix/ttmatrix_%s.rds", comparison[1])) %>%
    filter(fromId == id_hex, travel_time <= tt) %>% mutate(tipo = comparison[1])
  ttmatrix2 <- read_rds(sprintf("../../data/thesis/output_ttmatrix/ttmatrix_%s.rds", comparison[2])) %>%
    filter(fromId == id_hex, travel_time <= tt) %>% mutate(tipo = comparison[2])
  
  # ttmatrix1 <- left_join(ttmatrix1, hex, by = c("toId" = "id_hex")) %>% st_sf()
  # ttmatrix1 <- ttmatrix1 %>% filter(!is.na(routes))
  # mapview(ttmatrix1, zcol = "routes")
  # extract ttmatrix from desired hex
  # bring hex
  hex <- read_rds(sprintf("../../data/dissertacao/hex_agregados/hex_agregados_09.rds")) %>%
    select(id_hex)
  
  ttmatrix_sf <- rbind(left_join(ttmatrix1 %>% select(fromId, toId, travel_time, tipo), hex, by = c("toId" = "id_hex")),
                       left_join(ttmatrix2 %>% select(fromId, toId, travel_time, tipo), hex, by = c("toId" = "id_hex"))) %>%
    st_sf()
  
  mapview(ttmatrix_sf, zcol = "tipo")
  
  
}

# group these matrix
ttmatrix_sf1 <- ttmatrix_sf %>%
  group_by(tipo) %>% summarise()

ttmatrix_sf2 <- ttmatrix_sf %>%
  group_by(tipo) %>% summarise()

icons <- awesomeIcons(
  icon = "bus",
  library = "fa",
  # iconrColor = 'black',
  iconRotate = 10)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = limits, fillOpacity = 0, color = "black", weight = 2) %>%
  addAwesomeMarkers(data = hex %>% filter(id_hex == "898010451c7ffff") %>% st_centroid(),
                    icon = icons) %>% 
  addPolygons(data = ttmatrix_sf1 %>% filter(tipo == "forpadrao"), 
              color = "blue", stroke = FALSE, fillOpacity = 0.4) %>%
  addPolygons(data = ttmatrix_sf1 %>% filter(tipo == "forcorrigidocm"), 
              color = "green", stroke = FALSE, fillOpacity = 0.5)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = limits, fillOpacity = 0, color = "black", weight = 2) %>%
  addAwesomeMarkers(data = hex %>% filter(id_hex == "8980107b28bffff") %>% st_centroid(),
                    icon = icons) %>% 
  addPolygons(data = ttmatrix_sf2 %>% filter(tipo == "forpadrao"), 
              color = "blue", stroke = FALSE, fillOpacity = 0.4) %>%
  addPolygons(data = ttmatrix_sf2 %>% filter(tipo == "forcorrigidocm"), 
              color = "green", stroke = FALSE, fillOpacity = 0.5)

