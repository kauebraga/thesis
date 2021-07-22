options(java.parameters = '-Xmx10G')
library(r5r)
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(mapview)
library(googlesheets4) # install.packages("googlesheets4")
library(gtfstools) # install.packages('gtfstools')
library(readr)




points <- fread("../../otp/thesis/points/points_09.csv") %>%
  select(id = id_hex, lat = Y, lon = X) %>%
  mutate(op = 1)

r5r_core <- setup_r5(data_path = sprintf("../../otp/thesis/graphs/%s", "forcorrigidocm"), verbose = FALSE)

mode <- c("WALK", "TRANSIT")
max_walk_dist <- 1500   # meters
max_trip_duration <- 180 # minutes
departure_datetime <- as.POSIXct("10-09-2018 06:00:00", format = "%d-%m-%Y %H:%M:%S")


acess_one <- r5r::accessibility(r5r_core,
                                origins = points,
                                destinations = points,
                                opportunities_colname  = "op",
                                mode = mode,
                                departure_datetime = departure_datetime,
                                time_window = 120,
                                max_walk_dist = max_walk_dist,
                                max_trip_duration = max_trip_duration,
                                decay_function = "step",
                                cutoffs = 60
)

# abrir oportunidades com hexagonos
hexagonos_sf <- read_rds("../../data/dissertacao/hex_agregados/hex_agregados_09.rds") %>% select(id_hex) %>%
  ungroup()

acess_one_sf <- left_join(acess_one, hexagonos_sf,
                          by = c("from_id" = "id_hex")) %>%
  st_sf()

mapview(acess_one_sf, zcol = "accessibility")


# pontos problematico
# 8980104e417ffff e 8980104e4abffff: sao vizinhos e tem um acesso muito diferente

acess_one_sf %>%
  filter(from_id %in% c("8980104e417ffff", "8980104e4abffff")) %>%
  mapview(zcol = "accessibility")

# find snap for these guys
points_prob <- points %>% filter(id %in% c("8980104e417ffff", "8980104e4abffff"))
points_snap_prob <- r5r::find_snap(r5r_core, points = points_prob)

points_snap_prob %>%
  st_as_sf(coords = c("snap_lon", "snap_lat"), crs = 4326) %>%
  mapview()


point_downtown <- data.frame(id = "dt", 
                             lon = c(-38.50983893542815),
                             lat = c(-3.731970613021897))

prob_teste <- data.frame(id = "dt",
                         lat = -3.831697452524083, lon = -38.52237465672943)

# find detailed intineraries to downtown from these points
int_dt1 <- detailed_itineraries(r5r_core = r5r_core,
                                origins = points %>% filter(id %in% c("8980104e417ffff")),
                                destinations = point_downtown,
                                mode = mode,
                                departure_datetime = departure_datetime,
                                max_walk_dist = max_walk_dist,
                                max_trip_duration = max_trip_duration)

int_dt2 <- detailed_itineraries(r5r_core = r5r_core,
                                origins = points %>% filter(id %in% c("8980104e4abffff")),
                                destinations = point_downtown,
                                mode = mode,
                                departure_datetime = departure_datetime,
                                max_walk_dist = max_walk_dist,
                                max_trip_duration = max_trip_duration)

int_dt3 <- detailed_itineraries(r5r_core = r5r_core,
                                origins = prob_teste,
                                destinations = point_downtown,
                                mode = mode,
                                departure_datetime = departure_datetime,
                                max_walk_dist = max_walk_dist,
                                max_trip_duration = max_trip_duration)


points_snap_prob %>%
  st_as_sf(coords = c("snap_lon", "snap_lat"), crs = 4326) %>%
  mapview() +
mapview(int_dt1, zcol = "mode") + mapview(int_dt2, zcol = "mode") +
  mapview(int_dt3, zcol = "mode")
