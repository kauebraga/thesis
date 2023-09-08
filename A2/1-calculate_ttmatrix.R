options(java.parameters = '-Xmx28G')
library(r5r)
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(mapview)
library(googlesheets4) # install.packages("googlesheets4")
library(gtfstools) # install.packages('gtfstools')



# for padrao
path_antes <-  "../../otp/thesis/graphs/forpadrao"
r5r::setup_r5(data_path = path_antes, use_elevation = TRUE, overwrite = TRUE)

# for p50
path_depois <- "../../otp/thesis/graphs/forcorrigidocm"
r5r::setup_r5(data_path = path_depois, use_elevation = TRUE, overwrite = TRUE)

# for p85
path_depois <- "../../otp/thesis/graphs/forcorrigidoce"
r5r::setup_r5(data_path = path_depois, use_elevation = TRUE, overwrite = TRUE)





# calculate ttmatrix --------------------------------------------------------------------------

# gtfs_sigla <- "forcorrigidocm"

calcular_ttmatrix <- function(gtfs_sigla) {
  
  points <- fread("../../otp/thesis/points/points_09.csv") %>%
    select(id = id_hex, lat = Y, lon = X)
  
  mode <- c("WALK", "TRANSIT")
  max_walk_dist <- 1500   # meters
  max_trip_duration <- 180 # minutes
  departure_datetime <- as.POSIXct("10-09-2018 06:00:00", format = "%d-%m-%Y %H:%M:%S")
  
  
  # 1 - ttmatrix - antes ----------------------------------------------------
  
  
  
  r5r_core <- setup_r5(data_path = sprintf("../../otp/thesis/graphs/%s", gtfs_sigla), verbose = FALSE)
  
  # point_origin <- points %>% filter(id == "89801045a37ffff")
  r5r_core$setCsvOutput(here::here("../../otp/thesis/teste/"))
  
  # 3.1) calculate a travel time matrix
  ttm1 <- travel_time_matrix(r5r_core = r5r_core,
                             # origins = point_origin,
                             origins = points,
                             destinations = points,
                             mode = mode,
                             departure_datetime = departure_datetime,
                             time_window = 120,
                             max_walk_dist = max_walk_dist,
                             max_trip_duration = max_trip_duration,
                             breakdown = TRUE,
                             verbose = FALSE,
                             progress = TRUE)
  
  # salvar
  readr::write_rds(ttm1, sprintf("../../data/thesis/output_ttmatrix/ttmatrix_%s.rds", gtfs_sigla))
  
}


calcular_ttmatrix("forpadrao")
calcular_ttmatrix("forcorrigidocm")
calcular_ttmatrix("forcorrigidoce")
