options(java.parameters = '-Xmx10G')
library(r5r)
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(mapview)
library(googlesheets4) # install.packages("googlesheets4")
library(gtfstools) # install.packages('gtfstools')


points <- fread("../../otp/thesis/points/points_09.csv") %>%
  select(id = id_hex, lat = Y, lon = X)

r5r_core <- setup_r5(data_path = sprintf("../../otp/thesis/graphs/%s", "forcorrigidocm"), verbose = FALSE)

# check snap
points_snap <- r5r::find_snap(r5r_core, points = points)
