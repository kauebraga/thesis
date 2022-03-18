library(gtfstools)
library(dplyr)
library(data.table)

source("fun/calculate_route_frequency.R")

# open gtfs
gtfs1 <- gtfstools::read_gtfs("../../otp/thesis/graphs/forpadrao/GTFS_fortaleza_20180907.zip")
gtfs2 <- gtfstools::read_gtfs("../../otp/thesis/graphs/forcorrigidocm/gtfs_corrigido_50.zip")

# fix direction_id
gtfs1$trips <- gtfs1$trips[, direction_id := fifelse(stringr::str_sub(trip_id, -1) == "I", 0, 1)] 
gtfs2$trips <- gtfs2$trips[, direction_id := fifelse(stringr::str_sub(trip_id, -1) == "I", 0, 1)] 


gtfs1$calendar

# check frenquency?
frequency1 <- calculate_route_frequency(gtfs1, service_id1 = "U")
frequency2 <- calculate_route_frequency(gtfs2, service_id1 = "U") %>% select(-route_long_name)


# compare
frequencies <- full_join(frequency1, frequency2,
                         by = c("route_id", "direction_id"),
                         suffix = c("_pr", "_cr")) %>%
  mutate(dif = headway_mean_pr - headway_mean_cr)

# headway positivo: maior para o programado - menor a frequencia


boxplot(frequencies$dif)
summary(frequencies$dif)
