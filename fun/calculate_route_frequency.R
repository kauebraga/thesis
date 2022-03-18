calculate_route_frequency <- function(gtfs, route_id1 = NULL, service_id1 = NULL, 
                                      start_time1 = "06:00:00", end_time1 = "08:00:00") {
  
  # identify type of gtfs
  
  type_gtfs <- if (any(grepl(pattern = "frequencies", x = names(gtfs)))) {
    
    if(nrow(gtfs$frequencies) > 0) "frequencies" else "stop_times"
    
  } else "stop_times"
  
  # get trips
  trips <- gtfs$trips
  
  # filter trips by route
  trips <- if (!is.null(route_id1)) trips[route_id %in% route_id1] else trips
  
  # filter trips by service
  trips <- if (!is.null(service_id1)) trips[service_id %in% service_id1] else trips
  
  # get route info
  routes <- gtfs$routes[, .(route_id, route_long_name)]
  routes <- if (!is.null(route_id1)) routes[route_id %in% route_id1] else routes
  
  
  if (type_gtfs == "frequencies") {
    
    frequencies <- gtfs$frequencies[trip_id %in% trips$trip_id]
    
    frequencies[, start_time := as.ITime(start_time)]
    frequencies[, end_time := as.ITime(end_time)]
    
    # filter only peak hours
    frequencies1 <- frequencies[start_time >= as.ITime(start_time1)]
    frequencies1 <- frequencies1[end_time < as.ITime(end_time1)]
    
    # bring route id and direction_id
    frequencies1 <- frequencies1 %>% left_join(trips %>% dplyr::select(trip_id, route_id, direction_id))
    # frequencies1 <- frequencies1 %>% left_join(trips %>% dplyr::select(trip_id, route_id, direction_id, trip_headsign))
    # bring route info
    frequencies1 <- frequencies1 %>% left_join(routes, by = "route_id") %>% setDT()
    
    # calculate mean headways
    headways_pico <- frequencies1[, .(headway_mean = as.integer(mean(headway_secs/60, na.rm = TRUE))),
                                  by = .(route_id, direction_id, route_long_name)]
    
  } else {
    
    # get stoptimes
    stop_times <- gtfs$stop_times[trip_id %in% trips$trip_id]
    
    # bring route_id to stop_times
    stop_times <- merge(stop_times, 
                        trips[, .(trip_id, route_id, direction_id, service_id)], 
                        by = "trip_id",
                        sort = FALSE)
    
    
    stop_times[, arrival_time := as.ITime(arrival_time)]
    
    # get start of each trip
    stop_times_starts <- stop_times[, .(arrival_time = data.table::first(arrival_time),
                                        n_stops = .N,
                                        ttime_trip = (last(arrival_time) - first(arrival_time))/60), 
                                    by = .(service_id, route_id, trip_id, direction_id)]
    
    setorder(stop_times_starts, route_id, direction_id, arrival_time)
    
    stop_times_starts_pico <- stop_times_starts[between(arrival_time, as.ITime(start_time1), as.ITime(end_time1))]
    
    stop_times_starts_pico[, headway := arrival_time - lag(arrival_time),
                           by = .(service_id, route_id, direction_id)]
    
    stop_times_starts_pico[, headway := as.integer(headway) / 60]
    
    # bring route info
    stop_times_starts_pico <- stop_times_starts_pico %>% left_join(routes, by = "route_id") %>% setDT()
    
    # calculate mean headway by direction
    headways_pico <- stop_times_starts_pico[, .(headway_mean = as.integer(mean(headway, na.rm = TRUE))),
                                            by = .(route_id, direction_id, route_long_name)]
    
  }
  
  return(headways_pico)
  
}