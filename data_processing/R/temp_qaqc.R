temp_qaqc <- function(realtime_file,
                      surface_sonde,
                      profiles,
                      qaqc_file,
                      maintenance_file,
                      input_file_tz,
                      focal_depths,
                      local_tzone,
                      config){
  
  d <- read_csv(realtime_file)
  d_therm <- d %>% dplyr::rename(timestamp = timestamp,
                                 depth = depth,
                                 value = value) %>%
    dplyr::mutate(variable = "temperature",
                  method = "thermistor",
                  hour = lubridate::hour(timestamp),
                  timestamp = as.Date(timestamp))%>%
    dplyr::rename(date = timestamp)
  
  
  d_prof <- readr::read_csv(profiles)%>%
    dplyr::rename(date = timestamp)%>%
    dplyr::mutate(date = as.Date(date),
           value = ifelse(value <= -0.1, 0.2, value))
  
  d <- d_therm %>% dplyr::mutate(depth = as.numeric(depth))
  d_prof <- d_prof %>% dplyr::mutate(depth = as.numeric(depth))
  
  d <- dplyr::bind_rows(d, d_prof)
  
  readr::write_csv(d, paste0(config$qaqc_data_location,"/observations_postQAQC_long.csv"))
}