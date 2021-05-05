config <- yaml::read_yaml(file.path(lake_directory,"data_processing","observation_processing.yml"))

config$data_location <- "./data"
config$qaqc_data_location <- "./qaqc_data"

source(file.path(lake_directory, "data_processing/R/met_qaqc.R"))

if(is.null(config$met_file)){
  met_qaqc(realtime_file = file.path(config$data_location, config$met_raw_obs_fname[1]),
           qaqc_file = file.path(config$data_location, config$met_raw_obs_fname[2]),
           cleaned_met_file_dir = config$qaqc_data_location,
           input_file_tz = "EST",
           local_tzone = config$local_tzone)
}else{
  file.copy(file.path(config$data_location,config$met_file), cleaned_met_file, overwrite = TRUE)
}