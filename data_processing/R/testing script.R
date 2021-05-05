create_obs_matrix_test <- function (cleaned_observations_file_long, obs_config, start_datetime_local, 
          end_datetime_local, local_tzone, modeled_depths) 
{
  full_time_local <- seq(start_datetime_local, end_datetime_local, 
                         by = "1 day")
  d <- readr::read_csv(cleaned_observations_file_long, col_types = readr::cols(date = col_date(format = ""),
                                                                                       hour = col_double(),
                                                                                       depth = col_double(),
                                                                                       value = col_double(),
                                                                                       variable = col_character(),
                                                                                       method = col_character()))
  obs_list <- list()
  for (i in 1:length(obs_config$state_names_obs)) {
    message("Extracting ", obs_config$target_variable[i])
    obs_tmp <- array(NA, dim = c(length(full_time_local), 
                                 length(modeled_depths)))
    for (k in 1:length(full_time_local)) {
      for (j in 1:length(modeled_depths)) {
        d1 <- d %>% dplyr::filter(variable == obs_config$target_variable[i])
        if (nrow(d1) == 0) {
          warning("No observations for ", obs_config$target_variable[i])
        }
        d1 <- d1 %>% dplyr::filter(date == lubridate::as_date(full_time_local[k]))
        if (nrow(d1) == 0) {
          warning("No observations for ", obs_config$target_variable[i], 
                  " on ", lubridate::as_date(full_time_local[k]))
        }
        d1 <- d1 %>% dplyr::filter((is.na(hour) | hour ==
                                      lubridate::hour(full_time_local[33])))
        if (nrow(d1) == 0) {
          warning("No observations for ", obs_config$target_variable[i],
                  " on ", lubridate::as_date(full_time_local[k]),
                  " at ", lubridate::hour(full_time_local[k]),
                  ":00:00")
        }
        d1 <- d1 %>% dplyr::filter(abs(depth - modeled_depths[j]) < 
                                     obs_config$distance_threshold[i])
        if (nrow(d1) == 0) {
        }
        if (nrow(d1) >= 1) {
          if (nrow(d1) > 1) {
            warning("There are multiple observations for ", 
                    obs_config$target_variable[i], "\nUsing the first observation")
          }
          obs_tmp[k, j] <- d1$value[1]
        }
      }
    }
    if (sum(is.na(obs_tmp)) == (dim(obs_tmp)[1] * dim(obs_tmp)[2])) {
      warning("All values are NA for ", obs_config$target_variable[i])
    }
    obs_list[[i]] <- obs_tmp
  }
  obs <- array(NA, dim = c(length(obs_config$state_names_obs), 
                           length(full_time_local), length(modeled_depths)))
  for (i in 1:nrow(obs_config)) {
    obs[i, , ] <- obs_list[[i]]
  }
  return(obs)
}
