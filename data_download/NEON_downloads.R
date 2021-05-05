# Function to extract and clean up the NEON data from NEON
download_neon_files <- function(siteID_neon, siteID, ECtower, products, buoy_products){

        # Download newest products
        neonstore::neon_download(product = products, site = siteID_neon)
        
        # Store the NEON met data products
        neonstore::neon_store("SECPRE_30min-basic")
        neonstore::neon_store("2DWSD_30min-basic")
        neonstore::neon_store("SLRNR_30min-basic")
        neonstore::neon_store("SAAT_30min-basic")
        neonstore::neon_store("RH_30min-basic")
        neonstore::neon_store("BP_30min-basic")

        
        # Tidy up the met data
        # Airtemp
        airtemp <- neonstore::neon_table(table = "SAAT_30min-basic", site = siteID) %>%
          select(endDateTime, tempSingleMean)%>%
          mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
          select(-endDateTime)%>%
          group_by(time) %>%
          summarize_at(c("tempSingleMean"), mean, na.rm = TRUE)%>%
          arrange(time)%>%
          mutate(time = time - 6*3600)
        
        # Radiation
        radiation <- neonstore::neon_table(table = "SLRNR_30min-basic", site = siteID) %>%
          select(endDateTime, inSWMean, inLWMean) %>%
          mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
          select(-endDateTime)%>%
          group_by(time) %>%
          summarize_at(c("inSWMean", "inLWMean"), mean, na.rm = TRUE)%>%
          arrange(time)%>%
          mutate(time = time - 6*3600)
        
        # Humidity
        humidity <- neonstore::neon_table(table = "RH_30min-basic", site = siteID) %>% 
          select(endDateTime, RHMean)%>%
          mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
          select(-endDateTime)%>%
          group_by(time) %>%
          summarize_at(c("RHMean"), mean, na.rm = TRUE)%>%
          arrange(time)%>%
          mutate(time = time - 6*3600)
        
        # Precipitation
        precip  <- neonstore::neon_table(table = "SECPRE_30min-basic", site = ECtower) %>%
          select(endDateTime, secPrecipBulk) %>%
          mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
          select(-endDateTime)%>%
          group_by(time) %>%
          summarize_at(c("secPrecipBulk"), sum, na.rm = TRUE)%>%
          arrange(time)%>%
          mutate(time = time - 6*3600)
        
        # Wind Speed
        windspeed <- neonstore::neon_table(table = "2DWSD_30min-basic", site = siteID)%>%  
          select(endDateTime, windSpeedMean)%>%
          mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
          select(-endDateTime)%>%
          group_by(time) %>%
          summarize_at(c("windSpeedMean"), sum, na.rm = TRUE)%>%
          arrange(time)%>%
          mutate(time = time - 6*3600)
        
        # Pressure
        pressure <- neonstore::neon_table(table = "BP_30min-basic", site = siteID) %>%
          select(endDateTime, staPresMean)%>%
          mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
          select(-endDateTime)%>%
          group_by(time) %>%
          summarize_at(c("staPresMean"), mean, na.rm = TRUE)%>%
          arrange(time)%>%
          mutate(time = time - 6*3600)
        
        
        met_target <- full_join(radiation, airtemp, by = "time")%>%
          full_join(., humidity, by = "time")%>%
          full_join(., windspeed, by = "time")%>%
          full_join(., precip, by = "time")%>%
          full_join(., pressure, by = "time")%>%
          rename(ShortWave = inSWMean, LongWave = inLWMean, AirTemp = tempSingleMean,
                 RelHum = RHMean, WindSpeed = windSpeedMean, Rain = secPrecipBulk, Pressure = staPresMean)%>%
          mutate(Rain = Rain/3600)%>% # convert from mm/hr to m/d 
          mutate(Pressure = Pressure*1000)%>%
          mutate(ShortWave = ifelse(ShortWave<=0,0,ShortWave))%>%
          filter(time >= "2018-08-06")
        
        met_target <- as.data.frame(met_target)
        write_csv(met_target, "./data/met_data_w_gaps.csv")
        
        
        # Download newest products
        neonstore::neon_download(product = buoy_products, site = siteID)
        
        # Store the NEON buoy data products
        neonstore::neon_store("TSD_30_min-basic")
        neonstore::neon_store("dep_secchi-basic")
        neonstore::neon_store("dep_profileData-basic")
        
        # Water temperature by depth
        # ----------------------------------------------------------------------------------------
        water_temp <- neonstore::neon_table(table = "TSD_30_min-basic", site = siteID)%>% 
          select(endDateTime, thermistorDepth, tsdWaterTempMean) %>%
          arrange(endDateTime, thermistorDepth)%>%
          rename(depth = thermistorDepth)%>%
          rename(value = tsdWaterTempMean)%>%
          rename(timestamp = endDateTime)%>%
          mutate(variable = "temperature",
                 hour = lubridate::hour(timestamp- 6*3600),
                 method = "thermistor",
                 value = ifelse(is.nan(value), NA, value))%>%
          select(timestamp, hour, depth, value, variable, method)%>%
                mutate(timestamp = as.Date(timestamp))%>%
                rename(date = timestamp)
        
        temp_profiles <- neonstore::neon_table(table = "dep_profileData-basic", site = siteID)%>%
                select(date, sampleDepth, waterTemp) %>%
                arrange(date, sampleDepth)%>%
                rename(depth = sampleDepth)%>%
                rename(value = waterTemp)%>%
                rename(timestamp = date)%>%
                mutate(variable = "temperature",
                       hour = NA,
                       method = "profile",
                       value = ifelse(is.nan(value), NA, value))%>%
                select(timestamp, hour, depth, value, variable, method)%>%
                mutate(timestamp = as.Date(timestamp))%>%
                rename(date = timestamp)

        d <- dplyr::bind_rows(water_temp,temp_profiles)
        d$value <- as.numeric(d$value)
        write_csv(d, "./qaqc_data/observations_postQAQC_long.csv", col_types = readr::cols(value = col_double()))

        secchi <- neonstore::neon_table(table = "dep_secchi-basic", site = siteID) %>%
                select(date, secchiMeanDepth) %>%
                arrange(date)%>%
                mutate(hour = lubridate::hour(date- 6*3600))%>%
                mutate(depth = NA)%>%
                rename(timestamp = date)%>%
                rename(value = secchiMeanDepth)%>%
                mutate(variable = "secchi",
                       method = "secchi")%>%
                select(timestamp, hour, depth, value, variable, method)%>%
                mutate(timestamp = timestamp - 6*3600)
        
        kw <- secchi %>% select(value) %>% na.omit(.)%>%
                summarize(kw = 1.7/mean(value))
        
        
        
        write_csv(secchi, "./data/secchi_data.csv")

}