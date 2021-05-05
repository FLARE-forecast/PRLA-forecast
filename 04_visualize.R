setwd("/Users/ryanmcclure/Documents/BARC-forecast/")

output <- flare::combine_forecast_observations(saved_file,
                                               config$qaqc_data_location,
                                               extra_historical_days = 10)
focal_depths <- c(0.1,2,4,6)
obs <- output$obs
full_time_local_extended <- output$full_time_local_extended
diagnostic_list <- output$diagnostic_list
state_list <- output$state_list
forecast <- output$forecast    # 0 == not forecast and 1 == forecast
par_list <- output$par_list
obs_list <- output$obs_list
state_names <- output$state_names
par_names <- output$par_names
diagnostics_names <- output$diagnostics_names
full_time_local <- output$full_time_local
obs_long <- output$obs_long
depths <- output$depths

temp <- state_list[["temp"]]

forecast_start <- min(which(forecast == 1))
forecast_end <- length(forecast)

full_time_local_plotting <-seq(max(full_time_local) - lubridate::days(26), max(full_time_local), by = "1 day")
forecast_index <- which(full_time_local_plotting == max(full_time_local) - lubridate::days(26))


if(length(depths) == 14){
  depth_colors <- c("firebrick4",
                    "firebrick3",
                    "firebrick1",
                    "DarkOrange1",
                    "gold",
                    "gold3",
                    "greenyellow",
                    "medium sea green",
                    "sea green",
                    "cyan4",
                    "DeepSkyBlue4",
                    "blue2",
                    "blue4",
                    "dodgerblue4")
}


png_file_name <- paste0('lake_barco_',full_time_local[which.max(forecast == 1)-1],'_forecast.png')
png(png_file_name,width = 6, height = 6, units = 'in', res=300)

plot(full_time_local_plotting,rep(-99,length(full_time_local_plotting)),ylim=c(4,35),xlim = c(full_time_local_plotting[1], max(full_time_local_plotting)), xlab = 'date',ylab = expression(~degree~C))
title(paste0('Lake Barco water temperature forecast starting: ',full_time_local[which.max(forecast == 1)-1]),cex.main=0.9)
tmp_day <- full_time_local[-1][1]
axis(1, at=full_time_local - lubridate::hours(lubridate::hour(full_time_local[1])),las=2, cex.axis=0.7, tck=-0.01,labels=FALSE)

depth_colors_index = 0
for(i in 1:length(depths)){
  if(length(which(depths[i]  == depths)) >= 1 ){
    depth_colors_index <- i
    points(full_time_local_plotting[1:forecast_index], obs[1:forecast_index,i,1],type='l',col=depth_colors[depth_colors_index],lwd=1.5)
    index <- which(focal_depths == depths[i])
    if(length(index) == 1){
      temp_means <- rep(NA, length(full_time_local))
      temp_upper <- rep(NA, length(full_time_local))
      temp_lower <- rep(NA, length(full_time_local))
      for(k in 2:length(temp_means)){
        temp_means[k] <- mean(temp[k,i,])
        temp_lower[k] <- quantile(temp[k,i,], 0.05)
        temp_upper[k] <- quantile(temp[k,i,], 0.95)
      }
      points(full_time_local, temp_means,type='l',lty='dashed',col=depth_colors[depth_colors_index],lwd=2)
      points(full_time_local, temp_upper,type='l',lty='dotted',col=depth_colors[depth_colors_index],lwd=2)
      points(full_time_local, temp_lower,type='l',lty='dotted',col=depth_colors[depth_colors_index],lwd=2)
    }
  }
}

abline(v = full_time_local[which.max(forecast == 1)-1])
text(full_time_local[58],5,'past')
text(full_time_local[62],5,'future')

legend("bottomleft",c("0.1m","2m","4m","6m"),
       text.col=c("firebrick3", "gold3", "cyan4", "dodgerblue4"), cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
legend('topright', c('mean','95% confidence bounds'), lwd=1.5, lty=c('dashed','dotted'),bty='n',cex = 1)

dev.off()
