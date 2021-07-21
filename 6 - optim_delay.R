
df <- read.csv("all_data.csv")

df$date <- as.Date(df$date)
#a bunch of commands to get the lag via parameters and remaking the dataframe with appropriate lags
delayed_df <- df[,c(1:3, 8, 15:20)]
delayed_df <- subset(delayed_df, delayed_df$data != 15)
delayed_df <- subset(delayed_df, delayed_df$data != 4.11)
#outliers
delayed_df <- subset(delayed_df, delayed_df$site != 'TMH')


x<-function(par)
{
  delayed_df$date <- as.Date(delayed_df$date)
  delay_days <- round(par[1]*exp(par[2]*delayed_df$min_dist)) #delay in days per distance
  effective_date <- delayed_df$date - delay_days
  effective_date[which(effective_date<="2003-01-22")]= NA #removing dates before the beginning of salinity data collection
  
  for (ii in 1:length(effective_date)){
    if (!is.na(effective_date[ii])){
      effective_date[ii] <- as.character(delayed_df$date[which.min(abs(effective_date[ii] - delayed_df$date))])
    }
  }
  
  for (i in unique(delayed_df$site)){
    delayed_df$main_traffic <-  delayed_df$min_dist_traffic[match(effective_date, delayed_df$date)]
    delayed_df$neo_traffic <- delayed_df$min_dist_neo_traffic[match(effective_date, delayed_df$date)]
  }
  
  
  delayed_df[delayed_df == 0] <- NA
  delayed_df <- delayed_df[complete.cases(delayed_df), ]
  
  
  return(summary(lm(data ~ Gatun_flow_dist + Cano_Quebrado_flow_dist + Trinidad_flow_dist + Ciri_Grande_flow_dist + main_traffic + neo_traffic, data = delayed_df, na.action=na.exclude))$r.squared)
}


x(par = c(31.9594, 0.0001))


o <- optim(par = c(31.9594, 0.0001),x)

