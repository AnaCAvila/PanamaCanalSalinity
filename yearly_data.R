
df <- read.csv('all_data.csv')

###################################

df <- subset(df, df$data != 15)
df <- subset(df, df$data != 4.11)
#outliers


df <- subset(df, df$site != 'TMH')
df <- df[complete.cases(df), ] #Selected only where we have traffic data.

plot(data ~ min_dist_traffic, data = df, main="All variables", ylab = "salinity")

#adding yearly averages


library(lubridate)
library(dplyr)
annual_traffic <- read.csv('annual_traffic.csv')


#round dates down to year
df$year <- format(as.Date(df$date, format="%d/%m/%Y"),"%Y")

#find mean salinity by month
df <- df %>%
  group_by(year) %>%
  mutate(mean = mean(data))

df <- df %>%
  group_by(year) %>%
  mutate(mean = mean(data))

pred_ocean <- lm(Oceangoing ~ Total, data=annual_traffic)

annual_traffic$Oceangoing[7:9] <- predict(pred_ocean, newdata = data.frame(Total = annual_traffic$Total[7:9]))

df$percentage <- annual_traffic$percentage[match(df$year, annual_traffic$year)]
df$Oceangoing <- annual_traffic$Oceangoing[match(df$year, annual_traffic$year)]

df$percentage <- ifelse(is.na(df$percentage), 0, df$percentage)
df$percentage

df$imin_dist <- df$percentage/df$min_dist
df$iOcean <- df$Oceangoing/df$min_dist




Gatun <- subset(freshwater, freshwater$station == "Gatun")
Ciri_Grande <- subset(freshwater, freshwater$station == "Ciri_Grande")
Trinidad <- subset(freshwater, freshwater$station == "Trinidad")
Cano_Quebrado <- subset(freshwater, freshwater$station == "Cano_Quebrado")

#find mean salinity by month
Gatun <- Gatun %>%
  group_by(year) %>%
  mutate(min = min(q), max = max(q), range = max-min, mean=mean(q))

#find mean salinity by month
Ciri_Grande <- Ciri_Grande %>%
  group_by(year) %>%
  mutate(min = min(q), max = max(q), range = max-min, mean=mean(q))

#find mean salinity by month
Cano_Quebrado <- Cano_Quebrado %>%
  group_by(year) %>%
  mutate(min = min(q), max = max(q), range = max-min, mean=mean(q))

#find mean salinity by month
Trinidad <- Trinidad %>%
  group_by(year) %>%
  mutate(min = min(q), max = max(q), range = max-min, mean=mean(q))

Trinidad <- Trinidad[-c(1, 3:5)]
Trinidad <- unique(Trinidad)

Cano_Quebrado <- Cano_Quebrado[-c(1, 3:5)]
Cano_Quebrado <- unique(Cano_Quebrado)

Ciri_Grande <- Ciri_Grande[-c(1, 3:5)]
Ciri_Grande <- unique(Ciri_Grande)

Gatun <- Gatun[-c(1, 3:5)]
Gatun <- unique(Gatun)


write.csv(df, 'yearly_all_data.csv')


df$Ciri_Grande_max <- Ciri_Grande$max[match(as.numeric(df$year), as.numeric(Ciri_Grande$year))]
df$Trinidad_max <- Trinidad$max[match(as.numeric(df$year), as.numeric(Trinidad$year))]
df$Cano_Quebrado_max <- Cano_Quebrado$max[match(as.numeric(df$year), as.numeric(Cano_Quebrado$year))]
df$Gatun_max <- Gatun$max[match(as.numeric(df$year), as.numeric(Gatun$year))]


df$Ciri_Grande_min <- Ciri_Grande$min[match(as.numeric(df$year), as.numeric(Ciri_Grande$year))]
df$Trinidad_min <- Trinidad$min[match(as.numeric(df$year), as.numeric(Trinidad$year))]
df$Cano_Quebrado_min <- Cano_Quebrado$min[match(as.numeric(df$year), as.numeric(Cano_Quebrado$year))]
df$Gatun_min <- Gatun$min[match(as.numeric(df$year), as.numeric(Gatun$year))]

df$Ciri_Grande_range <- Ciri_Grande$range[match(as.numeric(df$year), as.numeric(Ciri_Grande$year))]
df$Trinidad_range <- Trinidad$range[match(as.numeric(df$year), as.numeric(Trinidad$year))]
df$Cano_Quebrado_range <- Cano_Quebrado$range[match(as.numeric(df$year), as.numeric(Cano_Quebrado$year))]
df$Gatun_range <- Gatun$range[match(as.numeric(df$year), as.numeric(Gatun$year))]

df$Ciri_Grande_mean <- Ciri_Grande$mean[match(as.numeric(df$year), as.numeric(Ciri_Grande$year))]
df$Trinidad_mean <- Trinidad$mean[match(as.numeric(df$year), as.numeric(Trinidad$year))]
df$Cano_Quebrado_mean <- Cano_Quebrado$mean[match(as.numeric(df$year), as.numeric(Cano_Quebrado$year))]
df$Gatun_mean <- Gatun$mean[match(as.numeric(df$year), as.numeric(Gatun$year))]

#what if I just look at el nino as a predictor?


df$nino_mean <- nino$mean[match(as.numeric(df$year), as.numeric(nino$year))]
df$nino_min <- nino$min[match(as.numeric(df$year), as.numeric(nino$year))]
df$nino_max <- nino$max[match(as.numeric(df$year), as.numeric(nino$year))]
df$nino_range <- nino$range[match(as.numeric(df$year), as.numeric(nino$year))]







###########################################







fit2 <- lm(mean ~ Oceangoing/min_dist + percentage/min_dist, data=df)
summary(fit2)

pred2 <- predict(fit2, newdata = data.frame(df))


plot(df$data, pred2, xlab = 'Observed', ylab = 'Predicted')
abline(0, 1, col='red')





fit2 <- lm(mean ~ Oceangoing/min_dist + percentage/min_dist + nino_max, data=df)
summary(fit2)

fit3 <- lm(mean ~ Oceangoing/min_dist + percentage/min_dist + nino_max, data=df)
summary(fit)

#distance doesn't matter? that doesn't make any sense.

pred2 <- predict(fit2, newdata = data.frame(df))


plot(df$data, pred2, xlab = 'Observed', ylab = 'Predicted')
abline(0, 1, col='red')
















