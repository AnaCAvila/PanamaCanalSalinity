#checking nino and freshwater data
library(zoo)

names(nino)[1] <- 'year'

nino$date <- as.yearmon(with(nino, paste(year, MON, sep="-")), "%Y-%m")

nino$Ciri_Grande_flow <- freshwater_monthly$Ciri_Grande[match(as.yearmon(format(nino$date)), as.yearmon(freshwater_monthly$date))]
nino$Trinidad_flow <- freshwater_monthly$Trinidad[match(as.yearmon(format(nino$date)), as.yearmon(freshwater_monthly$date))]
nino$Cano_Quebrado_flow <- freshwater_monthly$Cano_Quebrado[match(as.yearmon(format(nino$date)), as.yearmon(freshwater_monthly$date))]
nino$Gatun_flow <- freshwater_monthly$Gatun[match(as.yearmon(format(nino$date)), as.yearmon(freshwater_monthly$date))]



nino$Ciri_Grande_max <- Ciri_Grande$max[match(as.numeric(nino$year), as.numeric(Ciri_Grande$year))]
nino$Trinidad_max <- Trinidad$max[match(as.numeric(nino$year), as.numeric(Trinidad$year))]
nino$Cano_Quebrado_max <- Cano_Quebrado$max[match(as.numeric(nino$year), as.numeric(Cano_Quebrado$year))]
nino$Gatun_max <- Gatun$max[match(as.numeric(nino$year), as.numeric(Gatun$year))]


nino$Ciri_Grande_min <- Ciri_Grande$min[match(as.numeric(nino$year), as.numeric(Ciri_Grande$year))]
nino$Trinidad_min <- Trinidad$min[match(as.numeric(nino$year), as.numeric(Trinidad$year))]
nino$Cano_Quebrado_min <- Cano_Quebrado$min[match(as.numeric(nino$year), as.numeric(Cano_Quebrado$year))]
nino$Gatun_min <- Gatun$min[match(as.numeric(nino$year), as.numeric(Gatun$year))]

nino$Ciri_Grande_range <- Ciri_Grande$range[match(as.numeric(nino$year), as.numeric(Ciri_Grande$year))]
nino$Trinidad_range <- Trinidad$range[match(as.numeric(nino$year), as.numeric(Trinidad$year))]
nino$Cano_Quebrado_range <- Cano_Quebrado$range[match(as.numeric(nino$year), as.numeric(Cano_Quebrado$year))]
nino$Gatun_range <- Gatun$range[match(as.numeric(nino$year), as.numeric(Gatun$year))]




nino <- nino %>%
  group_by(year) %>%
  mutate(min = min(ANOM.1), max = max(ANOM.1), range = max-min, mean = min(ANOM.1))

plot(nino$Gatun_range, nino$range)


