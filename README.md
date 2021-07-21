# PanamaCanalSalinity

1 - Data Analysis.R
Input: ACP_salinity.csv
	stations.csv
Output:
	Shallow
	

-	Opens and cleans salinity and stations data
-	Separates between Shallow, Deep and NA depth dataframes
o	Trends look similar for Shallow (surface) and Deep (bottom) water measurements, although average salinity for deep measurements are higher.
-	Plots the trends for each station in separate PDF files
-	Plots the trends for delta salinity (salinity(month) – salinity(month-1))


2 – Data Analysis.R
Input:
	freshwater.RDS
	monthly_traffic.rds
	monthly_traffic_2018.csv
Output:
	Traffic.csv
	Delay.csv
	
-	Makes distances dataframe
o	From the coordinates of each station and each river/lock
	Cano Quebrado, Gatun, Ciri Grande, Trinidad, Atlantic and Pacific
	Makes min_dist -> minimum distance to a lock, be Atlantic or Pacific (by selecting the smallest of the two distances)
-	Checks avg salinity levels before and after neopanamax (just for curiosity)
o	Average before neopanamax
o	Average after neopanamax
o	Average while selecting only stations that have had data collected before and after the opening of neopanamax (some stations only have recent data)
o	Average while only selecting stations that have coordinates (some of them don’t)
-	Subsets distances dataframe between Lake, Rivers and Alajuela stations, so that only Lake stations are used to build the lake model.
-	Makes traffic dataframe
o	Uses monthly_traffic.RDS (came from Dat, it’s old data) and monthly_traffic_2018.csv (came from ACP’s website, it’s from 2018 to 2020).
o	Extrapolates neo information for the 2018 year (first 12 months), since it wasn’t given, but we know it was on average 15% of total traffic.
o	Possible sources of error are a potential incompleteness of ACP data – I’m unsure if they’ve included all small traffic, which may not have been economically significant for them.
-	Estimates delay – makes delay dataframe
o	Sees how long in days did each station take to show a positive delta in salinity after neopanamax’s opening date.
-	Plots El Niño and freshwater flow data


2 - Data analysis (only Deep/Shallow data).R
-	All the same as 2 - Data analysis.R, but selecting only deep/shallow subset.

Freshwater.R
Input:
	freshwater.RDS
Output:
	Freshwater_monthly.csv

Selects and plots monthly freshwater data for the 4 rivers.



