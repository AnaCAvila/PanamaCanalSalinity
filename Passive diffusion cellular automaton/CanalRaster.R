#Making Panama Canal Raster
#Ana Avila, May 15th 2021 (ana.avila@mail.mcgill.ca)




for(destination in neighbor[,10]){
  print(destination)
  #print(as.integer(prob_trans[destination,10]))
}

for(value in prob_trans[,10]){
  print(value)
}


print(prob_trans[9,10])




setwd("/Users/anavitorino/Desktop/Salinity")
library(rgdal)
library(ggplot2)
library(raster)


Panama_Watersheds <- readOGR(dsn=".",layer="Panama_Cuencas___Watersheds")
plot(Panama_Watersheds)

Canal = subset(Panama_Watersheds, WSD_ID=="115")
plot(Canal)

#visualizing just the canal ##########
Canal_fortify <- fortify(Canal)

ggplot(Canal_fortify)+geom_polygon(aes(long,lat,group=group),colour="grey90")+ 
  coord_cartesian(xlim = c(-80.1, -79.52), ylim = c(8.955,9.35))
#########################

writeOGR(Canal, ".", "Canal", 
         driver = "ESRI Shapefile")

sln_Canal <- as(Canal, "SpatialLinesDataFrame")
plot(sln_Canal)

## raster template
rst_template <- raster(ncols = 100, nrows = 100, 
                       crs = projection(sln_Canal), 
                       ext = extent(sln_Canal))


## rasterize
rst_Canal <- rasterize(sln_Canal, rst_template) #this is taking too long to run
#plot(rst_Canal, col = "grey75", legend = FALSE, xlab = "lon", ylab = "lat")
#plot(rasterToPolygons(rst_Canal), add = TRUE)



#############################################



Canal_raster <- raster("canal2.jpg", layer=0, xmn=0, xmx=700, ymn=0, ymx=500)

plot(Canal_raster)

NAvalue(Canal_raster) <- 50
plot(Canal_raster)


#471 rows
#727 columns
fun <- function(x) { x[x<300] <- 0; return(x) }
Canal_raster <- calc(Canal_raster, fun)


plot(Canal_raster)

canal <- as.matrix(Canal_raster)


points(207,355)
points(590,80)
points(505,180) #Alajuela



#original neighbor matrix
#cell to xy
#xytocell raster function

#ptm - prob of receiving - 9 (prob of staying) so probabilities sum to 100%
#normalize so that the sum of each row is 1

#1-30k - how much salinity there are in each location


#cell content value - other things will move into it.

#think about how to do one fitted kernel for water from cell one

#null models and building from that


#think of this question on your own as well, be critical!! be proactive
#conceptual challenge
  #water input
  #depth


#model water velocity. Think of it as a vector field.


#at every moment in time, iterate over 
#pseudocode that. what is going to look like
  #what are the moving parts?
  #feedback loop between vector and salinity





