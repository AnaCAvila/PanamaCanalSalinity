#Running the Panama Canal  diffusion model on an even grid, with a constant probability of movement.
#Ana Avila, May 2021 (ana.avila@mail.mcgill.ca)

##########################
##  making raster image ############################################################
##########################

library(rgdal)
library(ggplot2)
library(raster)


Panama_Watersheds <- readOGR(dsn=".",layer="Panama_Cuencas___Watersheds")
plot(Panama_Watersheds)

Canal = subset(Panama_Watersheds, WSD_ID=="115")
plot(Canal)

# visualizing just the canal ##########

Canal_fortify <- fortify(Canal)

ggplot(Canal_fortify)+geom_polygon(aes(long,lat,group=group),colour="grey90")+ 
  coord_cartesian(xlim = c(-80.1, -79.52), ylim = c(8.955,9.35))

# rasterizing #########
# at some point, the rasterizing process worked. Now, it's taking too long to run.


writeOGR(Canal, ".", "Canal", 
         driver = "ESRI Shapefile")

sln_Canal <- as(Canal, "SpatialLinesDataFrame")
plot(sln_Canal)

## raster template
rst_template <- raster(ncols = 100, nrows = 100, 
                       crs = projection(sln_Canal), 
                       ext = extent(sln_Canal))

## rasterize
rst_Canal <- rasterize(sln_Canal, rst_template)
#plot(rst_Canal, col = "grey75", legend = FALSE, xlab = "lon", ylab = "lat")
#plot(rasterToPolygons(rst_Canal), add = TRUE)


################## using screenshot from fortify as a jpg reference


Canal_raster <- raster("canal.jpg", layer=0, xmn=0, xmx=700, ymn=0, ymx=500)
NAvalue(Canal_raster) <- 50
plot(Canal_raster)

fun <- function(x) { x[x<300] <- 0; return(x) } #set all non na values to zero
Canal_raster <- calc(Canal_raster, fun)
plot(Canal_raster)

canal <- as.matrix(Canal_raster)

list <- which(is.na(canal))

canal <- matrix(1:length(canal), nrow = nrow(canal), ncol = ncol(canal))


for (index in list){
  canal[index] <- NA
}

######################################################################


#mat <- matrix(1:25, 5, 5)

m2<-cbind(NA,rbind(NA,canal,NA),NA)
addresses <- expand.grid(x = 1:ncol(m2), y = 1:nrow(m2))

ret<-c()
for(i in 1:-1)
  for(j in 1:-1)
    if(i!=0 || j !=0)
      ret<-rbind(ret,m2[addresses$x+i+1+nrow(m2)*(addresses$y+j)]) 

write.csv(ret, file='neighbor.csv')
#first do it in one dimension, and then in three

neighbor <- ret

#########################
## PROBABILITY MATRIX #####################################################
########################


prob_trans <- data.frame(matrix(NA, nrow = nrow(neighbor), ncol = ncol(neighbor)))
prob_trans[,] <- 0.3



####################################
## SALINITY MATRIX AND ITERATIONS ################################################
####################################


t <- 3
salt <- array(0L, dim=c(nrow(canal),ncol(canal),t))

for (value in 1:t){
  salt[,,value]<-canal
  salt[,,value][9:10] <- 3 #coordinates of the locks
  salt[,,value][40:41] <- 3 #coordinates of the locks
}
salt



for (value in 2:t-1){ #remember R begins with zero
  
  #adding salinities to neighbor matrix (diffusion):
  for (source in 1:ncol(neighbor)){
    
    indexes <- c()
    counter <- 0
    
    for(destination in neighbor[,source]){
      if(!is.na(destination)){
        if (which(neighbor[,source] == destination) %notin% indexes){
          indexes <- which(neighbor[,source] == destination)
        }
      }
    }
    
    indexes <- as.numeric(indexes)
    
    for(destination in neighbor[,source]){
      if(!is.na(destination)){
        counter <- counter + 1
        diffused <- (salt[,,value][source]*(0.3))/length(indexes)
        neighbor[indexes[counter],source] <- neighbor[indexes[counter],source] + diffused
      }
    }
    
    
    received <- rowSums(neighbor, na.rm = TRUE)
    print(received)
    for (i in 1:length(received)){
      salt[,,value+1][i] <- salt[,,value][i]*(1-0.3) + received[i]
      salt[,,value+1][9:10] <- 3
      salt[,,value+1][40:41] <- 3 #coordinates of the locks (constant)
    }
  }
  neighbor <- neighbor*0
}


salt









