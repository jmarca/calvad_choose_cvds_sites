library(sp)

library(read.ogr)

## source('/read.ogr.R',chdir=TRUE)

## load the wim and vds data
source('./fetch.vds.R')

source('./fetch.wim.R')

source('./groupsites.R')

limit <- 16 ## 500 km is too long...

## invoke group sites with plyr
library(parallel)
makeForkCluster(nnodes=3)
ddply(as.data.frame(vds.df),.(freeway_id,freeway_dir),groupsites,limit,wim.df,.progress = "text",.parallel=TRUE)






library(OpenStreetMap)

upperleft <-  c(max(df@data$latitude)+0.5,min(df@data$longitude)-0.1)
lowerright <- c(min(df@data$latitude)-0.5,max(df@data$longitude)+0.1)


#mp <- OpenStreetMap::openmap(upperleft,lowerright,7, 'stamen-toner' )
## 11 crashies RAM
mpt <- OpenStreetMap::openmap(upperleft,lowerright,9, 'stamen-terrain' )

mpw <- OpenStreetMap::openmap(upperleft,lowerright,9, 'stamen-watercolor' )


map_longlat <- openproj(mpt)

mapw_longlat <- openproj(mpw)


areamap <- ggplot2::autoplot(map_longlat)


c <-  geom_point(data=wim.df@data,
                 aes(x=longitude, y=latitude), colour="grey50", alpha=0.5,size = 2)
d <-
    geom_point(data=wim.df@data,
               aes(x=longitude, y=latitude), fill = "dark grey", colour = "black", alpha = 1/3,size = 3)


cvds <- geom_point(data=vds.df@data[cvds.covering.set,],
                aes(x=longitude, y=latitude),
                fill = "grey", colour = "blue", alpha = 1/3,size = 3)

cwim <- geom_point(data=wim.df@data[wim.covering.set,],
                aes(x=longitude, y=latitude),
                fill = "grey", colour = "red", alpha = 1/3,size = 3)


vdsassignedtowim <- geom_point(data=vds.df@data[wim.covered.idx,], aes(x=longitude, y=latitude, color=factor(group)))


vdsassigned <- geom_point(data=vds.df@data, aes(x=longitude, y=latitude, color=factor(group)))



vdsonly <- geom_point(data=vds.df@data, aes(x=longitude, y=latitude))


png("map_sites.", width = 8, height = 10)

areamap

areamap + c + d

areamap + vdsonly

areamap + c + d + vdsonly

areamap+c+d+vdsonly+cwim+vdsassignedtowim

areamap+c+d+vdsonly+cwim+vdsassigned

areamap + c + d + cwim


dev.off()
