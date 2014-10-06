library(sp)

library(read.ogr)

## source('/read.ogr.R',chdir=TRUE)

## load the wim and vds data
source('./fetch.vds.R')

source('./fetch.wim.R')


limit <- 16 ## 500 km is too long...

## invoke group sites with plyr
#library(parallel)
#makeForkCluster(nnodes=3)

ddply(as.data.frame(vds.df),.(freeway_id,freeway_dir),groupsites,limit,wim.df,.progress = "text",.parallel=TRUE)

## that didn't work out well!

freeways <- unique(vds.df@data[,c('freeway_id','freeway_dir')])

tests.df <-  groupsites(as.data.frame(sample.data.405),16,wim.df)

tests.df <- groupsitesr::groupsites(as.data.frame(vds.df)[vds.df@data$freeway_id==freeways[2,1] &  vds.df@data$freeway_dir==freeways[2,2],]
                                   ,limit
                                   ,wim.df)

tests.df <- groupsitesr::groupsites(as.data.frame(vds.df)[vds.df@data$freeway_id==freeways[3,1] & vds.df@data$freeway_dir==freeways[3,2],]
          ,limit,wim.df)

tests.df <- groupsitesr::groupsites(as.data.frame(vds.df)[vds.df@data$freeway_id==freeways[4,1] & vds.df@data$freeway_dir==freeways[4,2],]
          ,limit,wim.df)

tests.df <- groupsitesr::groupsites(as.data.frame(vds.df)[vds.df@data$freeway_id==2 & vds.df@data$freeway_dir=='E',]
                                   ,limit
                                   ,wim.df)



## problem with single element lists

groupsites(as.data.frame(vds.df)[vds.df@data$freeway_id==30 & vds.df@data$freeway_dir=='E',]
          ,limit,wim.df)

as.data.frame(vds.df)[vds.df@data$freeway_id==30 & vds.df@data$freeway_dir=='E',]

## problem with single element lists

library(plyr)

grouped.vds.df <- plyr::ddply(as.data.frame(vds.df),.(freeway_id,freeway_dir)
                             ,groupsitesr::groupsites
                             ,limit
                             ,wim.df)


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
