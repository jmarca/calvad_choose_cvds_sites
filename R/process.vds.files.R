library(sp)

wim.km <- spDists(wim.df,longlat=TRUE)

vds.km <- spDists(vds.df,longlat=TRUE)

wim.vds.km <- spDists(wim.df,vds.df,longlat=TRUE)

dim(vds.df)

limit <- 16 ## 500 km is too long



covered <-list()

## first assign locations to the WIM stations
## use the maximum cover first, then move down the list

wim.covering.set <- c()
close.idx <- wim.vds.km<limit
covered.idx <- rep(FALSE,dim(vds.df)[1])
while( sum(close.idx[,!covered.idx]) > 0  ){
    if(sum(!covered.idx)==1){
        convert.site <- which.min(covered.idx)
    }else{
        ranking <- rowSums(close.idx[,!covered.idx])
        convert.site <- which.max(ranking)
    }
    new.sites.idx <- close.idx[convert.site,]
    ## drop ones already covered
    new.sites.idx[covered.idx] <- FALSE
    vds.df@data$group[new.sites.idx] <- paste(c('wim',wim.df@data[convert.site,c('site_no','direction')]),collapse='.')
    covered.idx <- covered.idx | close.idx[convert.site,]
    wim.covering.set <- c(wim.covering.set,convert.site)
}

wim.covered.idx <-  covered.idx


## reset things
cvds.covering.set <- c()
close.idx <- vds.km<limit  ## now looking at vds to vds distances
## keep covered.idx the same to avoid double counting

while(
    sum(close.idx[,!covered.idx])
    > 0
){
    if(sum(!covered.idx)==1){
        convert.site <- which.min(covered.idx)
    }else{
        ranking <- rowSums(close.idx[,!covered.idx])
        convert.site <- which.max(ranking)
    }
    new.sites.idx <- close.idx[convert.site,]
    ## drop ones already covered
    new.sites.idx[covered.idx] <- FALSE
    vds.df[new.sites.idx,'group'] <- vds.df@data$id[convert.site]
    covered.idx <- covered.idx | close.idx[convert.site,]
    cvds.covering.set <- c(cvds.covering.set,convert.site)
}
vds.covered.idx <- covered.idx
vds.covered.idx[wim.covered.idx] <- FALSE

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


pdf("map_sites.pdf", width = 8, height = 10)

areamap

areamap + c + d

areamap + vdsonly

areamap + c + d + vdsonly

areamap+c+d+vdsonly+cwim+vdsassignedtowim

areamap+c+d+vdsonly+cwim+vdsassigned

areamap + c + d + cwim


dev.off()
