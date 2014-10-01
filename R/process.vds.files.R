library(sp)

library(read.ogr)

## source('/read.ogr.R',chdir=TRUE)

## load the wim and vds data
source('./fetch.vds.R')

source('./fetch.wim.R')

wim.km <- spDists(wim.df,longlat=TRUE)

vds.km <- spDists(vds.df,longlat=TRUE)

wim.vds.km <- spDists(wim.df,vds.df,longlat=TRUE)

dim(vds.df)



limit <- 16 ## 500 km is too long...

covered <-list()

## first assign locations to the WIM stations
## use the maximum cover first, then move down the list


p4s <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

groupsites <- function(dfv,distance=16){
    ## cheating here, but too lazy to do otherwise
    freeway_dir <- dfv$freeway_dir[1]
    freeway_id <-  dfv$freeway_id[1]
    limit.wim.idx <- wim.df@data$direction == freeway_dir &
        wim.df@data$direction == freeway_dir

    ## re-create spatial points data frame objects
    coordinates(dfv) <- c('coords.x1','coords.x2')
    proj4string(dfv) <- p4s
    vds.km <- spDists(dfv,longlat=TRUE)
    wim.covering.set <- c()
    covered.idx <- rep(FALSE,dim(vds.df)[1])
    if(sum(limit.wim.idx)>0){
        wim.vds.km <- spDists(wim.df[limit.wim.idx,],dfv,longlat=TRUE)
        close.idx <- wim.vds.km < limit

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
            dfv$group[new.sites.idx] <- paste(c('wim',wim.df@data[limit.wim.idx,][convert.site,c('site_no','direction')]),collapse='.')
            covered.idx <- covered.idx | close.idx[convert.site,]
            wim.covering.set <- c(wim.covering.set,convert.site)
        }
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
        dfv[new.sites.idx,'group'] <- dfv$id[convert.site]
        covered.idx <- covered.idx | close.idx[convert.site,]
        cvds.covering.set <- c(cvds.covering.set,convert.site)
    }
    vds.covered.idx <- covered.idx
    vds.covered.idx[wim.covered.idx] <- FALSE

    ## return something useful
    return (as.data.frame(dfv))
}


## test it out

lim.df.vds.idx <- vds.df@data$freeway_dir == 'N' & vds.df@data$freeway_id == 5

limited.vds <- vds.df[lim.df.vds.idx,]

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
