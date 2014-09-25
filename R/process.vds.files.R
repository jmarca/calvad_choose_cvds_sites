
wim.km <- spDists(wim.df,longlat=TRUE)

vds.km <- spDists(vds.df,longlat=TRUE)

wim.vds.km <- spDists(wim.df,vds.df,longlat=TRUE)

dim(vds.df)

limit <- 16 ## 500 km is too long
km.limited <- km

close.idx <- km.limited<limit
km.limited[!close.idx] <- limit+1

## first assign locations to the WIM stations



covered.idx <- rep(FALSE,length(ranking))
covering.set <- c()
covered <-list()
## start the loop here
while( sum(!covered.idx) > 0 ){
    if(sum(!covered.idx)==1){
        convert.site <- which.min(covered.idx)
    }else{
        ranking <- rowSums(close.idx[,!covered.idx])
        convert.site <- which.max(ranking)
    }
    new.sites.idx <- close.idx[convert.site,]
    ## drop ones already covered
    new.sites.idx[covered.idx] <- FALSE
    ##    covered[[length(covering.set)+1]] <- list(cvd.idx=new.sites.idx,cvd.site=convert.site)
    df[new.sites.idx,'group'] <- df@data$id[convert.site]
    covered.idx <- covered.idx | close.idx[convert.site,]
    covering.set <- c(covering.set,convert.site)
}

covering.set

df[covering.set,'group'] <- 'source'


##df@data$rid = rownames(df@data)
##vds.points <- fortify(df,region="rid")
##vds.df <- join(vds.points,df@data,by="rid")


sjvmap <- ggplot2::ggplot(df@data, aes(x=longitude, y=latitude, group=freeway_id)) +
    geom_point()+ coord_equal()

sjvmap

library(OpenStreetMap)

upperleft <-  c(max(df@data$latitude)+0.5,min(df@data$longitude)-0.1)
lowerright <- c(min(df@data$latitude)-0.5,max(df@data$longitude)+0.1)

#mp <- OpenStreetMap::openmap(upperleft,lowerright,7, 'stamen-watercolor' )
#mp <- OpenStreetMap::openmap(upperleft,lowerright,7, 'stamen-toner' )
## 11 crashies RAM
mpt <- OpenStreetMap::openmap(upperleft,lowerright,9, 'stamen-terrain' )

pdf("map_sites.pdf", width = 8, height = 10)

map_longlat <- openproj(mpt)

b <- ggplot2::autoplot(map_longlat)

b <- b +
    geom_point(data=df@data, aes(x=longitude, y=latitude, color=factor(group)))

b <- b +
    geom_point(data=df@data[df@data$group=='source',], aes(x=longitude, y=latitude, size=2))

b <- b + coord_equal()

b

dev.off()
