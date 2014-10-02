
p4s <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

groupsites <- function(dfv,distance=16,priority.sites){

    ## cheating here, but too lazy to do otherwise
    freeway_dir <- dfv$freeway_dir[1]
    freeway_id <-  dfv$freeway_id[1]
    limit.wim.idx <- wim.df@data$direction == freeway_dir &
        wim.df@data$direction == freeway_dir

    ## re-create spatial points data frame objects
    sp::coordinates(dfv) <- c('coords.x1','coords.x2')
    proj4string(dfv) <- p4s
    vds.km <- spDists(dfv,longlat=TRUE)
    wim.covering.set <- c()
    covered.idx <- rep(FALSE,dim(dfv)[1])
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
