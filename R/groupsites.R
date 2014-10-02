
p4s <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

groupsites <- function(dfv,distance=16,priority.sites=NULL){

    ## cheating here, but too lazy to do otherwise
    freeway_dir <- dfv$freeway_dir[1]
    freeway_id <-  dfv$freeway_id[1]

    print(paste('processing: '
               ,freeway_dir
               ,freeway_id
               ,'with'
               ,dim(dfv)[1]
               ,'points'))

    ## re-create spatial points data frame objects
    sp::coordinates(dfv) <- c('coords.x1','coords.x2')
    proj4string(dfv) <- p4s
    vds.km <- spDists(dfv,longlat=TRUE)

    wim.covering.set <- c()
    covered.idx <- rep(FALSE,dim(dfv)[1])

    if(!is.null(priority.sites)){
        using.wim <- TRUE
        limit.wim.idx <- priority.sites@data$direction == freeway_dir &
            priority.sites@data$freeway_id == freeway_id
        if(length(priority.sites@data$direction)==0){
            using.wim <- FALSE
            limit.wim.idx <- priority.sites@data$freeway_dir == freeway_dir &
                priority.sites@data$freeway_id == freeway_id
        }

        if(sum(limit.wim.idx)>0){
            allowed.sites <- priority.sites[limit.wim.idx,]

            wim.vds.km <- spDists(allowed.sites,dfv,longlat=TRUE)
            close.idx <- wim.vds.km < distance

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

                if(using.wim){
                    dfv@data$group[new.sites.idx] <-
                        paste(c('wim',
                                allowed.sites@data[convert.site,
                                                   c('site_no',
                                                     'direction')])
                             ,collapse='.')
                }else{ ## VDS site, not WIM site
                    dfv@data$group[new.sites.idx] <-
                        allowed.sites@data[convert.site,'id']
                }
                covered.idx <- covered.idx | close.idx[convert.site,]
                wim.covering.set <- c(wim.covering.set,convert.site)
            }
        }
    }


    wim.covered.idx <-  covered.idx
    ## reset things
    cvds.covering.set <- c()
    close.idx <- vds.km<distance  ## now looking at vds to vds distances
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
        dfv@data$group[new.sites.idx] <- dfv@data$id[convert.site]
        covered.idx <- covered.idx | close.idx[convert.site,]
        cvds.covering.set <- c(cvds.covering.set,convert.site)
    }

    vds.covered.idx <- covered.idx
    vds.covered.idx[wim.covered.idx] <- FALSE
    dfv@data$group <- as.factor(dfv@data$group)
    ## return something useful
    return (as.data.frame(dfv))
}
