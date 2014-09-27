source('../components/jmarca-rstats_couch_utils/couchUtils.R')
library(testthat)

get.vds.wim.pairs <- function(year,trackingdb='vdsdata%2ftracking'){
  docs <- couch.allDocs(trackingdb
                        , query=list(
                            'startkey'=paste('%5b%22',year,'%22','%5d',sep='')
                            ,'endkey' =paste('%5b%22',year+1,'%22','%5d',sep='')
                            ,'reduce'='false')
                        , view='_design/vds/_view/pairRData'
                        , include.docs = FALSE)
  rows <- docs$rows
  records <- sapply(rows,function(r){
      ## parse out wim info
      x = r$key[[3]]
      m <- regexec("^wim\\.([0-9]+)\\.([NSEW])",x)
      wim.info <- regmatches(x,m)[[1]]
      return (list('year'=as.numeric(r$key[[1]]),
                   'vds_id'=as.numeric(r$key[[2]]),
                   'doc'=r$key[[3]],
                   'wim_id'=as.integer(wim.info[2]),
                   'direction'=wim.info[3]
                   ))
  })
  if(length(records)==0){
      return(data.frame())
  }
  ## convert to a dataframe
  df.pairs <- data.frame(year=unlist(records[1,])
                        ,vds_id=unlist(records[2,])
                        ,wim_id=unlist(records[4,])
                        ,direction=unlist(records[5,])
                        ,doc=unlist(records[3,])
                        ,stringsAsFactors=FALSE)
  df.pairs
}


wim.query <- "select w.* from wim_geoview w join carb_airdistricts_aligned_03 c on (ST_Contains(c.geom4326, w.geom)) where c.name ~* 'joaquin' and  wim_type ~* 'data'"

wim.df <- read.ogr::readOgrSQL(wim.query)


expect_that(dim(wim.df),equals(c(28,9)))
