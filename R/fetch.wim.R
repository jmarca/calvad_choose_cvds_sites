source('../components/jmarca-rstats_couch_utils/couchUtils.R')
library(testthat)


wim.query <- "select w.* from wim_geoview w join carb_airdistricts_aligned_03 c on (ST_Contains(c.geom4326, w.geom)) where c.name ~* 'joaquin' and  wim_type ~* 'data'"

wim.query <- "select w.* from wim_geoview w where wim_type ~* 'data'"




wim.df <- read.ogr::readOgrSQL(wim.query)


expect_that(dim(wim.df),equals(c(28,9)))
