library(testthat)

## just going to start hacking and refactor later

## still have RAM issues on laptop.  do only SJV

vds.query <- "select v.* from vds_current_view v join carb_airdistricts_aligned_03 c on (ST_Contains(c.geom4326, v.geom)) where c.name ~* 'joaquin' and vdstype='ML'"


vds.query <- "select v.* from vds_current_view v  where vdstype='ML'"

vds.df <- read.ogr::readOgrSQL(vds.query)
