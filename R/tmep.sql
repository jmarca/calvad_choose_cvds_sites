create or replace view wim_geoview as
WITH  active_wim as (
     select distinct w.site_no
     from wim_data w
     where ts between '2009-12-30' and '2010-12-31'
),
wld AS (
     SELECT DISTINCT wim_lane_dir.site_no,
        wim_lane_dir.direction,
        wim_lane_dir.facility
     FROM wim_lane_dir join active_wim using(site_no)
     ORDER BY wim_lane_dir.site_no
)
SELECT ws.site_no,
    ws.loc,
    ws.wim_type,
    ws.cal_pm,
    ws.latitude,
    ws.longitude,
    ws.last_modified,
    COALESCE(wld.facility, wf.freeway_id) AS freeway_id,
    wld.direction,
    geom_points_4326.geom
   FROM wim_stations ws
   JOIN wim_points_4326 wp ON ws.site_no = wp.wim_id
   JOIN geom_points_4326 USING (gid)
   JOIN wim_freeway wf ON ws.site_no = wf.wim_id
   JOIN wld USING (site_no)
  ORDER BY COALESCE(wld.facility, wf.freeway_id), wld.direction, ws.site_no;
