FROM (
  FROM pv_users
  SELECT TRANSFORM(pv_users.userid, pv_users.date1)
  USING 'map_script'
  AS dt, uid
  CLUSTER BY dt) map_output
INSERT OVERWRITE TABLE pv_users_reduced
  SELECT TRANSFORM(map_output.dt, map_output.uid)
  USING 'reduce_script'
  AS (date1 INT, count INT)
