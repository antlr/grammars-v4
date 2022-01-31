FROM (
  FROM pv_users
  MAP pv_users.userid, pv_users.date1
  USING 'map_script'
  CLUSTER BY key) map_output
INSERT OVERWRITE TABLE pv_users_reduced
  REDUCE map_output.key, map_output.value
  USING 'reduce_script'
  AS date1, count
