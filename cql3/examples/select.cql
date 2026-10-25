SELECT event_id, 
  dateOf(created_at) AS creation_date,
  blobAsText(content) AS content 
  FROM timeline;

  SELECT COUNT(*) 
FROM system.IndexInfo;

SELECT lastname 
FROM cycling.cyclist_name 
LIMIT 50000;

SELECT id, lastname, teams 
FROM cycling.cyclist_career_teams 
WHERE id=5b6962dd-3f90-4c93-8f61-eabfa4a803e2;


SELECT * FROM cycling.cyclist_category;

SELECT * FROM cycling.cyclist_category WHERE category = 'SPRINT';

SELECT category, points, lastname FROM cycling.cyclist_category;

SELECT * From cycling.cyclist_name LIMIT 3;

SELECT * FROM cycling.cyclist_cat_pts WHERE category = 'GC' ORDER BY points ASC;

SELECT race_name, point_id, lat_long AS CITY_LATITUDE_LONGITUDE FROM cycling.route;

SELECT * FROM cycling.upcoming_calendar WHERE year = 2015 AND month = 06;

select json name, checkin_id, time_stamp from checkin;

select name, checkin_id, toJson(time_stamp) from checkin;

SELECT * FROM cycling.calendar WHERE race_id IN (100, 101, 102) AND (race_start_date, race_end_date) IN (('2015-01-01','2015-02-02'), ('2016-01-01','2016-02-02'));

SELECT * FROM cycling.calendar WHERE race_id IN (100, 101, 102) AND (race_start, race_end) >= ('2015-01-01', '2015-02-02');

SELECT * FROM cycling.race_times WHERE race_name = '17th Santos Tour Down Under' and race_time >= '19:15:19' AND race_time <= '19:15:39';

