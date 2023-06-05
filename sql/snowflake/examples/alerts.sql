CREATE OR REPLACE ALERT myalert
  WAREHOUSE = mywarehouse
  SCHEDULE = '1 minute'
  IF( EXISTS(
    SELECT gauge_value FROM gauge WHERE gauge_value>200))
  THEN
INSERT INTO gauge_value_exceeded_history VALUES (current_timestamp());

ALTER ALERT myalert RESUME;

ALTER ALERT my_alert SET WAREHOUSE = my_other_warehouse;

ALTER ALERT my_alert SET SCHEDULE = '2 minutes';

ALTER ALERT my_alert MODIFY CONDITION EXISTS (SELECT gauge_value FROM gauge WHERE gauge_value>300);

ALTER ALERT my_alert MODIFY ACTION CALL my_procedure();

DROP ALERT myalert;

SHOW ALERTS;

DESC ALERT myalert;
