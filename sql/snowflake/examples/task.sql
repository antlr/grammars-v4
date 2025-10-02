ALTER TASK db_name.sch_name.task_name SUSPEND;
ALTER TASK sch_name.task_name SUSPEND;

EXECUTE TASK t;
EXECUTE TASK t RETRY LAST;