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