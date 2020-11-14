ALTER KEYSPACE cycling
WITH REPLICATION = { 
  'class' : 'NetworkTopologyStrategy',
  'datacenter1' : 3 } 
 AND DURABLE_WRITES = false ;
 