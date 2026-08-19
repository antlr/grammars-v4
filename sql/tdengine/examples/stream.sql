create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.stream_t1 into stream_outdb.stream_out as select _twstart, avg(c1) from stream_querydb.stream_t2;
create stream stream_streamdb_2.s1 interval(1s) sliding(1s) from stream_triggerdb.stream_t1 into stream_outdb.stream_out as select _twstart, avg(c1) from stream_querydb.stream_t2;
create stream stream_streamdb.s1_2 interval(1s) sliding(1s) from stream_triggerdb.stream_t1 into stream_outdb.stream_out as select _twstart, avg(c1) from stream_querydb.stream_t2;
create stream stream_streamdb.s1_2 interval(1s) sliding(1s) from stream_triggerdb_2.stream_t1 into stream_outdb.stream_out as select _twstart, avg(c1) from stream_querydb.stream_t2;
create stream stream_streamdb.s1_2 interval(1s) sliding(1s) from stream_triggerdb_2.stream_t1 into stream_outdb.stream_out as select _twstart, avg(c1) from stream_querydb.stream_t2;
create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.stream_t1 stream_options(expired_time(1000a)) into stream_outdb.stream_out as select _twstart, avg(c1) from stream_querydb.stream_t2;
create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.stream_t1 stream_options(ignore_disorder) into stream_outdb.stream_out as select _twstart, avg(c1) from stream_querydb.stream_t2;
create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.stream_t1 stream_options(delete_recalc) into stream_outdb.stream_out as select _twstart, avg(c1) from stream_querydb.stream_t2;
create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.stream_t1 stream_options(fill_history('2025-06-11 15:17:14')) into stream_outdb.stream_out as select _twstart, avg(c1) from stream_querydb.stream_t2;
-- window
create stream stream_streamdb.s1 count_window(20, 10, c1, c2) from stream_triggerdb.stream_t1 into stream_outdb.stream_out as select _tlocaltime, avg(c1) from stream_querydb.stream_t2;
create stream stream_streamdb.s1 period(1s) from stream_triggerdb.stream_t1 into stream_outdb.stream_out as select _tlocaltime, avg(c1) from stream_querydb.stream_t;
create stream stream_streamdb.s1 period(1m, 1s) from stream_triggerdb.stream_t1 into stream_outdb.stream_out as select _tlocaltime, avg(c1) from stream_querydb.stream_t2;

create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.st1 partition by tbname into stream_outdb.stream_out as select _tlocaltime, avg(c1) from stream_querydb.stream_t2;
create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.st1 partition by tbname, tag1, tag2, tag3 into stream_outdb.stream_out as select _tlocaltime, avg(c1) from stream_querydb.stream_t2;

create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.stream_t1 notify('ws://localhost:8080') on (window_close) into stream_outdb.stream_out as select _tlocaltime, avg(c1) from stream_querydb.stream_t2;
create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.stream_t1 notify('ws://localhost:8080', 'ws://localhost:8080/notify') on (window_close|window_open) into stream_outdb.stream_out as select _tlocaltime, avg(c1) from stream_querydb.stream_t2;

create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.st1 partition by tag1, tag2, tag3 into stream_outdb.stream_out output_subtable(concat('t_', tag2)) as select ts, c1, c2 from stream_querydb.stream_t2;
create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.st1 partition by tag1, tag2, tag3 into stream_outdb.stream_out (out_ts, out_c1 primary key, out_c2) as select ts, c1, c2 from stream_querydb.stream_t2;
create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.st1 partition by tag1, tag2, tag3 into stream_outdb.stream_out tags(out_tag1 int as cast(tag1 + 1 as int), out_tag2 varchar(20) as upper(tag2), out_tag3 timestamp as tag3) as select ts, c1, c2 from stream_querydb.stream_t2;

create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.st1 partition by tbname into stream_outdb.stream_out as select _tprev_ts, avg(c1) from stream_querydb.stream_t2;
create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.st1 partition by tbname into stream_outdb.stream_out as select _tcurrent_ts, avg(c1) from stream_querydb.stream_t2;
create stream stream_streamdb.s1 interval(1s) sliding(1s) from stream_triggerdb.st1 partition by tbname into stream_outdb.stream_out as select _twstart, avg(c1) from stream_querydb.stream_t2;