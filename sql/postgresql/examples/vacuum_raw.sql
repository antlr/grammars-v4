VACUUM FULL vactst;
VACUUM (FULL) vactst;
VACUUM (FULL, FREEZE) vactst;
VACUUM (ANALYZE, FULL) vactst;
VACUUM FULL pg_am;
VACUUM FULL pg_class;
VACUUM FULL pg_database;
VACUUM FULL vaccluster;
VACUUM FULL vactst;
VACUUM (DISABLE_PAGE_SKIPPING) vaccluster;
VACUUM (PARALLEL 2) pvactst;
VACUUM (PARALLEL 2) pvactst;
VACUUM (PARALLEL 0) pvactst; -- disable parallel vacuum
VACUUM (PARALLEL -1) pvactst; -- error
VACUUM (PARALLEL 2, INDEX_CLEANUP FALSE) pvactst;
VACUUM (PARALLEL 2, FULL TRUE) pvactst; -- error, cannot use both PARALLEL and FULL
VACUUM (PARALLEL) pvactst; -- error, cannot use PARALLEL option without parallel degree
VACUUM (PARALLEL 1, FULL FALSE) tmp; -- parallel vacuum disabled for temp tables
VACUUM (PARALLEL 0, FULL TRUE) tmp; -- can specify parallel disabled (even though that's implied by FULL)
VACUUM (INDEX_CLEANUP TRUE, FULL TRUE) no_index_cleanup;
VACUUM (FULL TRUE) no_index_cleanup;
VACUUM no_index_cleanup;
VACUUM no_index_cleanup;
VACUUM (INDEX_CLEANUP FALSE) vaccluster;
VACUUM (INDEX_CLEANUP FALSE) vactst; -- index cleanup option is ignored if no indexes
VACUUM (INDEX_CLEANUP FALSE, FREEZE TRUE) vaccluster;
VACUUM (TRUNCATE FALSE) vac_truncate_test;
VACUUM vac_truncate_test;
VACUUM (TRUNCATE FALSE, FULL TRUE) vac_truncate_test;
VACUUM (ANALYZE) vacparted;
VACUUM (FULL) vacparted;
VACUUM (FREEZE) vacparted;
VACUUM ANALYZE vacparted(a,b,a);
VACUUM vaccluster, vactst;
VACUUM vacparted, does_not_exist;
VACUUM (FREEZE) vacparted, vaccluster, vactst;
VACUUM (FREEZE) does_not_exist, vaccluster;
VACUUM ANALYZE vactst, vacparted (a);
VACUUM ANALYZE vactst (does_not_exist), vacparted (b);
VACUUM FULL vacparted, vactst;
VACUUM FULL vactst, vacparted (a, b), vaccluster (i);
VACUUM (SKIP_LOCKED) vactst;
VACUUM (SKIP_LOCKED, FULL) vactst;
VACUUM vactst;
VACUUM vacowned;
VACUUM (ANALYZE) vacowned;
VACUUM pg_catalog.pg_class;
VACUUM (ANALYZE) pg_catalog.pg_class;
VACUUM pg_catalog.pg_authid;
VACUUM (ANALYZE) pg_catalog.pg_authid;
VACUUM vacowned_parted;
VACUUM vacowned_part1;
VACUUM vacowned_part2;
VACUUM (ANALYZE) vacowned_parted;
VACUUM (ANALYZE) vacowned_part1;
VACUUM (ANALYZE) vacowned_part2;
VACUUM vacowned_parted;
VACUUM vacowned_part1;
VACUUM vacowned_part2;
VACUUM (ANALYZE) vacowned_parted;
VACUUM (ANALYZE) vacowned_part1;
VACUUM (ANALYZE) vacowned_part2;
VACUUM vacowned_parted;
VACUUM vacowned_part1;
VACUUM vacowned_part2;
VACUUM (ANALYZE) vacowned_parted;
VACUUM (ANALYZE) vacowned_part1;
VACUUM (ANALYZE) vacowned_part2;
VACUUM vacowned_parted;
VACUUM vacowned_part1;
VACUUM vacowned_part2;
VACUUM (ANALYZE) vacowned_parted;
VACUUM (ANALYZE) vacowned_part1;
VACUUM (ANALYZE) vacowned_part2;
