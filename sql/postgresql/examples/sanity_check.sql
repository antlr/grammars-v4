VACUUM;

--
-- sanity check, if we don't have indices the test will take years to
-- complete.  But skip TOAST relations (since they will have varying
-- names depending on the current OID counter) as well as temp tables
-- of other backends (to avoid timing-dependent behavior).
--

-- temporarily disable fancy output, so catalog changes create less diff noise
\a\t

SELECT relname, relhasindex
   FROM pg_class c LEFT JOIN pg_namespace n ON n.oid = relnamespace
   WHERE relkind IN ('r', 'p') AND (nspname ~ '^pg_temp_') IS NOT TRUE
   ORDER BY relname;

-- restore normal output mode
\a\t

--
-- another sanity check: every system catalog that has OIDs should have
-- a unique index on OID.  This ensures that the OIDs will be unique,
-- even after the OID counter wraps around.
-- We exclude non-system tables from the check by looking at nspname.
--
SELECT relname, nspname
 FROM pg_class c LEFT JOIN pg_namespace n ON n.oid = relnamespace JOIN pg_attribute a ON (attrelid = c.oid AND attname = 'oid')
 WHERE relkind = 'r' and c.oid < 16384
     AND ((nspname ~ '^pg_') IS NOT FALSE)
     AND NOT EXISTS (SELECT 1 FROM pg_index i WHERE indrelid = c.oid
                     AND indkey[0] = a.attnum AND indnatts = 1
                     AND indisunique AND indimmediate);

-- check that relations without storage don't have relfilenode
SELECT relname, relkind
  FROM pg_class
 WHERE relkind IN ('v', 'c', 'f', 'p', 'I')
       AND relfilenode <> 0;
