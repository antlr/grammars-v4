--
-- TYPE_SANITY
-- Sanity checks for common errors in making type-related system tables:
-- pg_type, pg_class, pg_attribute, pg_range.
--
-- None of the SELECTs here should ever find any matching entries,
-- so the expected output is easy to maintain ;-).
-- A test failure indicates someone messed up an entry in the system tables.
--
-- NB: we assume the oidjoins test will have caught any dangling links,
-- that is OID or REGPROC fields that are not zero and do not match some
-- row in the linked-to table.  However, if we want to enforce that a link
-- field can't be 0, we have to check it here.

-- **************** pg_type ****************

-- Look for illegal values in pg_type fields.

SELECT p1.oid, p1.typname
FROM pg_type as p1
WHERE p1.typnamespace = 0 OR
    (p1.typlen <= 0 AND p1.typlen != -1 AND p1.typlen != -2) OR
    (p1.typtype not in ('b', 'c', 'd', 'e', 'p', 'r')) OR
    NOT p1.typisdefined OR
    (p1.typalign not in ('c', 's', 'i', 'd')) OR
    (p1.typstorage not in ('p', 'x', 'e', 'm'));

-- Look for "pass by value" types that can't be passed by value.

SELECT p1.oid, p1.typname
FROM pg_type as p1
WHERE p1.typbyval AND
    (p1.typlen != 1 OR p1.typalign != 'c') AND
    (p1.typlen != 2 OR p1.typalign != 's') AND
    (p1.typlen != 4 OR p1.typalign != 'i') AND
    (p1.typlen != 8 OR p1.typalign != 'd');

-- Look for "toastable" types that aren't varlena.

SELECT p1.oid, p1.typname
FROM pg_type as p1
WHERE p1.typstorage != 'p' AND
    (p1.typbyval OR p1.typlen != -1);

-- Look for complex types that do not have a typrelid entry,
-- or basic types that do.

SELECT p1.oid, p1.typname
FROM pg_type as p1
WHERE (p1.typtype = 'c' AND p1.typrelid = 0) OR
    (p1.typtype != 'c' AND p1.typrelid != 0);

-- Look for types that should have an array type according to their typtype,
-- but don't.  We exclude composites here because we have not bothered to
-- make array types corresponding to the system catalogs' rowtypes.
-- NOTE: as of v10, this check finds pg_node_tree, pg_ndistinct, smgr.

SELECT p1.oid, p1.typname
FROM pg_type as p1
WHERE p1.typtype not in ('c','d','p') AND p1.typname NOT LIKE E'\\_%'
    AND NOT EXISTS
    (SELECT 1 FROM pg_type as p2
     WHERE p2.typname = ('_' || p1.typname)::name AND
           p2.typelem = p1.oid and p1.typarray = p2.oid);

-- Make sure typarray points to a varlena array type of our own base
SELECT p1.oid, p1.typname as basetype, p2.typname as arraytype,
       p2.typelem, p2.typlen
FROM   pg_type p1 LEFT JOIN pg_type p2 ON (p1.typarray = p2.oid)
WHERE  p1.typarray <> 0 AND
       (p2.oid IS NULL OR p2.typelem <> p1.oid OR p2.typlen <> -1);

-- Look for range types that do not have a pg_range entry
SELECT p1.oid, p1.typname
FROM pg_type as p1
WHERE p1.typtype = 'r' AND
   NOT EXISTS(SELECT 1 FROM pg_range r WHERE rngtypid = p1.oid);

-- Look for range types whose typalign isn't sufficient
SELECT p1.oid, p1.typname, p1.typalign, p2.typname, p2.typalign
FROM pg_type as p1
     LEFT JOIN pg_range as r ON rngtypid = p1.oid
     LEFT JOIN pg_type as p2 ON rngsubtype = p2.oid
WHERE p1.typtype = 'r' AND
    (p1.typalign != (CASE WHEN p2.typalign = 'd' THEN 'd'::"char"
                          ELSE 'i'::"char" END)
     OR p2.oid IS NULL);

-- Text conversion routines must be provided.

SELECT p1.oid, p1.typname
FROM pg_type as p1
WHERE (p1.typinput = 0 OR p1.typoutput = 0);

-- Check for bogus typinput routines

SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typinput = p2.oid AND NOT
    ((p2.pronargs = 1 AND p2.proargtypes[0] = 'cstring'::regtype) OR
     (p2.pronargs = 2 AND p2.proargtypes[0] = 'cstring'::regtype AND
      p2.proargtypes[1] = 'oid'::regtype) OR
     (p2.pronargs = 3 AND p2.proargtypes[0] = 'cstring'::regtype AND
      p2.proargtypes[1] = 'oid'::regtype AND
      p2.proargtypes[2] = 'int4'::regtype));

-- Check for type of the variadic array parameter's elements.
-- provariadic should be ANYOID if the type of the last element is ANYOID,
-- ANYELEMENTOID if the type of the last element is ANYARRAYOID,
-- ANYCOMPATIBLEOID if the type of the last element is ANYCOMPATIBLEARRAYOID,
-- and otherwise the element type corresponding to the array type.

SELECT oid::regprocedure, provariadic::regtype, proargtypes::regtype[]
FROM pg_proc
WHERE provariadic != 0
AND case proargtypes[array_length(proargtypes, 1)-1]
	WHEN '"any"'::regtype THEN '"any"'::regtype
	WHEN 'anyarray'::regtype THEN 'anyelement'::regtype
	WHEN 'anycompatiblearray'::regtype THEN 'anycompatible'::regtype
	ELSE (SELECT t.oid
		  FROM pg_type t
		  WHERE t.typarray = proargtypes[array_length(proargtypes, 1)-1])
	END  != provariadic;

-- Check that all and only those functions with a variadic type have
-- a variadic argument.
SELECT oid::regprocedure, proargmodes, provariadic
FROM pg_proc
WHERE (proargmodes IS NOT NULL AND 'v' = any(proargmodes))
    IS DISTINCT FROM
    (provariadic != 0);

-- As of 8.0, this check finds refcursor, which is borrowing
-- other types' I/O routines
SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typinput = p2.oid AND p1.typtype in ('b', 'p') AND NOT
    (p1.typelem != 0 AND p1.typlen < 0) AND NOT
    (p2.prorettype = p1.oid AND NOT p2.proretset)
ORDER BY 1;

-- Varlena array types will point to array_in
-- Exception as of 8.1: int2vector and oidvector have their own I/O routines
SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typinput = p2.oid AND
    (p1.typelem != 0 AND p1.typlen < 0) AND NOT
    (p2.oid = 'array_in'::regproc)
ORDER BY 1;

-- typinput routines should not be volatile
SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typinput = p2.oid AND p2.provolatile NOT IN ('i', 's');

-- Composites, domains, enums, ranges should all use the same input routines
SELECT DISTINCT typtype, typinput
FROM pg_type AS p1
WHERE p1.typtype not in ('b', 'p')
ORDER BY 1;

-- Check for bogus typoutput routines

-- As of 8.0, this check finds refcursor, which is borrowing
-- other types' I/O routines
SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typoutput = p2.oid AND p1.typtype in ('b', 'p') AND NOT
    (p2.pronargs = 1 AND
     (p2.proargtypes[0] = p1.oid OR
      (p2.oid = 'array_out'::regproc AND
       p1.typelem != 0 AND p1.typlen = -1)))
ORDER BY 1;

SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typoutput = p2.oid AND NOT
    (p2.prorettype = 'cstring'::regtype AND NOT p2.proretset);

-- typoutput routines should not be volatile
SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typoutput = p2.oid AND p2.provolatile NOT IN ('i', 's');

-- Composites, enums, ranges should all use the same output routines
SELECT DISTINCT typtype, typoutput
FROM pg_type AS p1
WHERE p1.typtype not in ('b', 'd', 'p')
ORDER BY 1;

-- Domains should have same typoutput as their base types
SELECT p1.oid, p1.typname, p2.oid, p2.typname
FROM pg_type AS p1 LEFT JOIN pg_type AS p2 ON p1.typbasetype = p2.oid
WHERE p1.typtype = 'd' AND p1.typoutput IS DISTINCT FROM p2.typoutput;

-- Check for bogus typreceive routines

SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typreceive = p2.oid AND NOT
    ((p2.pronargs = 1 AND p2.proargtypes[0] = 'internal'::regtype) OR
     (p2.pronargs = 2 AND p2.proargtypes[0] = 'internal'::regtype AND
      p2.proargtypes[1] = 'oid'::regtype) OR
     (p2.pronargs = 3 AND p2.proargtypes[0] = 'internal'::regtype AND
      p2.proargtypes[1] = 'oid'::regtype AND
      p2.proargtypes[2] = 'int4'::regtype));

-- As of 7.4, this check finds refcursor, which is borrowing
-- other types' I/O routines
SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typreceive = p2.oid AND p1.typtype in ('b', 'p') AND NOT
    (p1.typelem != 0 AND p1.typlen < 0) AND NOT
    (p2.prorettype = p1.oid AND NOT p2.proretset)
ORDER BY 1;

-- Varlena array types will point to array_recv
-- Exception as of 8.1: int2vector and oidvector have their own I/O routines
SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typreceive = p2.oid AND
    (p1.typelem != 0 AND p1.typlen < 0) AND NOT
    (p2.oid = 'array_recv'::regproc)
ORDER BY 1;

-- Suspicious if typreceive doesn't take same number of args as typinput
SELECT p1.oid, p1.typname, p2.oid, p2.proname, p3.oid, p3.proname
FROM pg_type AS p1, pg_proc AS p2, pg_proc AS p3
WHERE p1.typinput = p2.oid AND p1.typreceive = p3.oid AND
    p2.pronargs != p3.pronargs;

-- typreceive routines should not be volatile
SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typreceive = p2.oid AND p2.provolatile NOT IN ('i', 's');

-- Composites, domains, enums, ranges should all use the same receive routines
SELECT DISTINCT typtype, typreceive
FROM pg_type AS p1
WHERE p1.typtype not in ('b', 'p')
ORDER BY 1;

-- Check for bogus typsend routines

-- As of 7.4, this check finds refcursor, which is borrowing
-- other types' I/O routines
SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typsend = p2.oid AND p1.typtype in ('b', 'p') AND NOT
    (p2.pronargs = 1 AND
     (p2.proargtypes[0] = p1.oid OR
      (p2.oid = 'array_send'::regproc AND
       p1.typelem != 0 AND p1.typlen = -1)))
ORDER BY 1;

SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typsend = p2.oid AND NOT
    (p2.prorettype = 'bytea'::regtype AND NOT p2.proretset);

-- typsend routines should not be volatile
SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typsend = p2.oid AND p2.provolatile NOT IN ('i', 's');

-- Composites, enums, ranges should all use the same send routines
SELECT DISTINCT typtype, typsend
FROM pg_type AS p1
WHERE p1.typtype not in ('b', 'd', 'p')
ORDER BY 1;

-- Domains should have same typsend as their base types
SELECT p1.oid, p1.typname, p2.oid, p2.typname
FROM pg_type AS p1 LEFT JOIN pg_type AS p2 ON p1.typbasetype = p2.oid
WHERE p1.typtype = 'd' AND p1.typsend IS DISTINCT FROM p2.typsend;

-- Check for bogus typmodin routines

SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typmodin = p2.oid AND NOT
    (p2.pronargs = 1 AND
     p2.proargtypes[0] = 'cstring[]'::regtype AND
     p2.prorettype = 'int4'::regtype AND NOT p2.proretset);

-- typmodin routines should not be volatile
SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typmodin = p2.oid AND p2.provolatile NOT IN ('i', 's');

-- Check for bogus typmodout routines

SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typmodout = p2.oid AND NOT
    (p2.pronargs = 1 AND
     p2.proargtypes[0] = 'int4'::regtype AND
     p2.prorettype = 'cstring'::regtype AND NOT p2.proretset);

-- typmodout routines should not be volatile
SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typmodout = p2.oid AND p2.provolatile NOT IN ('i', 's');

-- Array types should have same typmodin/out as their element types

SELECT p1.oid, p1.typname, p2.oid, p2.typname
FROM pg_type AS p1, pg_type AS p2
WHERE p1.typelem = p2.oid AND NOT
    (p1.typmodin = p2.typmodin AND p1.typmodout = p2.typmodout);

-- Array types should have same typdelim as their element types

SELECT p1.oid, p1.typname, p2.oid, p2.typname
FROM pg_type AS p1, pg_type AS p2
WHERE p1.typarray = p2.oid AND NOT (p1.typdelim = p2.typdelim);

-- Look for array types whose typalign isn't sufficient

SELECT p1.oid, p1.typname, p1.typalign, p2.typname, p2.typalign
FROM pg_type AS p1, pg_type AS p2
WHERE p1.typarray = p2.oid AND
    p2.typalign != (CASE WHEN p1.typalign = 'd' THEN 'd'::"char"
                         ELSE 'i'::"char" END);

-- Check for bogus typanalyze routines

SELECT p1.oid, p1.typname, p2.oid, p2.proname
FROM pg_type AS p1, pg_proc AS p2
WHERE p1.typanalyze = p2.oid AND NOT
    (p2.pronargs = 1 AND
     p2.proargtypes[0] = 'internal'::regtype AND
     p2.prorettype = 'bool'::regtype AND NOT p2.proretset);

-- there does not seem to be a reason to care about volatility of typanalyze

-- domains inherit their base type's typanalyze

SELECT d.oid, d.typname, d.typanalyze, t.oid, t.typname, t.typanalyze
FROM pg_type d JOIN pg_type t ON d.typbasetype = t.oid
WHERE d.typanalyze != t.typanalyze;

-- range_typanalyze should be used for all and only range types
-- (but exclude domains, which we checked above)

SELECT t.oid, t.typname, t.typanalyze
FROM pg_type t LEFT JOIN pg_range r on t.oid = r.rngtypid
WHERE t.typbasetype = 0 AND
    (t.typanalyze = 'range_typanalyze'::regproc) != (r.rngtypid IS NOT NULL);

-- array_typanalyze should be used for all and only array types
-- (but exclude domains, which we checked above)
-- As of 9.2 this finds int2vector and oidvector, which are weird anyway

SELECT t.oid, t.typname, t.typanalyze
FROM pg_type t
WHERE t.typbasetype = 0 AND
    (t.typanalyze = 'array_typanalyze'::regproc) !=
    (typelem != 0 AND typlen < 0)
ORDER BY 1;

-- **************** pg_class ****************

-- Look for illegal values in pg_class fields

SELECT p1.oid, p1.relname
FROM pg_class as p1
WHERE relkind NOT IN ('r', 'i', 'S', 't', 'v', 'm', 'c', 'f', 'p') OR
    relpersistence NOT IN ('p', 'u', 't') OR
    relreplident NOT IN ('d', 'n', 'f', 'i');

-- All tables and indexes should have an access method.
SELECT p1.oid, p1.relname
FROM pg_class as p1
WHERE p1.relkind NOT IN ('S', 'v', 'f', 'c') and
    p1.relam = 0;

-- Conversely, sequences, views, types shouldn't have them
SELECT p1.oid, p1.relname
FROM pg_class as p1
WHERE p1.relkind IN ('S', 'v', 'f', 'c') and
    p1.relam != 0;

-- Indexes should have AMs of type 'i'
SELECT pc.oid, pc.relname, pa.amname, pa.amtype
FROM pg_class as pc JOIN pg_am AS pa ON (pc.relam = pa.oid)
WHERE pc.relkind IN ('i') and
    pa.amtype != 'i';

-- Tables, matviews etc should have AMs of type 't'
SELECT pc.oid, pc.relname, pa.amname, pa.amtype
FROM pg_class as pc JOIN pg_am AS pa ON (pc.relam = pa.oid)
WHERE pc.relkind IN ('r', 't', 'm') and
    pa.amtype != 't';

-- **************** pg_attribute ****************

-- Look for illegal values in pg_attribute fields

SELECT p1.attrelid, p1.attname
FROM pg_attribute as p1
WHERE p1.attrelid = 0 OR p1.atttypid = 0 OR p1.attnum = 0 OR
    p1.attcacheoff != -1 OR p1.attinhcount < 0 OR
    (p1.attinhcount = 0 AND NOT p1.attislocal);

-- Cross-check attnum against parent relation

SELECT p1.attrelid, p1.attname, p2.oid, p2.relname
FROM pg_attribute AS p1, pg_class AS p2
WHERE p1.attrelid = p2.oid AND p1.attnum > p2.relnatts;

-- Detect missing pg_attribute entries: should have as many non-system
-- attributes as parent relation expects

SELECT p1.oid, p1.relname
FROM pg_class AS p1
WHERE p1.relnatts != (SELECT count(*) FROM pg_attribute AS p2
                      WHERE p2.attrelid = p1.oid AND p2.attnum > 0);

-- Cross-check against pg_type entry
-- NOTE: we allow attstorage to be 'plain' even when typstorage is not;
-- this is mainly for toast tables.

SELECT p1.attrelid, p1.attname, p2.oid, p2.typname
FROM pg_attribute AS p1, pg_type AS p2
WHERE p1.atttypid = p2.oid AND
    (p1.attlen != p2.typlen OR
     p1.attalign != p2.typalign OR
     p1.attbyval != p2.typbyval OR
     (p1.attstorage != p2.typstorage AND p1.attstorage != 'p'));

-- **************** pg_range ****************

-- Look for illegal values in pg_range fields.

SELECT p1.rngtypid, p1.rngsubtype
FROM pg_range as p1
WHERE p1.rngtypid = 0 OR p1.rngsubtype = 0 OR p1.rngsubopc = 0;

-- rngcollation should be specified iff subtype is collatable

SELECT p1.rngtypid, p1.rngsubtype, p1.rngcollation, t.typcollation
FROM pg_range p1 JOIN pg_type t ON t.oid = p1.rngsubtype
WHERE (rngcollation = 0) != (typcollation = 0);

-- opclass had better be a btree opclass accepting the subtype.
-- We must allow anyarray matches, cf opr_sanity's binary_coercible()

SELECT p1.rngtypid, p1.rngsubtype, o.opcmethod, o.opcname
FROM pg_range p1 JOIN pg_opclass o ON o.oid = p1.rngsubopc
WHERE o.opcmethod != 403 OR
    ((o.opcintype != p1.rngsubtype) AND NOT
     (o.opcintype = 'pg_catalog.anyarray'::regtype AND
      EXISTS(select 1 from pg_catalog.pg_type where
             oid = p1.rngsubtype and typelem != 0 and typlen = -1)));

-- canonical function, if any, had better match the range type

SELECT p1.rngtypid, p1.rngsubtype, p.proname
FROM pg_range p1 JOIN pg_proc p ON p.oid = p1.rngcanonical
WHERE pronargs != 1 OR proargtypes[0] != rngtypid OR prorettype != rngtypid;

-- subdiff function, if any, had better match the subtype

SELECT p1.rngtypid, p1.rngsubtype, p.proname
FROM pg_range p1 JOIN pg_proc p ON p.oid = p1.rngsubdiff
WHERE pronargs != 2
    OR proargtypes[0] != rngsubtype OR proargtypes[1] != rngsubtype
    OR prorettype != 'pg_catalog.float8'::regtype;
