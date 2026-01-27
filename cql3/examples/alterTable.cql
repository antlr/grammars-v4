ALTER TABLE cycling_comments 
WITH compression = { 
   'sstable_compression' : 'DeflateCompressor', 
   'chunk_length_kb' : 64 };

ALTER TABLE t
ALTER IF EXISTS col1 DROP MASKED;

ALTER TABLE t
ALTER IF EXISTS col1 MASKED WITH DEFAULT;