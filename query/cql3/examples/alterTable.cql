ALTER TABLE cycling_comments 
WITH compression = { 
   'sstable_compression' : 'DeflateCompressor', 
   'chunk_length_kb' : 64 };

