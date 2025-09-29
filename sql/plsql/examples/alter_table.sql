ALTER TABLE test1 SPLIT PARTITION p_MAXV
    AT (TO_DATE('01/01/2030', 'DD/MM/YYYY'))
    INTO (partition p_2029 TABLESPACE users, partition p_MAXV) ONLINE;
    
ALTER TABLE fruit ANNOTATIONS (Visibility 'Everyone');

ALTER TABLE fruit ANNOTATIONS (drop Visibility);

ALTER TABLE fruit ANNOTATIONS (add Visibility 'Everyone');

ALTER TABLE fruit MODIFY (id ANNOTATIONS (Visibility 'Hidden'));

ALTER TABLE fruit MODIFY (id ANNOTATIONS (drop Visibility));

ALTER TABLE fruit MODIFY (id ANNOTATIONS (add Visibility 'Hidden'));    