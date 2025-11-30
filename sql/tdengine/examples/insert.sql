INSERT INTO t1 VALUES (now, 1, 'beijing', 3, 4, 5);
INSERT INTO t1 (ts, c1, c2, c3, c4, c5) VALUES (now, 1, 'beijing', 3, 4, 5);
INSERT INTO t1 VALUES (now, 1, 'beijing', 3, 4, 5)(now+1s, 2, 'shanghai', 6, 7, 8)(now+2s, 3, 'guangzhou', 9, 10, 11);
INSERT INTO st1s1 VALUES (now, 1, 'beijing') st1s2 VALUES (now, 10, '131028');
INSERT INTO st1s1 VALUES (now, 1, 'beijing')(now+1s, 2, 'shanghai')(now+2s, 3, 'guangzhou') st1s2 VALUES (now, 10, '131028')(now+1s, 20, '132028');

INSERT INTO st1s1 USING st1 TAGS(1, 'wxy', now) VALUES (now, 1, 'beijing')(now+1s, 2, 'shanghai')(now+2s, 3, 'guangzhou');
INSERT INTO st1s1 USING st1 (tag1, tag2) TAGS(1, 'wxy') (ts, c1, c2) VALUES (now, 1, 'beijing')(now+1s, 2, 'shanghai')(now+2s, 3, 'guangzhou');
INSERT INTO st1s1 (ts, c1, c2) USING st1 (tag1, tag2) TAGS(1, 'wxy') VALUES (now, 1, 'beijing')(now+1s, 2, 'shanghai')(now+2s, 3, 'guangzhou');
INSERT INTO st1s1 USING st1 (tag1, tag2) TAGS(1, 'wxy') (ts, c1, c2) VALUES (now, 1, 'beijing') st1s2 (ts, c1, c2) USING st1 TAGS(2, 'abc', now) VALUES (now+1s, 2, 'shanghai');