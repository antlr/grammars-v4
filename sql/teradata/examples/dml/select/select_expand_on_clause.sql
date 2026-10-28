-- Expansion on an Anchor Point Using ANCHOR_SECOND
SELECT BEGIN(expd)
FROM t2
EXPAND ON pd AS expd BY ANCHOR ANCHOR_SECOND;

-- Expansion by interval
SELECT id, quantity, expd
FROM tdate1
EXPAND ON pd AS expd BY INTERVAL '1' MONTH;

-- Expansion on an Anchor point at time
SELECT BEGIN(xyz)
FROM test1
EXPAND ON duration AS xyz BY ANCHOR MONTH_END AT TIME '20:00:00';

-- EXPAND ON for a Moving Average
SELECT stock_id, CAST (p AS DATE), AVG(price)
        OVER (PARTITION BY stock_id
                ORDER BY p ROWS
                2 PRECEDING)
FROM (SELECT stock_id, price, BEGIN(p)
        FROM stk
        EXPAND ON validity AS p
            BY ANCHOR DAY AT TIME'17:59:59'
            FOR PERIOD(TIMESTAMP '2006-01-01 17:59:59',
                       TIMESTAMP '2006-01-05 18:00:00')) AS dt;
