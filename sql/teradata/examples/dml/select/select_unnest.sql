SELECT *
  FROM TABLE (UNNEST(arr_col)) AS t(v);

-- with key expr
SELECT *
  FROM TABLE (UNNEST(id, arr_col)) AS t(k, v);

-- with key expr and ordinality
SELECT *
  FROM TABLE (UNNEST(id, arr_col) WITH ORDINALITY) AS t(k, v, pos);
