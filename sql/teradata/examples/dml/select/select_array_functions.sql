select array_agg (arr_col, NEW arr_type())
     , array_agg (arr_col ORDER BY id ASC, NEW arr_type())
     , cardinality(arr_col)
     , array_concat(arr_col, other_arr_col, 1, 5)
     , ARRAY_GT(arr_col, other_arr_col)
     , ARRAY_GE(arr_col, other_arr_col)
     , ARRAY_LT(arr_col, other_arr_col, 2, 3)
     , ARRAY_LE(arr_col, other_arr_col)
     , ARRAY_EQ(arr_col, other_arr_col)
     , ARRAY_NE(arr_col, other_arr_col)
     , ARRAY_ADD(arr_col, other_arr_col)
     , ARRAY_SUB(arr_col, other_arr_col)
     , ARRAY_MUL(arr_col, other_arr_col, 1, 4)
     , ARRAY_DIV(arr_col, other_arr_col)
     , ARRAY_MOD(arr_col, other_arr_col)
     , ARRAY_SUM(arr_col)
     , ARRAY_AVG(arr_col, 2, 3)
     , ARRAY_MAX(arr_col)
     , ARRAY_MIN(arr_col)
     , ARRAY_COUNT_DISTINCT(arr_col)  AS count_distinct_on_all_elements
     , ARRAY_COUNT_DISTINCT(arr_col, 1, 4) AS count_distinct_with_scope_ref
     , ARRAY_COUNT_DISTINCT(arr_col, 1, 4, 3+2) AS count_distinct_with_scope_ref_and_matching_expr
     , ARRAY_GET(arr_col, 3)
     , ARRAY_COMPARE(arr_col, other_arr_col) AS compare_on_all_elements
     , ARRAY_COMPARE(arr_col, other_arr_col, 2, 5) AS compare_with_scope_ref
     , ARRAY_COMPARE(arr_col, other_arr_col, 2, 5, 1) AS compare_with_scope_ref_and_nulls_equal_flag
     , ARRAY_UPDATE(arr_col, 2+3) AS update_all
     , ARRAY_UPDATE(arr_col, 2+3, 0) AS update_single_value
     , ARRAY_UPDATE(arr_col, 100/10, 1, 4) AS update_on_scope_reference
     , ARRAY_UPDATE_STRIDE(arr_col, 2+3, 2) AS update_stride_all
     , ARRAY_UPDATE_STRIDE(arr_col, 2+3, 2, 0) AS update_stride_single_value
     , ARRAY_UPDATE_STRIDE(arr_col, 2+3, 2, 1, 5) AS update_stride_on_scope_reference
     , OEXISTS(arr_col, 5)
     , OEXISTS(arr_col, 5)
     , OPRIOR(arr_col, 5)
     , ONEXT(arr_col, 5)
     , OTRIM(arr_col)
     , OTRIM(arr_col, 2)
     , OCOUNT(arr_col)
     , OLIMIT(arr_col)
     , OFIRST(arr_col)
     , OLAST(arr_col)
     , ODELETE(arr_col)
from tbl;
