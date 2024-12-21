SELECT NEW some_arr(1, 2, 3, 4, 5) AS constructed_array
     , arr_col[1]
     , multi_arr [1, 2]
     , arr_col.cardinality()
     , arr_col || other_arr_col
     , arr_col.array_concat (other_arr_col, 1, 5)
     , arr_col.ARRAY_GT(other_arr_col)
     , arr_col.ARRAY_GE(other_arr_col)
     , arr_col.ARRAY_LT(other_arr_col, 2, 3)
     , arr_col.ARRAY_LE(other_arr_col)
     , arr_col.ARRAY_EQ(other_arr_col)
     , arr_col.ARRAY_NE(other_arr_col)
     , arr_col.ARRAY_ADD(other_arr_col)
     , arr_col.ARRAY_SUB(other_arr_col)
     , arr_col.ARRAY_MUL(other_arr_col, 1, 4)
     , arr_col.ARRAY_DIV(other_arr_col)
     , arr_col.ARRAY_MOD(other_arr_col)
     , arr_col.ARRAY_SUM()
     , arr_col.ARRAY_AVG(2, 3)
     , arr_col.ARRAY_MAX()
     , arr_col.ARRAY_MIN()
     , arr_col.ARRAY_COUNT_DISTINCT()  AS count_distinct_on_all_elements
     , arr_col.ARRAY_COUNT_DISTINCT(1, 4) AS count_distinct_with_scope_ref
     , arr_col.ARRAY_COUNT_DISTINCT(1, 4, 3+2) AS count_distinct_with_scope_ref_and_matching_expr
     , arr_col.ARRAY_GET(3)
     , arr_col.ARRAY_COMPARE(other_arr_col) AS compare_on_all_elements
     , arr_col.ARRAY_COMPARE(other_arr_col, 2, 5) AS compare_with_scope_ref
     , arr_col.ARRAY_COMPARE(other_arr_col, 2, 5, 1) AS compare_with_scope_ref_and_nulls_equal_flag
     , arr_col.ARRAY_UPDATE(2+3) AS update_all
     , arr_col.ARRAY_UPDATE(2+3, 0) AS update_single_value
     , arr_col.ARRAY_UPDATE(100/10, 1, 4) AS update_on_scope_reference
     , arr_col.ARRAY_UPDATE_STRIDE(2+3, 2) AS update_stride_all
     , arr_col.ARRAY_UPDATE_STRIDE(2+3, 2, 0) AS update_stride_single_value
     , arr_col.ARRAY_UPDATE_STRIDE(2+3, 2, 1, 5) AS update_stride_on_scope_reference
     , arr_col.OEXISTS(5)
     , arr_col.OEXISTS(5)
     , arr_col.OPRIOR(5)
     , arr_col.ONEXT(5)
     , arr_col.OTRIM()
     , arr_col.OTRIM(2)
     , arr_col.OCOUNT()
     , arr_col.OLIMIT()
     , arr_col.OFIRST()
     , arr_col.OLAST()
     , arr_col.ODELETE()
  FROM tbl_with_arrays;
