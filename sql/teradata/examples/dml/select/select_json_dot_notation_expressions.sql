SELECT json_col.val                   AS object_member
     , json_col..numbers              AS recursive_descend_object_member
     , json_col.obj.*                 AS all_object_members
     , json_col.obj..*                AS recursive_descend_all_object_members
     , json_col.numbers[1]            AS indexed_element
     , json_col.numbers[*]            AS all_array_elements
     , json_col.obj..[1]              AS recursive_descend_array_indexed_element
     , json_col.obj..[*]              AS recursive_descend_array_all_elements
     , json_col.numbers[1:2]          AS array_slice
     , json_col.numbers[1:2:3]        AS array_slice_with_step
     , json_col.obj..[1:2]            AS recursive_descend_array_slice
     , json_col.obj.inner_obj.obj.*   AS long_expression
  FROM tbl;
