SELECT ARRAY_TO_JSON(arr_col) RETURNS JSON(20) CHARACTER SET LATIN AS atj_returns_type
     , ARRAY_TO_JSON(arr_col) RETURNS STYLE json_col AS atj_returns_style
     , BSON_CHECK(bson_col)
     , BSON_CHECK(bson_col, 'LAX')
     , td_sysfnlib.DATASIZE(json_col) AS datasize_json
     , GEOJSONFROMGEOM(geom_col)
     , GEOJSONFROMGEOM(geom_col, 3)
     , GEOJSONFROMGEOM(geom_col) RETURNS VARCHAR(10) CHARACTER SET UNICODE
     , GEOMFROMGEOJSON (geojson_col, 2)
     , JSON_CHECK('{}')
     , JSONGETVALUE(json_col, '$.a' AS INTEGER) AS get_int_value
     , JSONMETADATA(json_col)
     , td_sysfnlib.NVP2JSON(nvp_str)
     , td_sysfnlib.NVP2JSON(nvp_str, '|', '=') AS nvp2json_with_name_and_value_delimiters
     , td_sysfnlib.NVP2JSON(nvp_str, '|', '=', '1234567890') AS nvp2json_with_ignore_characters
  FROM tbl;
