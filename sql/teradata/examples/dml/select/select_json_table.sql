SELECT * FROM JSON_TABLE
( ON (SELECT id, jsonCol FROM my_table WHERE id=1)
        USING rowexpr('$.stores[*]')
        colexpr('[ {"jsonpath" : "$.name",
                "type" : "CHAR(20)"},
                {"jsonpath" : "$.type",
                "type" : "VARCHAR(20)"},
                {"jsonpath" : "$.name",
                "type" : "VARCHAR(20)",
                "fromRoot":true} ]')
        ) AS t(id, storeName, "type", supervisorName);
