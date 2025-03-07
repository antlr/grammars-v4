SELECT * FROM TD_JSONSHRED(
    ON (SELECT id, j FROM tbl)
        USING
        ROWEXPR('items')
        COLEXPR('name', 'price')
        RETURNTYPES('VARCHAR(20)', 'DECIMAL(10,2)')
) t;
