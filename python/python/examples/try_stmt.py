# try_stmt: TRY COLON suite (except_clause+ else_clause? finaly_clause? | finaly_clause)

# TRY COLON suite except_clause
try:
    pass
except:
    pass

# TRY COLON suite except_clause except_clause else_clause
try:
    pass
except Exception as ex:
    pass
except:
    pass
else:
    pass

# TRY COLON suite except_clause finaly_clause
try:
    pass
except Exception:
    pass
finally:
    pass

# TRY COLON suite finaly_clause
try:
    pass
finally:
    pass
