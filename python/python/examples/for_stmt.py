# for_stmt: ASYNC? FOR exprlist IN testlist COLON suite else_clause?

# FOR exprlist IN testlist COLON suite
for x in range(1):
    pass

# FOR exprlist IN testlist COLON suite (else_clause)?
for x in range(1):
    x
else:
    pass

# async_stmt must be inside async function
async def f():
    # ASYNC for_stmt
    async for _ in range(5):
        pass
