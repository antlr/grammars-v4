CREATE TABLE [Monty].[Spam]
(
    Meat int
)
WITH
(
    CLUSTERED COLUMNSTORE INDEX,
    DISTRIBUTION = HASH(Meat)
);

CREATE TABLE [Monty].[Eggs]
(
    Ham int,
    Chilli int,
    Milk int
)
WITH
(
    CLUSTERED INDEX(Ham DESC, Chilli ASC, Milk),
    DISTRIBUTION = ROUND_ROBIN
);

CREATE TABLE [Monty].[Coconut]
(
    LumberJack int
)
WITH
(
    DISTRIBUTION = REPLICATE,
    HEAP
);
