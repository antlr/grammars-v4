ALTER APPLICATION ROLE weekly_receipts
WITH NAME = receipts_ledger;
GO
ALTER APPLICATION ROLE receipts_ledger
WITH NAME = weekly_ledger,
PASSWORD = '897yUUbv77bsrEE00nk2i',
DEFAULT_SCHEMA = Production;
GO
