-- Setting a Query Band For a Session
SET QUERY_BAND = 'org=Finance;report=Fin123;' FOR SESSION;

-- Removing a Query Band From a Session
SET QUERY_BAND = NONE FOR SESSION;

SET QUERY_BAND = '' FOR SESSION;

-- Setting a Query Band Using FOR SESSION VOLATILE
SET QUERY_BAND = 'cat=siamese;dog=akita;'
UPDATE FOR SESSION VOLATILE;

-- Setting a Query Band for the Current Transaction
SET QUERY_BAND = 'Document=XY1234;Universe=East;' FOR TRANSACTION;
