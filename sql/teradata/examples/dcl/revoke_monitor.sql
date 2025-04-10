-- Revoking All Privileges From a User
REVOKE MONITOR PRIVILEGES
FROM ted;

-- Revoking Specific Privileges From a User
REVOKE ABORTSESSION, MONSESSION, SETSESSRATE FROM pls;
