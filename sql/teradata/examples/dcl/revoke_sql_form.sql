-- Revoking the INSERT Privilege
REVOKE INSERT
ON personnel.employee
FROM UserA;

-- Revoking Access That Requires Database Privileges
REVOKE ALL PRIVILEGES
ON personnel
FROM UserA;

-- Using ALL BUT When Revoking Privileges
REVOKE ALL BUT SELECT
ON personnel.department
FROM UserA;
