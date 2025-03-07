-- Granting CONNECT THROUGH privilege to a Permanent User
GRANT CONNECT THROUGH viewpoint
TO PERMANENT sbd
WITH ROLE admin;

-- Specifying Roles for a Proxy Connection
GRANT CONNECT THROUGH dcm
TO dg120, ks392, lm190
WITH ROLE salesrole1, salesrole2, salesrole3;

-- Specifying WITHOUT ROLE for a Proxy Connection
GRANT CONNECT THROUGH trm
TO PERMANENT accting
WITHOUT ROLE;

-- Specifying the WITH TRUST_ONLY Option
GRANT CONNECT THROUGH pxy_user WITH TRUST_ONLY TO PERMANENT user_name WITHOUT ROLE;