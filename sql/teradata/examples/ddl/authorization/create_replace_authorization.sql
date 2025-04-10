-- creating an invoker authorization
CREATE AUTHORIZATION mydb.auth_cloud
AS INVOKER TRUSTED
USER 'service-user'
PASSWORD 'secret';

-- replacing authorization
REPLACE AUTHORIZATION mydb.auth_cloud
USER 'service-user'
PASSWORD 'begin key\n
abcdefg\n
end key\n';
