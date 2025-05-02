create secret s
TYPE = PASSWORD
USERNAME = 'u'
PASSWORD = 'p'
COMMENT = 'c';

alter secret s unset comment;

alter secret s set password='p' username='u';
alter secret s set username='u' password='p';
alter secret s set comment='c' username='u' password='p';

alter secret s set secret_string='s' comment='c';
alter secret s set comment='c' secret_string='s' ;

alter secret s set api_authentication='s' comment='c';
alter secret s set comment='c' api_authentication='s' ;

alter secret s set oauth_scopes=('','') comment='';
alter secret s set comment='c' oauth_scopes = ('','');

alter secret s set oauth_refresh_token='t' comment='c';
alter secret s set oauth_refresh_token_expiry_time='' comment='';
alter secret s set oauth_refresh_token='' oauth_refresh_token_expiry_time='' comment='';
alter secret s set oauth_refresh_token_expiry_time='' oauth_refresh_token='' comment='';

drop secret s;
drop secret if exists s;

show secrets;
show secrets like 's%';
show secrets in account;
show secrets in db;
show secrets in database d;
show secrets in schema s;
show secrets in schema d.s;
show secrets in application a;
show secrets in application package p;
