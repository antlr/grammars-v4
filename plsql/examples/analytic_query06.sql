select trim(both ' ' from '  a  ') from dual where trim(:a) is not null
