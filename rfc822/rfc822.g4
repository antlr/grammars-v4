date_time   : (day ',' )? date time    ;   
                                            

day         :  'Mon'  | 'Tue' |  'Wed'  | 'Thu'
            |  'Fri'  | 'Sat' |  'Sun';

date        :  2DIGIT+ month 2DIGIT       ;
                                           

month       :  'Jan'  |  'Feb' |  'Mar'  |  'Apr'
            |  'May'  |  'Jun' |  'Jul'  |  'Aug'
            |  'Sep'  |  'Oct' |  'Nov'  |  'Dec';

time        :  hour zone                    ;

hour        :  2DIGIT ':' 2DIGIT [':' 2DIGIT];
                                            

zone        :  'UT'  | 'GMT'                
                                            
            |  'EST' | 'EDT'               
            |  'CST' | 'CDT'               
            |  'MST' | 'MDT'               
            |  'PST' | 'PDT'               
            |  1ALPHA                                                    
            | ( ('+' | '-') 4DIGIT ) ; 

WS
   : [ \r\n\t] -> skip
   ;


2DIGIT: ALPHANUMERIC ALPHANUMERIC;
4DIGIT: ALPHANUMERIC ALPHANUMERIC ALPHANUMERIC ALPHANUMERIC;

fragment ASCII                   : [\u0000-\u007F];
fragment ALPHANUMERIC            : [a-zA-Z0-9];
fragment NOTALPHANUMERIC         : ~[a-zA-Z0-9];                                           