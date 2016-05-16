/*
[The 'BSD licence']
Copyright (c) 2016 Tom Everett
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES// LOSS OF USE,
DATA, OR PROFITS// OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

grammar rfc822_message;


message     :  fields ( CRLF text+ )+;      
                                          
                                           

fields      :    dates                     
                 source                  
               destination+                
                optional_field*    ;      

source      : (  trace )?                  
                 originator                
              (  resent )?  ;              

trace       :    return                    
               received+   ;             

return      :  'Return-path' ':' route_addr ;

received    :  'Received'    ':'            
                  ('from' domain)?          
                  ('by'   domain) ?          
                  ('via'  atom)    ?       
                 ('with' atom) *            
                  ('id'   msg_id)   ?        
                  ('for'  addr_spec])?       



                   ';'    date_time   ;     

originator  :   authentic                  
              ( 'Reply-To'   ':' address+)? ;

authentic   :   'From'       ':'   mailbox  
            | ( 'Sender'     ':'   mailbox 
                'From'       ':' mailbox+) ;
                                            

resent      :   resent_authentic
              ( 'Resent-Reply-To'  ':' address+ )?;

resent_authentic 
            :   'Resent-From'      ':'   mailbox
            | ( 'Resent-Sender'    ':'   mailbox
                'Resent-From'      ':' mailbox+  );

dates       :   orig_date                  
              ( resent_date )?;             

orig_date   :  'Date'        ':'   date_time;

resent_date :  'Resent-Date' ':'   date_time;

destination :  'To'          ':' address  
            /  'Resent-To'   ':' address
            /  'cc'          ':' address  
            /  'Resent-cc'   ':' address
            /  'bcc'         ':'  address * 
            /  'Resent-bcc'  ':'  address*;

optional_field :
            /  'Message-ID'        ':'   msg_id
            /  'Resent-Message-ID' ':'   msg_id
            /  'In-Reply-To'       ':'  (phrase / msg_id)*
            /  'References'        ':'  (phrase / msg_id)*
            /  'Keywords'          ':'  phrase*
            /  'Subject'           ':'  text*
            /  'Comments'          ':'  text*
            /  'Encrypted'         ':' word (word?);
                      

msg_id      :  '<' addr_spec '>'   ;






