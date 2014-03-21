/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2013-2014 by Bart Kiers
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */
 
/*
 * An iCalendar grammar for ANTLR v4 based on:
 * https://tools.ietf.org/html/rfc5545
 *
 * For more information, and unit tests, see the GitHub repository:
 * https://github.com/bkiers/ICalParser
 */
grammar ICalendar;

////////////////////////////// parser rules //////////////////////////////
parse
 : icalstream EOF
 ;

// 3.4 - iCalendar Object
icalstream
 : CRLF* icalobject (CRLF+ icalobject)* CRLF*
 ;

icalobject
 : k_begin COL k_vcalendar CRLF 
   calprop*? 
   component+?
   k_end COL k_vcalendar
 ;

calprop
 : prodid
 | version
 | calscale
 | method
 | x_prop
 | iana_prop
 ;

// 3.7.1 - Calendar Scale
calscale
 : k_calscale (SCOL other_param)* COL k_gregorian CRLF
 ;

// 3.7.2 - Method
method
 : k_method (SCOL other_param)* COL iana_token CRLF
 ;

// 3.7.3 - Product Identifier
prodid
 : k_prodid (SCOL other_param)* COL text CRLF
 ;

// 3.7.4 - Version
version
 : k_version (SCOL other_param)* COL vervalue CRLF
 ;

vervalue
 : float_num SCOL float_num
 | float_num
 ;

component
 : eventc 
 | todoc 
 | journalc 
 | freebusyc 
 | timezonec 
 | iana_comp 
 | x_comp
 ;

iana_comp
 : k_begin COL iana_token CRLF
   contentline+?
   k_end COL iana_token CRLF
 ;

x_comp
 : k_begin COL x_name CRLF
   contentline+?
   k_end COL x_name CRLF
 ;

contentline
 : name (SCOL icalparameter)* COL value CRLF
 ;

name 
 : iana_token
 | x_name
 ;

value
 : value_char*
 ;

// 3.6.1 - Event Component
eventc
 : k_begin COL k_vevent CRLF
   eventprop*?
   alarmc*?
   k_end COL k_vevent CRLF
 ;

// 3.6.2 - To-Do Component
todoc
 : k_begin COL k_vtodo CRLF
   todoprop*? 
   alarmc*?
   k_end COL k_vtodo CRLF
 ;

// 3.6.3 - Journal Component
journalc
 : k_begin COL k_vjournal CRLF
   jourprop*?
   k_end COL k_vjournal CRLF
 ;

// 3.6.4 - Free/Busy Component
freebusyc
 : k_begin COL k_vfreebusy CRLF
   fbprop*?
   k_end COL k_vfreebusy CRLF
 ;

// 3.6.5 - Time Zone Component
timezonec
 : k_begin COL k_vtimezone CRLF
   timezoneprop*?
   k_end COL k_vtimezone CRLF
 ;

// 3.6.6 - Alarm Component
alarmc
 : k_begin COL k_valarm CRLF
   alarmprop+?
   k_end COL k_valarm CRLF
 ;

eventprop
 : dtstamp
 | uid
 | dtstart
 | clazz
 | created
 | description
 | geo
 | last_mod
 | location
 | organizer
 | priority
 | seq
 | status
 | summary
 | transp
 | url
 | recurid
 | rrule
 | dtend
 | duration
 | attach
 | attendee
 | categories
 | comment
 | contact
 | exdate
 | rstatus
 | related
 | resources
 | rdate
 | x_prop
 | iana_prop
 ;

todoprop
 : dtstamp
 | uid
 | clazz
 | completed
 | created
 | description
 | dtstart
 | geo
 | last_mod
 | location
 | organizer
 | percent
 | priority
 | recurid
 | seq
 | status
 | summary
 | url
 | rrule
 | due
 | duration
 | attach
 | attendee
 | categories
 | comment
 | contact
 | exdate
 | rstatus
 | related
 | resources
 | rdate
 | x_prop
 | iana_prop
 ;

jourprop
 : dtstamp
 | uid
 | clazz
 | created
 | dtstart
 | last_mod
 | organizer
 | recurid
 | seq
 | status
 | summary
 | url
 | rrule
 | attach
 | attendee
 | categories
 | comment
 | contact
 | description
 | exdate
 | related
 | rdate
 | rstatus
 | x_prop
 | iana_prop
 ;

fbprop
 : dtstamp
 | uid
 | contact
 | dtstart
 | dtend
 | organizer
 | url
 | attendee
 | comment
 | freebusy
 | rstatus
 | x_prop
 | iana_prop
 ;

timezoneprop
 : tzid
 | last_mod
 | tzurl
 | standardc
 | daylightc
 | x_prop
 | iana_prop
 ;

tzprop
 : dtstart
 | tzoffsetto
 | tzoffsetfrom
 | rrule
 | comment
 | rdate
 | tzname
 | x_prop
 | iana_prop
 ;

alarmprop
 : action
 | description
 | trigger
 | summary
 | attendee
 | duration
 | repeat
 | attach
 | x_prop
 | iana_prop
 ;

standardc
 : k_begin COL k_standard CRLF
   tzprop*?
   k_end COL k_standard CRLF
 ;

daylightc
 : k_begin COL k_daylight CRLF
   tzprop*?
   k_end COL k_daylight CRLF
 ;

// 3.8.1.1 - Attachment
attach
 : k_attach attachparam* ( COL uri 
                         | SCOL k_encoding ASSIGN k_base D6 D4 SCOL k_value ASSIGN k_binary COL binary
                         )
   CRLF
 ;

attachparam
 : SCOL fmttypeparam
 | SCOL other_param
 ;

// 3.8.1.2 - Categories
categories
 : k_categories catparam* COL text (COMMA text)* CRLF
 ;

catparam
 : SCOL languageparam
 | SCOL other_param
 ;

// 3.8.1.3 - Classification
clazz
 : k_class (SCOL other_param)* COL classvalue CRLF
 ;

classvalue
 : k_public
 | k_private
 | k_confidential
 | iana_token
 | x_name
 ;

// 3.8.1.4 - Comment
comment
 : k_comment commparam* COL text CRLF
 ;

commparam
 : SCOL altrepparam
 | SCOL languageparam
 | SCOL other_param
 ;

// 3.8.1.5 - Description
description
 : k_description descparam* COL text CRLF
 ;

descparam
 : SCOL altrepparam
 | SCOL languageparam
 | SCOL other_param
 ;

// 3.8.1.6 - Geographic Position
geo
 : k_geo (SCOL other_param)* COL geovalue CRLF
 ;

geovalue
 : float_num SCOL float_num
 ;

// 3.8.1.7 - Location
location
 : k_location locparam* COL text CRLF
 ;

locparam
 : SCOL altrepparam
 | SCOL languageparam
 | SCOL other_param
 ;

// 3.8.1.8 - Percent Complete
percent
 : k_percent_complete (SCOL other_param)* COL integer CRLF
 ;

// 3.8.1.9 - Priority
priority
 : k_priority (SCOL other_param)* COL priovalue CRLF
 ;

priovalue
 : integer
 ;
               
// 3.8.1.10 - Resources
resources
 : k_resources resrcparam* COL text (COMMA text)* CRLF
 ;

resrcparam
 : SCOL altrepparam
 | SCOL languageparam
 | SCOL other_param
 ;

// 3.8.1.11 - Status
status
 : k_status (SCOL other_param)* COL statvalue CRLF
 ;

statvalue
 : statvalue_event
 | statvalue_todo
 | statvalue_jour
 ;

statvalue_event
 : k_tentative
 | k_confirmed
 | k_cancelled
 ;

statvalue_todo
 : k_needs_action
 | k_completed
 | k_in_progress
 | k_cancelled
 ;

statvalue_jour
 : k_draft
 | k_final
 | k_cancelled
 ;

// 3.8.1.12 - Summary
summary
 : k_summary summparam* COL text CRLF
 ;

summparam
 : SCOL altrepparam
 | SCOL languageparam
 | SCOL other_param
 ;

// 3.8.2.1 - Date-Time Completed
completed
 : k_completed (SCOL other_param)* COL date_time CRLF
 ;

// 3.8.2.2 - Date-Time End
dtend
 : k_dtend dtendparam* COL date_time_date CRLF
 ;

dtendparam
 : SCOL k_value ASSIGN k_date_time
 | SCOL k_value ASSIGN k_date
 | SCOL tzidparam
 | SCOL other_param
 ;

// 3.8.2.3 - Date-Time Due
due
 : k_due dueparam* COL date_time_date CRLF
 ;

dueparam
 : SCOL k_value ASSIGN k_date_time
 | SCOL k_value ASSIGN k_date
 | SCOL tzidparam
 | SCOL other_param
 ;

// 3.8.2.4 - Date-Time Start
dtstart
 : k_dtstart dtstparam* COL date_time_date CRLF
 ;

dtstparam
 : SCOL k_value ASSIGN k_date_time
 | SCOL k_value ASSIGN k_date
 | SCOL tzidparam
 | SCOL other_param
 ;

// 3.8.2.5 - Duration
duration
 : k_duration (SCOL other_param)* COL dur_value CRLF
 ;

// 3.8.2.6 - Free/Busy Time
freebusy
 : k_freebusy fbparam* COL fbvalue CRLF
 ;

fbparam
 : SCOL fbtypeparam
 | SCOL other_param 
 ;

fbvalue
 : period (COMMA period)*
 ;

// 3.8.2.7 - Time Transparency
transp
 : k_transp (SCOL other_param)* COL transvalue CRLF
 ;

transvalue
 : k_opaque
 | k_transparent
 ;

// 3.8.3.1 - Time Zone Identifier
tzid
 : k_tzid (SCOL other_param)* COL FSLASH? text CRLF
 ;

// 3.8.3.2.  Time Zone Name
tzname
 : k_tzname tznparam* COL text CRLF
 ;

tznparam
 : SCOL languageparam
 | SCOL other_param
 ;

// 3.8.3.3 - Time Zone Offset From
tzoffsetfrom
 : k_tzoffsetfrom (SCOL other_param)* COL utc_offset CRLF
 ;

// 3.8.3.4 - Time Zone Offset To
tzoffsetto
 : k_tzoffsetto (SCOL other_param)* COL utc_offset CRLF
 ;

// 3.8.3.5.  Time Zone URL
tzurl
 : k_tzurl (SCOL other_param)* COL uri CRLF
 ;

// 3.8.4.1 - Attendee
attendee
 : k_attendee attparam* COL cal_address CRLF
 ;

attparam
 : SCOL cutypeparam
 | SCOL memberparam
 | SCOL roleparam
 | SCOL partstatparam
 | SCOL rsvpparam
 | SCOL deltoparam
 | SCOL delfromparam
 | SCOL sentbyparam
 | SCOL cnparam
 | SCOL dirparam
 | SCOL languageparam
 | SCOL other_param
 ;

// 3.8.4.2 - Contact
contact
 : k_contact contparam* COL text CRLF
 ;

contparam
 : SCOL altrepparam
 | SCOL languageparam
 | SCOL other_param
 ;

// 3.8.4.3 - Organizer
organizer
 : k_organizer orgparam* COL cal_address CRLF
 ;

orgparam
 : SCOL cnparam
 | SCOL dirparam
 | SCOL sentbyparam
 | SCOL languageparam
 | SCOL other_param
 ;

// 3.8.4.4 - Recurrence ID
recurid
 : k_recurrence_id ridparam* COL date_time_date CRLF
 ;

ridparam
 : SCOL k_value ASSIGN k_date_time
 | SCOL k_value ASSIGN k_date
 | SCOL tzidparam
 | SCOL rangeparam
 | SCOL other_param
 ;

// 3.8.4.5.  Related To
related
 : k_related_to relparam* COL text CRLF
 ;

relparam
 : SCOL reltypeparam
 | SCOL other_param
 ;

// 3.8.4.6 - Uniform Resource Locator
url
 : k_url (SCOL other_param)* COL uri CRLF
 ;

// 3.8.4.7 - Unique Identifier
uid
 : k_uid (SCOL other_param)* COL text CRLF
 ;

// 3.8.5.1 - Exception Date-Times
exdate
 : k_exdate exdtparam* COL date_time_date (COMMA date_time_date)* CRLF
 ;

exdtparam
 : SCOL k_value ASSIGN k_date_time
 | SCOL k_value ASSIGN k_date
 | SCOL tzidparam
 | SCOL other_param
 ;

// 3.8.5.2 - Recurrence Date-Times
rdate
 : k_rdate rdtparam* COL rdtval (COMMA rdtval)* CRLF
 ;

rdtparam
 : SCOL k_value ASSIGN k_date_time
 | SCOL k_value ASSIGN k_date
 | SCOL k_value ASSIGN k_period
 | SCOL tzidparam
 | SCOL other_param
 ;

rdtval
 : date_time
 | date
 | period
 ;

date_time_date
 : date_time
 | date
 ;

// 3.8.5.3 - Recurrence Rule
rrule
 : k_rrule (SCOL other_param)* COL recur CRLF
 ;

// 3.8.6.1 - Action
action
 : k_action (SCOL other_param)* COL actionvalue CRLF
 ;

actionvalue
 : k_audio
 | k_display
 | k_email
 | iana_token
 | x_name
 ;

// 3.8.6.2 - Repeat Count
repeat
 : k_repeat (SCOL other_param)* COL integer CRLF
 ;

// 3.8.6.3 - Trigger
trigger
 : k_trigger trigrel* COL dur_value CRLF
 | k_trigger trigabs* COL date_time CRLF
 ;

trigrel
 : SCOL k_value ASSIGN  k_duration
 | SCOL trigrelparam
 | SCOL other_param
 ;

trigabs
 : SCOL k_value ASSIGN k_date_time
 | SCOL other_param  
 ;
           
// 3.8.7.1 - Date-Time Created
created
 : k_created (SCOL other_param)* COL date_time CRLF
 ;

// 3.8.7.2 - Date-Time Stamp
dtstamp
 : k_dtstamp (SCOL other_param)* COL date_time CRLF
 ;

// 3.8.7.3 - Last Modified
last_mod
 : k_last_modified (SCOL other_param)* COL date_time CRLF
 ;

// 3.8.7.4 - Sequence Number
seq
 : k_sequence (SCOL other_param)* COL integer CRLF
 ;

// 3.8.8.1 - IANA Properties
iana_prop
 : iana_token (SCOL icalparameter)* COL value CRLF
 ;

// 3.8.8.2 - Non-Standard Propertie
x_prop
 : x_name (SCOL icalparameter)* COL value CRLF
 ;

// 3.8.8.3 - Request Status
rstatus
 : k_request_status rstatparam* COL statcode SCOL text (SCOL text)?
 ;

rstatparam
 : SCOL languageparam
 | SCOL other_param
 ;

statcode
 : digit+ DOT digit+ (DOT digit+)?
 ;

param_name
 : iana_token
 | x_name
 ;

param_value
 : paramtext
 | quoted_string
 ;

paramtext
 : safe_char*
 ;

quoted_string
 : DQUOTE qsafe_char* DQUOTE
 ;
  
// iCalendar identifier registered with IANA
iana_token
 : (alpha | MINUS)+
 ;

// 3.2
icalparameter
 : altrepparam
 | cnparam
 | cutypeparam
 | delfromparam
 | deltoparam
 | dirparam
 | encodingparam
 | fmttypeparam
 | fbtypeparam
 | languageparam
 | memberparam
 | partstatparam
 | rangeparam
 | trigrelparam
 | reltypeparam
 | roleparam
 | rsvpparam
 | sentbyparam
 | tzidparam
 | valuetypeparam
 | other_param
 ;

// 3.2.1
altrepparam
 : k_altrep ASSIGN DQUOTE uri DQUOTE
 ;

// 3.2.2
cnparam
 : k_cn ASSIGN param_value
 ;

// 3.2.3
cutypeparam
 : k_cutype ASSIGN ( k_individual
                   | k_group
                   | k_resource
                   | k_room
                   | k_unknown
                   | x_name
                   | iana_token
                   )
 ;

// 3.2.4
delfromparam
 : k_delegated_from ASSIGN DQUOTE cal_address DQUOTE (COMMA DQUOTE cal_address DQUOTE)*
 ;

// 3.2.5
deltoparam
 : k_delegated_to ASSIGN DQUOTE cal_address DQUOTE (COMMA DQUOTE cal_address DQUOTE)*
 ;

// 3.2.6
dirparam
 : k_dir ASSIGN DQUOTE uri DQUOTE
 ;

// 3.2.7
encodingparam
 : k_encoding ASSIGN ( D8 k_bit
                     | k_base D6 D4
                     )
 ;

// 3.2.8
fmttypeparam
 : k_fmttype ASSIGN type_name FSLASH subtype_name
 ;

// 3.2.9
fbtypeparam
 : k_fbtype ASSIGN ( k_free
                   | k_busy
                   | k_busy_unavailable
                   | k_busy_tentative
                   | x_name
                   | iana_token
                   )
 ;

// 3.2.10
languageparam
 : k_language ASSIGN language
 ;

// 3.2.11
memberparam
 : k_member ASSIGN DQUOTE cal_address DQUOTE (COMMA DQUOTE cal_address DQUOTE)*
 ;

// 3.2.12
partstatparam
 : k_partstat ASSIGN ( partstat_event
                     | partstat_todo
                     | partstat_jour
                     )
 ;

// 3.2.13
rangeparam
 : k_range ASSIGN k_thisandfuture
 ;

// 3.2.14
trigrelparam
 : k_related ASSIGN ( k_start
                    | k_end
                    )
 ;

// 3.2.15
reltypeparam
 : k_reltype ASSIGN ( k_parent
                    | k_child
                    | k_sibling
                    | x_name
                    | iana_token
                    )
 ;

// 3.2.16
roleparam
 : k_role ASSIGN ( k_chair
                 | k_req_participant
                 | k_opt_participant 
                 | k_non_participant
                 | iana_token
                 | x_name
                 )
 ;

// 3.2.17
rsvpparam
 : k_rsvp ASSIGN ( k_true
                 | k_false
                 )
 ;

// 3.2.18
sentbyparam
 : k_sent_by ASSIGN DQUOTE cal_address DQUOTE
 ;

// 3.2.19
tzidparam
 : k_tzid ASSIGN FSLASH? paramtext
 ;

// 3.2.20
valuetypeparam
 : k_value ASSIGN valuetype
 ;

valuetype
 : k_binary
 | k_boolean
 | k_cal_address
 | k_date
 | k_date_time
 | k_duration
 | k_float
 | k_integer
 | k_period
 | k_recur
 | k_text
 | k_time
 | k_uri
 | k_utc_offset
 | x_name
 | iana_token
 ;

// 3.3.1 - A "BASE64" encoded character string, as defined by [RFC4648].
binary
 : b_chars b_end?
 ;

b_chars
 : b_char*
 ;

b_end
 : ASSIGN ASSIGN?
 ;

// 3.3.2
bool
 : k_true
 | k_false
 ;

// 3.3.3
cal_address
 : uri
 ;

// 3.3.4
date
 : date_value
 ;

// 3.3.5
date_time
 : date T time
 ;

// 3.3.6 
dur_value
 : MINUS P (dur_date | dur_time | dur_week)
 | PLUS? P (dur_date | dur_time | dur_week)
 ;

// 3.3.7
float_num
 : MINUS digits (DOT digits)?
 | PLUS? digits (DOT digits)?
 ;

digits
 : digit+
 ;

// 3.3.8
integer
 : MINUS digits
 | PLUS? digits
 ;

// 3.3.9
period
 : period_explicit
 | period_start
 ;

// 3.3.10
recur
 : recur_rule_part (SCOL recur_rule_part)*
 ;

// 3.3.11
text
 : (tsafe_char | COL | DQUOTE | ESCAPED_CHAR)*
 ;

// 3.3.12
time
 : time_hour time_minute time_second Z?
 ;

// 3.3.13 - As defined in Section 3 of [RFC3986].
uri
 : qsafe_char+
 ;

// 3.3.14
utc_offset
 : time_numzone
 ;

// Applications MUST ignore x-param and iana-param values they don't
// recognize.
other_param
 : iana_param
 | x_param
 ;

// Some other IANA-registered iCalendar parameter.
iana_param
 : iana_token ASSIGN param_value (COMMA param_value)*
 ;

// A non-standard, experimental parameter.
x_param
 : x_name ASSIGN param_value (COMMA param_value)*
 ;

// As defined in Section 4.2 of [RFC4288].
type_name
 : reg_name
 ;

// As defined in Section 4.2 of [RFC4288].
subtype_name
 : reg_name
 ;

// Between 1 and 127 chars allowed as defined in Section 4.2 of [RFC4288].
reg_name
 : reg_name_char+
 ;

// Loosely matched language (see [RFC5646]).
language
 : language_char+
 ;

partstat_event
 : k_needs_action
 | k_accepted
 | k_declined
 | k_tentative
 | k_delegated
 | x_name
 | iana_token
 ;

partstat_todo
 : k_needs_action
 | k_accepted
 | k_declined
 | k_tentative
 | k_delegated
 | k_completed
 | k_in_progress
 | x_name
 | iana_token
 ;

partstat_jour
 : k_needs_action
 | k_accepted
 | k_declined
 | x_name
 | iana_token
 ;

b_char
 : alpha
 | digit
 | PLUS
 | FSLASH
 ;

date_value
 : date_fullyear date_month date_mday
 ;

date_fullyear
 : digits_2 digits_2
 ;

date_month
 : digits_2
 ;

date_mday
 : digits_2
 ;

time_hour
 : digits_2
 ;

time_minute
 : digits_2
 ;

time_second
 : digits_2
 ;

dur_date
 : dur_day dur_time?
 ;

dur_day
 : digit+ D
 ;

dur_time
 : T? (dur_hour | dur_minute | dur_second)
 ;

dur_week
 : digit+ W
 ;

dur_hour
 : digit+ H dur_minute?
 ;

dur_minute
 : digit+ M dur_second?
 ;

dur_second
 : digit+ S
 ;

period_explicit
 : date_time FSLASH date_time
 ;

period_start
 : date_time FSLASH dur_value
 ;

recur_rule_part
 : k_freq ASSIGN freq
 | k_until ASSIGN enddate
 | k_count ASSIGN count
 | k_interval ASSIGN interval
 | k_bysecond ASSIGN byseclist
 | k_byminute ASSIGN byminlist
 | k_byhour ASSIGN byhrlist
 | k_byday ASSIGN bywdaylist
 | k_bymonthday ASSIGN bymodaylist
 | k_byyearday ASSIGN byyrdaylist
 | k_byweekno ASSIGN bywknolist
 | k_bymonth ASSIGN bymolist
 | k_bysetpos ASSIGN bysplist
 | k_wkst ASSIGN weekday
 ;

freq
 : k_secondly
 | k_minutely
 | k_hourly
 | k_daily
 | k_weekly
 | k_monthly
 | k_yearly
 ;

enddate
 : date 
 | date_time
 ;

count
 : digits
 ;

interval
 : digits
 ;

byseclist
 : digits_1_2 (COMMA digits_1_2)*
 ;

byminlist
 : digits_1_2 (COMMA digits_1_2)*
 ;

byhrlist
 : digits_1_2 (COMMA digits_1_2)*
 ;

bywdaylist
 : weekdaynum (COMMA weekdaynum)*
 ;

weekdaynum
 : ((PLUS | MINUS)? digits_1_2)? weekday
 ;

weekday
 : S U
 | M O
 | T U
 | W E
 | T H
 | F R
 | S A
 ;

bymodaylist
 : monthdaynum (COMMA monthdaynum)*
 ;

monthdaynum
 : (PLUS | MINUS)? digits_1_2
 ;

byyrdaylist
 : yeardaynum (COMMA yeardaynum)*
 ;

yeardaynum
 : (PLUS | MINUS)? ordyrday
 ;

ordyrday
 : digit (digit digit?)?
 ;

bywknolist
 : weeknum (COMMA weeknum)*
 ;

weeknum
 : (PLUS | MINUS)? digits_1_2
 ;

bymolist
 : digits_1_2 (COMMA digits_1_2)*
 ;

bysplist
 : yeardaynum (COMMA yeardaynum)*
 ;

digits_2
 : digit digit
 ;

digits_1_2
 : digit digit?
 ;

// Any character except CONTROL, DQUOTE, ";", ":", ","
safe_char
 : ~(CRLF | CONTROL | DQUOTE | SCOL | COL | COMMA)
 ;

// Any textual character
value_char
 : ~(CRLF | CONTROL | ESCAPED_CHAR)
 ;

// Any character except CONTROL and DQUOTE
qsafe_char
 : ~(CRLF | CONTROL | DQUOTE)
 ;

// Any character except CONTROLs not needed by the current
// character set, DQUOTE, ";", ":", "\", ","
tsafe_char
 : ~(CRLF | CONTROL | DQUOTE | SCOL | COL | BSLASH | COMMA)
 ;

time_numzone
 : (PLUS | MINUS) time_hour time_minute time_second?
 ;

reg_name_char
 : alpha
 | digit
 | EXCLAMATION
 | HASH
 | DOLLAR
 | AMP
 | DOT
 | PLUS
 | MINUS
 | CARET
 | USCORE
 ;

language_char
 : alpha
 | digit
 | MINUS
 | COL
 | WSP
 ;

// Reserved for experimental use.
x_name
 : X (alpha_num alpha_num alpha_num+ MINUS)? (alpha_num | MINUS)+
 ;

alpha_num
 : alpha
 | digit
 ;

// The digits: 0..9
digit
 : D0 
 | D1 
 | D2 
 | D3 
 | D4 
 | D5 
 | D6 
 | D7 
 | D8 
 | D9
 ;

// Any alpha char
alpha
 : A
 | B
 | C
 | D
 | E
 | F
 | G
 | H
 | I
 | J
 | K
 | L
 | M
 | N
 | O
 | P
 | Q
 | R
 | S
 | T
 | U
 | V
 | W
 | X
 | Y
 | Z
 ;

// Case insensitive keywords
k_accepted : A C C E P T E D;
k_action : A C T I O N;
k_address : A D D R E S S;
k_altrep : A L T R E P;
k_attach : A T T A C H;
k_attendee : A T T E N D E E;
k_audio : A U D I O;
k_base : B A S E;
k_begin : B E G I N;
k_binary : B I N A R Y;
k_bit : B I T;
k_boolean : B O O L E A N;
k_busy : B U S Y;
k_busy_unavailable : B U S Y MINUS U N A V A I L A B L E;
k_busy_tentative : B U S Y MINUS T E N T A T I V E;
k_byday : B Y D A Y;
k_byhour : B Y H O U R;
k_byminute : B Y M I N U T E;
k_bymonth : B Y M O N T H;
k_bymonthday : B Y M O N T H D A Y;
k_bysecond : B Y S E C O N D;
k_bysetpos : B Y S E T P O S;
k_byweekno : B Y W E E K N O;
k_byyearday : B Y Y E A R D A Y;
k_cal_address : C A L MINUS A D D R E S S;
k_calscale : C A L S C A L E;
k_cancelled : C A N C E L L E D;
k_categories : C A T E G O R I E S;
k_chair : C H A I R;
k_child : C H I L D;
k_class : C L A S S;
k_cn : C N;
k_comment : C O M M E N T;
k_completed : C O M P L E T E D;
k_confidential : C O N F I D E N T I A L;
k_confirmed : C O N F I R M E D;
k_contact : C O N T A C T;
k_count : C O U N T;
k_created : C R E A T E D;
k_cutype : C U T Y P E;
k_daily : D A I L Y;
k_date : D A T E;
k_date_time : D A T E MINUS T I M E;
k_daylight : D A Y L I G H T;
k_declined : D E C L I N E D;
k_delegated : D E L E G A T E D;
k_delegated_from : D E L E G A T E D MINUS F R O M;
k_delegated_to : D E L E G A T E D MINUS T O;
k_description : D E S C R I P T I O N;
k_dir : D I R;
k_display : D I S P L A Y;
k_draft : D R A F T;
k_dtend : D T E N D;
k_dtstamp : D T S T A M P;
k_dtstart : D T S T A R T;
k_due : D U E;
k_duration : D U R A T I O N;
k_email : E M A I L;
k_encoding : E N C O D I N G;
k_end : E N D;
k_exdate : E X D A T E;
k_false : F A L S E;
k_fbtype : F B T Y P E;
k_final : F I N A L;
k_float : F L O A T;
k_fmttype : F M T T Y P E;
k_fr : F R;
k_free : F R E E;
k_freebusy : F R E E B U S Y;
k_freq : F R E Q;
k_geo : G E O;
k_gregorian : G R E G O R I A N;
k_group : G R O U P;
k_hourly : H O U R L Y;
k_in_progress : I N MINUS P R O G R E S S;
k_individual : I N D I V I D U A L;
k_integer : I N T E G E R;
k_interval : I N T E R V A L;
k_language : L A N G U A G E;
k_last_modified : L A S T MINUS M O D I F I E D;
k_location : L O C A T I O N;
k_member : M E M B E R;
k_method : M E T H O D;
k_minutely : M I N U T E L Y;
k_mo : M O;
k_monthly : M O N T H L Y;
k_needs_action : N E E D S MINUS A C T I O N;
k_non_participant : N O N MINUS P A R T I C I P A N T;
k_opaque : O P A Q U E;
k_opt_participant : O P T MINUS P A R T I C I P A N T;
k_organizer : O R G A N I Z E R;
k_parent : P A R E N T;
k_participant : P A R T I C I P A N T;
k_partstat : P A R T S T A T;
k_percent_complete : P E R C E N T MINUS C O M P L E T E;
k_period : P E R I O D;
k_priority : P R I O R I T Y;
k_private : P R I V A T E;
k_process : P R O C E S S;
k_prodid : P R O D I D;
k_public : P U B L I C;
k_range : R A N G E;
k_rdate : R D A T E;
k_recur : R E C U R;
k_recurrence_id : R E C U R R E N C E MINUS I D;
k_relat : R E L A T;
k_related : R E L A T E D;
k_related_to : R E L A T E D MINUS T O;
k_reltype : R E L T Y P E;
k_repeat : R E P E A T;
k_req_participant : R E Q MINUS P A R T I C I P A N T;
k_request_status : R E Q U E S T MINUS S T A T U S;
k_resource : R E S O U R C E;
k_resources : R E S O U R C E S;
k_role : R O L E;
k_room : R O O M;
k_rrule : R R U L E;
k_rsvp : R S V P;
k_sa : S A;
k_secondly : S E C O N D L Y;
k_sent_by : S E N T MINUS B Y;
k_sequence : S E Q U E N C E;
k_sibling : S I B L I N G;
k_standard : S T A N D A R D;
k_start : S T A R T;
k_status : S T A T U S;
k_su : S U;
k_summary : S U M M A R Y;
k_tentative : T E N T A T I V E;
k_text : T E X T;
k_th : T H;
k_thisandfuture : T H I S A N D F U T U R E;
k_time : T I M E;
k_transp : T R A N S P;
k_transparent : T R A N S P A R E N T;
k_trigger : T R I G G E R;
k_true : T R U E;
k_tu : T U;
k_tzid : T Z I D;
k_tzname : T Z N A M E;
k_tzoffsetfrom : T Z O F F S E T F R O M;
k_tzoffsetto : T Z O F F S E T T O;
k_tzurl : T Z U R L;
k_uid : U I D;
k_unknown : U N K N O W N;
k_until : U N T I L;
k_uri : U R I;
k_url : U R L;
k_utc_offset : U T C MINUS O F F S E T;
k_valarm : V A L A R M;
k_value : V A L U E;
k_vcalendar : V C A L E N D A R;
k_version : V E R S I O N;
k_vevent : V E V E N T;
k_vfreebusy : V F R E E B U S Y;
k_vjournal : V J O U R N A L;
k_vtimezone : V T I M E Z O N E;
k_vtodo : V T O D O;
k_we : W E;
k_weekly : W E E K L Y;
k_wkst : W K S T;
k_yearly : Y E A R L Y;

////////////////////////////// lexer rules //////////////////////////////
LINE_FOLD
 : CRLF WSP -> skip
 ;

WSP
 : ' '
 | '\t'
 ;

ESCAPED_CHAR
 : '\\' (CRLF WSP)? '\\'
 | '\\' (CRLF WSP)? ';'
 | '\\' (CRLF WSP)? ','
 | '\\' (CRLF WSP)? N
 ;

CRLF
 : '\r'? '\n' 
 | '\r'
 ;

// All the ASCII controls except HTAB and CRLF
CONTROL
 : [\u0000-\u0008]
 | [\u000B-\u000C]
 | [\u000E-\u001F]
 | [\u007F]
 ;

A : [aA];
B : [bB];
C : [cC];
D : [dD];
E : [eE];
F : [fF];
G : [gG];
H : [hH];
I : [iI];
J : [jJ];
K : [kK];
L : [lL];
M : [mM];
N : [nN];
O : [oO];
P : [pP];
Q : [qQ];
R : [rR];
S : [sS];
T : [tT];
U : [uU];
V : [vV];
W : [wW];
X : [xX];
Y : [yY];
Z : [zZ];

EXCLAMATION : '!';
DQUOTE : '"';
HASH : '#';
DOLLAR : '$';
X25 : '%';
AMP : '&';
X27 : '\'';
X28 : '(';
X29 : ')';
X2A : '*';
PLUS : '+';
COMMA : ',';
MINUS : '-';
DOT : '.';
FSLASH : '/';
D0 : '0';
D1 : '1';
D2 : '2';
D3 : '3';
D4 : '4';
D5 : '5';
D6 : '6';
D7 : '7';
D8 : '8';
D9 : '9';
COL : ':';
SCOL : ';';
X3C : '<';
ASSIGN : '=';
X3E : '>';
X3F : '?';
X40 : '@';
X5B : '[';
BSLASH : '\\';
X5D : ']';
CARET : '^';
USCORE : '_';
X60 : '`';
X7B : '{';
X7C : '|';
X7D : '}';
X7E : '~';

NON_US_ASCII
 : .
 ;
