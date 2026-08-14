#!/usr/bin/awk -f

## usage: month_to_num(month)
## converts human readable month to the decimal representation
## returns the number, -1 if the month doesn't exist
function month_to_num(mon,    months, m) {
  # populate months[] array
  months["january"] =  1; months["february"] =  2; months["march"]     =  3;
  months["april"]   =  4; months["may"]      =  5; months["june"]      =  6;
  months["july"]    =  7; months["august"]   =  8; months["september"] =  9;
  months["october"] = 10; months["november"] = 11; months["december"]  = 12;

  # also populate abbreviations
  for (m in months) {
    months[substr(m, 1, 3)] = months[m];
  }

  # convert month to lowercase
  mon = tolower(mon);

  # check if month exists
  if (mon in months) {
    return months[mon];
  } else {
    return -1;
  }
}

## usage: day_to_num(day)
## converts human readable day to the decimal representation
## returns the number, -1 if the day doesn't exist
## like date +%w, sunday is 0
function day_to_num(day,    days, d) {
  # populate days[] array
    days["sunday"]    = 0; days["monday"]   = 1; days["tuesday"] = 2;
    days["wednesday"] = 3; days["thursday"] = 4; days["friday"]  = 5;
    days["saturday"]  = 6;

  # also populate abbreviations
    days["sun"]   = 0; days["mon"] = 1; days["tues"] = 2; days["wed"] = 3;
    days["thurs"] = 4; days["fri"] = 5; days["sat"]  = 6;

  # convert day to lowercase
    day = tolower(day);

  # check if day exists
  if (day in days) {
    return days[day];
  } else {
    return -1;
  }
}

## usage: hr_to_sec(timestamp)
## converts HH:MM:SS or MM:SS to seconds
## returns -1 if invalid format
function hr_to_sec(time,    t, l, i, j) {
  # check for valid format
  if (time !~ /^[0-9]+(:[0-9][0-9])?:[0-9][0-9]$/) {
    return -1;
  }

  # convert
  l = split(time, t, /:/);
  
  j = time = 0;
  for (i=l; i>0; i--) {
    time += t[i] * (60 ^ j++);
  }

  return time;
}

## usage: sec_to_hr(seconds)
## converts seconds to HH:MM:SS
function sec_to_hr(sec,    m, s) {
  s = sec % 60;
  sec = int(sec / 60);
  m = sec % 60;
  sec = int(sec / 60);

  return sprintf("%02d:%02d:%02d", sec, m, s);
}

## usage: ms_to_hr(milliseconds)
## converts milliseconds to a "time(1)"-similar human readable format, such
## as 1m4.356s
function ms_to_hr(ms,    m, s, ns) {
  ms = ms / 1000;
  s = int(ms);
  m = int(s / 60);
  ns = s % 60;

  return sprintf("%dm%0.3fs", m, ns + (ms - s));
}

## usage: add_day_suff(day_of_month)
## prepends the appropriate suffix to "day_of_month". for example,
## add_day_suff(1) will return "1st", and add_day_suff(22) will return "22nd"
## returns -1 if "day_of_month" is not a positive integer
function add_day_suff(day) {
  # make sure day is a positive int
  if (day !~ /^[0-9]+$/ || day <= 0) {
    return -1;
  }

  # append prefix
  if ((day > 3 && day < 21) || day ~ /[04-9]$/) {
    return day "th";
  } else if (day ~ /1$/) {
    return day "st";
  } else if (day ~ /2$/) {
    return day "nd";
  } else {
    return day "rd";
  }
}




# You can do whatever you want with this stuff, but a thanks is always
# appreciated
