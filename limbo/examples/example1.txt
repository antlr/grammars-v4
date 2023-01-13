implement Command;

 include "sys.m";
     sys: Sys;

 include "draw.m";

 include "sh.m";

 init(nil: ref Draw->Context, nil: list of string)
 {
     sys = load Sys Sys->PATH;
     sys->print("Hello World!\n");
 }
