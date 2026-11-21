
        declare 
                opt$mod(19) structure(modifier(8) byte)
                   data(1,1,1,0,0,0,0,0,                /* 0 access */
                        1,1,1,0,0,0,0,0,                /* 1 archive */
                        1,1,1,0,0,0,0,0,                /* 2 create */
                        1,0,0,0,0,0,0,1,                /* 3 default */
                        0,0,0,0,0,0,0,0,                /* 4 directory */
                        1,1,1,0,0,0,0,0,                /* 5 f1 */
                        1,1,1,0,0,0,0,0,
                        1,1,1,0,0,0,0,0,
                        1,1,1,0,0,0,0,0,
                        1,0,0,0,0,0,0,1,                /* 9 name */
                        1,0,0,0,0,0,0,1,                /* 10 password */
                        1,1,1,1,1,1,1,0,                /* 11 protect */
                        0,0,0,0,0,0,0,0,                /* 12 ro        */
                        0,0,0,0,0,0,0,0,                /* 13 rw        */
                        0,0,0,0,0,0,0,0,                /* 14 sys       */
                        1,1,1,0,0,0,0,0,                /* 15 update */
                        0,0,0,0,0,0,0,0,                /* 16 page */
                        0,0,0,0,0,0,0,0),               /* 17 nopage */

                options(*) byte
                        data('ACCESS0ARCHIVE0CREATE0DEFAULT0DIR0F10F20F30F40',
                             'NAME0PASSWORD0PROTECT0RO0RW0SYS',
                             '0UPDATE0PAGE0NOPAGE',0ffh),
                off$opt(20) byte data(0,7,15,22,30,34,37,40,43,46,51,60,68,71,
                                      74,78,85,90,96),
                mods(*) byte
                        data('OFF0ON0READ0WRITE0DELETE0NONE',0ffh),
                off$mods(7) byte data(0,4,7,12,18,25,29),

                end$list        byte data (0ffh),
                end$of$string   byte data(0),

                delimiters(*) byte data (0,'[]=, :;<>%\|"()/#!@&+-*?',0,0ffh),
                SPACE         byte data (5),    /* index in delim to space */
                RBRACKET      byte data(2),     /* ] in delim */
                ENDFF         byte data(25),
                EQUAL         byte data (3),
                LBRACKET      byte data (1),

                option$map(19)  byte,
                mods$map(19)    byte;

        declare
                sfamsg          byte initial(false),
                drvmsg          byte initial(false),
                j               byte initial(0),
                string$ptr      address,
                defpass         address,
                labname         address,
                passname        address,
                lendef          byte,
                lenpass         byte,
                lenlab          byte,
                buf$ptr         address,
                index           byte,
                endbuf          byte,
                mindex          byte,
                delimiter       byte;
$ eject

