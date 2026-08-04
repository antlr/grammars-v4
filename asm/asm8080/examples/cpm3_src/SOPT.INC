$eject
check$choice: procedure(index,mindex) byte;
                                        /* does this modifier go with this
                                           option? */
        declare
                index   byte,
                mindex  byte;

        return(opt$mod(index).modifier(mindex));

end check$choice;

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


                    * * *  Option scanner  * * *


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


separator: procedure(character) byte;

                                        /* determines if character is a 
                                           delimiter and which one */
        declare k       byte,
                character       byte;

        k = 1;
loop:   if delimiters(k) = end$list then return(0);
        if delimiters(k) = character then return(k);    /* null = 25 */
                k = k + 1;
                go to loop;

end separator;

opt$scanner:    procedure(list$ptr,off$ptr) byte;

             /* list$ptr        -       pointer to list of known strings
                off$ptr         -       pointer to offsets into known string
                                        list
                buf$ptr         -       pointer to input string

                Scans the known string list for an occurrance of the input
                string.  If the input string is not found in the known list
                then return(0). Otherwise, return the index of the known string
                that matches the input.

                1. Find the known string that matches the input string on the 
                   first letter.

                        do i = 1 to #known_strings
                                if Known_string(i,1) = input(1) then do

                                   if length(Known_string(i)) < end_of_input
                                       then return(0)

                                   do j = 2 to end_of_input

                                        if Known_string(i,j) ~= input(j) then
                                                go to again
                                   end

                                   go to 2
                                end
                 again: end

                        return (0)              !no matchs

                2. Test to see if the input string does not match another Known
                   string.  This may happen if the input string is not a
                   unique sub-string of the Known string, ie., DI is a 
                   sub-string of DIRECTORY and DISK.

                        index = i

                        do i = index+1 to #known_strings
                                do j = 1 to end of input

                                        if Known_string(i,j) ~= input(j) then
                                                go to next
                                end

                                return(0)       !not unique
                next:   end;

                        return(index)           !unique substring 

                                P.Balma   10/82  */

        declare
                buff            based buf$ptr (1) byte,
                off$ptr         address,
                list$ptr        address;

        declare
                i               byte,
                j               byte,
                list            based list$ptr (1) byte,
                offsets         based off$ptr (1) byte,
                wrd$pos         byte,
                character       byte,
                letter$in$word  byte,
                found$first     byte,
                start           byte,
                index           byte,
                save$index      byte,
                (len$new,len$found)     byte,
                valid           byte;

/*****************************************************************************/
/*                      internal subroutines                                 */
/*****************************************************************************/

check$in$list: procedure;
                                /* find known string that has a match with 
                                   input on the first character.  Set index
                                   = invalid if none found.   */
                        
        declare i       byte;

        i = start;
        wrd$pos = offsets(i);
        do while list(wrd$pos) <> end$list;
                i = i + 1;
                index = i;
                if list(wrd$pos) = character then return;
                wrd$pos = offsets(i);
        end;
                        /* could not find character */
        index = 0;
        return;
end check$in$list;

setup:  procedure;
        character = buff(0);
        call check$in$list;
        letter$in$word = wrd$pos;
                        /* even though no match may have occurred, position
                           to next input character.  */
        i = 1;
        character = buff(1);
end setup;

test$letter:    procedure;
                        /* test each letter in input and known string */

        letter$in$word = letter$in$word + 1;

                                        /* too many chars input? 0 means
                                           past end of known string */
        if list(letter$in$word) = end$of$string then valid = false;
        else
        if list(letter$in$word) <> character then valid = false;

        i = i + 1;
        character = buff(i);

end test$letter;

skip:   procedure;
                                        /* scan past the offending string;
                                           position buf$ptr to next string...
                                           skip entire offending string;
                                           ie., falseopt=mod, [note: comma or
                                           space is considered to be group 
                                           delimiter] */
        character = buff(i);
        delimiter = separator(character);
        do while ((delimiter <> 2) and (delimiter <> 4) and (delimiter <> 5)
                   and (delimiter <> 25));
                i = i + 1;
                character = buff(i);
                delimiter = separator(character);
        end;
        endbuf = i;
        buf$ptr = buf$ptr + endbuf + 1;
        return;
end skip;

eat$blanks: procedure;

        declare charac  based buf$ptr byte;

        do while(delimiter := separator(charac)) = SPACE;
                buf$ptr = buf$ptr + 1;
        end;

end eat$blanks;

/*****************************************************************************/
/*                      end of internals                                     */
/*****************************************************************************/


                                        /* start of procedure */
        call eat$blanks;
        start = 0;
        call setup;

                                        /* match each character with the option
                                           for as many chars as input 
                                           Please note that due to the array
                                           indices being relative to 0 and the
                                           use of index both as a validity flag
                                           and as a index into the option/mods
                                           list, index is forced to be +1 as an
                                           index into array and 0 as a flag*/

        do while index <> 0;
                start = index;
                delimiter = separator(character);

                                        /* check up to input delimiter */

                valid = true;           /* test$letter resets this */
                do while delimiter = 0;
                        call test$letter;
                        if not valid then go to exit1;
                        delimiter = separator(character);
                end;

                go to good;

                                        /* input ~= this known string;
                                           get next known string that 
                                           matches */
exit1:          call setup;
        end;
                                        /* fell through from above, did
                                           not find a good match*/
        endbuf = i;                     /* skip over string & return*/
        call skip;
        return(0);

                                        /* is it a unique match in options
                                           list? */
good:   endbuf = i;
        len$found = endbuf;
        save$index = index;
        valid = false;
next$opt:
                start = index;
                call setup;
                if index = 0 then go to finished;

                                        /* look at other options and check
                                           uniqueness */

                len$new = offsets(index + 1) - offsets(index) - 1;
                if len$new = len$found then do;
                        valid = true;
                        do j = 1 to len$found;
                                call test$letter;
                                if not valid then go to next$opt;
                        end;
                end;
                else go to nextopt;
                                        /* fell through...found another valid
                                           match --> ambiguous reference */
        call skip;              /* skip input field to next delimiter*/
        return(0);

finished:                       /* unambiguous reference */
        buf$ptr = buf$ptr + endbuf;
        call eat$blanks;
        if delimiter <> 0 then buf$ptr = buf$ptr + 1;
        else delimiter = SPACE;

        return(save$index);

end opt$scanner;

error$prt:      procedure;
        declare i       byte,
                t       address,
                char    based t byte;

        t = buf$ptr - endbuf - 1;
        do i = 1 to endbuf;
                call printchar(char);
                t = t + 1;
        end;

end error$prt;

