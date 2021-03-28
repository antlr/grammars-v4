#!/bin/bash

# Test Erlang.g4 grammar against Erlang/OTP's *.erl sources
# Note: a preprocessing pass has to be done beforehand (creates *.P files)

get() {
    URL="$1" ; SRC="$2"
    which wget 1>/dev/null 2>/dev/null
    if [ $? -eq 0 ]; then
        wget $URL -O maint
    else
        curl -O $URL
    fi
    unzip maint
    rm -f maint
    mv otp-maint/ $SRC/
}


SRC="$1"
PPED="$2"
PPED=${PPED:="${SRC}/preprocessed"}

[[ ! -d $SRC/ ]] && get 'https://codeload.github.com/erlang/otp/zip/maint' $SRC

O=$SRC/output.txt
tot=$(echo $(find $SRC -name '*.erl' | wc -l))
echo -n "${tot} files to test. (stop with both ^C and ^G). SKIPPED <> preprocessing failed. "
[[ $tot -gt 100 ]] && printf "\e[1;4m%s\e[0m" 'This will take a while!'
[[ $tot -eq   0 ]] && printf "\e[1;4m%s\n\e[0m" "No $SRC/'*.erl' files to check on!" && exit 1
echo

j=$tot ; oks=0 ; skd=0
for file in `find $SRC -name '*.erl'`; do
    ## Remove random appearances of ‘’.
    #sed -i 's///g' $file
    COLUMNS=$(tput cols)
    dir=`dirname $file`
    ppdir=$PPED
    mkdir -p $ppdir

    pped=$ppdir/`basename ${file%.erl}.P`
    ## Preprocessor
    erlc -I $dir/../include -I ${file%.erl}_data/ -o $ppdir -P $file 1>/dev/null 2>/dev/null
    if [ $? -eq 0 ]; then

        ## Syntax
        java org.antlr.v4.gui.TestRig Erlang forms -encoding utf8 $pped 1>$O 2>>$O

        if [ -s $O ]; then # $O is not empty
            printf "%s \033[0;33m%s\033[00m\033[0;31m%$(($COLUMNS - ${#file} -${#j} -1))s\033[00m\n" $j $pped ERROR
            cat $O
        else # $O is empty
            printf "%s \033[0;33m%s\033[00m\033[0;32m%$(($COLUMNS - ${#file} -${#j} -1))s\033[00m\n" $j $pped PASSED
            ((oks++))
        fi

    else # Preprocessing failed
        # Reasons: some of the files to include are not built yet (C code…) ; -I option is wrong (unlikely).
        printf "%s \033[0;33m%s\033[00m\033[0;34m%$(($COLUMNS - ${#file} -${#j} -1))s\033[00m\n" $j $file SKIPPED
        ((skd++))
        ((oks++))
    fi
    ((j--))
done
rm -f $O
echo

lhs="$(($tot - $oks)) errors, checked: $tot, skipped: $skd"
printf "\e[1;4m%s\e[0m\033[1;1m%$(($COLUMNS - ${#lhs}))s\033[00m\n" "$lhs" "$((100 * $oks / $tot))%"
