#

# set -x
# set -e

# Check requirements.
if ! command -v dotnet &> /dev/null
then
    echo "'dotnet' could not be found. Install Microsoft NET."
    exit 1
fi
if ! command -v trxml2 &> /dev/null
then
    local=1
fi
if ! command -v dotnet trxml2 -- --version &> /dev/null
then
    echo "'dotnet' could not be found. Install Microsoft NET."
    exit 1
fi

cwd=`pwd`

while getopts 'a:b:' opt; do
    case "$opt" in
        a)
            after="${OPTARG}"
            ;;
        b)
            before="${OPTARG}"
            ;;
    esac
done

if [ "$after" == "" ]
then
    echo "'after' not set."
    exit 1
fi
if [ "$before" == "" ]
then
    echo "'before' not set."
    exit 1
fi

#############################
#############################
# Get last commit/pr. Note, some of the PR merges don't
# fit the pattern, but we'll ignore them. Get "prior" commit before all these
# changes.
prs=( After Before )
com=( $after $before )
echo PRS = ${prs[@]}
echo COM = ${com[@]}
echo '#PRS' = ${#prs[@]}

# Clean up.
for ((i=0; i<${#prs[@]}; i++))
do
    rm -rf "$cwd/${prs[$i]}"
done
rm -rf `find . -name 'Generated-*'`

# The PR that is more recent is the first in the list.
# Get grammars changed for current PR. This will focus exactly on what to
# test. Note, we only consider .g4 changes, no examples, no duplicates.
tests=()
changes=`git diff --name-only ${com[0]} ${com[1]} | grep '[.]g4$' | sed 's#\(.*\)[/][^/]*$#\1#' | sort -u | grep -v _scripts | fgrep -v .github | fgrep -v examples | sort -u | tr -d '\r'`
echo Changed files = $changes

#############################
echo Computing grammars changed...
prefix=`pwd`
for g in ${changes[@]}
do
    if [ ! -d "$g" ]; then continue; fi
    pushd $g > /dev/null 2> /dev/null
    while true
    do
        if [ -f `pwd`/desc.xml ]
        then
            break
        elif [ `pwd` == "$prefix" ]
        then
            break
        fi
        cd ..
    done
    g=`pwd`
    g=${g##*$prefix}
    g=${g##/}
    if [ "$g" == "" ]; then continue; fi
    if [ -f desc.xml ]
    then
        if [ "$local" == "" ]
        then
            gtargets=`trxml2 desc.xml | fgrep -e '/desc/targets' | awk -F '=' '{print $2}' | tr ';' '\n' | fgrep -e 'Java' | fgrep -v 'JavaScript'`
        else
            gtargets=`dotnet trxml2 -- desc.xml | fgrep -e '/desc/targets' | awk -F '=' '{print $2}' | tr ';' '\n' | fgrep -e 'Java' | fgrep -v 'JavaScript'`
        fi
        if [ "$gtargets" == "" ]; then continue; fi
    fi
    tests=( ${tests[@]} $g )
    popd > /dev/null
done
echo Grammars to test = ${tests[@]}

#############################
echo Sanity checks.
if [ "${#tests[@]}" == "0" ]
then
    echo "No grammars to test."
    exit 0
fi
for ((i=0; i<${#com[@]}; i++))
do
    git checkout ${com[$i]}
    for ((g=0; g<${#tests[@]}; g++))
    do
        if [ ! -d ${tests[$g]} ]
        then
            echo "New grammar, cannot test against an old grammar."
            exit 0
        fi
    done
done

#############################
echo Build each grammar changed in PR.
for ((i=0; i<${#prs[@]}; i++))
do
    rm -rf "$cwd/${prs[$i]}"
    mkdir "$cwd/${prs[$i]}"
    git checkout ${com[$i]}
    for g in ${tests[@]}
    do
        echo Grammar $g
        pushd $g
        gg=`echo $g | tr '/' '-'`
        if [ "$local" == "" ]
        then
            trgen -t CSharp
            if [ "$?" != "0" ]
            then
                echo "Build failed. Stopping test."
                exit 0
            fi
        else
            dotnet trgen -- -t CSharp
            if [ "$?" != "0" ]
            then
                echo "Build failed. Stopping test."
                exit 0
            fi
        fi
        where=`echo Generated-CSharp* | tr ' ' '\n' | head -1`
        echo $where
        cd $where
        make
        if [ "$?" != "0" ]
        then
            echo "Build failed. Stopping test."
            exit 0
        fi
        popd
    cp -r $g "$cwd/${prs[$i]}/$gg"
    done
done

#===========================
echo Test each grammar and PR in turn.
for g in ${tests[@]}
do
    echo Grammar $g
    rm -f "$cwd"/p[0-1]*
    gg=`echo $g | tr '/' '-'`
    what=()
    for ((i=0; i<${#prs[@]}; i++))
    do
        pushd "$cwd/${prs[$i]}/$gg"
        where=`echo Generated-CSharp* | tr ' ' '\n' | head -1`
        echo $where
        cd $where
        if [ "${#what[@]}" -eq 0 ]
        then
            if [ "$local" == "" ]
            then
                what=`trxml2 desc.xml | grep inputs | head -1 | sed 's%^[^=]*=%%'`
            else
                what=`dotnet trxml2 -- desc.xml | grep inputs | head -1 | sed 's%^[^=]*=%%'`
            fi
            if [ "$what" == "" ]
            then
                dir=`pwd`
                p=`realpath -s --relative-to=$dir "$cwd/${prs[1]}/$gg/examples"`
                if [ "$local" == "" ]
                then
                    what=( `trglob $p | grep -v '.errors$' | grep -v '.tree$'` )
                else
                    what=( `dotnet trglob -- $p | grep -v '.errors$' | grep -v '.tree$'` )
                fi
            else
                dir=`pwd`
                p=`realpath -s --relative-to=$dir "$cwd/${prs[1]}/$gg/$what"`
                if [ "$local" == "" ]
                then
                    what=( `trglob $p | grep -v '.errors$' | grep -v '.tree$'` )
                else
                    what=( `dotnet trglob -- $p | grep -v '.errors$' | grep -v '.tree$'` )
                fi
            fi
            echo what = $what
            newwhat=()
            for f in ${what[@]}
            do
                if [ -d $f ]; then continue; fi
                newwhat=( ${newwhat[@]} $f )
            done
            what=( ${newwhat[@]} )
            if [ ${#what[@]} -eq 0 ]; then popd; continue; fi
        fi
        # Try first and scale number of times to work in 10 minutes tops.
        # Format is in seconds, in floating point format.
        runtime=`bash run.sh ${what[@]} 2>&1 | grep "Total Time" | awk '{print $3}'`
        times=`python -c "print(int(min(40,600/$runtime)))"`
        for ((j=1;j<=times;j++)); do
            bash run.sh ${what[@]} 2>&1 | grep "Total Time" | awk '{print $3}' >> "$cwd/p$i-$gg.txt"
        done
        popd
    done
    
    echo Graphing out.
    cd $cwd
    rm -f xx.m
    echo "pkg load statistics" >> xx.m
    for ((i=0; i<${#prs[@]}; i++))
    do
        echo "p$i=["`cat "$cwd/p$i-$gg.txt"`"];" >> xx.m
        echo "mp$i=mean(p$i);" >> xx.m
        echo "sd$i=std(p$i);" >> xx.m
        echo "printf('disp($i)\n');" >> xx.m
        echo "disp($i);" >> xx.m
        echo "printf('disp(p$i)\n');" >> xx.m
        echo "disp(p$i);" >> xx.m
        echo "printf('mp$i = %f\n', mp$i);" >> xx.m
        echo "printf('sd$i = %f\n', sd$i);" >> xx.m
    done
    echo -n "x = [" >> xx.m
    for ((i=1; i<=${#prs[@]}; i++))
    do
        echo -n " $i" >> xx.m
    done
    echo "];" >> xx.m
    echo -n "str = [ " >> xx.m
    for ((i=0; i<${#prs[@]}; i++))
    do
        if [ "$i" != "0" ]; then echo -n "; " >> xx.m; fi
        echo -n " '"PR${prs[$i]}"'" >> xx.m
    done
    echo " ];" >> xx.m
    echo -n "data = [" >> xx.m
    for ((i=0; i<${#prs[@]}; i++))
    do
        echo -n " mp$i" >> xx.m
    done
    echo " ];" >> xx.m
    echo -n "errhigh = [" >> xx.m
    for ((i=0; i<${#prs[@]}; i++))
    do
        echo -n " sd$i" >> xx.m
    done
    echo " ];" >> xx.m
    echo -n "errlow = [" >> xx.m
    for ((i=0; i<${#prs[@]}; i++))
    do
        echo -n " sd$i" >> xx.m
    done
    echo " ];" >> xx.m
    cat >> xx.m <<EOF
        bar(x,data);
        set(gca, 'XTickLabel', str, 'XTick', 1:numel(x));
        hold on
        er = errorbar(x,data,errlow,errhigh);
        hold off
        set(er, "color", [0 0 0])
        set(er, "linewidth", 3);
        set(er, "linestyle", "none");
        set(gca, "fontsize", 6)
        xlabel("Target");
        ylabel("Runtime (s)");
        title("Comparison of Runtimes")
        print("./times-$gg.svg", "-dsvg")
        [pval, t, df] = welch_test(p0, p1)
        if (abs(pval) < 0.03 && mp0/mp1 > 1.05)
          printf("The PR statistically and practically decreased performance for $gg.\n");
        else
          printf("The PR did not signficantly negatively alter performance for $gg.\n");
        endif
EOF
    echo ========
    cat xx.m
    echo ========
    cat xx.m | octave --no-gui

done
