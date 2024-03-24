#

set -x
set -e

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

function getopts-extra () {
    OPTARG=()
    declare i=0
    # if the next argument is not an option, then append it to array OPTARG
    while [[ ${OPTIND} -le $# && ${!OPTIND:0:1} != '-' ]]; do
        OPTARG[i]=${!OPTIND}
        let i++ OPTIND++
    done
}
cwd=`pwd`

#############################
#############################
# Get last commit/pr. Note, some of the PR merges don't
# fit the pattern, but we'll ignore them. Get "prior" commit before all these
# changes.
prs=( `git log --grep="[(][#][1-9][0-9]*[)]" --pretty=oneline | head -2 | awk '{print $NF}' | sed 's/[()#]//g'` )
com=( `git log --grep="[(][#][1-9][0-9]*[)]" --pretty=oneline | awk '{print $1}' | sed 's/[()#]//g' | head -2` )
pri=( `git log --grep="[(][#][1-9][0-9]*[)]" --pretty=oneline | head -2 | tail -1 | awk '{print $1}' | sed 's/[()#]//g'` )
echo PRS = ${prs[@]}
echo COM = ${com[@]}
echo PRI = ${pri[@]}
echo '#PRS' = ${#prs[@]}

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
echo Build each grammar changed in PR.
for ((i=0; i<${#prs[@]}; i++))
do
    rm -rf ./${prs[$i]}
    git clone 'https://github.com/antlr/grammars-v4.git' "${prs[$i]}"
    pushd ${prs[$i]}
    git checkout ${com[$i]}
    for g in ${tests[@]}
    do
        echo Grammar $g
        pushd $g
        if [ "$local" == "" ]
        then
            trgen -t CSharp
        else
            dotnet trgen -- -t CSharp
        fi
        where=`echo Generated-CSharp* | tr ' ' '\n' | head -1`
        echo $where
        cd $where
        make
        popd
    done
    popd
done

#===========================
echo Test each grammar and PR in turn.
for g in ${tests[@]}
do
    echo Grammar $g
    rm -f "$cwd"/p[0-1]*
    gg=`echo $g | tr '/' '-'`
    for ((i=0; i<${#prs[@]}; i++))
    do
        pushd ${prs[$i]}/$g
        where=`echo Generated-CSharp* | tr ' ' '\n' | head -1`
        echo $where
        cd $where
        if [ "$local" == "" ]
        then
            what=`trxml2 desc.xml | grep inputs | head -1 | sed 's%^[^=]*=%%'`
        else
            what=`dotnet trxml2 -- desc.xml | grep inputs | head -1 | sed 's%^[^=]*=%%'`
        fi
        if [ "$what" == "" ]
        then
            dir=`pwd`
            p=`realpath -s --relative-to=$dir "$cwd/${prs[1]}/$g/examples"`
            if [ "$local" == "" ]
            then
                what=( `trglob $p | grep -v '.errors$' | grep -v '.tree$'` )
            else
                what=( `dotnet trglob -- $p | grep -v '.errors$' | grep -v '.tree$'` )
            fi
        else
            dir=`pwd`
            p=`realpath -s --relative-to=$dir "$cwd/${prs[1]}/$g/$what"`
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
        for j in {1..40}
        do
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
		[h,p,ci,stats] = ttest2(p0, p1, 'alpha', 0.05, 'tail', 'left')
		if (h)
		  printf("The PR signficantly DECREASES performance for $gg.\n");
		else
		  printf("The PR did not signficantly decrease performance for $gg.\n");
		endif
		[h,p,ci,stats] = ttest2(p0, p1, 'alpha', 0.05, 'tail', 'right')
		if (h)
		  printf("The PR signficantly INCREASES performance for $gg.\n");
		else
		  printf("The PR did not signficantly increase performance for $gg.\n");
		endif
EOF
    cat xx.m | octave --no-gui

done