#!/usr/bin/bash
# set -x

myrealpath ()                                                                                                                                                                                   
{                                                                                                                                                                                             
    f=$@;                                                                                                                                                                                     
    if [ -d "$f" ]; then                                                                                                                                                                      
        base="";                                                                                                                                                                              
        dir="$f";                                                                                                                                                                             
    else                                                                                                                                                                                      
        base="/$(basename "$f")";                                                                                                                                                             
        dir=$(dirname "$f");                                                                                                                                                                  
    fi;                                                                                                                                                                                       
    dir=$(cd "$dir" && /bin/pwd);                                                                                                                                                             
    echo "$dir$base"                                                                                                                                                                          
}

# true or false
quiet=true

# Number of times to run Test.exe per grammar for statistical averaging
N=3
tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

# Get full path of this script.
full_path_script=$(myrealpath $0)
full_path_templates=$(dirname $full_path_script)/templates

# Sanity checks for required environment.
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     machine=Linux;;
    Darwin*)    machine=Mac;;
    CYGWIN*)    machine=Cygwin;;
    MINGW*)     machine=MinGw;;
    *)          machine="UNKNOWN:${unameOut}"
esac
if [[ "$machine" != "Linux" && "$machine" != "Mac" && "$machine" != "MinGw" ]]
then
    echo "This script runs only in a Linux environment. Skipping."
    exit 0
fi

thetime()
{
    local hh
    local DIFF
    DIFF=$1
    hh="$(($DIFF / 3600 ))"
    mm="$((($DIFF % 3600) / 60))"
    ss="$(($DIFF % 60))"
    printf "%02d:%02d:%02d" $hh $mm $ss
}

# Compute mean ± SEM from a list of values (skipping "n.a." entries).
# Usage: compute_stats val1 val2 val3 ...
compute_stats() {
    printf '%s\n' "$@" | awk '
    {
        if ($1 != "n.a." && $1 != "") {
            count++; sum += $1; v[count] = $1
        }
    }
    END {
        if (count == 0) { print "n.a. ± n.a."; exit }
        mean = sum / count
        if (count == 1) { printf "%.4g ± n.a.\n", mean; exit }
        ss = 0
        for (i = 1; i <= count; i++) ss += (v[i] - mean)^2
        sem = sqrt(ss / (count * (count - 1)))
        printf "%.4g ± %.4g\n", mean, sem
    }'
}

function getopts-extra () {
    OPTARG=()
    declare i=0
    # if the next argument is not an option, then append it to array OPTARG
    while [[ ${OPTIND} -le $# && ${!OPTIND:0:1} != '-' ]]; do
        OPTARG[i]=${!OPTIND}
        let i++ OPTIND++
    done
}

# filter="all" or "diff" or "agnostic"
filter="all"
# order="grammar" or "target"
order="grammar"
failed=()
succeeded=()
skipped=""
grammars=()
targets=()
tests=()
table_rows=()

# Get "root" of the repo clone.
cwd=`pwd`
prefix=`pwd`
pushd $prefix > /dev/null 2>&1
while true
do
    if [ -f `pwd`/_scripts/test.sh ]
    then
        break
    elif [ `pwd` == "/" ]
    then
        break
    fi
    cd ..
done
if [ ! -f `pwd`/_scripts/test.sh ]
then
    echo "Not in repo."
    exit 1
fi
prefix=`pwd`
popd > /dev/null 2>&1

rm -rf `find . -name 'Generated*' -type d`

# Pre-process long options not handled by getopts.
antlr_version=""
_args=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --antlr-version)
            if [[ -z "${2:-}" || "$2" == -* ]]; then
                echo "Error: --antlr-version requires a non-option value." >&2
                exit 2
            fi
            antlr_version="$2"
            shift 2
            ;;
        --antlr-version=*)
            antlr_version="${1#*=}"
            shift
            ;;
        *)
            _args+=("$1")
            shift
            ;;
    esac
done
set -- "${_args[@]}"

# Parse args, and update computation to perform.
order="grammars"
additional=()
antlr4jar=/tmp/antlr4-complete.jar
while getopts 'agthf' opt; do
    case "$opt" in
        a)
            getopts-extra "$@"
            antlr4jar="${OPTARG[0]}"
            ;;
        g)
            getopts-extra "$@"
            for a in "${OPTARG[@]}"
            do
                grammars+=( "$a" )
            done
            ;;
        t)
            getopts-extra "$@"
            for a in "${OPTARG[@]}"
            do
                targets+=( "$a" )
            done
            ;;
        o)
            order="$OPTARG"
            ;;
        f)
            getopts-extra "$@"
            filter="${OPTARG[0]}"
            for a in "${OPTARG[@]:1}"
            do
                additional+=( "$a" )
            done
            ;;
        ?|h)
            cat - <<EOF
NAME
       test - tests Antlr4 grammars

SYNOPSIS
       $(basename $0) ([-g ...] | [-s ...] | [-t ...])

DESCRIPTION
       Tests Antlr4 grammars. Each grammar requires pom.xml and desc.xml files.

OPTIONS
       -g
           Specifies a list of directories that are searched for grammars.
           A grammar requires pom.xml and desc.xml files for it to be tested.
           If the option is not specified, then the default is to search from
           the current directory. Searching is subject to filters.

       -f
           Specifies the type of testing to filter. Possible types are "diff",
           "all", or "agnostic". The default is "all".

       -t
           Specifies the targets to test. Possible targets are
           "CSharp", "Cpp", "Dart", "Go", "Java", "JavaScript", "Python3".
           All targets are tested by default.

       --antlr-version
           Specifies the ANTLR4 tool and runtime version to use (e.g. "4.13.1"),
           or "dev" to clone and build from the ANTLR4 dev branch. Passed
           directly to trgen. If omitted, trgen's default version is used.

EOF
            exit 0
            ;;
    esac
done

if [ "${#grammars[@]}" -gt 0 ]
then
    : # grammars explicitly specified via -g; skip discovery
elif [ "$filter" == "diff" ]
then
    if [ "${#additional[@]}" -eq 0 ]
    then
        diffs=`git diff --exit-code --name-only .`
        if [ "$diffs" != "" ]
        then
            grammars=()
            # Test grammars for the enclosing directories.
            directories=`git diff --name-only . 2> /dev/null | sed 's#\(.*\)[/][^/]*$#\1#' | sort -u | grep -v _scripts`
            for g in $directories
            do
                pushd $g > /dev/null
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
                if [ "$g" == "" ]
                then
                    g="."
                fi
                popd > /dev/null
                grammars+=( $g )
            done
            grammars=( $(for g in "${grammars[@]}"; do echo "${g}"; done | sort -u) )
        fi
    elif [ "${#additional[@]}" -eq 2 ]
    then
        grammars=()
        # Test grammars for the enclosing directories.
        directories=`git diff --name-only ${additional[0]} ${additional[1]} . 2> /dev/null | sed 's#\(.*\)[/][^/]*$#\1#' | sort -u | grep -v _scripts`
        for g in $directories
        do
            pushd $g > /dev/null
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
            if [ "$g" == "" ]
            then
                g="."
            fi
            popd > /dev/null
            grammars+=( $g )
        done
    else
        echo ouch
        exit 1
    fi
elif [ "$filter" == "all" ]
then
    grammars=()
    # Test grammars for the enclosing directories.
    directories=`find . -name desc.xml | sed 's#/desc.xml##' | sort -u`
    for g in $directories
    do
        pushd $g > /dev/null 2>&1
        g=`pwd`
        g=${g##*$prefix/}
        popd > /dev/null 2>&1
        grammars+=( $g )
    done
    grammars=( $(for g in "${grammars[@]}"; do echo "${g}"; done | sort -u) )
elif [ "$filter" != "all" ]
then
    echo Unknown filter $filter
    exit 1
fi

grammars=($(echo "${grammars[@]}" | tr ' ' '\n' | sort -u | tr '\n' ' '))

if [ "${#grammars[@]}" -eq 0 ]
then
    echo There are no grammars in current directory to test.
    exit 0
fi

if [ "$targets" == "" ]
then
    targets=( CSharp )
fi

# Compute cross product
for g in ${grammars[@]}
do
    for t in ${targets[@]}
    do
        tests+=( "$g,$t" )
    done
done

# Sort the list of <grammar, target> tuples if needed.
if [[ "$order" == "grammars" ]]; then sorted="1"; else sorted="2"; fi
#tests=`echo $tests | fmt -w 1 | sort -t ',' -k "$sorted"`

# Perform tests in order.
for test in ${tests[@]}
do
    all=( $(echo $test | tr "," "\n") )
    testname=`echo ${all[0]} | tr -d '\n' | tr -d ' '`
    target=${all[1]}
    if [ ! -d "$prefix/$testname" ]
    then
        echo not there.
        exit 1
    fi
    pushd "$prefix/$testname" > /dev/null

    if [ ! -f desc.xml ]
    then
        popd > /dev/null
        continue
    fi
    desc_targets=`dotnet trash xml2 desc.xml | grep '/desc/targets'`
    if [ "${PIPESTATUS[0]}" -ne 0 ]
    then
        echo "The desc.xml for $testname is malformed. Skipping."
        popd > /dev/null
        continue
    fi
    desc_targets="${desc_targets##*=}"
    desc_targets=`echo "$desc_targets" | tr ',' ' ' | tr ';' ' '`

    yes=false;
    for t in $desc_targets
    do
        if [ "$t" == "+all" ]; then yes=true; fi
        if [ "$t" == "-$target" ]; then yes=false; fi
        if [ "$t" == "$target" ]; then yes=true; fi
    done
    if [ "$yes" == "false" ]
    then
        popd > /dev/null
        continue
    fi

    if [ "$filter" == "agnostic" ]
    then
        # Test whether the grammars have actions.
        count=`dotnet trash parse -t ANTLRv4 *.g4 2> /dev/null | dotnet trash xgrep ' //(actionBlock | argActionBlock)' | dotnet trash text -c`
        if [ "$count" == "0" ]
        then
            echo "no actions => skipping $testname."
            popd > /dev/null
            continue
        fi
    fi

    # Generate driver source code.

    if [ $quiet != "true" ]; then echo "Generating driver for $testname."; fi
    trgen_opts=(-t "$target" --antlr-tool-path "$antlr4jar")
    [[ -n "$antlr_version" ]] && trgen_opts+=(--antlr-version "$antlr_version")
    bad=$(dotnet trash gen "${trgen_opts[@]}" 2>/dev/null)
    for i in $bad; do failed+=( "$testname/$target" ); done

    for d in `echo Generated-$target`
    do
        if [ ! -d $d ]; then continue; fi
        if [ ! -f $d/build.sh ]; then echo " no build.sh"; continue; fi

        # Build driver code.
        if [ $quiet != "true" ]; then echo "Building."; fi
        pushd $d > /dev/null
        date1=$(date +"%s")
        bash build.sh > output.txt 2>&1
        status="$?"
        date2=$(date +"%s")
        DIFF=$(($date2-$date1))
        if [ "$status" != "0" ]
        then
            continue
        fi

        echo $testname
    
        # Test generated parser on examples.
        cmd=`grep '^files2' test.sh`
        eval "$cmd"
        files=()
        for f in $files2
        do
            if [ -d "$f" ]; then continue; fi
                files+=( $f )
        done
        if [ ${#files[@]} -eq 0 ]
        then
            continue
        fi
        # Warmup run (discarded): lets Windows AV pre-scan the executable.
        printf '%s\n' "${files[@]}" | ./bin/Debug/net10.0/Test.exe -x > /dev/null 2>&1

        # Run Test.exe N times, tee raw output to stdout, capture for stats.
        run_pt=(); run_ot=(); run_tt=(); run_tps=(); run_warm_tps=(); run_speedup=()
        for ((run=1; run<=N; run++)); do
            echo "=== $testname run $run/$N ==="
            runfile="$tmpdir/$(echo "$testname" | tr '/' '_')_run${run}.txt"
            printf '%s\n' "${files[@]}" | ./bin/Debug/net10.0/Test.exe -x 2>&1 | tee "$runfile"
            run_pt+=(      "$(awk '/^PT:/{print $NF}'               "$runfile" | tail -1)" )
            run_ot+=(      "$(awk '/^OT:/{print $NF}'               "$runfile" | tail -1)" )
            run_tt+=(      "$(awk '/^TT:/{print $NF}'               "$runfile" | tail -1)" )
            run_tps+=(     "$(awk '/^TPS:/{print $NF}'              "$runfile" | tail -1)" )
            run_warm_tps+=("$(awk '/^Post-warmup TPS:/{print $NF}'  "$runfile" | tail -1)" )
            run_speedup+=( "$(awk '/^Post-warmup speed up:/{print $NF}' "$runfile" | tail -1)" )
        done
        pt_stat=$(      compute_stats "${run_pt[@]}")
        ot_stat=$(      compute_stats "${run_ot[@]}")
        tt_stat=$(      compute_stats "${run_tt[@]}")
        tps_stat=$(     compute_stats "${run_tps[@]}")
        warm_tps_stat=$(compute_stats "${run_warm_tps[@]}")
        speedup_stat=$( compute_stats "${run_speedup[@]}")
        table_rows+=("| ${testname} | ${pt_stat} | ${ot_stat} | ${tt_stat} | ${tps_stat} | ${warm_tps_stat} | ${speedup_stat} |")
        popd > /dev/null
    done
    popd > /dev/null
done

# Print markdown performance table
if [ ${#table_rows[@]} -gt 0 ]; then
    echo ""
    echo "## Performance Summary (N=${N} runs, mean ± SEM)"
    echo ""
    echo "| Grammar | PT (s) | OT (s) | TT (s) | TPS | Post-warmup TPS | Post-warmup Speed Up |"
    echo "|---------|--------|--------|--------|-----|-----------------|----------------------|"
    for row in "${table_rows[@]}"; do
        echo "$row"
    done
fi

if [[ "$failed" == "" ]]
then
    exit 0
else
    exit 1
fi
