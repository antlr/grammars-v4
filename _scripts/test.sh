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
jobs_limit=4

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
# Test dispatch order is target, then grammar.
order="targets"
failed=()
succeeded=()
skipped=""
grammars=()
targets=()
tests=()

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
order="targets"
additional=()
antlr4jar=/tmp/antlr4-complete.jar
while getopts 'agthfj:' opt; do
    case "$opt" in
        a)
            getopts-extra "$@"
            antlr4jar="${OPTARG[0]}"
        echo Antlr4jar is $antlr4jar
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
        j)
            if [[ ! "$OPTARG" =~ ^[1-9][0-9]*$ ]]; then
                echo "Error: -j requires a positive integer." >&2
                exit 2
            fi
            jobs_limit=$OPTARG
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

       -j
           Maximum number of grammar/target tests to run concurrently.
           The default is 4. Output is streamed with a grammar/target prefix.

       --antlr-version
           Specifies the ANTLR4 tool and runtime version to use (e.g. "4.13.1"),
           or "dev" to clone and build from the ANTLR4 dev branch. Passed
           directly to trgen. If omitted, trgen's default version is used.

EOF
            exit 0
            ;;
    esac
done

if [ "$filter" == "diff" ]
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
                echo Adding diff $g
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
            echo Adding diff $g
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
    directories=`find . -type d -name 'Generated*' -prune -o -name desc.xml -print | sed 's#/desc.xml##' | sort -u`
    for g in $directories
    do
        pushd $g > /dev/null 2>&1
        g=`pwd`
        g=${g##*$prefix/}
        popd > /dev/null 2>&1
        echo Adding $g
        grammars+=( $g )
    done
    grammars=( $(for g in "${grammars[@]}"; do echo "${g}"; done | sort -u) )
elif [ "$filter" != "all" ]
then
    echo Unknown filter $filter
    exit 1
fi

echo "Number of grammars before sorting and making unique: ${#grammars[@]}"
grammars=($(echo "${grammars[@]}" | tr ' ' '\n' | sort -u | tr '\n' ' '))
echo "Number of grammars after sorting and making unique: ${#grammars[@]}"

if [ "${#grammars[@]}" -eq 0 ]
then
    echo There are no grammars in current directory to test.
    exit 0
else
    echo Grammars to do are: ${grammars[@]}
fi

if [ "${#targets[@]}" -eq 0 ]
then
    targets=( Antlr4ng CSharp Cpp Dart Go Java JavaScript OphiRust Python3 Rust TypeScript )
fi

# A repeated -t value would otherwise run two workers in the same Generated
# directory. Keep the caller's order while removing duplicates.
declare -A target_seen=()
unique_targets=()
for t in "${targets[@]}"
do
    if [ -z "${target_seen[$t]+x}" ]
    then
        target_seen[$t]=1
        unique_targets+=( "$t" )
    fi
done
targets=( "${unique_targets[@]}" )

echo grammars = ${grammars[@]}
echo targets = ${targets[@]}
echo order = $order
echo filter = $filter

# Compute cross product
for g in ${grammars[@]}
do
    for t in ${targets[@]}
    do
        tests+=( "$g,$t" )
    done
done

# Dispatch by target first, then grammar, regardless of discovery order.
tests=( $(printf '%s\n' "${tests[@]}" | LC_ALL=C sort -t ',' -k2,2 -k1,1) )

echo Tests now ${tests[@]}
if [ "$jobs_limit" -gt "${#tests[@]}" ]; then jobs_limit=${#tests[@]}; fi
if [ "$antlr_version" = dev ] && [ "$jobs_limit" -gt 1 ]
then
    echo 'ANTLR dev builds share a checkout; running those tests serially.'
    jobs_limit=1
fi

# Run one independent grammar/target pair. Distinct target directories keep
# generated files separate even when jobs for the same grammar overlap.
run_pair()
{
    local testname=$1
    local target=$2
    local d status source_dir pair_dir
    local failed=0
    source_dir="$prefix/$testname"
    pair_dir="$run_dir/$target/$testname"
    mkdir -p -- "$pair_dir" || return 1
    # Keep every generated file and test output outside the source checkout.
    if ! (set -o pipefail; tar -C "$source_dir" --exclude='Generated*' --exclude='.git' -cf - . | tar -C "$pair_dir" -xf -)
    then
        echo "Could not copy $testname into the temporary test directory."
        return 1
    fi
    git -C "$pair_dir" init -q || return 1
    git -C "$pair_dir" config core.excludesfile "$prefix/.gitignore"
    git -C "$pair_dir" -c core.autocrlf=false add -A || return 1
    pushd "$pair_dir" > /dev/null || return 1

    echo ""
    echo "$testname,$target:"

    if [ ! -f desc.xml ]
    then
        echo No desc.xml for $testname
        failed=1
        popd > /dev/null
        return 1
    fi
    desc_targets=`dotnet trash xml2 desc.xml | grep '/desc/targets'`
    if [ "${PIPESTATUS[0]}" -ne 0 ]
    then
        echo "The desc.xml for $testname is malformed. Skipping."
        failed=1
        popd > /dev/null
        return 1
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
        echo "Intentionally skipping grammar $testname target $target."
        popd > /dev/null
        return 0
    fi

    if [ "$filter" == "agnostic" ]
    then
        # Test whether the grammars have actions.
        count=`dotnet trash parse -t ANTLRv4 *.g4 2> /dev/null | dotnet trash xpath ' //(actionBlock | argActionBlock)' | dotnet trash text -c`
        if [ "$count" == "0" ]
        then
            echo "no actions => skipping $testname."
            popd > /dev/null
            return 0
        fi
    fi

    # Generate driver source code.

    if [ $quiet != "true" ]; then echo "Generating driver for $testname."; fi
    build_start=$(date +"%s")
    trgen_opts=(-t "$target" --template-sources-directory "$full_path_templates" --antlr-tool-path "$antlr4jar")
    [[ -n "$antlr_version" ]] && trgen_opts+=(--antlr-version "$antlr_version")
    gen_output=$(dotnet trash gen "${trgen_opts[@]}" 2>&1)
    gen_status=$?
    if [ "$gen_status" -ne 0 ]; then
        echo " Build: $(thetime $(($(date +"%s") - build_start)))"
        echo ========
        echo "Generation of $testname for $target failed (exit $gen_status)."
        printf '%s\n' "$gen_output"
        echo ========
        popd > /dev/null
        return 1
    fi

    generated_dirs=()
    for d in Generated-$target-* Generated-$target; do
        if [ -d "$d" ]; then generated_dirs+=( "$d" ); fi
    done
    if [ "${#generated_dirs[@]}" -eq 0 ]; then
        echo " Build: $(thetime $(($(date +"%s") - build_start)))"
        echo "Generation of $testname for $target produced no driver directory."
        popd > /dev/null
        return 1
    fi

    built_dirs=()
    for d in "${generated_dirs[@]}"; do
        if [ ! -f "$d/build.sh" ]; then
            echo "No build.sh in $d."
            failed=1
            continue
        fi

        # Build driver code.
        if [ $quiet != "true" ]; then echo "Building."; fi
        pushd "$d" > /dev/null
        bash build.sh
        status="$?"
        if [ "$status" != "0" ]
        then
            echo ""
            echo ========
            echo "Build of $testname for $target failed."
            failed=1
            echo ========
        else
            built_dirs+=( "$d" )
        fi
        popd > /dev/null
    done
    echo " Build: $(thetime $(($(date +"%s") - build_start)))"

    for d in "${built_dirs[@]}"; do
        # Test generated parser on examples.
        if [ $quiet != "true" ]; then echo "Parsing."; fi
        pushd "$d" > /dev/null
        date1=$(date +"%s")
        bash test.sh
        status="$?"
        date2=$(date +"%s")
        DIFF=$(($date2-$date1))
        length=" Test "`thetime $DIFF`
        echo "$length"
        if [ "$status" != "0" ]
        then
            echo ""
            echo ========
            echo "Parsing tests of $testname for $target failed."
            failed=1
            echo ========
            else
            echo " Succeeded."      
        fi

        popd > /dev/null
    done
    popd > /dev/null
    [ "$failed" -eq 0 ]
}

# A worker returns its pool slot as soon as its pair completes. npm's global
# installs are serialized separately, since Antlr4ng and TypeScript builds
# both invoke npm install -g.
# Give each worker its own process group so cancellation also reaches its
# build/test scripts and their children, not just the immediate Bash worker.
set -m
temp_root=$(cd "${TMPDIR:-/tmp}" && pwd -P) || exit 1
run_dir=$(mktemp -d "$temp_root/trash-test.XXXXXXXX") || exit 1
run_dir=$(cd "$run_dir" && pwd -P) || exit 1
pids=()
cleanup_pool()
{
    local status=$?
    local pid
    local active=false
    trap - EXIT INT TERM
    for pid in "${pids[@]}"
    do
        if kill -0 -- "-$pid" 2>/dev/null
        then
            kill -TERM -- "-$pid" 2>/dev/null || true
            active=true
        fi
    done
    if [ "$active" = true ]
    then
        sleep 1
        for pid in "${pids[@]}"
        do
            kill -KILL -- "-$pid" 2>/dev/null || true
        done
    fi
    for pid in "${pids[@]}"
    do
        wait "$pid" 2>/dev/null || true
    done
    exec 3>&- 4>&-
    case "$run_dir" in
        "$temp_root"/trash-test.*)
            rm -rf -- "$run_dir"
            ;;
        *)
            echo "Refusing to remove unexpected temporary path: $run_dir" >&2
            ;;
    esac
    return "$status"
}
trap 'exit 130' INT
trap 'exit 143' TERM
trap cleanup_pool EXIT
mkdir -p -- "$run_dir/.config" || exit 1
cp -- "$prefix/.config/dotnet-tools.json" "$run_dir/.config/dotnet-tools.json" || exit 1
mkfifo "$run_dir/slots" "$run_dir/npm" || exit 1
exec 3<>"$run_dir/slots"
exec 4<>"$run_dir/npm"
rm -f -- "$run_dir/slots" "$run_dir/npm"
for ((i = 0; i < jobs_limit; i++))
do
    printf '.' >&3
done
printf '.' >&4

mkdir -p -- "$run_dir/results" || exit 1
for test_index in "${!tests[@]}"
do
    test=${tests[$test_index]}
    IFS=, read -r testname target <<< "$test"
    read -r -n 1 -u 3 slot
    (
        # Keep the pipeline and all commands it starts in this worker's group.
        set +m
        if [[ "$target" == Antlr4ng || "$target" == TypeScript ]]
        then
            read -r -n 1 -u 4 npm_slot
            npm_locked=true
        else
            npm_locked=false
        fi
        run_pair "$testname" "$target" 2>&1 |
            while IFS= read -r line || [ -n "$line" ]
            do
                printf '[%s,%s] %s\n' "$testname" "$target" "$line"
            done
        result=${PIPESTATUS[0]}
        printf '%s\n' "$result" > "$run_dir/results/$test_index"
        if [ "$npm_locked" = true ]; then printf '.' >&4; fi
        printf '.' >&3
        exit "$result"
    ) &
    pids+=( "$!" )
done

# All jobs have returned their pool slots once their result files exist.
# Bash may discard statuses for early jobs in a large run, so collect the
# recorded results rather than waiting for every historical PID.
for ((i = 0; i < jobs_limit; i++)); do read -r -n 1 -u 3 slot; done
for pid in "${pids[@]}"; do wait "$pid" 2>/dev/null || true; done
for test_index in "${!tests[@]}"
do
    result_file="$run_dir/results/$test_index"
    if [ ! -f "$result_file" ] || [ "$(cat "$result_file")" -ne 0 ]
    then
        failed+=( "${tests[$test_index]}" )
    fi
done
if [ "${#failed[@]}" -eq 0 ]
then
    exit 0
else
    printf 'Tests failed: %s\n' "${failed[*]}"
    exit 1
fi
