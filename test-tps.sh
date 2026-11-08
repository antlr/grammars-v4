#!/usr/bin/bash
#set -x

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

# Number of times to run app per grammar for statistical averaging
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

if [ "${#grammars[@]}" -gt 0 ]
then
    : # grammars explicitly specified via -g; skip discovery
elif [ "$filter" == "all" ]
then
    grammars=()
    # Test grammars for the enclosing directories.
    directories=`find . -name desc.xml | grep -v Generated | grep -v examples | sed 's#/desc.xml##' | sort -u`
    for g in $directories
    do
        pushd $g > /dev/null 2>&1
        g=`pwd`
        g=${g##*$prefix/}
        popd > /dev/null 2>&1
        grammars+=( $g )
    done
    grammars=( $(for g in "${grammars[@]}"; do echo "${g}"; done | sort -u) )
fi

grammars=($(echo "${grammars[@]}" | tr ' ' '\n' | sort -u | tr '\n' ' '))

if [ "${#grammars[@]}" -eq 0 ]
then
    echo There are no grammars in current directory to test.
    exit 0
fi

targets=( CSharp )

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

    # Generate driver source code.

    if [ $quiet != "true" ]; then echo "Generating driver for $testname."; fi
    trgen_opts=(-t "$target")
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
    
	generated_dir=$(pwd -P)
	trash() { (cd ".." && dotnet trash "$@"); }

        # Gather list of test inputs.
        cmd=`grep '^files2' test.sh`

        eval "$cmd"
	pushd ..
        files=()
	pwd
        for f in $files2
        do
            if [ -d "$f" ]; then continue; fi
            files+=( $f )
        done
        if [ ${#files[@]} -eq 0 ]
        then
            continue
        fi
	popd

	# Warmup run (discarded): lets Windows AV pre-scan the executable.
        printf '%s\n' "${files[@]}" | trash parse -p Generated-$target --no-output -x

        # Run app N times, tee raw output to stdout, capture for stats.
        run_pt=(); run_ot=(); run_tt=(); run_tps=(); run_warm_tps=(); run_speedup=()
        for ((run=1; run<=N; run++)); do
            echo "=== $testname run $run/$N ==="
            runfile="$tmpdir/$(echo "$testname" | tr '/' '_')_run${run}.txt"
            printf '%s\n' "${files[@]}" | trash parse -p Generated-$target --perf --no-output -x 2>&1 | tee "$runfile"
            run_pt+=(      "$(awk '/^PT:/{print $NF}'               "$runfile" | tail -1)" )
            run_ot+=(      "$(awk '/^OT:/{print $NF}'               "$runfile" | tail -1)" )
            run_tt+=(      "$(awk '/^TT:/{print $NF}'               "$runfile" | tail -1)" )
            run_tps+=(     "$(awk '/^(PR|TPS):/{print $NF}'         "$runfile" | tail -1)" )
            run_warm_tps+=("$(awk '/^Post-warmup (PR|TPS):/{print $NF}' "$runfile" | tail -1)" )
            run_speedup+=( "$(awk '/^Post-warmup speed up:/{print $NF}' "$runfile" | tail -1)" )
        done
        pt_stat=$(      compute_stats "${run_pt[@]}")
        ot_stat=$(      compute_stats "${run_ot[@]}")
        tt_stat=$(      compute_stats "${run_tt[@]}")
        tps_stat=$(     compute_stats "${run_tps[@]}")
        warm_tps_stat=$(compute_stats "${run_warm_tps[@]}")
        speedup_stat=$( compute_stats "${run_speedup[@]}")


	
        # Profile separately so ambiguity tracking does not affect parse timings.
        ambiguity_file="$tmpdir/$(echo "$testname" | tr '/' '_')_ambiguities.txt"

	printf '%s\n' "${files[@]}" | trash perf -p Generated-$target -x -c a > "$ambiguity_file" 2> /dev/null
	cp "$ambiguity_file" ./asdfasdfasdf
        ambiguity_count=$(awk 'NF == 1 && $1 ~ /^[0-9]+$/ { sum += $1 } END { print sum+0 }' "$ambiguity_file")

        # Count target-language blocks in every lexer, parser, and combined grammar.
        action_counts=$(dotnet trash parse -t ANTLRv4 ../*.g4 2> /dev/null |
            dotnet trash xpath '//actionBlock | //argActionBlock' |
            dotnet trash text --text -c)
        action_count=$(printf '%s\n' "$action_counts" |
            awk -F: 'NF > 1 && $NF ~ /^[0-9]+$/ { sum += $NF } END { print sum+0 }')

        row_number=$((${#table_rows[@]} + 1))
        table_rows+=("| ${row_number} | ${testname} | ${pt_stat} | ${ot_stat} | ${tt_stat} | ${tps_stat} | ${warm_tps_stat} | ${speedup_stat} | ${ambiguity_count} | ${action_count} |")
        popd > /dev/null
    done
    popd > /dev/null
done

# Write the markdown report in the repository root.
if [ ${#table_rows[@]} -gt 0 ]; then
    git_commit=$(git -C "$prefix" rev-parse HEAD 2>/dev/null) || git_commit="unknown"
    git_tags="none"
    if [[ "$git_commit" != "unknown" ]]; then
        tags_at_commit=$(git -C "$prefix" tag --points-at "$git_commit" 2>/dev/null |
            awk 'NF { printf "%s%s", separator, $0; separator=", " }')
        [[ -n "$tags_at_commit" ]] && git_tags="$tags_at_commit"
    fi
    {
        printf '## Performance Summary (N=%s runs, mean ± SEM)\n\n' "$N"
        printf 'Git commit: `%s`\n\nGit tag: `%s`\n\n' "$git_commit" "$git_tags"
        cat <<'EOF'
### Column key

| Label | Description |
|---|---|
| `Number` | Current row position in the Results table |
| `PT` | **Parse time** — sum of the parse-only time across all input files |
| `OT` | **Overhead time** — `TT − PT`; time spent on file I/O, lexer/parser construction, and post-parse work |
| `TT` | **Total time** — overall wall-clock time for the entire run |
| `PR` | **Parse rate** — `total tokens / PT`; pure parse throughput across all files |
| `Post-warmup PR` | PR computed from files 2…N only, excluding the first (warm-up) run; `n.a.` if only one file was parsed |
| `Post-warmup Speed Up` | Ratio of Post-warmup PR to first-file PR, showing the JIT/runtime warm-up benefit; `n.a.` if only one file was parsed |

### Results

EOF
        echo "| Number | Grammar | PT (s) | OT (s) | TT (s) | PR (t/s) | Post-warmup PR (t/s) | Post-warmup Speed Up | Ambiguities | Actions |"
        echo "|--------|---------|--------|--------|--------|-----|-----------------|----------------------|-------------|---------|"
        for row in "${table_rows[@]}"; do
            echo "$row"
        done
    } > "$prefix/performance.md"
fi

if [[ "$failed" == "" ]]
then
    exit 0
else
    exit 1
fi
