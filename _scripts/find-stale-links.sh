#!/bin/bash
#
# find-stale-links.sh - Find and optionally replace stale HTTP links with web.archive.org links
#
# Usage: find-stale-links.sh [--replace] [--verbose] [--timeout SECONDS] [--parallel N] [PATH...]
#
# Options:
#   --replace      Replace stale links with web.archive.org archived versions
#   --verbose      Print verbose output
#   --timeout N    Timeout for HTTP requests in seconds (default: 60)
#   --parallel N   Number of parallel URL checks (default: 10, use 1 to disable)
#   --help         Show this help message
#
# If no PATH is provided, searches the current directory.
# Always excludes .git/ directory.
#
# Timeouts (HTTP 000) are reported separately and do not cause failure.
# Only actual HTTP errors (404, 500, etc.) are counted as stale links.

set -euo pipefail

# Default values
REPLACE=false
VERBOSE=false
TIMEOUT=60
PARALLEL=10
PATHS=()

# Color output (if terminal)
if [[ -t 1 ]]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    BLUE='\033[0;34m'
    NC='\033[0m' # No Color
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    NC=''
fi

usage() {
    sed -n '3,14p' "$0" | sed 's/^# \?//'
    exit 0
}

log_verbose() {
    if [[ "$VERBOSE" == "true" ]]; then
        echo -e "${BLUE}[VERBOSE]${NC} $*" >&2
    fi
}

log_info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --replace)
            REPLACE=true
            shift
            ;;
        --verbose)
            VERBOSE=true
            shift
            ;;
        --timeout)
            TIMEOUT="$2"
            shift 2
            ;;
        --parallel)
            PARALLEL="$2"
            shift 2
            ;;
        --help|-h)
            usage
            ;;
        -*)
            log_error "Unknown option: $1"
            usage
            ;;
        *)
            PATHS+=("$1")
            shift
            ;;
    esac
done

# Default to current directory if no paths specified
if [[ ${#PATHS[@]} -eq 0 ]]; then
    PATHS=(".")
fi

# Temporary files for tracking results
STALE_LINKS_FILE=$(mktemp)
TIMEOUT_LINKS_FILE=$(mktemp)
CHECKED_URLS_FILE=$(mktemp)
URLS_TO_CHECK_FILE=$(mktemp)
URL_RESULTS_FILE=$(mktemp)
URL_OCCURRENCES_FILE=$(mktemp)
trap 'rm -f "$STALE_LINKS_FILE" "$TIMEOUT_LINKS_FILE" "$CHECKED_URLS_FILE" "$URLS_TO_CHECK_FILE" "$URL_RESULTS_FILE" "$URL_OCCURRENCES_FILE"' EXIT

# URL cache to avoid checking the same URL multiple times (used after parallel check)
# Format: URL_CACHE[url]="status:http_code" where status is OK, TIMEOUT, or STALE
declare -A URL_CACHE

# Check if a URL is accessible (returns 0 if accessible, 1 if stale)
check_url() {
    local url="$1"

    # Check cache first
    if [[ -v URL_CACHE["$url"] ]]; then
        return "${URL_CACHE[$url]}"
    fi

    log_verbose "Checking URL: $url"

    # Use curl to check the URL
    local http_code
    http_code=$(curl -s -o /dev/null -w "%{http_code}" \
        --connect-timeout "$TIMEOUT" \
        --max-time "$((TIMEOUT * 3))" \
        -L \
        -A "Mozilla/5.0 (compatible; StaleLinksChecker/1.0)" \
        "$url" 2>/dev/null || echo "000")

    log_verbose "URL: $url returned HTTP code: $http_code"

    # Consider various status codes
    # 401/403 mean the URL exists but requires authentication - not stale
    case "$http_code" in
        200|201|202|203|204|301|302|303|307|308|401|403)
            URL_CACHE["$url"]=0
            return 0
            ;;
        *)
            URL_CACHE["$url"]=1
            return 1
            ;;
    esac
}

# Check a single URL and output result (for parallel execution)
# Output format: OK|TIMEOUT|STALE<tab>HTTP_CODE<tab>URL
check_url_standalone() {
    local url="$1"
    local timeout="$2"

    # Use curl to check the URL
    local http_code
    http_code=$(curl -s -o /dev/null -w "%{http_code}" \
        --connect-timeout "$timeout" \
        --max-time "$((timeout * 3))" \
        -L \
        -A "Mozilla/5.0 (compatible; StaleLinksChecker/1.0)" \
        "$url" 2>/dev/null || echo "000")

    # Consider various status codes
    case "$http_code" in
        200|201|202|203|204|301|302|303|307|308|401|403|418|429)
            echo -e "OK\t$http_code\t$url"
            ;;
        000|000000)
            # 000 means timeout or connection failure
            echo -e "TIMEOUT\t$http_code\t$url"
            ;;
        *)
            # Actual HTTP error (404, 500, etc.)
            echo -e "STALE\t$http_code\t$url"
            ;;
    esac
}
export -f check_url_standalone

# Get the date when a line was added to a file using git blame
get_line_date() {
    local file="$1"
    local line_num="$2"

    # Use git blame to get the commit date for this line
    local blame_output
    blame_output=$(git blame -L "$line_num,$line_num" --date=format:'%Y%m%d' -- "$file" 2>/dev/null || echo "")

    if [[ -z "$blame_output" ]]; then
        # Fallback to current date if git blame fails
        date +%Y%m%d
        return
    fi

    # Extract the date from the blame output (format: hash (author date lineno) content)
    local commit_date
    commit_date=$(echo "$blame_output" | grep -oP '\d{8}' | head -1)

    if [[ -z "$commit_date" ]]; then
        # Try alternative extraction
        commit_date=$(git blame -L "$line_num,$line_num" --date=short -- "$file" 2>/dev/null | grep -oP '\d{4}-\d{2}-\d{2}' | head -1 | tr -d '-')
    fi

    if [[ -z "$commit_date" ]]; then
        date +%Y%m%d
    else
        echo "$commit_date"
    fi
}

# Get Wayback Machine archived URL
get_archive_url() {
    local url="$1"
    local target_date="$2"

    log_verbose "Looking for archive of $url around date $target_date"

    # Query the Wayback Machine API for available snapshots
    local api_response
    api_response=$(curl -s --connect-timeout "$TIMEOUT" --max-time "$((TIMEOUT * 2))" \
        "https://archive.org/wayback/available?url=$url&timestamp=$target_date" 2>/dev/null || echo "")

    if [[ -z "$api_response" ]]; then
        log_verbose "No response from Wayback Machine API"
        return 1
    fi

    log_verbose "API response: $api_response"

    # Check if archived_snapshots is empty or has no closest snapshot
    if echo "$api_response" | grep -q '"archived_snapshots"\s*:\s*{}'; then
        log_verbose "No archived snapshots available"
        # Try without timestamp to get any available snapshot
        api_response=$(curl -s --connect-timeout "$TIMEOUT" --max-time "$((TIMEOUT * 2))" \
            "https://archive.org/wayback/available?url=$url" 2>/dev/null || echo "")
        log_verbose "API response (no timestamp): $api_response"
        if echo "$api_response" | grep -q '"archived_snapshots"\s*:\s*{}'; then
            log_verbose "No archive found for $url"
            return 1
        fi
    fi

    # Extract the archived URL from the closest snapshot
    # JSON format: {"url":"...","archived_snapshots":{"closest":{"status":"200","available":true,"url":"http://web.archive.org/...","timestamp":"..."}}}
    local archived_url
    # Look for URL inside the closest snapshot (after "closest")
    archived_url=$(echo "$api_response" | grep -oP '"closest"\s*:\s*\{[^}]*"url"\s*:\s*"[^"]+' | grep -oP '"url"\s*:\s*"[^"]+$' | sed 's/"url"\s*:\s*"//')

    if [[ -n "$archived_url" && "$archived_url" != "null" && "$archived_url" == *"web.archive.org"* ]]; then
        echo "$archived_url"
        return 0
    fi

    log_verbose "No archive found for $url"
    return 1
}

# Extract URLs from a file (collection phase - no checking)
# Outputs: file<tab>line_num<tab>url to URL_OCCURRENCES_FILE
# Also outputs unique URLs to stdout for collection
collect_urls_from_file() {
    local file="$1"

    log_verbose "Collecting URLs from: $file"

    # Skip binary files
    if file "$file" | grep -qE 'binary|executable|data'; then
        log_verbose "Skipping binary file: $file"
        return
    fi

    # Skip most files in example directories (they're parse examples, not documentation)
    if echo "$file" | grep -qE '/examples?[^/]*/' ; then
        case "$file" in
            *.md)
                ;;
            *.tree|*.errors)
                log_verbose "Skipping test output file in example dir: $file"
                return
                ;;
            *)
                log_verbose "Skipping example file: $file"
                return
                ;;
        esac
    fi

    # Extract HTTP/HTTPS URLs from the file with line numbers
    local url_matches
    url_matches=$(grep -noP 'https?://[^\s<>"\]`'\'']+' "$file" 2>/dev/null || true)

    if [[ -z "$url_matches" ]]; then
        return
    fi

    while IFS= read -r match_line; do
        line_num="${match_line%%:*}"
        url="${match_line#*:}"

        # Clean up the URL
        url=$(echo "$url" | sed 's/[,;.!?>}:]*$//' | sed "s/'$//" | sed 's/\]$//')

        # Balance parentheses
        while [[ "$url" == *")" ]]; do
            local open_count close_count
            open_count=$(echo "$url" | tr -cd '(' | wc -c)
            close_count=$(echo "$url" | tr -cd ')' | wc -c)
            if [[ "$close_count" -gt "$open_count" ]]; then
                url="${url%)}"
            else
                break
            fi
        done

        [[ -z "$url" ]] && continue

        # Skip localhost and example URLs
        if echo "$url" | grep -qE '(localhost|127\.0\.0\.1|example\.com|example\.org)'; then
            continue
        fi

        # Skip already-archived URLs
        if echo "$url" | grep -qE 'web\.archive\.org|archive\.org/web'; then
            continue
        fi

        # Skip XML namespace URIs
        if echo "$url" | grep -qE '(maven\.apache\.org/POM|maven\.apache\.org/xsd|w3\.org/[0-9]{4}/XMLSchema|w3\.org/XML/|xml\.org/sax|purl\.org/dc|schemas\.microsoft\.com|schemas\.openxmlformats\.org)'; then
            continue
        fi

        # Record this occurrence
        printf '%s\t%s\t%s\n' "$file" "$line_num" "$url" >> "$URL_OCCURRENCES_FILE"
        # Output URL for unique collection
        echo "$url"
    done <<< "$url_matches"
}

# Process URL check results and handle stale links
process_results() {
    log_verbose "Processing URL check results..."

    # Load URL results into cache
    # Format: status<tab>http_code<tab>url
    while IFS=$'\t' read -r status http_code url; do
        URL_CACHE["$url"]="$status:$http_code"
    done < "$URL_RESULTS_FILE"

    # Process each URL occurrence using cached results
    while IFS=$'\t' read -r file line_num url; do
        echo "$url" >> "$CHECKED_URLS_FILE"

        local cached="${URL_CACHE[$url]:-STALE:000}"
        local status="${cached%%:*}"
        local http_code="${cached#*:}"

        case "$status" in
            OK)
                log_verbose "URL is accessible: $url"
                ;;
            TIMEOUT)
                log_warn "Timeout (HTTP $http_code): $file:$line_num - $url"
                printf '%s\t%s\t%s\t%s\n' "$file" "$line_num" "$url" "$http_code" >> "$TIMEOUT_LINKS_FILE"
                ;;
            STALE)
                log_warn "Stale link (HTTP $http_code): $file:$line_num - $url"
                printf '%s\t%s\t%s\t%s\n' "$file" "$line_num" "$url" "$http_code" >> "$STALE_LINKS_FILE"

                if [[ "$REPLACE" == "true" ]]; then
                    local line_date
                    line_date=$(get_line_date "$file" "$line_num")
                    log_verbose "Line was added on: $line_date"

                    local archive_url
                    if archive_url=$(get_archive_url "$url" "$line_date"); then
                        log_info "Found archive: $archive_url"
                        sed -i "s|$url|$archive_url|g" "$file"
                        log_info "Replaced in $file: $url -> $archive_url"
                    else
                        log_warn "No archive available for: $url"
                    fi
                fi
                ;;
        esac
    done < "$URL_OCCURRENCES_FILE"
}

# Main execution
main() {
    log_info "Starting stale link check..."
    log_info "Replace mode: $REPLACE"
    log_info "Timeout: ${TIMEOUT}s"
    log_info "Parallel jobs: $PARALLEL"
    log_info "Paths: ${PATHS[*]}"

    # Find all text files, excluding .git directory
    local find_args=()
    for path in "${PATHS[@]}"; do
        find_args+=("$path")
    done
    find_args+=(-type f)
    find_args+=(-not -path '*/.git/*')
    find_args+=(-not -path '*/node_modules/*')
    find_args+=(-not -path '*/.gradle/*')
    find_args+=(-not -path '*/build/*')
    find_args+=(-not -path '*/target/*')
    find_args+=(-not -name '*.jar')
    find_args+=(-not -name '*.class')
    find_args+=(-not -name '*.exe')
    find_args+=(-not -name '*.dll')
    find_args+=(-not -name '*.so')
    find_args+=(-not -name '*.dylib')
    find_args+=(-not -name '*.png')
    find_args+=(-not -name '*.jpg')
    find_args+=(-not -name '*.jpeg')
    find_args+=(-not -name '*.gif')
    find_args+=(-not -name '*.ico')
    find_args+=(-not -name '*.pdf')
    find_args+=(-not -name '*.zip')
    find_args+=(-not -name '*.tar')
    find_args+=(-not -name '*.gz')
    find_args+=(-not -name '*.lock')

    # Phase 1: Collect all URLs from all files
    log_info "Phase 1: Collecting URLs from files..."
    local file_count=0
    while IFS= read -r file; do
        collect_urls_from_file "$file" >> "$URLS_TO_CHECK_FILE"
        file_count=$((file_count + 1))
    done < <(find "${find_args[@]}" 2>/dev/null || true)

    log_info "Processed $file_count files"

    # Get unique URLs to check
    local unique_urls_file
    unique_urls_file=$(mktemp)
    sort -u "$URLS_TO_CHECK_FILE" > "$unique_urls_file"
    local unique_count
    unique_count=$(wc -l < "$unique_urls_file" | tr -d ' ')

    if [[ "$unique_count" -eq 0 ]]; then
        log_info "No URLs found to check"
        rm -f "$unique_urls_file"
        exit 0
    fi

    log_info "Found $unique_count unique URLs to check"

    # Phase 2: Check URLs in parallel
    log_info "Phase 2: Checking URLs (parallel=$PARALLEL)..."

    # Use xargs for parallel execution
    cat "$unique_urls_file" | xargs -P "$PARALLEL" -I {} bash -c 'check_url_standalone "$@"' _ {} "$TIMEOUT" >> "$URL_RESULTS_FILE"

    rm -f "$unique_urls_file"

    # Phase 3: Process results
    log_info "Phase 3: Processing results..."
    process_results

    # Report results
    local stale_count
    stale_count=$(wc -l < "$STALE_LINKS_FILE" | tr -d ' ')
    local timeout_count
    timeout_count=$(wc -l < "$TIMEOUT_LINKS_FILE" | tr -d ' ')
    local checked_count
    checked_count=$(wc -l < "$CHECKED_URLS_FILE" | tr -d ' ')
    local final_unique_count
    final_unique_count=$(sort -u "$CHECKED_URLS_FILE" | wc -l | tr -d ' ')

    log_info "Checked $final_unique_count unique URLs ($checked_count total occurrences)"

    # Report timeouts (warnings only, don't fail)
    if [[ "$timeout_count" -gt 0 ]]; then
        log_warn "Found $timeout_count URL(s) that timed out (not counted as stale):"
        echo ""
        echo "=== TIMEOUT REPORT ==="
        while IFS=$'\t' read -r report_file report_line_num report_url report_code; do
            echo "  $report_file:$report_line_num (HTTP $report_code)"
            echo "    $report_url"
        done < "$TIMEOUT_LINKS_FILE"
        echo "======================"
        echo ""
    fi

    # Report stale links (actual errors)
    if [[ "$stale_count" -gt 0 ]]; then
        log_error "Found $stale_count stale link(s):"
        echo ""
        echo "=== STALE LINKS REPORT ==="
        while IFS=$'\t' read -r report_file report_line_num report_url report_code; do
            echo "  $report_file:$report_line_num (HTTP $report_code)"
            echo "    $report_url"
        done < "$STALE_LINKS_FILE"
        echo "=========================="

        if [[ "$REPLACE" == "false" ]]; then
            log_info "Run with --replace to automatically replace stale links with web.archive.org versions"
        fi

        # Return non-zero exit code to indicate stale links were found
        exit 1
    else
        log_info "No stale links found!"
        exit 0
    fi
}

main
