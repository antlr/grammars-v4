// Generated from trgen <version>
use std::env;
use std::fs::{self, File};
use std::io::{self, BufRead, Write};
use std::process;
use std::sync::{Arc, Mutex};
use std::time::Instant;

use antlr4_runtime::{
    CommonTokenStream, ErrorListener, InputStream, IntStream, Recognizer, SyntaxErrorEvent,
};

mod r#gen;
use r#gen::<ophirust_lexer_name>;
use r#gen::<ophirust_parser_name>;

// Shared state for counting and recording syntax errors.
// Uses Arc\<Mutex\<...>> because add_error_listener requires Send + 'static.
struct ListenerState {
    error_count: usize,
    messages: Vec\<String>,
}

struct CountingErrorListener {
    quiet: bool,
    state: Arc\<Mutex\<ListenerState>>,
}

// Implement for any Recognizer so the bound
// `for\<'a> ErrorListener\<dyn Recognizer + 'a> + Send + 'static` is satisfied.
impl\<R: Recognizer + ?Sized> ErrorListener\<R> for CountingErrorListener {
    fn syntax_error(
        &mut self,
        _recognizer: &R,
        event: &SyntaxErrorEvent\<'_>,
    ) {
        if !self.quiet {
            eprintln!("line {}:{} {}", event.line, event.column, event.message);
        }
        let mut state = self.state.lock().unwrap();
        state.error_count += 1;
        state.messages.push(format!("line {}:{} {}", event.line, event.column, event.message));
    }
}

struct Flags {
    inputs: Vec\<String>,
    is_fns: Vec\<bool>,
    prefix: String,
    show_tokens: bool,
    show_tree: bool,
    show_trace: bool,
    tee: bool,
    quiet: bool,
    output_dir: Option\<String>,
}

fn parse_input(
    input_name: &str,
    idx: i32,
    flags: &Flags,
) -> (usize, usize, f64) {
    let out_name: String = if let Some(ref odir) = flags.output_dir {
        let abs = std::fs::canonicalize(input_name)
            .unwrap_or_else(|_| std::env::current_dir().unwrap_or_default().join(input_name));
        let rootless: std::path::PathBuf = abs.components()
            .filter(|c| !matches!(c,
                std::path::Component::Prefix(_) |
                std::path::Component::RootDir))
            .collect();
        let p = std::path::Path::new(odir).join(rootless);
        if let Some(parent) = p.parent() {
            std::fs::create_dir_all(parent).ok();
        }
        p.to_string_lossy().into_owned()
    } else {
        input_name.to_string()
    };
    let lex_state = Arc::new(Mutex::new(ListenerState {
        error_count: 0,
        messages: Vec::new(),
    }));
    let parse_state = Arc::new(Mutex::new(ListenerState {
        error_count: 0,
        messages: Vec::new(),
    }));

    let input = match fs::read_to_string(input_name) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {}: {}", input_name, e);
            return (1, 0, 0.0);
        }
    };

    let istream = InputStream::new(&input);
    let mut lexer = <ophirust_lexer_name>::new(istream);
    lexer.remove_error_listeners();
    lexer.add_error_listener(CountingErrorListener {
        quiet: flags.quiet,
        state: Arc::clone(&lex_state),
    });

    // Note: show_tokens before feeding to CommonTokenStream is not easily
    // supported by this runtime's API; use a debugger or tracing instead.
    let _ = flags.show_tokens;

    let token_stream = CommonTokenStream::new(lexer);
    let mut parser = <ophirust_parser_name>::new(token_stream);
    parser.remove_error_listeners();
    parser.add_error_listener(CountingErrorListener {
        quiet: flags.quiet,
        state: Arc::clone(&parse_state),
    });

    let start = Instant::now();
    let tree = parser.<ophirust_start_symbol>().expect("parser initialization failed");
    let elapsed = start.elapsed();
    let parse_seconds = elapsed.as_secs_f64();

    // Get token count from the buffered token stream.
    // IntStream::size() returns the total number of tokens (including EOF).
    // If this does not compile, check antlr4_runtime's CommonTokenStream API.
    let token_count = parser.token_stream_mut().size() as usize;

    // Build the tree string while parser is still in scope (borrows &parser).
    let tree_str = if flags.show_tree {
        let rule_names: Vec\<&str> = parser.rule_names().iter().map(|s| s.as_str()).collect();
        Some(parser.node(tree).to_string_tree_with_names(&rule_names))
    } else {
        None
    };

    let lex_err = lex_state.lock().unwrap().error_count;
    let parse_err = parse_state.lock().unwrap().error_count;
    let error_count = lex_err + parse_err;

    // Write the tree file (always when -tee -tree, regardless of errors,
    // so baselines can be committed and diffed by the test harness).
    if let Some(ref ts) = tree_str {
        if flags.tee {
            let mut f = File::create(format!("{}.tree", out_name)).unwrap();
            write!(f, "{}", ts).ok();
        } else {
            eprintln!("{}", ts);
        }
    }

    // Write the errors file only when there are actual errors so that
    // empty .errors files are never left behind for Maven to misinterpret.
    if flags.tee && error_count > 0 {
        let lex_msgs = lex_state.lock().unwrap().messages.clone();
        let parse_msgs = parse_state.lock().unwrap().messages.clone();
        let mut f = File::create(format!("{}.errors", out_name)).unwrap();
        for msg in lex_msgs.iter().chain(parse_msgs.iter()) {
            writeln!(f, "{}", msg).ok();
        }
    }

    if !flags.quiet {
        eprint!(
            "{}OphiRust {} {} {} {:.3} s {} tokens {:.0} tps\n",
            flags.prefix,
            idx,
            input_name,
            if error_count > 0 { "fail" } else { "success" },
            parse_seconds,
            token_count,
            token_count as f64 / parse_seconds,
        );
    }

    if error_count > 0 { (1, token_count, parse_seconds) } else { (0, token_count, parse_seconds) }
}

fn main() {
    let mut flags = Flags {
        inputs: Vec::new(),
        is_fns: Vec::new(),
        prefix: String::new(),
        show_tokens: false,
        show_tree: false,
        show_trace: false,
        tee: false,
        quiet: false,
        output_dir: None,
    };

    let args: Vec\<String> = env::args().collect();
    let mut i = 1;
    while i \< args.len() {
        match args[i].as_str() {
            "-tokens" => flags.show_tokens = true,
            "-tree" => flags.show_tree = true,
            "-prefix" => {
                i += 1;
                flags.prefix = format!("{} ", args[i]);
            }
            "-input" => {
                i += 1;
                flags.inputs.push(args[i].clone());
                flags.is_fns.push(false);
            }
            "-tee" => flags.tee = true,
            "-o" => {
                i += 1;
                flags.output_dir = Some(args[i].clone());
                flags.tee = true;
            }
            "-q" => flags.quiet = true,
            "-trace" => flags.show_trace = true,
            "-x" => {
                let stdin = io::stdin();
                for line in stdin.lock().lines() {
                    let ln = line.unwrap();
                    flags.inputs.push(ln);
                    flags.is_fns.push(true);
                }
            }
            other => {
                if other.to_string().as_bytes()[0] != b'-' {
                    flags.inputs.push(other.to_string());
                    flags.is_fns.push(true);
                }
            }
        }
        i += 1;
    }

    if flags.inputs.is_empty() {
        process::exit(1);
    } else {
        let mut exit_code = 0;
        let mut total_tokens: usize = 0;
        let mut total_parse_seconds: f64 = 0.0;
        let mut first_file_tokens: usize = 0;
        let mut first_file_parse_seconds: f64 = 0.0;
        let start_all = Instant::now();
        for (idx, input) in flags.inputs.iter().enumerate() {
            let (rc, tc, ps) = parse_input(input, idx as i32, &flags);
            total_tokens += tc;
            total_parse_seconds += ps;
            if idx == 0 {
                first_file_tokens = tc;
                first_file_parse_seconds = ps;
            }
            if rc > 0 {
                exit_code = 1;
            }
        }
        let elapsed = start_all.elapsed();
        if !flags.quiet {
            let overall_seconds = elapsed.as_secs_f64();
            let warm_tokens = total_tokens - first_file_tokens;
            let warm_seconds = total_parse_seconds - first_file_parse_seconds;
            let warm_tps = if flags.inputs.len() > 1 && warm_seconds > 0.0 {
                format!("{:.0}", warm_tokens as f64 / warm_seconds)
            } else {
                "n.a.".to_string()
            };
            let first_tps = if first_file_parse_seconds > 0.0 {
                first_file_tokens as f64 / first_file_parse_seconds
            } else {
                0.0
            };
            let speedup = if flags.inputs.len() > 1 && warm_seconds > 0.0 && first_tps > 0.0 {
                format!("{:.2}", (warm_tokens as f64 / warm_seconds) / first_tps)
            } else {
                "n.a.".to_string()
            };
            eprintln!("{}PT: {:.3}", flags.prefix, total_parse_seconds);
            eprintln!("{}OT: {:.3}", flags.prefix, overall_seconds - total_parse_seconds);
            eprintln!("{}TT: {:.3}", flags.prefix, overall_seconds);
            eprintln!("{}TPS: {:.0}", flags.prefix, total_tokens as f64 / total_parse_seconds);
            eprintln!("{}Post-warmup TPS: {}", flags.prefix, warm_tps);
            eprintln!("{}Post-warmup speed up: {}", flags.prefix, speedup);
        }
        process::exit(exit_code as i32);
    }
}
