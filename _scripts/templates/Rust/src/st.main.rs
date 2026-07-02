// Generated from trgen <version>
use std::env;
use std::fs::File;
use std::io::{self, BufRead, Write};
use std::process;
use std::time::Instant;
use antlr4rust::common_token_stream::CommonTokenStream;
use antlr4rust::error_listener::ErrorListener;
use antlr4rust::input_stream::InputStream;
use antlr4rust::recognizer::Recognizer;
use antlr4rust::token::Token;
use antlr4rust::TokenSource;
use std::fs;
mod r#gen;
use r#gen::<lexer_name>;
use r#gen::<parser_name>;
use std::rc::Rc;
use std::cell::RefCell;
use antlr4rust::token_factory::TokenFactory;
use antlr4rust::errors::ANTLRError;
use std::error::Error;
use std::fmt::Display;
use antlr4rust::{Parser as AntlrParser};
use antlr4rust::tree::*;

fn parse_input(
    input_name: &str,
    idx: i32,
    flags: &Flags,
) -> (usize, usize, f64) {
        let writer: Rc\<RefCell\<Box\<dyn Write>>> = Rc::new(RefCell::new(
                if flags.tee {
                        Box::new(File::create(format!("{}.errors", input_name)).unwrap()) as Box\<dyn Write>
                } else {
                        Box::new(io::sink()) as Box\<dyn Write>
                }
        ));

    let my_string_result = fs::read_to_string(input_name);
    let input = my_string_result.unwrap(); // Panics if Err
    let istream = InputStream::new(input.as_bytes());
    let mut lexer = <lexer_name>::new(istream);
    lexer.remove_error_listeners();
    let lec = Rc::new(RefCell::new(0));
    let lel = Box::new(ParserErrorListener {
        quiet: flags.quiet,
                tee: flags.tee,
        error_count: Rc::clone(&lec),
                output: Rc::clone(&writer),
        });
    lexer.add_error_listener(lel);

    if flags.show_tokens {
        let mut i: isize = 0;
        loop {
                let token = lexer.next_token();
                let token_type = token.get_token_type();
                token.set_token_index(i);
                i = i + 1;
                eprintln!("{}", token.to_string());
                if token_type == -1 {
                        break;
                }
        }
        // no lexer.reset();
    }

    let token_stream = CommonTokenStream::new(lexer);
    let mut parser = <parser_name>::new(token_stream);
    parser.remove_error_listeners();
    let pec = Rc::new(RefCell::new(0));
    let pel = Box::new(ParserErrorListener {
        quiet: flags.quiet,
        tee: flags.tee,
        error_count: Rc::clone(&pec),
        output: Rc::clone(&writer),
        });
    parser.add_error_listener(pel);

    let start = Instant::now();
    let tree = parser.<start_symbol>().expect("parsing failed setup");
    let elapsed = start.elapsed();
    let parse_seconds = elapsed.as_secs_f64();
    let token_count = parser.get_input_stream_mut().size() as usize;

    let error_cnt = *lec.borrow() + *pec.borrow();

    if flags.show_tree {
        let tree_str = tree.to_string_tree(&*parser);
        if flags.tee {
            let mut f = File::create(format!("{}.tree", input_name)).unwrap();
            write!(f, "{}", tree_str).ok();
        } else {
            eprintln!("{}", tree_str);
        }
    }

    if !flags.quiet {
        eprint!("{}Rust {} {} {} {:.3} s {} tokens {:.0} tps\n",
            flags.prefix, idx, input_name,
            if error_cnt > 0 { "fail" } else { "success" },
            parse_seconds,
            token_count,
            token_count as f64 / parse_seconds
        );
    }

    if error_cnt > 0 { (1, token_count, parse_seconds) } else { (0, token_count, parse_seconds) }
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
            let first_tps = if first_file_parse_seconds > 0.0 { first_file_tokens as f64 / first_file_parse_seconds } else { 0.0 };
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

struct ParserErrorListener {
    quiet: bool,
    tee: bool,
    error_count: Rc\<RefCell\<i32>>,
    output: Rc\<RefCell\<Box\<dyn Write>>>,
}

impl\<'a, T: Recognizer\<'a>> ErrorListener\<'a, T> for ParserErrorListener {
    fn syntax_error(
        &self,
        _recognizer: &T,
        _offending_symbol: Option\<&\<T::TF as TokenFactory\<'a>>::Inner>,
        line: isize,
        column: isize,
        msg: &str,
        _error: Option\<&ANTLRError>,
    ) {
        *self.error_count.borrow_mut() += 1;
        if self.tee {
            writeln!(self.output.borrow_mut().as_mut(), "line {}:{} {}", line, column, msg).ok();
        }
        if !self.quiet {
            eprintln!("line {}:{} {}", line, column, msg);
        }
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct ParseError {
    pub source: Option\<Box\<dyn Error + Send + Sync + 'static>>,
    pub pos: (isize, isize),
    pub msg: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter\<'_>) -> std::fmt::Result {
        write!(
            f,
            "ERROR: \<input>:{}:{}: {}",
            self.pos.0, self.pos.1, self.msg
        )?;
        Ok(())
    }
}

impl Error for ParseError {}
