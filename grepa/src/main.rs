use std::fs::File;
use std::io::{self, BufRead};
use std::process;

use clap::Parser;

use repa::{Regex, RegexError};

#[derive(Clone, Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// A regular expression pattern.
    pattern: String,
    /// A Presburger formula.
    formula: String,
    /// An input filename to find. If it is omitted, we use stdin instead.
    input: Option<String>,
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    let regex = match Regex::compile(args.pattern.as_str(), args.formula.as_str()) {
        Ok(regex) => regex,
        Err(err) => {
            println!("\u{1b}[1;31merror:\u{1b}[0m {}", err);
            match err {
                RegexError::PatternParsingError(err) => {
                    println!();
                    println!("    {}", args.pattern);
                    println!("    {}^", " ".repeat(err.offset()));
                    println!();
                }
                RegexError::FormulaParsingError(err) => {
                    println!();
                    println!("    {}", args.formula);
                    println!("    {}^", " ".repeat(err.offset()));
                    println!();
                }
            }
            process::exit(1);
        }
    };

    let file = File::open(args.input.unwrap_or("/dev/stdin".to_string()))?;
    let lines = io::BufReader::new(file).lines();

    let mut ok = false;

    for read_line in lines {
        let line = read_line?;
        let mut matches = Vec::new();
        let mut last_offset = 0;
        while let Some(m) = regex.execute(&line, last_offset) {
            matches.push((m.capture[0], m.capture[1]));
            if m.capture[0] == m.capture[1] {
                last_offset += 1;
            } else {
                last_offset = m.capture[1];
            }
            if last_offset > line.len() {
                break;
            }
        }
        if matches.is_empty() {
            continue;
        }
        ok = true;
        last_offset = 0;
        for (i, j) in matches {
            print!(
                "{}\u{1b}[33m{}\u{1b}[0m",
                &line[last_offset..i],
                &line[i..j]
            );
            last_offset = j;
        }
        println!("{}", &line[last_offset..]);
    }

    if ok {
        process::exit(0);
    }
    process::exit(1);
}
