use std::io::{self, Write};

use wolfram_parser::{fmt_as_expr::FmtAsExpr, ParseOptions, StringifyMode};


// #if DIAGNOSTICS
// #include "Diagnostics.h"
// #endif // DIAGNOSTICS

#[derive(Copy, Clone)]
enum ApiMode {
    Expression,
    Tokenize,
    Leaf,
    SafeString,
}

#[derive(Copy, Clone)]
enum OutputMode {
    None,
    Print,
    #[allow(dead_code)]
    PrintDryrun,
    SyntaxQ,
}

fn main() {
    let mut file_input = None;
    let mut api_mode = ApiMode::Expression;
    let mut output_mode = OutputMode::Print;

    let args: Vec<String> = std::env::args().skip(1).collect();

    let mut i = 0;
    loop {
        if i >= args.len() {
            break;
        }

        let arg = &args[i];

        if arg == "-file" {
            i += 1;
            file_input = Some(args[i].clone());
        } else if arg == "-tokenize" {
            api_mode = ApiMode::Tokenize;
        } else if arg == "-leaf" {
            api_mode = ApiMode::Leaf;
        } else if arg == "-safestring" {
            api_mode = ApiMode::SafeString;
        } else if arg == "-n" {
            output_mode = OutputMode::None;
        } else if arg == "-check" || arg == "-syntaxq" || arg == "-syntaxQ" {
            output_mode = OutputMode::SyntaxQ;
        } else {
            panic!()
        }

        i += 1;
    }

    let result = match file_input {
        Some(file_input) => read_file(&file_input, api_mode, output_mode),
        None => read_std_in(api_mode, output_mode),
    };

    return result;
}

fn read_std_in(mode: ApiMode, output_mode: OutputMode) {
    loop {
        let mut input = String::new();

        print!(">>> ");
        io::stdout().flush().unwrap();

        io::stdin().read_line(&mut input).unwrap();

        handle(input.trim_end().as_bytes(), mode, output_mode)
    }

    // #if DIAGNOSTICS
    //         DiagnosticsPrint();
    // #endif // DIAGNOSTICS
}

fn read_file(file: &str, mode: ApiMode, output_mode: OutputMode) {
    let fb: Vec<u8> = std::fs::read(file).expect("error reading file");

    handle(fb.as_slice(), mode, output_mode)

    // #if DIAGNOSTICS
    //     DiagnosticsPrint();
    // #endif // DIAGNOSTICS
}

fn handle(input: &[u8], mode: ApiMode, output_mode: OutputMode) {
    match mode {
        ApiMode::Tokenize => {
            let result =
                wolfram_parser::tokenize_bytes(input, &ParseOptions::default())
                    .unwrap();
            output(output_mode, FmtAsExpr(&result));
        },
        ApiMode::Leaf => {
            let result = wolfram_parser::parse_to_token(
                input,
                &ParseOptions::default(),
                StringifyMode::Normal,
            );
            output(output_mode, FmtAsExpr(result.node_seq()));
        },
        ApiMode::SafeString => {
            let result =
                wolfram_parser::safe_string(input, &ParseOptions::default())
                    .unwrap();
            output(output_mode, result);
        },
        ApiMode::Expression => {
            let result = wolfram_parser::parse_bytes_cst(
                input,
                &ParseOptions::default(),
            );
            output(output_mode, FmtAsExpr(result.node_seq()));
        },
    }
}

fn output<T: std::fmt::Display>(mode: OutputMode, value: T) {
    match mode {
        OutputMode::Print => {
            println!("{value}");
        },
        OutputMode::PrintDryrun => {
            let mut buffer = Vec::new();

            write!(buffer, "{value}\n").unwrap();
        },
        OutputMode::None | OutputMode::SyntaxQ => {},
    }
}
