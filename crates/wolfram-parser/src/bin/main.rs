use std::io::{self, Write};

use wolfram_parser::{
    fmt_as_expr::FmtAsExpr, ParseOptions, QuirkSettings, StringifyMode,
};


// #if DIAGNOSTICS
// #include "Diagnostics.h"
// #endif // DIAGNOSTICS

#[derive(Copy, Clone)]
enum ApiMode {
    CstExpr,
    Cst,
    Ast,
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
    let mut api_mode = ApiMode::CstExpr;
    let mut output_mode = OutputMode::Print;
    let mut quirks = QuirkSettings::default();

    let args: Vec<String> = std::env::args().skip(1).collect();

    let mut i = 0;
    loop {
        if i >= args.len() {
            break;
        }

        let arg = &args[i];

        match &**arg {
            "-file" => {
                i += 1;
                file_input = Some(args[i].clone());
            },
            "-tokenize" => api_mode = ApiMode::Tokenize,
            "-leaf" => api_mode = ApiMode::Leaf,
            "-safestring" => api_mode = ApiMode::SafeString,
            "--cst" => api_mode = ApiMode::Cst,
            "--ast" => api_mode = ApiMode::Ast,
            "-n" => output_mode = OutputMode::None,
            "-check" | "-syntaxq" | "-syntaxQ" => {
                output_mode = OutputMode::SyntaxQ;
            },
            "--flatten-times" => {
                quirks.flatten_times = true;
            },
            _ => panic!("unrecognized argument: {arg}"),
        }

        i += 1;
    }

    let result = match file_input {
        Some(file_input) => {
            read_file(&file_input, api_mode, output_mode, quirks)
        },
        None => read_std_in(api_mode, output_mode, quirks),
    };

    return result;
}

fn read_std_in(mode: ApiMode, output_mode: OutputMode, quirks: QuirkSettings) {
    loop {
        let mut input = String::new();

        print!(">>> ");
        io::stdout().flush().unwrap();

        io::stdin().read_line(&mut input).unwrap();

        handle(input.trim_end().as_bytes(), mode, output_mode, quirks)
    }

    // #if DIAGNOSTICS
    //         DiagnosticsPrint();
    // #endif // DIAGNOSTICS
}

fn read_file(
    file: &str,
    mode: ApiMode,
    output_mode: OutputMode,
    quirks: QuirkSettings,
) {
    let fb: Vec<u8> = std::fs::read(file).expect("error reading file");

    handle(fb.as_slice(), mode, output_mode, quirks)

    // #if DIAGNOSTICS
    //     DiagnosticsPrint();
    // #endif // DIAGNOSTICS
}

fn handle(
    input: &[u8],
    mode: ApiMode,
    output_mode: OutputMode,
    quirks: QuirkSettings,
) {
    let mut opts = ParseOptions::default();
    opts.quirk_settings = quirks;

    match mode {
        ApiMode::Tokenize => {
            let result = wolfram_parser::tokenize_bytes(input, &opts).unwrap();
            output(output_mode, FmtAsExpr(&result));
        },
        ApiMode::Leaf => {
            let result = wolfram_parser::parse_to_token(
                input,
                &opts,
                StringifyMode::Normal,
            );
            output(output_mode, FmtAsExpr(&result.syntax));
        },
        ApiMode::SafeString => {
            let result = wolfram_parser::safe_string(input, &opts).unwrap();
            output(output_mode, result);
        },
        ApiMode::CstExpr => {
            let result = wolfram_parser::parse_bytes_cst_seq(input, &opts);
            output(output_mode, FmtAsExpr(&result.syntax));
        },
        ApiMode::Cst => {
            let result = wolfram_parser::parse_bytes_cst_seq(input, &opts);
            output(output_mode, format!("{:#?}", result.syntax));
        },
        ApiMode::Ast => {
            let result = wolfram_parser::parse_bytes_ast_seq(input, &opts);
            output(output_mode, format!("{:#?}", result.syntax));
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
