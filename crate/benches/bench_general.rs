use criterion::{criterion_group, criterion_main, Criterion};

use wolfram_code_parse::{
    EncodingMode, FirstLineBehavior, ParserSession, SourceConvention::LineColumn,
};

fn parse_tokens(input: &str) {
    let tab_width = 4;
    let mut session = ParserSession::new(
        input.as_bytes(),
        LineColumn,
        tab_width,
        FirstLineBehavior::NotScript,
        EncodingMode::Normal,
    );

    session.tokenize();
}

fn benchmark(c: &mut Criterion) {
    c.bench_function("tokenize 2 + 2", |b| b.iter(|| parse_tokens("2 + 2")));

    let boxes_wl = include_str!("../../CodeParser/Kernel/Boxes.wl");

    c.bench_function("tokenize Boxes.wl", |b| b.iter(|| parse_tokens(boxes_wl)));
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
