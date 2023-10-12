use std::{fs, path::Path};

use criterion::{criterion_group, criterion_main, Criterion, SamplingMode};

use wolfram_parser::ParseOptions;

fn tokenize(input: &str) {
    tokenize_bytes(input.as_bytes())
}

fn tokenize_bytes(input: &[u8]) {
    wolfram_parser::tokenize_bytes(input, &ParseOptions::default()).unwrap();
}

fn parse(input: &str) {
    wolfram_parser::parse_cst_seq(input, &ParseOptions::default());
}

fn parse_bytes(input: &[u8]) {
    wolfram_parser::parse_bytes_cst_seq(input, &ParseOptions::default());
}

fn benchmark(c: &mut Criterion) {
    c.bench_function("tokenize 2 + 2", |b| b.iter(|| tokenize("2 + 2")));

    let boxes_wl = include_str!("../../../CodeParser/Kernel/Boxes.wl");
    c.bench_function("tokenize Boxes.wl", |b| b.iter(|| tokenize(boxes_wl)));
    c.bench_function("parse CST of Boxes.wl", |b| b.iter(|| parse(boxes_wl)));

    benchmark_large_files(c);
}

fn benchmark_large_files(c: &mut Criterion) {
    let mut group = c.benchmark_group("large files");
    group.sampling_mode(SamplingMode::Flat);
    group.sample_size(10);

    //------------
    // Large files
    //------------

    let relief_plot =
        fs::read(Path::new("../../Tests/files/large/ReliefPlot.nb")).unwrap();
    group.bench_function("parse CST of ReliefPlot.nb", |b| {
        b.iter(|| parse_bytes(&relief_plot))
    });

    let expanded_company_data_new = fs::read(Path::new(
        "../../Tests/files/large/expandedCompanyDataNew1.m",
    ))
    .unwrap();
    group.bench_function("parse CST of expandedCompanyDataNew1.m", |b| {
        b.iter(|| parse_bytes(&expanded_company_data_new))
    });

    //-------------
    // Medium files
    //-------------

    group.sampling_mode(SamplingMode::Auto);
    group.sample_size(30);

    let geomagnetic_models =
        fs::read(Path::new("../../Tests/files/large/geomagneticmodels.m"))
            .unwrap();
    group.bench_function("parse CST of geomagneticmodels.m", |b| {
        b.iter(|| parse_bytes(&geomagnetic_models))
    });
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
