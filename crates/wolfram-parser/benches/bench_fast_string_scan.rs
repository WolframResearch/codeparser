use std::{fs, path::Path};

use criterion::{criterion_group, criterion_main, Criterion, SamplingMode};

use wolfram_parser::ParseOptions;

fn parse_tokens(input: &str) {
    parse_tokens_u8(input.as_bytes())
}

fn parse_tokens_u8(input: &[u8]) {
    wolfram_parser::tokenize_bytes(input, &ParseOptions::default()).unwrap();
}

fn benchmark(c: &mut Criterion) {
    println!("\n==== Legend ====");
    println!("FSS: FAST_STRING_SCAN = true");
    println!("================\n");

    c.bench_function("[FSS] tokenize 2 + 2", |b| {
        b.iter(|| parse_tokens("2 + 2"))
    });

    let boxes_wl = include_str!("../../../CodeParser/Kernel/Boxes.wl");
    c.bench_function("[FSS] tokenize Boxes.wl", |b| {
        b.iter(|| parse_tokens(boxes_wl))
    });

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
        fs::read(Path::new("../Tests/files/large/ReliefPlot.nb")).unwrap();
    group.bench_function("[FSS] tokenize ReliefPlot.nb", |b| {
        b.iter(|| parse_tokens_u8(&relief_plot))
    });

    let expanded_company_data_new =
        fs::read(Path::new("../Tests/files/large/expandedCompanyDataNew1.m"))
            .unwrap();
    group.bench_function("[FSS] tokenize expandedCompanyDataNew1.m", |b| {
        b.iter(|| parse_tokens_u8(&expanded_company_data_new))
    });

    //-------------
    // Medium files
    //-------------

    group.sampling_mode(SamplingMode::Auto);
    group.sample_size(30);

    let geomagnetic_models =
        fs::read(Path::new("../Tests/files/large/geomagneticmodels.m"))
            .unwrap();
    group.bench_function("[FSS] tokenize geomagneticmodels.m", |b| {
        b.iter(|| parse_tokens_u8(&geomagnetic_models))
    });
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
