use clap::Parser;

mod compilerEngine;
mod jackAnalyzer;
mod jackTokenizer;
mod keyword;
mod symbol;
mod tokenType;

#[derive(Parser, Debug)]
pub struct Args {
    #[clap(short)]
    input: String,
    #[clap(short)]
    out: String,
    #[clap(short)]
    debug: bool,
}

fn main() {
    println!("Hello, world!");
    env_logger::init();

    let args = Args::parse();
}

/***
 * TODO:
 * 1.
 * - impl jackTokenizer API
 * - impl test program which uses jackTokenizer and generates specific output XML
 * 2.
 * - impl jackAnalyzer
 * - impl compilerEngine
 *      as input, pass jackTokenizer object
 *      only compile class is public method
 */
