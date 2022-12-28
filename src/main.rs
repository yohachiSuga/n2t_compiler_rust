use std::{
    fs::{self, File},
    io::{BufReader, BufWriter},
    path::Path,
};

use clap::Parser;
use log::{debug, info};

use crate::compilerEngine::CompilerEngine;

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
    debug: bool,
}

fn internal_main(args: Args) {
    let mut files = vec![];
    if Path::new(&args.input).is_dir() {
        for entry in fs::read_dir(&args.input).unwrap() {
            if let Ok(e) = entry {
                let path = e.path();
                match path.extension() {
                    Some(ext) => {
                        if ext == "jack" {
                            info!("load jack file {:?}", path);
                            files.push(path.to_str().unwrap().to_string());
                        }
                    }
                    None => {
                        // do nothing for folders
                    }
                }
            } else {
                panic!("panic");
            }
        }
    } else {
        info!("load jack file {}", args.input);
        files.push(args.input);
    }

    if files.len() == 0 {
        panic!("no jack file found");
    }

    debug!("compile target {:?}", files);
    for file in files {
        let out = Path::new(&file).with_extension("out.xml");
        let reader = BufReader::new(File::open(file).unwrap());
        let writer = BufWriter::new(File::create(out).unwrap());
        let mut engine = CompilerEngine::new(reader, writer);
        engine.compile_class();
    }
}

fn main() {
    env_logger::init();

    let args = Args::parse();
    internal_main(args);
}
