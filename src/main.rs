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
    #[clap(short, value_delimiter = ',', default_value = "vm")]
    emit: Vec<String>,
    #[clap(short)]
    debug: bool,
}

#[derive(Debug)]
pub struct EmitOptions {
    emit_xml: bool,
    emit_vm: bool,
}

impl EmitOptions {
    const XML: &str = "xml";
    const VM: &str = "vm";

    pub fn new(emits: &Vec<String>) -> EmitOptions {
        let mut emit_xml = false;
        let mut emit_vm = false;

        for emit in emits {
            if EmitOptions::XML.eq(&emit.to_lowercase()) {
                emit_xml = true;
            }

            if EmitOptions::VM.eq(&emit.to_lowercase()) {
                emit_vm = true;
            }
        }

        EmitOptions { emit_xml, emit_vm }
    }

    pub fn is_emit_xml(&self) -> bool {
        self.emit_xml
    }

    pub fn is_emit_vm(&self) -> bool {
        self.emit_vm
    }
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
        let emits = EmitOptions::new(&args.emit);
        debug!("emits: {:?}", emits);

        let out = Path::new(&file).with_extension("out.xml");
        let reader = BufReader::new(File::open(file).unwrap());
        let writer = BufWriter::new(File::create(out).unwrap());
        let mut engine = CompilerEngine::new(reader, writer, emits);
        engine.compile_class();
    }
}

fn main() {
    env_logger::init();

    let args = Args::parse();
    internal_main(args);
}
