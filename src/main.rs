use std::{
    fs::{self, File},
    io::{BufReader, BufWriter},
    path::Path,
    thread::panicking,
};

use clap::Parser;
use log::{debug, info};

use crate::{compilerEngine::CompilerEngine, vmWriter::VMWriter};

mod compilerEngine;
mod error;
mod jackAnalyzer;
mod jackTokenizer;
mod keyword;
mod symbol;
mod symbolTable;
mod tokenType;
mod vmWriter;

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
    emit_extend_xml: bool,
    emit_vm: bool,
}

impl EmitOptions {
    const XML: &str = "xml";
    const EX_XML: &str = "ex-xml";
    const VM: &str = "vm";

    pub fn new(emits: &Vec<String>) -> EmitOptions {
        let mut emit_xml = false;
        let mut emit_vm = false;
        let mut emit_ex_xml = false;

        for emit in emits {
            if EmitOptions::XML.eq(&emit.to_lowercase()) {
                info!("xml is enabled");
                emit_xml = true;
            }

            if EmitOptions::EX_XML.eq(&emit.to_lowercase()) {
                info!("ex-xml is enabled");
                emit_ex_xml = true;
            }

            if EmitOptions::VM.eq(&emit.to_lowercase()) {
                info!("vm is enabled");
                emit_vm = true;
            }
        }

        if emit_vm && (emit_xml || emit_ex_xml) {
            panic!("does not emit vm and xml at the same time.");
        }

        EmitOptions {
            emit_xml,
            emit_extend_xml: emit_ex_xml,
            emit_vm,
        }
    }

    pub fn is_emit_xml(&self) -> bool {
        self.emit_xml
    }

    pub fn is_emit_ex_xml(&self) -> bool {
        self.emit_extend_xml
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

        let reader = BufReader::new(File::open(&file).unwrap());
        let writer;
        let vm_writer;
        {
            if emits.emit_extend_xml || emits.emit_xml {
                let out = Path::new(&file).with_extension("out.xml");
                writer = BufWriter::new(File::create(out).unwrap());
                vm_writer = VMWriter::new(BufWriter::new(File::open("/dev/null").unwrap()));
            } else if emits.emit_vm {
                let out = Path::new(&file).with_extension("out.vm");
                vm_writer = VMWriter::new(BufWriter::new(File::create(out).unwrap()));
                writer = BufWriter::new(File::open("/dev/null").unwrap());
            } else {
                panic!("cannot emit xml and vm at the same time.")
            }
        }

        let mut engine = CompilerEngine::new(reader, writer, vm_writer, emits);
        engine.compile_class();
    }
}

fn main() {
    env_logger::init();

    let args = Args::parse();
    internal_main(args);
}
