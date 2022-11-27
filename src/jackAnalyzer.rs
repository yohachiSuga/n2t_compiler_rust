use std::{
    fs::{self, File},
    io::BufWriter,
    path::Path,
};

use log::info;

use crate::Args;

struct jackAnalyzer {}

impl jackAnalyzer {
    fn new(args: Args) {
        let mut files = vec![];
        if Path::new(&args.input).is_dir() {
            for entry in fs::read_dir(&args.input).unwrap() {
                if let Ok(e) = entry {
                    let path = e.path();
                    match path.extension() {
                        Some(ext) => {
                            if ext == "vm" {
                                info!("load vm file {:?}", path);
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
            info!("load vm file {}", args.input);
            files.push(args.input);
        }

        if files.len() == 0 {
            panic!("no length");
        }

        // let file = File::create(args.out).unwrap();
        // let mut writer = BufWriter::new(file);
        // let mut writer = writer::CodeWriter::new(writer);
    }
}
