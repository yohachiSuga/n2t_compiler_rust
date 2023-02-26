use thiserror::Error;

#[derive(Error, Debug)]
#[error("{message:} ({code:})")]
pub struct CompilerError {
    message: String,
    pub code: i64,
}

impl CompilerError {
    pub fn new(message: &str, code: i64) -> CompilerError {
        CompilerError {
            message: message.to_string(),
            code,
        }
    }
}

pub const NOT_EXIST_TABLE: i64 = -1;
pub const ALREADY_DEFINED: i64 = -2;
pub const UNKOWN_ERROR: i64 = -3;
