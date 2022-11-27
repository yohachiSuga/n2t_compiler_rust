use crate::{keyword::KeyWord, symbol::Symbol};

#[derive(Debug)]
pub enum TokenType {
    KEYWORD(KeyWord),
    SYMBOL(Symbol),
    IDENTIFIER(String),
    INT_CONST(u64),
    STRING_CONST(String),
}
