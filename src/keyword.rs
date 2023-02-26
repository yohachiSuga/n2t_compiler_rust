use strum::IntoEnumIterator;

#[derive(
    Debug,
    PartialEq,
    Clone,
    strum_macros::EnumString,
    strum_macros::Display,
    strum_macros::IntoStaticStr,
    strum_macros::EnumIter,
)]
pub enum KeyWord {
    #[strum(serialize = "class")]
    CLASS,
    #[strum(serialize = "method")]
    METHOD,
    #[strum(serialize = "function")]
    FUNCTION,
    #[strum(serialize = "constructor")]
    CONSTRUCTOR,
    #[strum(serialize = "int")]
    INT,
    #[strum(serialize = "boolean")]
    BOOLEAN,
    #[strum(serialize = "char")]
    CHAR,
    #[strum(serialize = "void")]
    VOID,
    #[strum(serialize = "var")]
    VAR,
    #[strum(serialize = "static")]
    STATIC,
    #[strum(serialize = "field")]
    FIELD,
    #[strum(serialize = "let")]
    LET,
    #[strum(serialize = "do")]
    DO,
    #[strum(serialize = "if")]
    IF,
    #[strum(serialize = "else")]
    ELSE,
    #[strum(serialize = "while")]
    WHILE,
    #[strum(serialize = "return")]
    RETURN,
    #[strum(serialize = "true")]
    TRUE,
    #[strum(serialize = "false")]
    FALSE,
    #[strum(serialize = "null")]
    NULL,
    #[strum(serialize = "this")]
    THIS,
    // SPECIAL KEYWORD to store identifier and its name
    IDENTIFIER(String),
}
