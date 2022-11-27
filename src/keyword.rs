use strum::IntoEnumIterator;

#[derive(
    Debug,
    PartialEq,
    Clone,
    Copy,
    strum_macros::EnumString,
    strum_macros::Display,
    strum_macros::IntoStaticStr,
    strum_macros::EnumIter,
)]
pub enum KeyWord {
    #[strum(serialize = "class")]
    CLASS,
    METHOD,
    FUNCTION,
    CONSTRUCTOR,
    #[strum(serialize = "int")]
    INT,
    BOOLEAN,
    CHAR,
    VOID,
    VAR,
    STATIC,
    FIELD,
    LET,
    DO,
    IF,
    ELSE,
    WHILE,
    RETURN,
    TRUE,
    FALSE,
    NULL,
    THIS,
}
