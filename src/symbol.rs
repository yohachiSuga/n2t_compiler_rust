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
pub enum Symbol {
    #[strum(serialize = "{")]
    left_curly_bracket,
    #[strum(serialize = "}")]
    right_curly_bracket,
    #[strum(serialize = "(")]
    left_bracket,
    #[strum(serialize = ")")]
    right_bracket,
    #[strum(serialize = "[")]
    left_square_bracket,
    #[strum(serialize = "]")]
    right_square_bracket,

    #[strum(serialize = ".")]
    period,

    #[strum(serialize = ",")]
    comma,
    #[strum(serialize = ";")]
    semicolon,
    #[strum(serialize = "+")]
    plus,
    #[strum(serialize = "-")]
    minus,
    #[strum(serialize = "*")]
    star,
    #[strum(serialize = "/")]
    slash,
    #[strum(serialize = "&")]
    ampersand,
    #[strum(serialize = "|")]
    pipe,
    #[strum(serialize = "<")]
    lt,
    #[strum(serialize = ">")]
    bt,
    #[strum(serialize = "=")]
    equal,
    #[strum(serialize = "~")]
    tilde,
}
