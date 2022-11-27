use std::{
    collections::VecDeque,
    io::{BufRead, Lines, Read},
    iter::Peekable,
    str::FromStr,
};

use log::{debug, info};
use regex::Regex;

use crate::{
    keyword::KeyWord,
    symbol::Symbol,
    tokenType::{self, TokenType},
};

struct JackTokenizer<R: BufRead> {
    line_iter: Peekable<Lines<R>>,
    // line_token_iter: Option<Peekable<Lines<R>>>,
    // line: String,
    curr_token: Option<String>,
    line_tokens: VecDeque<String>,
    remover: Regex,
}

impl<R> JackTokenizer<R>
where
    R: Read + BufRead,
{
    fn new(reader: R) -> JackTokenizer<R> {
        JackTokenizer {
            line_iter: reader.lines().peekable(),
            line_tokens: VecDeque::new(),
            // line_token_iter: None,
            // line: String::new(),
            curr_token: None,
            remover: Regex::new(r"/\*[\s\S]*?\*/|//.*").unwrap(),
            // remover: Regex::new(r"^(.*)(//)?.*").unwrap(),
            // remover: Regex::new(r"^(.*)//.*").unwrap(),
        }
    }

    fn handlePreDelimiter(&self, token_cand: &str) -> TokenType {
        match token_cand.parse::<u64>() {
            Ok(num) => TokenType::INT_CONST(num),
            Err(_) => {
                // shall be keyword or identifier
                match KeyWord::from_str(&token_cand) {
                    Ok(keyword) => TokenType::KEYWORD(keyword),
                    Err(_) => TokenType::IDENTIFIER(token_cand.to_string()),
                }
            }
        }
    }

    fn generateTokens(&self, input: &str) -> VecDeque<TokenType> {
        // Delimiter: whitespace or symbol
        let mut found_tokens = VecDeque::new();
        let mut is_string_const = false;

        let mut token_cand = String::with_capacity(30);
        for c in input.chars() {
            if (is_string_const) {
                token_cand.push(c);
                if c == '"' {
                    is_string_const = false;
                    found_tokens.push_back(TokenType::STRING_CONST(token_cand.clone()));
                    token_cand.clear();
                    continue;
                }
            }

            if (c.is_whitespace()) {
                // put stacked element
                let token = self.handlePreDelimiter(&token_cand);
                found_tokens.push_back(token);
                token_cand.clear();
                continue;
            }

            if (c == '"') {
                is_string_const = true;
                continue;
            }

            let c_string = c.to_string();
            if (Symbol::from_str(&c_string).is_ok()) {
                if !token_cand.is_empty() {
                    // handle pre-symbol element
                    let token = self.handlePreDelimiter(&token_cand);
                    found_tokens.push_back(token);
                    token_cand.clear();
                }
                found_tokens.push_back(TokenType::SYMBOL(Symbol::from_str(&c_string).unwrap()));
                continue;
            }

            // some part of word
            token_cand.push(c);
        }

        if is_string_const {
            panic!("string does not terminate.");
        }
        found_tokens
    }

    fn hasMoreTokens(&mut self) -> bool {
        if !self.line_tokens.is_empty() {
            return true;
        }
        if self.line_tokens.is_empty() {
            if self.line_iter.peek().is_none() {
                return false;
            } else {
                // need to take care comment sentence after final code block
                loop {
                    match self.line_iter.next() {
                        Some(line) => match line {
                            Ok(line) => {
                                let trimmed_line = line.trim();
                                // TODO: remove multiline-comments
                                let replaced = self.remover.replace_all(trimmed_line, "");
                                let replaced = replaced.trim();
                                if !replaced.is_empty() {
                                    let mut tokens = VecDeque::new();
                                    let mut c = replaced.chars();
                                    loop {
                                        let mut token_cand = String::new();
                                        match c.next() {
                                            Some(character) => {
                                                if (character.is_whitespace()) {
                                                    debug!(
                                                        "insert token {} skip white space",
                                                        token_cand
                                                    );
                                                    tokens.push_back(token_cand.clone());
                                                    token_cand.clear();
                                                } else {
                                                    token_cand.push(character);
                                                }
                                            }
                                            None => break,
                                        }
                                    }

                                    self.line_tokens = tokens;
                                    debug!("set line tokens {:?}", self.line_tokens);
                                    return true;
                                } else {
                                    info!("token is empty.");
                                }
                            }
                            Err(_) => {
                                panic!("failed to line iterator next")
                            }
                        },
                        None => {
                            info!("hasMoreTokens finish reading");
                            return false;
                        }
                    }
                }
            }
        }

        // do not reach here
        return false;
    }

    fn advance(&mut self) {
        if self.line_tokens.is_empty() {
            if self.hasMoreTokens() {
                info!("set token");
            } else {
                info!("not advanced because empty.");
                return;
            }
        }
        self.curr_token = self.line_tokens.pop_front();
        debug!("current token:{:?}", self.curr_token);
    }

    fn tokenType() -> tokenType::TokenType {
        todo!()
    }

    fn keyword() -> KeyWord {
        todo!()
    }

    fn symbol() -> String {
        todo!()
    }

    fn identifier() -> String {
        todo!()
    }

    fn intVal() -> i64 {
        todo!()
    }

    fn stringVal() -> String {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::BufReader};

    use log::info;
    use regex::Regex;

    use super::JackTokenizer;

    #[test]
    fn regex_test() {
        // let remover = Regex::new(r"^(.*)?(//)?.*").unwrap();
        let remover = Regex::new(r"/\*[\s\S]*?\*/|//.*").unwrap();
        let caps = remover.replace_all("abc // test commenet", "");

        // assert_eq!(Some(3), caps.as_ref().and_then(|caps| Some(caps.len())));
        eprintln!("{:?}", caps);
        // assert_eq!("abc //", caps.as_ref().unwrap().get(0).unwrap().as_str());
        // assert_eq!("abc ", caps.as_ref().unwrap().get(1).unwrap().as_str());

        let caps = remover.replace_all("", "");
        eprintln!("{:?}", caps);
        // assert_eq!(None, caps.as_ref().and_then(|caps| Some(caps.len())));

        let caps = remover.replace_all("// test ", "");
        eprintln!("{:?}", caps);
        // assert_eq!(3, caps.as_ref().unwrap().len());

        let caps = remover.replace_all(
            "class { 
             /** test */",
            "",
        );
        eprintln!("{:?}", caps.trim());

        let caps = remover.captures("/** test comment */");
        eprintln!("{:?}", caps);
        let caps = remover.captures("/* test comment */");
        eprintln!("{:?}", caps);
        // assert_eq!(
        //     Some(3),
        //     remover
        //         .captures("class { ")
        //         .and_then(|caps| Some(caps.len()))
        // );
    }

    #[test]
    fn test_tokenizer() {
        env_logger::init();
        let file = File::open("./ArrayTest/Main.jack").unwrap();
        let reader = BufReader::new(file);
        let mut tokenizer = JackTokenizer::new(reader);
        let token = tokenizer.generateTokens("class Array {");
        assert_eq!(token.len(), 3);
        let token = tokenizer.generateTokens("class Array{");
        assert_eq!(token.len(), 3);
        let token = tokenizer.generateTokens("int main();");
        info!("{:?}", token);
        assert_eq!(token.len(), 5);
    }

    #[test]
    fn test_simple() {
        env_logger::init();
        let file = File::open("./ArrayTest/Main.jack").unwrap();
        let reader = BufReader::new(file);
        let mut tokenizer = JackTokenizer::new(reader);
        while (tokenizer.hasMoreTokens()) {
            tokenizer.advance();
        }
    }
}
