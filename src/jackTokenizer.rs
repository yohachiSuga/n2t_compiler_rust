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
    curr_token: Option<TokenType>,
    line_tokens: VecDeque<TokenType>,
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
            curr_token: None,
            remover: Regex::new(r"/\*[\s\S]*?\*/|//.*").unwrap(),
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
            if is_string_const {
                if c == '"' {
                    is_string_const = false;
                    debug!("END STRING CONSTANT:{token_cand}");
                    found_tokens.push_back(TokenType::STRING_CONST(token_cand.clone()));
                    token_cand.clear();
                    continue;
                }

                token_cand.push(c);
                continue;
            }

            if c.is_whitespace() {
                if token_cand.len() == 0 {
                    continue;
                }
                // put stacked element
                let token = self.handlePreDelimiter(&token_cand);
                found_tokens.push_back(token);
                token_cand.clear();
                continue;
            }

            if c == '"' {
                debug!("start STRING CONSTANT:{token_cand}");
                is_string_const = true;
                continue;
            }

            let c_string = c.to_string();
            if Symbol::from_str(&c_string).is_ok() {
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
            panic!("string does not terminate. something wrong??");
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
                let mut is_multiple_line_comment = false;
                loop {
                    match self.line_iter.next() {
                        Some(line) => match line {
                            Ok(line) => {
                                let trimmed_line = line.trim();
                                let replaced = self.remover.replace_all(trimmed_line, "");
                                let replaced = replaced.trim();

                                if !replaced.is_empty() {
                                    if replaced.starts_with("/**") {
                                        is_multiple_line_comment = true;
                                    }
                                    if replaced.ends_with("*/") {
                                        is_multiple_line_comment = false;
                                        continue;
                                    }
                                    if is_multiple_line_comment {
                                        debug!("skip multiple line comments");
                                        continue;
                                    }

                                    // fill line_tokens
                                    self.line_tokens.extend(self.generateTokens(replaced));
                                    debug!("set line tokens {:?}", self.line_tokens);
                                    return true;
                                } else {
                                    debug!("commented line.");
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
        info!("current token:{:?}", self.curr_token);
    }

    fn tokenType(&self) -> &tokenType::TokenType {
        match &self.curr_token {
            Some(curr_token) => &curr_token,
            None => {
                panic!("not have current token");
            }
        }
    }

    fn keyword(&self) -> &KeyWord {
        match &self.curr_token {
            Some(curr_token) => match curr_token {
                TokenType::KEYWORD(key) => key,
                _ => {
                    panic!("do not called keyword")
                }
            },
            None => {
                panic!("not have current token");
            }
        }
    }

    fn symbol(&self) -> String {
        match &self.curr_token {
            Some(curr_token) => match curr_token {
                TokenType::SYMBOL(string) => string.to_string(),
                _ => {
                    panic!("do not called keyword")
                }
            },
            None => {
                panic!("not have current token");
            }
        }
    }

    fn identifier(&self) -> String {
        match &self.curr_token {
            Some(curr_token) => match curr_token {
                TokenType::IDENTIFIER(string) => string.to_string(),
                _ => {
                    panic!("do not called keyword")
                }
            },
            None => {
                panic!("not have current token");
            }
        }
    }

    fn intVal(&self) -> u64 {
        match &self.curr_token {
            Some(curr_token) => match curr_token {
                TokenType::INT_CONST(num) => *num,
                _ => {
                    panic!("do not called keyword")
                }
            },
            None => {
                panic!("not have current token");
            }
        }
    }

    fn stringVal(&self) -> &String {
        match &self.curr_token {
            Some(curr_token) => match curr_token {
                TokenType::STRING_CONST(string) => string,
                _ => {
                    panic!("do not called keyword")
                }
            },
            None => {
                panic!("not have current token");
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{
        fs::File,
        io::{BufReader, BufWriter, LineWriter, Write},
        process::{Command, ExitStatus},
    };

    use log::info;
    use regex::Regex;

    use crate::tokenType::TokenType;

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
    fn test_remove_comment() {
        // env_logger::init();
        let file = File::open("./Square/SquareGame.jack").unwrap();
        let reader = BufReader::new(file);
        let mut tokenizer = JackTokenizer::new(reader);
    }

    #[test]
    fn test_generate_token() {
        // env_logger::init();
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

        let token =
            tokenizer.generateTokens("let a[i] = Keyboard.readInt(\"ENTER THE NEXT NUMBER: \");");
        info!("{:?}", token);
        assert_eq!(token.len(), 13);
    }

    fn escape_xml_attribute(input: &str) -> &str {
        if input == "<" {
            return "&lt;";
        }
        if input == ">" {
            return "&gt;";
        }
        if input == "&" {
            return "&amp;";
        }
        return input;
    }

    #[test]
    fn test_tokenizer() {
        env_logger::init();

        let src_files = vec![
            "./ArrayTest/Main.jack",
            "./Square/Main.jack",
            "./Square/Square.jack",
            "./Square/SquareGame.jack",
        ];

        let comp_files = vec![
            "./ArrayTest/MainT.xml",
            "./Square/MainT.xml",
            "./Square/SquareT.xml",
            "./Square/SquareGameT.xml",
        ];
        for (i, src_file_path) in src_files.iter().enumerate() {
            let src_file = File::open(src_file_path).unwrap();
            let out_file_path = format!("{src_file_path}.out.xml");
            let out_file = File::create(&out_file_path).unwrap();

            let reader = BufReader::new(src_file);
            let mut tokenizer = JackTokenizer::new(reader);

            let mut writer = LineWriter::new(out_file);
            writer.write("<tokens>\n".to_string().as_bytes()).unwrap();
            while tokenizer.hasMoreTokens() {
                tokenizer.advance();
                let token_type = tokenizer.tokenType();
                match token_type {
                    TokenType::KEYWORD(key) => {
                        writer
                            .write(format!("<keyword> {} </keyword>\n", key.to_string()).as_bytes())
                            .unwrap();
                    }
                    TokenType::SYMBOL(symbol) => {
                        writer
                            .write(
                                format!(
                                    "<symbol> {} </symbol>\n",
                                    escape_xml_attribute(&symbol.to_string())
                                )
                                .as_bytes(),
                            )
                            .unwrap();
                    }
                    TokenType::IDENTIFIER(identifier) => {
                        writer
                            .write(
                                format!("<identifier> {} </identifier>\n", identifier).as_bytes(),
                            )
                            .unwrap();
                    }
                    TokenType::INT_CONST(num) => {
                        writer
                            .write(
                                format!("<integerConstant> {} </integerConstant>\n", num)
                                    .as_bytes(),
                            )
                            .unwrap();
                    }
                    TokenType::STRING_CONST(string) => {
                        writer
                            .write(
                                format!("<stringConstant> {} </stringConstant>\n", string)
                                    .as_bytes(),
                            )
                            .unwrap();
                    }
                }
            }
            writer.write("</tokens>".to_string().as_bytes()).unwrap();
            writer.flush().unwrap();

            println!("compare {out_file_path} with {}", comp_files[i]);
            let mut cmd = Command::new("diff");
            cmd.args(["-w", &out_file_path, comp_files[i]]);
            assert!(cmd.status().unwrap().success());
        }
    }
}
