use std::io::{BufRead, Read, Write};

use log::{debug, info};

use crate::{
    jackTokenizer::JackTokenizer,
    keyword::KeyWord,
    symbol::{self, Symbol},
    tokenType::TokenType,
};

macro_rules! advance_token {
    ($tokenizer:expr) => {
        if !$tokenizer.hasMoreTokens() {
            panic!("no more tokens");
        }
        $tokenizer.advance();
    };
}

macro_rules! token_check {
    ($tokenizer:expr, $symbol_type: pat, $symbol_str: expr, $token_type:pat, $token:ident) => {
        if let $token_type = $tokenizer.tokenType() {
            if let $symbol_type = $token {
                $token
            } else {
                panic!("{:?} is acceptable. input:{:?}", $symbol_str, $token)
            }
        } else {
            panic!();
            // panic!("only {:?} can be acceptable.", $symbol_type)
        }
    };
}

macro_rules! const_ident_check {
    ($tokenizer:expr, $token_type:pat, $token:ident) => {
        if let $token_type = $tokenizer.tokenType() {
            $token
        } else {
            panic!("not const or identifier token type")
        }
    };
}

macro_rules! symbol_check {
    ($tokenizer:expr, $symbol_type: pat, $symbol_str: expr) => {
        if let TokenType::SYMBOL(symbol) = $tokenizer.tokenType() {
            if let $symbol_type = symbol {
                symbol
            } else {
                panic!("{:?} is acceptable. input:{:?}", $symbol_str, symbol)
            }
        } else {
            panic!();
            // panic!("only {:?} can be acceptable.", $symbol_type)
        }
    };
}

macro_rules! keyword_check {
    ($tokenizer:expr, $keyword_type: pat, $keyword_str: expr) => {
        token_check!(
            $tokenizer,
            $keyword_type,
            $keyword_str,
            TokenType::KEYWORD(symbol),
            symbol
        )
    };
}

macro_rules! identifier_check {
    ($tokenizer:expr) => {
        const_ident_check!($tokenizer, TokenType::IDENTIFIER(symbol), symbol)
    };
}

macro_rules! advance_and_write_symbol {
    ($engine:expr, $symbol_type: pat, $symbol_str: expr) => {
        advance_token!($engine.tokenizer);
        let symbol = symbol_check!($engine.tokenizer, $symbol_type, $symbol_str);
        write_symbol_xml!($engine.writer, symbol)
    };
}

macro_rules! advance_and_write_keyword {
    ($engine:expr, $keyword_type: pat, $keyword_str: expr) => {
        advance_token!($engine.tokenizer);
        let keyword = keyword_check!($engine.tokenizer, $keyword_type, $keyword_str);
        write_keyword_xml!($engine.writer, keyword);
    };
}

macro_rules! advance_and_write_identifier {
    ($engine:expr) => {
        advance_token!($engine.tokenizer);
        let identifier = identifier_check!($engine.tokenizer);
        write_identifier_xml!($engine.writer, identifier);
    };
}

macro_rules! write_xml_start_tag {
    ($writer:expr,$tag_name:expr) => {
        $writer
            .write(format!("<{}>\n", $tag_name,).as_bytes())
            .unwrap();
    };
}
macro_rules! write_xml_end_tag {
    ($writer:expr,$tag_name:expr) => {
        $writer
            .write(format!("</{}>\n", $tag_name,).as_bytes())
            .unwrap();
    };
}

macro_rules! write_xml {
    ($writer:expr,$symbol:expr,$tag_name:expr) => {
        $writer
            .write(
                format!(
                    "<{}>{}</{}>\n",
                    $tag_name,
                    escape_xml_attribute(&$symbol.to_string()),
                    $tag_name,
                )
                .as_bytes(),
            )
            .unwrap();
    };
}

macro_rules! write_keyword_xml {
    ($writer:expr,$symbol:expr) => {
        write_xml!($writer, $symbol, "keyword");
    };
}

macro_rules! write_symbol_xml {
    ($writer:expr,$symbol:expr) => {
        write_xml!($writer, $symbol, "symbol");
    };
}

macro_rules! write_identifier_xml {
    ($writer:expr,$identifier:expr) => {
        write_xml!($writer, $identifier, "identifier");
    };
}

macro_rules! check_and_write_symbol {
    ($engine:expr, $symbol_type:expr,$symbol_str:expr) => {
        let symbol = symbol_check!($engine.tokenizer, $symbol_type, $symbol_str);
        write_symbol_xml!($engine.writer, symbol);
    };
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

struct CompilerEngine<R: BufRead, W> {
    tokenizer: JackTokenizer<R>,
    writer: W,
}

impl<R, W> CompilerEngine<R, W>
where
    R: Read + BufRead,
    W: Write,
{
    pub fn new(reader: R, writer: W) -> CompilerEngine<R, W> {
        CompilerEngine {
            tokenizer: JackTokenizer::new(reader),
            writer,
        }
    }

    fn compileClassName(&mut self) {
        advance_and_write_identifier!(self);
    }

    pub fn compileClass(&mut self) {
        info!("parse class ");
        write_xml_start_tag!(self.writer, KeyWord::CLASS);

        advance_and_write_keyword!(self, KeyWord::CLASS, KeyWord::CLASS.to_string());

        self.compileClassName();

        advance_and_write_symbol!(
            self,
            Symbol::left_curly_bracket,
            Symbol::left_curly_bracket.to_string()
        );

        // classvarDec*
        self.compileClassVarDec();
        // subroutineDec*
        self.compileSubroutineDecList();

        advance_and_write_symbol!(
            self,
            Symbol::right_curly_bracket,
            Symbol::right_curly_bracket.to_string()
        );

        write_xml_end_tag!(self.writer, KeyWord::CLASS);
    }

    fn checkClassVarDec(&mut self) -> bool {
        self.tokenizer.advance();
        let result = match self.tokenizer.tokenType() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::STATIC | KeyWord::FIELD => true,
                _ => false,
            },
            _ => false,
        };
        self.tokenizer.back();
        result
    }

    fn compileClassVarDec(&mut self) {
        let tagname = "classVarDec";
        // (static|field) type varName (,varName*);
        info!("parse classVarDec ");
        write_xml_start_tag!(self.writer, &tagname);
        while self.checkClassVarDec() {
            self.tokenizer.advance();
            debug!("classVarDec {:?}", self.tokenizer.tokenType());
            match self.tokenizer.tokenType() {
                TokenType::KEYWORD(keyword) => match keyword {
                    KeyWord::STATIC | KeyWord::FIELD => {
                        write_keyword_xml!(self.writer, keyword);

                        self.compileType();

                        self.compileVarNameList();

                        advance_and_write_symbol!(
                            self,
                            Symbol::semicolon,
                            Symbol::semicolon.to_string()
                        );
                    }
                    _ => {
                        panic!("only keyword is acceptable.")
                    }
                },
                _ => {
                    panic!("only keyword is acceptable.")
                }
            }
        }
        write_xml_end_tag!(self.writer, &tagname);
    }

    fn checkVoid(&mut self) -> bool {
        self.tokenizer.advance();
        let result = match self.tokenizer.tokenType() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::VOID => true,
                _ => false,
            },
            TokenType::IDENTIFIER(_) => false,
            _ => false,
        };
        self.tokenizer.back();
        return result;
    }

    fn checkType(&mut self) -> bool {
        self.tokenizer.advance();
        let result = match self.tokenizer.tokenType() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::INT | KeyWord::BOOLEAN | KeyWord::CHAR => true,
                _ => false,
            },
            TokenType::IDENTIFIER(_) => true,
            _ => false,
        };
        self.tokenizer.back();
        return result;
    }

    fn compileType(&mut self) {
        self.tokenizer.advance();
        match self.tokenizer.tokenType() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::INT | KeyWord::BOOLEAN | KeyWord::CHAR => {
                    write_keyword_xml!(self.writer, keyword);
                }
                _ => {
                    panic!("only int, boolean or char is acceptable");
                }
            },
            TokenType::IDENTIFIER(ident) => {
                write_identifier_xml!(self.writer, ident);
            }
            _ => {
                panic!("keyword or identifier is acceptable for type.");
            }
        }
    }

    fn compileStatements(&mut self) {
        // => if tokenType os not keyword, exit
        let tagname = "statements";
        write_xml_start_tag!(self.writer, tagname);
        loop {
            self.tokenizer.advance();
            if self.tokenizer.hasMoreTokens() {
                match self.tokenizer.tokenType() {
                    TokenType::KEYWORD(keyword) => match keyword {
                        KeyWord::LET
                        | KeyWord::DO
                        | KeyWord::IF
                        | KeyWord::WHILE
                        | KeyWord::RETURN => {
                            self.parseStatement();
                        }
                        _ => {
                            info!(
                                "compile Statements, get not statement keyword: input:{:?}",
                                keyword
                            );
                            self.tokenizer.back();
                            break;
                        }
                    },
                    _ => {
                        info!(
                            "compile Statements, not keyword : input:{:?}",
                            self.tokenizer.tokenType()
                        );
                        self.tokenizer.back();
                        break;
                    }
                }
            } else {
                info!("parse statements but no more tokens");
                break;
            }
        }
        write_xml_end_tag!(self.writer, tagname);
    }

    fn parseWhileAndIf(&mut self, keyword: &str) {
        write_keyword_xml!(self.writer, keyword.to_string());

        advance_and_write_symbol!(self, Symbol::left_bracket, Symbol::left_bracket.to_string());

        self.compileExpression();

        advance_and_write_symbol!(
            self,
            Symbol::right_bracket,
            Symbol::right_bracket.to_string()
        );

        advance_and_write_symbol!(
            self,
            Symbol::left_curly_bracket,
            Symbol::left_curly_bracket.to_string()
        );

        self.compileStatements();

        advance_and_write_symbol!(
            self,
            Symbol::right_curly_bracket,
            Symbol::right_curly_bracket.to_string()
        );
    }

    fn parseStatement(&mut self) {
        match &self.tokenizer.tokenType() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::LET => {
                    self.compileLetStatement(&keyword.to_string());
                }
                KeyWord::DO => {
                    self.compileDoStatement(&keyword.to_string());
                }
                KeyWord::IF => {
                    // internall pre-read for else
                    self.compileIfStatement(&keyword.to_string());
                }
                KeyWord::WHILE => self.compileWhileStatement(&keyword.to_string()),
                KeyWord::RETURN => self.compileReturnStatement(&keyword.to_string()),
                _ => {
                    panic!("parse statement but unacceptable keyword:{:?}", keyword);
                }
            },
            _ => {
                panic!("not keyword token {:?}", self.tokenizer.tokenType())
            }
        }
    }

    fn compileVarDecList(&mut self) {
        let tagname = "varDec";
        write_xml_start_tag!(self.writer, tagname);
        while self.checkVarDec() {
            self.compileVarDec();
        }
        write_xml_end_tag!(self.writer, tagname);
    }

    fn checkVarDec(&mut self) -> bool {
        advance_token!(self.tokenizer);
        let result = match self.tokenizer.tokenType() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::VAR => true,
                _ => false,
            },
            _ => false,
        };

        self.tokenizer.back();
        result
    }

    fn compileVarDec(&mut self) {
        // var type varname (,varname)* ;
        advance_and_write_keyword!(self, KeyWord::VAR, KeyWord::VAR.to_string());
        self.compileType();
        self.compileVarNameList();
        advance_and_write_symbol!(self, Symbol::semicolon, Symbol::semicolon.to_string());
    }

    fn compileVarNameList(&mut self) {
        // name(,name)*
        loop {
            advance_token!(self.tokenizer);
            match self.tokenizer.tokenType() {
                TokenType::IDENTIFIER(id) => {
                    write_identifier_xml!(self.writer, id);
                }
                _ => {
                    panic!("not acceptable token type for subroutineName");
                }
            }

            advance_token!(self.tokenizer);
            match self.tokenizer.tokenType() {
                TokenType::SYMBOL(symbol) => match symbol {
                    Symbol::comma => {
                        write_identifier_xml!(self.writer, symbol);
                    }
                    _ => {
                        self.tokenizer.back();
                        break;
                    }
                },
                _ => {
                    self.tokenizer.back();
                    break;
                }
            }
        }
    }

    fn compileVarName(&mut self) {
        advance_token!(self.tokenizer);
        match self.tokenizer.tokenType() {
            TokenType::IDENTIFIER(id) => {
                write_identifier_xml!(self.writer, id);
            }
            _ => {
                panic!("not acceptable token type for subroutineName");
            }
        }
    }

    fn compileLetStatement(&mut self, keyword: &str) {
        // let varName ( [exp])? = exp;
        info!("parse let");
        let tagname = "letStatement";
        write_xml_start_tag!(self.writer, tagname);
        write_keyword_xml!(self.writer, keyword.to_string());

        self.compileVarName();

        // check [ or =
        advance_token!(self.tokenizer);
        let cand_symbol = match self.tokenizer.tokenType() {
            TokenType::SYMBOL(symbol) => symbol,
            _ => {
                panic!("[ or = only acceptable.");
            }
        };

        let cand_symbol = if let Symbol::left_square_bracket = &cand_symbol {
            write_symbol_xml!(self.writer, Symbol::left_square_bracket);

            self.compileExpression();

            advance_and_write_symbol!(
                self,
                Symbol::right_square_bracket,
                Symbol::right_square_bracket.to_string()
            );

            advance_token!(self.tokenizer);
            match &self.tokenizer.tokenType() {
                TokenType::SYMBOL(symbol) => symbol,
                _ => {
                    panic!("not symbol, equal symbol is expected.")
                }
            }
        } else {
            cand_symbol
        };

        let equal = symbol_check!(self.tokenizer, Symbol::equal, Symbol::equal.to_string());
        write_symbol_xml!(self.writer, equal);

        self.compileExpression();

        advance_and_write_symbol!(self, Symbol::semicolon, Symbol::semicolon.to_string());
        write_xml_end_tag!(self.writer, tagname);
    }

    fn compileIfStatement(&mut self, keyword: &str) {
        // if (exp) {states} (else {states})?
        info!("parse if ");
        let tagname = "ifStatement";
        write_xml_start_tag!(self.writer, tagname);
        self.parseWhileAndIf(&keyword.to_string());

        // check else
        advance_token!(self.tokenizer);
        match self.tokenizer.tokenType() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::ELSE => {
                    // else { states }
                    info!("found else");
                    write_keyword_xml!(self.writer, keyword.to_string());

                    advance_and_write_symbol!(
                        self,
                        Symbol::left_curly_bracket,
                        Symbol::left_curly_bracket.to_string()
                    );

                    self.compileStatements();

                    advance_and_write_symbol!(
                        self,
                        Symbol::right_curly_bracket,
                        Symbol::right_curly_bracket.to_string()
                    );
                }
                _ => {
                    self.tokenizer.back();
                    info!("no else keyword");
                }
            },
            _ => {
                self.tokenizer.back();
                info!("no keyword after if.");
            }
        }
        write_xml_end_tag!(self.writer, tagname);
    }

    fn compileWhileStatement(&mut self, keyword: &str) {
        // while (exp){states}
        let tagname = "whileStatement";
        write_xml_start_tag!(self.writer, tagname);
        self.parseWhileAndIf(&keyword.to_string());
        write_xml_end_tag!(self.writer, tagname);
    }

    fn compileDoStatement(&mut self, keyword: &str) {
        info!("parse do");
        let tagname = "doStatement";
        write_xml_start_tag!(self.writer, tagname);
        write_keyword_xml!(self.writer, keyword.to_string());

        self.compileSubroutineCall();

        advance_and_write_symbol!(self, Symbol::semicolon, Symbol::semicolon.to_string());
        write_xml_end_tag!(self.writer, tagname);
    }

    fn checkParameterList(&mut self) -> bool {
        self.checkType()
    }

    fn compileParameterList(&mut self) {
        // type name(, type name)*
        let tagname = "parameterList";
        write_xml_start_tag!(self.writer, tagname);
        while self.checkParameterList() {
            self.compileType();
            self.compileVarName();

            advance_token!(self.tokenizer);
            match self.tokenizer.tokenType() {
                TokenType::SYMBOL(symbol) => match symbol {
                    Symbol::comma => {
                        write_identifier_xml!(self.writer, symbol);
                    }
                    _ => {
                        self.tokenizer.back();
                        break;
                    }
                },
                _ => {
                    self.tokenizer.back();
                    break;
                }
            }
        }
        write_xml_end_tag!(self.writer, tagname);
    }

    fn compileSubroutineName(&mut self) {
        advance_token!(self.tokenizer);
        match self.tokenizer.tokenType() {
            TokenType::IDENTIFIER(id) => {
                write_identifier_xml!(self.writer, id);
            }
            _ => {
                panic!(
                    "not acceptable token type for subroutineName {:?}",
                    self.tokenizer.tokenType()
                );
            }
        }
    }

    fn compileSubroutineDecList(&mut self) {
        while self.checkSubroutineDec() {
            self.compileSubroutineDec();
        }
    }

    fn checkSubroutineDec(&mut self) -> bool {
        advance_token!(self.tokenizer);
        let ret = match self.tokenizer.tokenType() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::METHOD | KeyWord::FUNCTION | KeyWord::CONSTRUCTOR => true,
                _ => false,
            },
            _ => false,
        };
        self.tokenizer.back();
        ret
    }

    fn compileSubroutineDec(&mut self) {
        // A B name (params) body
        info!("compile subroutineDec");
        let tagname = "subroutineDec";
        write_xml_start_tag!(self.writer, tagname);
        advance_token!(self.tokenizer);
        match self.tokenizer.tokenType() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::METHOD | KeyWord::FUNCTION | KeyWord::CONSTRUCTOR => {
                    write_keyword_xml!(self.writer, keyword);
                }
                _ => {
                    panic!("keyword but only constructor or function or method are acceptable.")
                }
            },
            _ => {
                panic!("only constructor or function or method are acceptable.")
            }
        }

        if self.checkVoid() {
            advance_token!(self.tokenizer);
            write_keyword_xml!(self.writer, KeyWord::VOID);
        } else {
            self.compileType();
        }
        self.compileSubroutineName();

        advance_and_write_symbol!(self, Symbol::left_bracket, Symbol::left_bracket.to_string());

        self.compileParameterList();

        // TODO: fail here
        advance_and_write_symbol!(
            self,
            Symbol::right_bracket,
            Symbol::right_bracket.to_string()
        );

        self.compileSubRoutineBody();
        write_xml_end_tag!(self.writer, tagname);
    }

    fn compileSubroutineCall(&mut self) {
        // TODO: subroutine name or classname (currently same)
        self.compileSubroutineName();
        advance_token!(self.tokenizer);
        // name() or class.name();
        let left_bracket = match self.tokenizer.tokenType() {
            TokenType::SYMBOL(symbol) => match symbol {
                Symbol::left_bracket => {
                    // name();
                    symbol
                }
                Symbol::period => {
                    // class.name();
                    write_symbol_xml!(self.writer, symbol);

                    self.compileSubroutineName();

                    advance_token!(self.tokenizer);
                    symbol_check!(
                        self.tokenizer,
                        Symbol::left_bracket,
                        Symbol::left_bracket.to_string()
                    );
                    &Symbol::left_bracket
                }
                _ => {
                    panic!("not acceptable symbol {symbol} for token next to subroutineName")
                }
            },
            _ => {
                panic!("not acceptable token type for token next to subroutineName")
            }
        };

        write_symbol_xml!(self.writer, left_bracket);
        // exp list
        self.compileExpressionList();

        advance_and_write_symbol!(
            self,
            Symbol::right_bracket,
            Symbol::right_bracket.to_string()
        );
    }

    fn compileSubRoutineBody(&mut self) {
        info!("parse subroutine body");
        let tagname = "subroutineBody";
        write_xml_start_tag!(self.writer, tagname);

        advance_and_write_symbol!(
            self,
            Symbol::left_curly_bracket,
            Symbol::left_curly_bracket.to_string()
        );

        // varDec*
        self.compileVarDecList();
        // statements
        self.compileStatements();

        advance_and_write_symbol!(
            self,
            Symbol::right_curly_bracket,
            Symbol::right_curly_bracket.to_string()
        );
        write_xml_end_tag!(self.writer, tagname);
    }

    fn compileReturnStatement(&mut self, keyword: &str) {
        // return exp? ;
        info!("parse return");
        let tagname = "returnStatement";
        write_xml_start_tag!(self.writer, tagname);
        write_keyword_xml!(self.writer, keyword.to_string());

        // pre-read
        if self.checkExpression() {
            self.compileTerm();
        }

        advance_and_write_symbol!(self, Symbol::semicolon, Symbol::semicolon.to_string());
        write_xml_end_tag!(self.writer, tagname);
    }

    fn checkExpression(&mut self) -> bool {
        self.checkTerm()
    }

    fn compileExpression(&mut self) {
        // For subject-1, need to support only varname
        let tagname = "expression";
        write_xml_start_tag!(self.writer, tagname);
        self.compileTerm();
        write_xml_end_tag!(self.writer, tagname);
    }

    fn compileExpressionList(&mut self) {
        let tagname = "expressionList";
        write_xml_start_tag!(self.writer, tagname);

        // through-pass
        // TODO:();
        write_xml_end_tag!(self.writer, tagname);
    }

    fn compileTerm(&mut self) {
        // term (op term)*
        // TODO: support only identifier
        let tagname = "term";
        write_xml_start_tag!(self.writer, tagname);
        advance_token!(self.tokenizer);
        match self.tokenizer.tokenType() {
            TokenType::IDENTIFIER(identifier) => {
                write_identifier_xml!(self.writer, identifier);
            }
            _ => {
                panic!("not identifier");
            }
        }
        write_xml_end_tag!(self.writer, tagname);
    }

    fn checkTerm(&mut self) -> bool {
        // term (op term)*
        // TODO: support only identifier
        advance_token!(self.tokenizer);
        let result = match self.tokenizer.tokenType() {
            TokenType::IDENTIFIER(_) => true,
            _ => false,
        };
        self.tokenizer.back();
        result
    }
}
#[cfg(test)]
mod tests {
    use std::{
        fs::File,
        io::{BufReader, BufWriter},
    };

    use super::CompilerEngine;

    #[test]
    fn test_compiler_engine() {
        env_logger::init();
        let inputs = vec![
            "./ExpressionLessSquare/Main.jack",
            // "./ExpressionLessSquare/Square.jack",
            // "./ExpressionLessSquare/SquareGame.jack",
        ];

        let outputs = vec![
            "./ExpressionLessSquare/Main.out.xml",
            // "./ExpressionLessSquare/Square.out.xml",
            // "./ExpressionLessSquare/SquareGame.out.xml",
        ];

        for (i, input) in inputs.iter().enumerate() {
            let reader = BufReader::new(File::open(input).unwrap());
            let writer = BufWriter::new(File::create(outputs.get(i).unwrap()).unwrap());
            let mut engine = CompilerEngine::new(reader, writer);
            engine.compileClass();
        }
    }
}
