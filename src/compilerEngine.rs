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
        if !$tokenizer.has_more_tokens() {
            panic!("no more tokens");
        }
        $tokenizer.advance();
    };
}

macro_rules! token_check {
    ($tokenizer:expr, $symbol_type: pat, $symbol_str: expr, $token_type:pat, $token:ident) => {
        if let $token_type = $tokenizer.token_type() {
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
        if let $token_type = $tokenizer.token_type() {
            $token
        } else {
            panic!("not const or identifier token type")
        }
    };
}

macro_rules! symbol_check {
    ($tokenizer:expr, $symbol_type: pat, $symbol_str: expr) => {
        if let TokenType::SYMBOL(symbol) = $tokenizer.token_type() {
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

pub struct CompilerEngine<R: BufRead, W> {
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

    fn compile_class_name(&mut self) {
        advance_and_write_identifier!(self);
    }

    pub fn compile_class(&mut self) {
        info!("parse class ");
        write_xml_start_tag!(self.writer, KeyWord::CLASS);

        advance_and_write_keyword!(self, KeyWord::CLASS, KeyWord::CLASS.to_string());

        self.compile_class_name();

        advance_and_write_symbol!(
            self,
            Symbol::left_curly_bracket,
            Symbol::left_curly_bracket.to_string()
        );

        // classvarDec*
        self.compile_class_var_dec();
        // subroutineDec*
        self.compile_subroutine_declist();

        advance_and_write_symbol!(
            self,
            Symbol::right_curly_bracket,
            Symbol::right_curly_bracket.to_string()
        );

        write_xml_end_tag!(self.writer, KeyWord::CLASS);
    }

    fn check_class_var_dec(&mut self) -> bool {
        self.tokenizer.advance();
        let result = match self.tokenizer.token_type() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::STATIC | KeyWord::FIELD => true,
                _ => false,
            },
            _ => false,
        };
        self.tokenizer.back();
        result
    }

    fn compile_class_var_dec(&mut self) {
        let tagname = "classVarDec";
        // (static|field) type varName (,varName*);
        info!("parse classVarDec ");
        while self.check_class_var_dec() {
            self.tokenizer.advance();
            debug!("classVarDec {:?}", self.tokenizer.token_type());
            write_xml_start_tag!(self.writer, &tagname);
            match self.tokenizer.token_type() {
                TokenType::KEYWORD(keyword) => match keyword {
                    KeyWord::STATIC | KeyWord::FIELD => {
                        write_keyword_xml!(self.writer, keyword);

                        self.compile_type();

                        self.compile_var_namelist();

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
            write_xml_end_tag!(self.writer, &tagname);
        }
    }

    fn check_void(&mut self) -> bool {
        self.tokenizer.advance();
        let result = match self.tokenizer.token_type() {
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

    fn check_type(&mut self) -> bool {
        self.tokenizer.advance();
        let result = match self.tokenizer.token_type() {
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

    fn compile_type(&mut self) {
        self.tokenizer.advance();
        match self.tokenizer.token_type() {
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

    fn compile_statements(&mut self) {
        // => if tokenType os not keyword, exit
        let tagname = "statements";
        write_xml_start_tag!(self.writer, tagname);
        loop {
            self.tokenizer.advance();
            if self.tokenizer.has_more_tokens() {
                match self.tokenizer.token_type() {
                    TokenType::KEYWORD(keyword) => match keyword {
                        KeyWord::LET
                        | KeyWord::DO
                        | KeyWord::IF
                        | KeyWord::WHILE
                        | KeyWord::RETURN => {
                            self.compile_statement();
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
                            self.tokenizer.token_type()
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

    fn compile_while_and_if(&mut self, keyword: &str) {
        write_keyword_xml!(self.writer, keyword.to_string());

        advance_and_write_symbol!(self, Symbol::left_bracket, Symbol::left_bracket.to_string());

        self.compile_exp();

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

        self.compile_statements();

        advance_and_write_symbol!(
            self,
            Symbol::right_curly_bracket,
            Symbol::right_curly_bracket.to_string()
        );
    }

    fn compile_statement(&mut self) {
        match &self.tokenizer.token_type() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::LET => {
                    self.compile_let_statement(&keyword.to_string());
                }
                KeyWord::DO => {
                    self.compile_do_statement(&keyword.to_string());
                }
                KeyWord::IF => {
                    // internall pre-read for else
                    self.compile_if_statement(&keyword.to_string());
                }
                KeyWord::WHILE => self.compile_while_statement(&keyword.to_string()),
                KeyWord::RETURN => self.compile_return_statement(&keyword.to_string()),
                _ => {
                    panic!("parse statement but unacceptable keyword:{:?}", keyword);
                }
            },
            _ => {
                panic!("not keyword token {:?}", self.tokenizer.token_type())
            }
        }
    }

    fn compileVarDecList(&mut self) {
        while self.check_var_dec() {
            self.compile_var_dec();
        }
    }

    fn check_var_dec(&mut self) -> bool {
        advance_token!(self.tokenizer);
        let result = match self.tokenizer.token_type() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::VAR => true,
                _ => false,
            },
            _ => false,
        };

        self.tokenizer.back();
        result
    }

    fn compile_var_dec(&mut self) {
        // var type varname (,varname)* ;
        let tagname = "varDec";
        write_xml_start_tag!(self.writer, tagname);
        advance_and_write_keyword!(self, KeyWord::VAR, KeyWord::VAR.to_string());
        self.compile_type();
        self.compile_var_namelist();
        advance_and_write_symbol!(self, Symbol::semicolon, Symbol::semicolon.to_string());
        write_xml_end_tag!(self.writer, tagname);
    }

    fn compile_var_namelist(&mut self) {
        // name(,name)*
        loop {
            advance_token!(self.tokenizer);
            match self.tokenizer.token_type() {
                TokenType::IDENTIFIER(id) => {
                    write_identifier_xml!(self.writer, id);
                }
                _ => {
                    panic!("not acceptable token type for subroutineName");
                }
            }

            advance_token!(self.tokenizer);
            match self.tokenizer.token_type() {
                TokenType::SYMBOL(symbol) => match symbol {
                    Symbol::comma => {
                        write_symbol_xml!(self.writer, symbol);
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

    fn compile_var_name(&mut self) {
        advance_token!(self.tokenizer);
        match self.tokenizer.token_type() {
            TokenType::IDENTIFIER(id) => {
                write_identifier_xml!(self.writer, id);
            }
            _ => {
                panic!("not acceptable token type for subroutineName");
            }
        }
    }

    fn compile_let_statement(&mut self, keyword: &str) {
        // let varName ( [exp])? = exp;
        info!("parse let");
        let tagname = "letStatement";
        write_xml_start_tag!(self.writer, tagname);
        write_keyword_xml!(self.writer, keyword.to_string());

        self.compile_var_name();

        // check [ or =
        advance_token!(self.tokenizer);
        let cand_symbol = match self.tokenizer.token_type() {
            TokenType::SYMBOL(symbol) => symbol,
            _ => {
                panic!("[ or = only acceptable.");
            }
        };

        let cand_symbol = if let Symbol::left_square_bracket = &cand_symbol {
            write_symbol_xml!(self.writer, Symbol::left_square_bracket);

            self.compile_exp();

            advance_and_write_symbol!(
                self,
                Symbol::right_square_bracket,
                Symbol::right_square_bracket.to_string()
            );

            advance_token!(self.tokenizer);
            match &self.tokenizer.token_type() {
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

        self.compile_exp();

        advance_and_write_symbol!(self, Symbol::semicolon, Symbol::semicolon.to_string());
        write_xml_end_tag!(self.writer, tagname);
    }

    fn compile_if_statement(&mut self, keyword: &str) {
        // if (exp) {states} (else {states})?
        info!("parse if ");
        let tagname = "ifStatement";
        write_xml_start_tag!(self.writer, tagname);
        self.compile_while_and_if(&keyword.to_string());

        // check else
        advance_token!(self.tokenizer);
        match self.tokenizer.token_type() {
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

                    self.compile_statements();

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

    fn compile_while_statement(&mut self, keyword: &str) {
        // while (exp){states}
        let tagname = "whileStatement";
        write_xml_start_tag!(self.writer, tagname);
        self.compile_while_and_if(&keyword.to_string());
        write_xml_end_tag!(self.writer, tagname);
    }

    fn compile_do_statement(&mut self, keyword: &str) {
        info!("parse do");
        let tagname = "doStatement";
        write_xml_start_tag!(self.writer, tagname);
        write_keyword_xml!(self.writer, keyword.to_string());

        self.compile_subroutine_call(false);

        advance_and_write_symbol!(self, Symbol::semicolon, Symbol::semicolon.to_string());
        write_xml_end_tag!(self.writer, tagname);
    }

    fn check_paramlist(&mut self) -> bool {
        self.check_type()
    }

    fn compile_paramlist(&mut self) {
        // type name(, type name)*
        let tagname = "parameterList";
        write_xml_start_tag!(self.writer, tagname);
        while self.check_paramlist() {
            self.compile_type();
            self.compile_var_name();

            advance_token!(self.tokenizer);
            match self.tokenizer.token_type() {
                TokenType::SYMBOL(symbol) => match symbol {
                    Symbol::comma => {
                        write_symbol_xml!(self.writer, symbol);
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

    fn compile_subroutine_name(&mut self) {
        advance_token!(self.tokenizer);
        match self.tokenizer.token_type() {
            TokenType::IDENTIFIER(id) => {
                write_identifier_xml!(self.writer, id);
            }
            _ => {
                panic!(
                    "not acceptable token type for subroutineName {:?}",
                    self.tokenizer.token_type()
                );
            }
        }
    }

    fn compile_subroutine_declist(&mut self) {
        while self.check_subroutine_dec() {
            self.compile_subroutine_dec();
        }
    }

    fn check_subroutine_dec(&mut self) -> bool {
        advance_token!(self.tokenizer);
        let ret = match self.tokenizer.token_type() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::METHOD | KeyWord::FUNCTION | KeyWord::CONSTRUCTOR => true,
                _ => false,
            },
            _ => false,
        };
        self.tokenizer.back();
        ret
    }

    fn compile_subroutine_dec(&mut self) {
        // A B name (params) body
        info!("compile subroutineDec");
        let tagname = "subroutineDec";
        write_xml_start_tag!(self.writer, tagname);
        advance_token!(self.tokenizer);
        match self.tokenizer.token_type() {
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

        if self.check_void() {
            advance_token!(self.tokenizer);
            write_keyword_xml!(self.writer, KeyWord::VOID);
        } else {
            self.compile_type();
        }
        self.compile_subroutine_name();

        advance_and_write_symbol!(self, Symbol::left_bracket, Symbol::left_bracket.to_string());

        self.compile_paramlist();

        // TODO: fail here
        advance_and_write_symbol!(
            self,
            Symbol::right_bracket,
            Symbol::right_bracket.to_string()
        );

        self.compile_subroutine_body();
        write_xml_end_tag!(self.writer, tagname);
    }

    fn compile_subroutine_call(&mut self, pre_read_name: bool) {
        info!("parse subroutineCall");
        // subroutine name or classname (currently same)
        if !pre_read_name {
            self.compile_subroutine_name();
        }

        advance_token!(self.tokenizer);
        // name() or class.name();
        let left_bracket = match self.tokenizer.token_type() {
            TokenType::SYMBOL(symbol) => match symbol {
                Symbol::left_bracket => {
                    // name();
                    symbol
                }
                Symbol::period => {
                    // class.name();
                    write_symbol_xml!(self.writer, symbol);

                    self.compile_subroutine_name();

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
        self.compile_explist();

        advance_and_write_symbol!(
            self,
            Symbol::right_bracket,
            Symbol::right_bracket.to_string()
        );
    }

    fn compile_subroutine_body(&mut self) {
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
        self.compile_statements();

        advance_and_write_symbol!(
            self,
            Symbol::right_curly_bracket,
            Symbol::right_curly_bracket.to_string()
        );
        write_xml_end_tag!(self.writer, tagname);
    }

    fn compile_return_statement(&mut self, keyword: &str) {
        // return exp? ;
        info!("parse return");
        let tagname = "returnStatement";
        write_xml_start_tag!(self.writer, tagname);
        write_keyword_xml!(self.writer, keyword.to_string());

        // pre-read
        if self.check_exp() {
            self.compile_exp();
        }

        advance_and_write_symbol!(self, Symbol::semicolon, Symbol::semicolon.to_string());
        write_xml_end_tag!(self.writer, tagname);
    }

    fn check_exp(&mut self) -> bool {
        self.check_term()
    }

    fn compile_exp(&mut self) {
        // term (op term)*
        info!("parse expression");
        let tagname = "expression";
        write_xml_start_tag!(self.writer, tagname);
        self.compile_term();

        while self.check_op() {
            advance_token!(self.tokenizer);
            match self.tokenizer.token_type() {
                TokenType::SYMBOL(symbol) => {
                    write_symbol_xml!(self.writer, symbol);
                    self.compile_term();
                }
                _ => {
                    panic!("not operator");
                }
            }
        }

        write_xml_end_tag!(self.writer, tagname);
    }

    fn check_op(&mut self) -> bool {
        advance_token!(self.tokenizer);
        let result = match self.tokenizer.token_type() {
            TokenType::SYMBOL(symbol) => match symbol {
                Symbol::plus
                | Symbol::minus
                | Symbol::star
                | Symbol::slash
                | Symbol::ampersand
                | Symbol::pipe
                | Symbol::lt
                | Symbol::bt
                | Symbol::equal => true,
                _ => false,
            },
            _ => false,
        };
        self.tokenizer.back();
        result
    }

    fn compile_explist(&mut self) {
        info!("parse expressionList");
        let tagname = "expressionList";
        write_xml_start_tag!(self.writer, tagname);

        while self.check_exp() {
            self.compile_exp();

            advance_token!(self.tokenizer);
            match self.tokenizer.token_type() {
                TokenType::SYMBOL(symbol) => match symbol {
                    Symbol::comma => {
                        write_symbol_xml!(self.writer, symbol);
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

    fn compile_term(&mut self) {
        // term
        info!("parse term");
        let tagname = "term";

        write_xml_start_tag!(self.writer, tagname);
        advance_token!(self.tokenizer);
        match self.tokenizer.token_type() {
            TokenType::IDENTIFIER(identifier) => {
                // varName | varName[] | SubroutineCall (func() or  class.func() )
                write_identifier_xml!(self.writer, identifier);

                advance_token!(self.tokenizer);
                match self.tokenizer.token_type() {
                    TokenType::SYMBOL(symbol) => match symbol {
                        Symbol::left_square_bracket => {
                            write_symbol_xml!(self.writer, symbol);

                            self.compile_exp();

                            advance_and_write_symbol!(
                                self,
                                Symbol::right_square_bracket,
                                Symbol::right_square_bracket.to_string()
                            );
                        }
                        Symbol::left_bracket | Symbol::period => {
                            self.tokenizer.back();
                            self.compile_subroutine_call(true);
                        }
                        _ => {
                            // do nothing
                            self.tokenizer.back();
                        }
                    },
                    _ => {
                        // do nothing
                        self.tokenizer.back();
                    }
                }
            }
            TokenType::KEYWORD(keyword) => {
                if self.check_keyword_const(keyword) {
                    write_keyword_xml!(self.writer, keyword);
                }
            }
            TokenType::STRING_CONST(string) => {
                let tagname = "stringConstant";
                write_xml!(self.writer, string, tagname);
            }
            TokenType::INT_CONST(num) => {
                let tagname = "integerConstant";
                write_xml!(self.writer, &num.to_string(), tagname);
            }
            TokenType::SYMBOL(symbol) => {
                // (expression) | unaryOp Term
                match symbol {
                    Symbol::left_bracket => {
                        write_symbol_xml!(self.writer, symbol);

                        self.compile_exp();

                        advance_and_write_symbol!(
                            self,
                            Symbol::right_bracket,
                            Symbol::right_bracket.to_string()
                        );
                    }
                    Symbol::minus | Symbol::tilde => {
                        write_symbol_xml!(self.writer, symbol);
                        self.compile_term();
                    }
                    _ => {
                        panic!("accetable only ( - ~ input: {:?}", symbol);
                    }
                }
            }
            _ => {
                panic!("not identifier");
            }
        }
        write_xml_end_tag!(self.writer, tagname);
    }

    fn check_keyword_const(&self, keyword: &KeyWord) -> bool {
        match keyword {
            KeyWord::TRUE | KeyWord::FALSE | KeyWord::NULL | KeyWord::THIS => true,
            _ => false,
        }
    }

    fn check_term(&mut self) -> bool {
        // term (op term)*
        advance_token!(self.tokenizer);
        let result = match self.tokenizer.token_type() {
            TokenType::STRING_CONST(_) | TokenType::INT_CONST(_) | TokenType::IDENTIFIER(_) => true,
            TokenType::KEYWORD(keyword) => self.check_keyword_const(keyword),
            TokenType::SYMBOL(symbol) => {
                // (expression) | unaryOp Term
                match symbol {
                    Symbol::left_bracket | Symbol::minus | Symbol::tilde => true,
                    _ => false,
                }
            }
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
        process::Command,
    };

    use super::CompilerEngine;

    #[test]
    fn test_compiler_engine() {
        env_logger::init();
        let inputs = vec![
            "./ExpressionLessSquare/Main.jack",
            "./ExpressionLessSquare/Square.jack",
            "./ExpressionLessSquare/SquareGame.jack",
            "./ArrayTest/Main.jack",
            "./Square/Main.jack",
            "./Square/Square.jack",
            "./Square/SquareGame.jack",
        ];

        let outputs = vec![
            "./ExpressionLessSquare/Main.out.xml",
            "./ExpressionLessSquare/Square.out.xml",
            "./ExpressionLessSquare/SquareGame.out.xml",
            "./ArrayTest/Main.out.xml",
            "./Square/Main.out.xml",
            "./Square/Square.out.xml",
            "./Square/SquareGame.out.xml",
        ];

        let comps = vec![
            "./ExpressionLessSquare/Main.xml",
            "./ExpressionLessSquare/Square.xml",
            "./ExpressionLessSquare/SquareGame.xml",
            "./ArrayTest/Main.xml",
            "./Square/Main.xml",
            "./Square/Square.xml",
            "./Square/SquareGame.xml",
        ];

        for (i, input) in inputs.iter().enumerate() {
            let reader = BufReader::new(File::open(input).unwrap());
            let writer = BufWriter::new(File::create(outputs.get(i).unwrap()).unwrap());
            {
                let mut engine = CompilerEngine::new(reader, writer);
                engine.compile_class();
            }
            let mut cmd = Command::new("diff");
            cmd.args(["-w", &outputs[i], comps[i]]);
            assert!(cmd.status().unwrap().success());
        }
    }
}
