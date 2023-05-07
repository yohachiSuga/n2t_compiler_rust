use core::panic;
use std::{
    env::var,
    io::{BufRead, Read, Write},
};

use log::{debug, info};

use crate::{
    error::ALREADY_DEFINED,
    jackTokenizer::JackTokenizer,
    keyword::{self, KeyWord},
    symbol::Symbol,
    symbolTable::{Kind, SymbolElement, SymbolTable},
    tokenType::TokenType,
    vmWriter::{Command, Segment, VMWriter},
    EmitOptions,
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
        if $engine.emits.is_emit_xml() {
            write_symbol_xml!($engine, symbol);
        }
    };
}

macro_rules! advance_and_write_keyword {
    ($engine:expr, $keyword_type: pat, $keyword_str: expr) => {
        advance_token!($engine.tokenizer);
        let keyword = keyword_check!($engine.tokenizer, $keyword_type, $keyword_str);
        if $engine.emits.is_emit_xml() {
            write_keyword_xml!($engine, keyword);
        }
    };
}

macro_rules! advance_and_write_identifier {
    ($engine:expr) => {
        advance_token!($engine.tokenizer);
        let identifier = identifier_check!($engine.tokenizer);
        if $engine.emits.is_emit_xml() {
            write_identifier_xml!($engine, identifier);
        }
    };
}

macro_rules! write_xml_start_tag {
    ($engine:expr,$tag_name:expr) => {
        if $engine.emits.is_emit_xml() {
            $engine
                .writer
                .write(format!("<{}>\n", $tag_name,).as_bytes())
                .unwrap();
        }
    };
}
macro_rules! write_xml_end_tag {
    ($engine:expr,$tag_name:expr) => {
        if $engine.emits.is_emit_xml() {
            $engine
                .writer
                .write(format!("</{}>\n", $tag_name,).as_bytes())
                .unwrap();
        }
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
    ($engine:expr,$symbol:expr) => {
        if $engine.emits.is_emit_xml() {
            write_xml!($engine.writer, $symbol, "keyword");
        }
    };
}

macro_rules! write_symbol_xml {
    ($engine:expr,$symbol:expr) => {
        if $engine.emits.is_emit_xml() {
            write_xml!($engine.writer, $symbol, "symbol");
        }
    };
}

macro_rules! write_identifier_xml {
    ($engine:expr,$identifier:expr) => {
        if $engine.emits.is_emit_xml() {
            write_xml!($engine.writer, $identifier, "identifier");
        }
    };
}

macro_rules! write_int_xml {
    ($engine:expr,$num:expr) => {
        if $engine.emits.is_emit_xml() {
            let tagname = "integerConstant";
            write_xml!($engine.writer, $num.to_string(), tagname);
        }
    };
}

macro_rules! write_str_xml {
    ($engine:expr,$string:expr) => {
        if $engine.emits.is_emit_xml() {
            let tagname = "stringConstant";
            write_xml!($engine.writer, $string, tagname);
        }
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

fn generate_symbol_xml_presence(
    name: &str,
    category: &str,
    defined: bool,
    attr: Option<&str>,
    idx: Option<usize>,
) -> String {
    if attr.is_some() && idx.is_some() {
        format!("<name>{name}</name><category>{category}</category><defined>{}</defined><kind>{}</kind><idx>{}</idx>", if defined {"defined"} else {"used"},attr.unwrap(),idx.unwrap())
    } else {
        format!("<name>{name}</name><category>{category}</category>")
    }
}

fn generate_symbol_xml_presence_from_symbol(
    name: &str,
    symbol: &SymbolElement,
    defined: bool,
) -> String {
    generate_symbol_xml_presence(
        name,
        &symbol.kind.to_string(),
        defined,
        Some(&symbol.kind.to_string()),
        Some(symbol.index),
    )
}

struct VarNameContext<'a> {
    is_expected_be_define: bool,
    // if type_keyword is None, is_expected_to_be_define shall be false. (collect from SymbolTable)
    type_keyword: Option<&'a KeyWord>,
    // if kind is None, is_expected_to_be_define shall be false. (collect from SymbolTable)
    kind: Option<Kind>,
    is_advance: bool,
    token: Option<&'a str>,
}

struct ClassNameContext<'a> {
    name: &'a str,    //class name
    is_advance: bool, // if is_advance is set to true, advance and use next token instead of name
}

struct IfContext {
    counter: usize,
}

struct WhileContext {
    counter: usize,
}

pub struct CompilerEngine<R: BufRead, W> {
    tokenizer: JackTokenizer<R>,
    writer: W,
    vm_writer: VMWriter<W>,
    emits: EmitOptions,
    symbol_table: SymbolTable,
    current_class_name: Option<String>,
    current_subroutine_name: Option<String>,
    current_subroutine_type: Option<String>,
    current_subroutine_ret_type: Option<KeyWord>,
    if_ctx: IfContext,
    while_ctx: WhileContext,
}

enum IfWhile {
    IF,
    WHILE,
}

impl<R, W> CompilerEngine<R, W>
where
    R: Read + BufRead,
    W: Write,
{
    pub fn new(
        reader: R,
        writer: W,
        vm_writer: VMWriter<W>,
        emits: EmitOptions,
    ) -> CompilerEngine<R, W> {
        CompilerEngine {
            tokenizer: JackTokenizer::new(reader),
            writer: writer,
            vm_writer: vm_writer,
            symbol_table: SymbolTable::new(),
            current_class_name: None,
            current_subroutine_name: None,
            current_subroutine_type: None,
            current_subroutine_ret_type: None,
            emits,
            if_ctx: IfContext { counter: 0 },
            while_ctx: WhileContext { counter: 0 },
        }
    }

    fn compile_class_name(&mut self, ctx: ClassNameContext) {
        if ctx.is_advance {
            // advance
            self.tokenizer.advance();
            match self.tokenizer.token_type() {
                TokenType::KEYWORD(keyword) => {
                    panic!("only identifier is acceptable")
                }
                TokenType::IDENTIFIER(ident) => {
                    if self.emits.is_emit_ex_xml() {
                        let content = generate_symbol_xml_presence(
                            ident,
                            {
                                if self.current_class_name.as_ref().unwrap().eq(ident) {
                                    "CLASS_MYSELF"
                                } else {
                                    "CLASS_DEFINED_EXTERNAL"
                                }
                            },
                            false,
                            None,
                            None,
                        );
                        write_identifier_xml!(self, content);
                    } else {
                        write_identifier_xml!(self, ident);
                    }
                }
                _ => {
                    panic!("keyword or identifier is acceptable for type.");
                }
            }
        } else {
            if self.emits.is_emit_ex_xml() {
                let content = generate_symbol_xml_presence(
                    ctx.name,
                    if self.current_class_name.as_ref().unwrap().eq(ctx.name) {
                        "CLASS_MYSELF"
                    } else {
                        "CLASS_DEFINED_EXTERNAL"
                    },
                    false,
                    None,
                    None,
                );
                write_identifier_xml!(self, content);
            } else {
                write_identifier_xml!(self, ctx.name);
            }
        }
    }

    pub fn compile_class(&mut self) {
        info!("parse class ");
        write_xml_start_tag!(self, KeyWord::CLASS);

        advance_and_write_keyword!(self, KeyWord::CLASS, KeyWord::CLASS.to_string());

        self.tokenizer.advance();
        match self.tokenizer.token_type() {
            TokenType::KEYWORD(keyword) => {
                panic!("only identifier is acceptable")
            }
            TokenType::IDENTIFIER(ident) => {
                self.current_class_name = Some(ident.clone());
            }
            _ => {
                panic!("keyword or identifier is acceptable for type.");
            }
        }

        self.compile_class_name(ClassNameContext {
            name: &self.current_class_name.as_ref().unwrap().clone(),
            is_advance: false,
        });

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

        write_xml_end_tag!(self, KeyWord::CLASS);
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
            write_xml_start_tag!(self, &tagname);
            match self.tokenizer.token_type() {
                TokenType::KEYWORD(keyword) => match keyword {
                    KeyWord::STATIC | KeyWord::FIELD => {
                        // TODO: to avoid keyword.clone().
                        let keyword = keyword.clone();
                        write_keyword_xml!(self, keyword);

                        let type_keyword = self.compile_type();

                        match type_keyword {
                            Some(type_keyword) => {
                                self.compile_var_namelist(&keyword, &type_keyword);

                                advance_and_write_symbol!(
                                    self,
                                    Symbol::semicolon,
                                    Symbol::semicolon.to_string()
                                );
                            }
                            None => {
                                panic!("not type is acceptable");
                            }
                        }
                    }
                    _ => {
                        panic!("only keyword is acceptable.")
                    }
                },
                _ => {
                    panic!("only keyword is acceptable.")
                }
            }
            write_xml_end_tag!(self, &tagname);
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

    fn compile_type(&mut self) -> Option<KeyWord> {
        self.tokenizer.advance();
        let keyword = match self.tokenizer.token_type() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::INT | KeyWord::BOOLEAN | KeyWord::CHAR => {
                    write_keyword_xml!(self, keyword);
                    keyword.clone()
                }
                _ => {
                    panic!("only int, boolean or char is acceptable");
                }
            },
            TokenType::IDENTIFIER(ident) => KeyWord::IDENTIFIER(ident.clone()),
            _ => {
                panic!("keyword or identifier is acceptable for type.");
            }
        };

        match &keyword {
            KeyWord::IDENTIFIER(ident) => {
                self.compile_class_name(ClassNameContext {
                    name: &ident.to_string(),
                    is_advance: false,
                });
            }
            _ => {
                // do nothing
            }
        }

        Some(KeyWord::IDENTIFIER(keyword.to_string()))
    }

    fn compile_statements(&mut self) {
        // => if tokenType os not keyword, exit
        let tagname = "statements";
        write_xml_start_tag!(self, tagname);
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
        write_xml_end_tag!(self, tagname);
    }

    fn compile_while_and_if(&mut self, keyword: &str, if_while: &IfWhile) {
        write_keyword_xml!(self, keyword.to_string());

        advance_and_write_symbol!(self, Symbol::left_bracket, Symbol::left_bracket.to_string());

        if self.emits.emit_vm {
            match if_while {
                IfWhile::IF => {
                    // do nothing
                }
                IfWhile::WHILE => {
                    self.vm_writer
                        .write_label(&format!("WHILE_EXP{}", self.while_ctx.counter - 1));
                }
            }
        }

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

        if self.emits.emit_vm {
            match if_while {
                IfWhile::IF => {
                    self.vm_writer
                        .write_if(&format!("IF_TRUE{}", self.if_ctx.counter - 1));
                    // TODO: handle if `else` does not defined
                    self.vm_writer
                        .write_goto(&format!("IF_FALSE{}", self.if_ctx.counter - 1));
                    self.vm_writer
                        .write_label(&format!("IF_TRUE{}", self.if_ctx.counter - 1));
                }
                IfWhile::WHILE => {
                    // because true = -1
                    self.vm_writer.write_arithmetic(Command::NOT);
                    self.vm_writer
                        .write_if(&format!("WHILE_END{}", self.while_ctx.counter - 1));
                }
            }
        }

        // to save counter
        let current_while_ctx_counter = self.while_ctx.counter;

        self.compile_statements();

        if self.emits.emit_vm {
            match if_while {
                IfWhile::IF => {
                    // do nothing
                }
                IfWhile::WHILE => {
                    self.vm_writer
                        .write_goto(&format!("WHILE_EXP{}", current_while_ctx_counter - 1));
                    self.vm_writer
                        .write_label(&format!("WHILE_END{}", current_while_ctx_counter - 1));
                }
            }
        }

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
                    // TODO: avoid clone
                    self.compile_let_statement(&keyword.clone());
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
        write_xml_start_tag!(self, tagname);
        advance_and_write_keyword!(self, KeyWord::VAR, KeyWord::VAR.to_string());

        let type_keyword = self.compile_type();
        match type_keyword {
            Some(type_keyword) => {
                self.compile_var_namelist(&KeyWord::VAR, &type_keyword);
                advance_and_write_symbol!(self, Symbol::semicolon, Symbol::semicolon.to_string());
                write_xml_end_tag!(self, tagname);
            }
            None => {
                panic!("compile_var_dec failed. unknown keyword is defined.");
            }
        }
    }

    fn compile_var_namelist(&mut self, keyword: &KeyWord, type_keyword: &KeyWord) {
        // name(,name)*
        loop {
            advance_token!(self.tokenizer);
            match self.tokenizer.token_type() {
                TokenType::IDENTIFIER(id) => {
                    let result = self.symbol_table.define(
                        id.to_string(),
                        Kind::from(keyword),
                        type_keyword.to_string(),
                    );

                    if self.emits.is_emit_ex_xml() {
                        let content = match result {
                            Ok(element) => {
                                generate_symbol_xml_presence_from_symbol(id, element, true)
                            }
                            Err(e) => {
                                if e.code == ALREADY_DEFINED {
                                    let element = self.symbol_table.find(&id);
                                    generate_symbol_xml_presence_from_symbol(
                                        id,
                                        element
                                            .expect("error code ALREADY_DEFINED, but not exist?"),
                                        false,
                                    )
                                } else {
                                    panic!("unexpected compilation error {:?}", e);
                                }
                            }
                        };
                        write_identifier_xml!(self, content);
                    } else {
                        write_identifier_xml!(self, id);
                    }
                }
                _ => {
                    panic!("not acceptable token type for subroutineName");
                }
            }

            advance_token!(self.tokenizer);
            match self.tokenizer.token_type() {
                TokenType::SYMBOL(symbol) => match symbol {
                    Symbol::comma => {
                        write_symbol_xml!(self, symbol);
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

    fn compile_var_name(&mut self, ctx: VarNameContext) -> String {
        let id = if ctx.is_advance {
            advance_token!(self.tokenizer);
            match self.tokenizer.token_type() {
                TokenType::IDENTIFIER(id) => id.to_string(),
                _ => {
                    panic!("not acceptable token type for subroutineName");
                }
            }
        } else {
            ctx.token.as_ref().unwrap().to_string()
        };

        // define table
        let (is_defined, symbol) = match self.symbol_table.find(&id) {
            Some(result) => {
                if ctx.is_expected_be_define {
                    panic!("id : {id} is already defined. something wrong.");
                }
                (false, result)
            }
            None => {
                if !ctx.is_expected_be_define {
                    panic!("id : {id} is expected not to be defined. something wrong.");
                }

                let result = self.symbol_table.define(
                    id.to_string(),
                    ctx.kind.expect("kind shall be defined."),
                    ctx.type_keyword
                        .expect("type keyword shall be defined")
                        .to_string(),
                );
                (true, result.unwrap())
            }
        };

        if self.emits.is_emit_ex_xml() {
            let content = generate_symbol_xml_presence_from_symbol(&id, symbol, is_defined);
            write_identifier_xml!(self, content);
        } else {
            write_identifier_xml!(self, id);
        }
        return id;
    }

    fn compile_let_statement(&mut self, keyword: &KeyWord) {
        // let varName ( [exp])? = exp;
        info!("parse let");
        let tagname = "letStatement";
        write_xml_start_tag!(self, tagname);
        write_keyword_xml!(self, keyword.to_string());

        let var_name = self.compile_var_name(VarNameContext {
            is_expected_be_define: false,
            type_keyword: None,
            kind: None,
            is_advance: true,
            token: None,
        });

        // check [ or =
        advance_token!(self.tokenizer);
        let cand_symbol = match self.tokenizer.token_type() {
            TokenType::SYMBOL(symbol) => symbol,
            _ => {
                panic!("[ or = only acceptable.");
            }
        };

        let is_handle_array;
        let cand_symbol = if let Symbol::left_square_bracket = &cand_symbol {
            is_handle_array = true;
            write_symbol_xml!(self, Symbol::left_square_bracket);

            self.compile_exp();

            if self.emits.emit_vm {
                self.write_vm_push_from_symbol_table(&var_name);
            }

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
            is_handle_array = false;
            cand_symbol
        };

        let equal = symbol_check!(self.tokenizer, Symbol::equal, Symbol::equal.to_string());
        write_symbol_xml!(self, equal);

        self.compile_exp();

        if self.emits.emit_vm {
            if is_handle_array {
                // store current top stack value (If let Y = X, X shall be the top.)
                self.vm_writer.write_pop(Segment::TEMP, 0);

                self.vm_writer.write_pop(Segment::POINTER, 1);
                self.vm_writer.write_push(Segment::TEMP, 0);
                self.vm_writer.write_pop(Segment::THAT, 0);
            } else {
                let symbol = self
                    .symbol_table
                    .find(&var_name)
                    .expect(&format!("{} does not define in symbol table.", var_name));
                self.vm_writer
                    .write_pop(Segment::from(&symbol.kind), symbol.index as u32);
            }
        }

        advance_and_write_symbol!(self, Symbol::semicolon, Symbol::semicolon.to_string());
        write_xml_end_tag!(self, tagname);
    }

    fn compile_if_statement(&mut self, keyword: &str) {
        // if (exp) {states} (else {states})?
        info!("parse if ");
        let tagname = "ifStatement";
        write_xml_start_tag!(self, tagname);

        self.if_ctx.counter += 1;
        // counter may change after self.compile_statements
        let current_if_ctx_counter = self.if_ctx.counter;
        self.compile_while_and_if(&keyword.to_string(), &IfWhile::IF);

        // check else
        advance_token!(self.tokenizer);
        let mut is_else_exist = false;
        match self.tokenizer.token_type() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::ELSE => {
                    // else { states }
                    info!("found else");
                    is_else_exist = true;
                    write_keyword_xml!(self, keyword.to_string());

                    advance_and_write_symbol!(
                        self,
                        Symbol::left_curly_bracket,
                        Symbol::left_curly_bracket.to_string()
                    );

                    if self.emits.emit_vm {
                        self.vm_writer
                            .write_goto(&format!("IF_END{}", current_if_ctx_counter - 1));
                        self.vm_writer
                            .write_label(&format!("IF_FALSE{}", current_if_ctx_counter - 1));
                    }

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

        if self.emits.emit_vm {
            if is_else_exist {
                self.vm_writer
                    .write_label(&format!("IF_END{}", current_if_ctx_counter - 1));
            } else {
                self.vm_writer
                    .write_label(&format!("IF_FALSE{}", current_if_ctx_counter - 1));
            }
        }

        write_xml_end_tag!(self, tagname);
    }

    fn compile_while_statement(&mut self, keyword: &str) {
        // while (exp){states}
        let tagname = "whileStatement";
        write_xml_start_tag!(self, tagname);
        self.while_ctx.counter += 1;
        self.compile_while_and_if(&keyword.to_string(), &IfWhile::WHILE);
        write_xml_end_tag!(self, tagname);
    }

    fn compile_do_statement(&mut self, keyword: &str) {
        info!("parse do");
        let tagname = "doStatement";
        write_xml_start_tag!(self, tagname);
        write_keyword_xml!(self, keyword.to_string());

        self.compile_subroutine_call(None);

        if self.emits.emit_vm {
            self.vm_writer.write_pop(Segment::TEMP, 0);
        }
        advance_and_write_symbol!(self, Symbol::semicolon, Symbol::semicolon.to_string());
        write_xml_end_tag!(self, tagname);
    }

    fn check_paramlist(&mut self) -> bool {
        self.check_type()
    }

    fn compile_paramlist(&mut self) {
        // type name(, type name)*
        let tagname = "parameterList";
        write_xml_start_tag!(self, tagname);
        while self.check_paramlist() {
            let type_keyword = self.compile_type();
            match type_keyword {
                Some(type_keyword) => {
                    self.compile_var_name(VarNameContext {
                        is_expected_be_define: true,
                        type_keyword: Some(&type_keyword),
                        kind: Some(Kind::Argument),
                        is_advance: true,
                        token: None,
                    });
                }
                None => {
                    panic!("cannot compile perameter")
                }
            }

            advance_token!(self.tokenizer);
            match self.tokenizer.token_type() {
                TokenType::SYMBOL(symbol) => match symbol {
                    Symbol::comma => {
                        write_symbol_xml!(self, symbol);
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
        write_xml_end_tag!(self, tagname);
    }

    fn compile_subroutine_name(&mut self, pre_read_token: Option<&str>) {
        let token = match pre_read_token {
            Some(token) => token,
            None => {
                advance_token!(self.tokenizer);
                match self.tokenizer.token_type() {
                    TokenType::IDENTIFIER(id) => id,
                    _ => {
                        panic!(
                            "not acceptable token type for subroutineName {:?}",
                            self.tokenizer.token_type()
                        );
                    }
                }
            }
        };

        self.current_subroutine_name = Some(token.to_string());
        if self.emits.is_emit_ex_xml() {
            let content = generate_symbol_xml_presence(token, "SUBROUTINE", false, None, None);
            write_identifier_xml!(self, content);
        } else {
            write_identifier_xml!(self, token);
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
        write_xml_start_tag!(self, tagname);
        advance_token!(self.tokenizer);
        match self.tokenizer.token_type() {
            TokenType::KEYWORD(keyword) => match keyword {
                KeyWord::METHOD => {
                    write_keyword_xml!(self, keyword);
                    self.current_subroutine_type = Some(keyword.to_string());
                }
                KeyWord::FUNCTION => {
                    write_keyword_xml!(self, keyword);
                    self.current_subroutine_type = Some(keyword.to_string());
                }
                KeyWord::CONSTRUCTOR => {
                    write_keyword_xml!(self, keyword);
                    self.current_subroutine_type = Some(keyword.to_string());
                }
                _ => {
                    panic!("keyword but only constructor or function or method are acceptable.")
                }
            },
            _ => {
                panic!("only constructor or function or method are acceptable.")
            }
        }
        debug!(
            "########### CURRENT_SUBROUTINE_TYPE{}",
            self.current_subroutine_type.as_ref().unwrap().to_string()
        );

        self.symbol_table.start_subroutine();
        // clear context
        self.if_ctx.counter = 0;
        self.while_ctx.counter = 0;

        if self.check_void() {
            advance_token!(self.tokenizer);
            write_keyword_xml!(self, KeyWord::VOID);

            self.current_subroutine_ret_type = Some(KeyWord::VOID);
        } else {
            let return_type = self.compile_type();
            self.current_subroutine_ret_type = Some(return_type.unwrap());
        }
        self.compile_subroutine_name(None);

        advance_and_write_symbol!(self, Symbol::left_bracket, Symbol::left_bracket.to_string());

        self.compile_paramlist();

        advance_and_write_symbol!(
            self,
            Symbol::right_bracket,
            Symbol::right_bracket.to_string()
        );

        self.compile_subroutine_body();
        write_xml_end_tag!(self, tagname);
    }

    fn compile_subroutine_call(&mut self, pre_read_identifier: Option<&str>) {
        info!("parse subroutineCall");
        // subroutine name or classname (currently same)
        let id = match pre_read_identifier {
            Some(id) => id.to_string(),
            None => {
                advance_token!(self.tokenizer);
                match self.tokenizer.token_type() {
                    TokenType::IDENTIFIER(id) => id.to_string(),
                    _ => {
                        panic!("not supported token type is found. in subroutine call")
                    }
                }
            }
        };

        advance_token!(self.tokenizer);
        // name() or class.name();
        let mut is_compile_subroutine_name = false;
        let mut is_compile_class_name = false;
        match self.tokenizer.token_type() {
            TokenType::SYMBOL(symbol) => match symbol {
                Symbol::left_bracket => {
                    // name();
                    is_compile_subroutine_name = true;
                }
                Symbol::period => {
                    is_compile_class_name = true;
                }
                _ => {
                    panic!("not acceptable symbol {symbol} for token next to subroutineName")
                }
            },
            _ => {
                panic!("not acceptable token type for token next to subroutineName")
            }
        };

        // write func or class.func
        // TODO: need to handle `func` (But this is defined as method always??)
        let mut call_func_name = String::from("");
        let mut is_method = false;
        {
            if is_compile_subroutine_name {
                self.compile_subroutine_name(Some(&id));
                // current token shall be already left_bracket.

                // if no class name, it shall be method.
                call_func_name = format!(
                    "{}.{}",
                    self.current_class_name.as_ref().unwrap(),
                    self.current_subroutine_name.as_ref().unwrap()
                );
                is_method = true;

                // push `this` as arguments
                if self.emits.emit_vm {
                    self.vm_writer.write_push(Segment::POINTER, 0);
                }
            }

            if is_compile_class_name {
                self.compile_class_name(ClassNameContext {
                    name: &id,
                    is_advance: false,
                });
                write_symbol_xml!(self, Symbol::period);
                self.compile_subroutine_name(None);
                // current token is not left_bracket, so need to advance.
                advance_token!(self.tokenizer);

                let symbol = self.symbol_table.find(&id);
                match symbol {
                    Some(symbol) => {
                        if self.emits.emit_vm {
                            self.vm_writer
                                .write_push(Segment::from(&symbol.kind), symbol.index as u32);
                        }
                        call_func_name = format!(
                            "{}.{}",
                            symbol._type,
                            self.current_subroutine_name.as_ref().unwrap()
                        );
                        is_method = true;
                    }
                    None => {
                        call_func_name =
                            format!("{}.{}", id, self.current_subroutine_name.as_ref().unwrap());
                    }
                };
            }
        }
        write_symbol_xml!(self, Symbol::left_bracket);
        // exp list
        let mut exp_counter = self.compile_explist();

        // to add 'this'
        if is_method {
            exp_counter += 1;
        }

        // write call f
        if self.emits.emit_vm {
            self.vm_writer
                .write_call(&call_func_name, exp_counter as u32);

            // function return is stored on statement side
        }

        advance_and_write_symbol!(
            self,
            Symbol::right_bracket,
            Symbol::right_bracket.to_string()
        );
    }

    fn compile_subroutine_body(&mut self) {
        info!("parse subroutine body");
        let tagname = "subroutineBody";
        write_xml_start_tag!(self, tagname);

        advance_and_write_symbol!(
            self,
            Symbol::left_curly_bracket,
            Symbol::left_curly_bracket.to_string()
        );

        // varDec*
        self.compileVarDecList();

        if self.emits.emit_vm {
            self.vm_writer.write_func(
                &format!(
                    "{}.{}",
                    self.current_class_name.as_ref().unwrap(),
                    self.current_subroutine_name.as_ref().unwrap(),
                ),
                self.symbol_table.kind_count(&Kind::Var) as u32,
            );

            // special snippet for ctor/method.
            if self.current_subroutine_type.is_some() {
                if self.current_subroutine_type.as_ref().unwrap()
                    == &KeyWord::CONSTRUCTOR.to_string()
                {
                    debug!(
                        "write vm for ctor args:{}",
                        self.symbol_table.kind_count(&Kind::Field)
                    );
                    self.vm_writer.write_push(
                        Segment::CONST,
                        self.symbol_table.kind_count(&Kind::Field) as u32,
                    );
                    self.vm_writer.write_call("Memory.alloc", 1);
                    self.vm_writer.write_pop(Segment::POINTER, 0);
                }

                if self.current_subroutine_type.as_ref().unwrap() == &KeyWord::METHOD.to_string() {
                    debug!("write vm for method");
                    self.vm_writer.write_push(Segment::ARG, 0);
                    self.vm_writer.write_pop(Segment::POINTER, 0);
                }
            }
        }

        // statements
        self.compile_statements();

        advance_and_write_symbol!(
            self,
            Symbol::right_curly_bracket,
            Symbol::right_curly_bracket.to_string()
        );
        write_xml_end_tag!(self, tagname);
    }

    fn compile_return_statement(&mut self, keyword: &str) {
        // return exp? ;
        info!("parse return");
        let tagname = "returnStatement";
        write_xml_start_tag!(self, tagname);
        write_keyword_xml!(self, keyword.to_string());

        // pre-read
        if self.check_exp() {
            self.compile_exp();
        }

        advance_and_write_symbol!(self, Symbol::semicolon, Symbol::semicolon.to_string());
        write_xml_end_tag!(self, tagname);

        if self.emits.emit_vm {
            match &self.current_subroutine_ret_type {
                Some(keyword) => match keyword {
                    KeyWord::VOID => {
                        self.vm_writer.write_push(Segment::CONST, 0);
                    }
                    _ => {
                        info!("keyword: {}", keyword);
                    }
                },
                None => todo!(),
            }
            self.vm_writer.write_return();
        }
    }

    fn check_exp(&mut self) -> bool {
        self.check_term()
    }

    fn write_op(&mut self, symbol: Symbol) {
        match symbol {
            Symbol::plus
            | Symbol::minus
            | Symbol::ampersand
            | Symbol::pipe
            | Symbol::lt
            | Symbol::bt
            | Symbol::equal => {
                self.vm_writer
                    .write_arithmetic(Command::from(symbol.clone()));
            }
            Symbol::star => self.vm_writer.write_call("Math.multiply", 2),
            Symbol::slash => self.vm_writer.write_call("Math.divide", 2),
            _ => panic!("{} is not opeator.", symbol),
        }
    }

    fn compile_exp(&mut self) {
        // term (op term)*
        info!("parse expression");
        let tagname = "expression";
        write_xml_start_tag!(self, tagname);
        self.compile_term();

        while self.check_op() {
            advance_token!(self.tokenizer);
            let mut is_term = false;
            let captured_symbol;
            match self.tokenizer.token_type() {
                TokenType::SYMBOL(symbol) => {
                    // this symbol shall be op
                    write_symbol_xml!(self, symbol);
                    is_term = true;
                    captured_symbol = symbol.clone();
                }
                _ => {
                    panic!("not operator");
                }
            }

            if is_term {
                self.compile_term();

                if self.emits.emit_vm {
                    let symbol = captured_symbol.clone();
                    self.write_op(symbol);
                }
            }
        }

        write_xml_end_tag!(self, tagname);
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

    fn compile_explist(&mut self) -> usize {
        let mut exp_counter = 0;
        info!("parse expressionList");
        let tagname = "expressionList";
        write_xml_start_tag!(self, tagname);

        while self.check_exp() {
            exp_counter += 1;
            self.compile_exp();

            advance_token!(self.tokenizer);
            match self.tokenizer.token_type() {
                TokenType::SYMBOL(symbol) => match symbol {
                    Symbol::comma => {
                        write_symbol_xml!(self, symbol);
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

        write_xml_end_tag!(self, tagname);
        exp_counter
    }

    fn write_vm_push_from_symbol_table(&mut self, var_name: &str) {
        if self.emits.emit_vm {
            let symbol = self
                .symbol_table
                .find(&var_name)
                .expect(&format!("cannot find {} on symbol table.", var_name));

            debug!(
                "write vm push symbol {var_name} kind:{} idx:{}",
                Segment::from(&symbol.kind),
                symbol.index
            );
            self.vm_writer
                .write_push(Segment::from(&symbol.kind), symbol.index as u32);
        }
    }

    fn compile_term(&mut self) {
        // term
        info!("parse term");
        let tagname = "term";

        write_xml_start_tag!(self, tagname);
        advance_token!(self.tokenizer);
        match self.tokenizer.token_type() {
            TokenType::IDENTIFIER(identifier) => {
                // varName | varName[] | SubroutineCall (func() or  class.func() )
                // write_identifier_xml!(self, identifier);
                let identifier = identifier.to_string();

                let mut is_write_exp = false;
                advance_token!(self.tokenizer);
                match self.tokenizer.token_type() {
                    TokenType::SYMBOL(symbol) => match symbol {
                        Symbol::left_square_bracket => {
                            is_write_exp = true;
                        }
                        Symbol::left_bracket | Symbol::period => {
                            self.tokenizer.back();
                            self.compile_subroutine_call(Some(&identifier));
                        }
                        _ => {
                            // TODO: to be commonized at the followings
                            let var_name_ctx = VarNameContext {
                                is_expected_be_define: false,
                                type_keyword: None,
                                kind: None,
                                is_advance: false,
                                token: Some(&identifier),
                            };
                            self.compile_var_name(var_name_ctx);
                            self.write_vm_push_from_symbol_table(&identifier);

                            self.tokenizer.back();
                        }
                    },
                    _ => {
                        let var_name_ctx = VarNameContext {
                            is_expected_be_define: false,
                            type_keyword: None,
                            kind: None,
                            is_advance: false,
                            token: Some(&identifier),
                        };
                        self.compile_var_name(var_name_ctx);
                        self.write_vm_push_from_symbol_table(&identifier);

                        self.tokenizer.back();
                    }
                }

                if is_write_exp {
                    debug!("found [");
                    let var_name_ctx = VarNameContext {
                        is_expected_be_define: false,
                        type_keyword: None,
                        kind: None,
                        is_advance: false,
                        token: Some(&identifier),
                    };
                    self.compile_var_name(var_name_ctx);

                    write_symbol_xml!(self, Symbol::left_square_bracket);

                    // to handle array
                    if self.emits.emit_vm {
                        self.vm_writer.write_arithmetic(Command::ADD);
                    }

                    self.compile_exp();

                    self.write_vm_push_from_symbol_table(&identifier);
                    self.vm_writer.write_arithmetic(Command::ADD);
                    self.vm_writer.write_pop(Segment::POINTER, 1);
                    self.vm_writer.write_push(Segment::THAT, 0);
                    advance_and_write_symbol!(
                        self,
                        Symbol::right_square_bracket,
                        Symbol::right_square_bracket.to_string()
                    );
                }
            }
            TokenType::KEYWORD(keyword) => {
                if self.check_keyword_const(keyword) {
                    write_keyword_xml!(self, keyword);

                    if self.emits.emit_vm {
                        match keyword {
                            KeyWord::TRUE => {
                                self.vm_writer.write_push(Segment::CONST, 0);
                                self.vm_writer.write_arithmetic(Command::NOT);
                            }
                            KeyWord::FALSE => {
                                self.vm_writer.write_push(Segment::CONST, 0);
                            }
                            KeyWord::NULL => {
                                self.vm_writer.write_push(Segment::CONST, 0);
                            }
                            KeyWord::THIS => {
                                self.vm_writer.write_push(Segment::POINTER, 0);
                            }
                            _ => {
                                panic!("keyword shall be true, false, null or this.")
                            }
                        }
                    }
                }
            }
            TokenType::STRING_CONST(string) => {
                write_str_xml!(self, string);

                if self.emits.emit_vm {
                    // ctor for string
                    self.vm_writer
                        .write_push(Segment::CONST, string.len() as u32);
                    self.vm_writer.write_call("String.new", 1);

                    // append asciicode char
                    for c in string.chars() {
                        self.vm_writer.write_push(Segment::CONST, c as u32);
                        self.vm_writer.write_call("String.appendChar", 2);
                    }
                }
            }
            TokenType::INT_CONST(num) => {
                // TODO: what is maximum value of integer?
                self.vm_writer.write_push(Segment::CONST, *num as u32);
                write_int_xml!(self, num);
            }
            TokenType::SYMBOL(symbol) => {
                // (expression) | unaryOp Term
                match symbol {
                    Symbol::left_bracket => {
                        write_symbol_xml!(self, symbol);

                        self.compile_exp();

                        advance_and_write_symbol!(
                            self,
                            Symbol::right_bracket,
                            Symbol::right_bracket.to_string()
                        );
                    }
                    Symbol::minus => {
                        write_symbol_xml!(self, symbol);
                        self.compile_term();

                        if self.emits.emit_vm {
                            self.vm_writer.write_arithmetic(Command::NEG);
                        }
                    }
                    Symbol::tilde => {
                        write_symbol_xml!(self, symbol);
                        self.compile_term();

                        if self.emits.emit_vm {
                            self.vm_writer.write_arithmetic(Command::NOT);
                        }
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
        info!("end parse term");
        write_xml_end_tag!(self, tagname);
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

    use log::info;

    use crate::{vmWriter::VMWriter, EmitOptions};

    use super::CompilerEngine;

    #[test]
    fn test_compiler_engine_xml() {
        env_logger::try_init();
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
            let vm_writer = VMWriter::new(BufWriter::new(File::open("/dev/null").unwrap()));
            {
                let emits = EmitOptions::new(&vec!["xml".to_string()]);
                let mut engine = CompilerEngine::new(reader, writer, vm_writer, emits);
                engine.compile_class();
            }
            let mut cmd = Command::new("diff");
            cmd.args(["-w", &outputs[i], comps[i]]);
            assert!(cmd.status().unwrap().success());
        }
    }

    #[test]
    fn test_compiler_engine_ex_xml() {
        env_logger::try_init();
        let inputs = vec![
            "./ExpressionLessSquare/Main.jack",
            "./ExpressionLessSquare/Square.jack",
            "./ExpressionLessSquare/SquareGame.jack",
            "./ArrayTest/Main.jack",
            "./Square/Main.jack",
            "./Square/Square.jack",
            "./Square/SquareGame.jack",
            "./bankaccount.jack",
        ];

        let outputs = vec![
            "./ExpressionLessSquare/Main.out.ex.xml",
            "./ExpressionLessSquare/Square.out.ex.xml",
            "./ExpressionLessSquare/SquareGame.out.ex.xml",
            "./ArrayTest/Main.out.ex.xml",
            "./Square/Main.out.ex.xml",
            "./Square/Square.out.ex.xml",
            "./Square/SquareGame.out.ex.xml",
            "./bankaccount.out.ex.xml",
        ];

        for (i, input) in inputs.iter().enumerate() {
            let reader = BufReader::new(File::open(input).unwrap());
            let writer = BufWriter::new(File::create(outputs.get(i).unwrap()).unwrap());
            let vm_writer = VMWriter::new(BufWriter::new(File::open("/dev/null").unwrap()));
            {
                let emits = EmitOptions::new(&vec!["xml".to_string(), "ex-xml".to_string()]);
                let mut engine = CompilerEngine::new(reader, writer, vm_writer, emits);
                engine.compile_class();
            }
        }
    }

    #[test]
    fn test_compiler_engine_vm() {
        env_logger::try_init();
        let inputs = vec![
            "./Seven/Main.jack",
            "./ConvertToBin/Main.jack",
            // "./ExpressionLessSquare/Main.jack",
            // "./ExpressionLessSquare/Square.jack",
            // "./ExpressionLessSquare/SquareGame.jack",
            // "./ArrayTest/Main.jack",
            "./Square/Main.jack",
            "./Square/Square.jack",
            "./Square/SquareGame.jack",
            // "./bankaccount.jack",
        ];

        let comps = vec![
            "./Seven/Main.vm",
            "./ConvertToBin/Main.vm",
            // "./ExpressionLessSquare/Main.out.ex.xml",
            // "./ExpressionLessSquare/Square.out.ex.xml",
            // "./ExpressionLessSquare/SquareGame.out.ex.xml",
            // "./ArrayTest/Main.out.ex.xml",
            "./Square/Main.vm",
            "./Square/Square.vm",
            "./Square/SquareGame.vm",
            // "./bankaccount.out.ex.xml",
        ];

        let outputs = vec![
            "./Seven/Main.vm.out",
            "./ConvertToBin/Main.vm.out",
            // "./ExpressionLessSquare/Main.out.ex.xml",
            // "./ExpressionLessSquare/Square.out.ex.xml",
            // "./ExpressionLessSquare/SquareGame.out.ex.xml",
            // "./ArrayTest/Main.out.ex.xml",
            "./Square/Main.vm.out",
            "./Square/Square.vm.out",
            "./Square/SquareGame.vm.out",
            // "./bankaccount.out.ex.xml",
        ];

        for (i, input) in inputs.iter().enumerate() {
            info!("########### TEST FILE: {input} ################");
            let reader = BufReader::new(File::open(input).unwrap());
            let vm_writer = VMWriter::new(BufWriter::new(
                File::create(outputs.get(i).unwrap()).unwrap(),
            ));
            let dev_null_writer = BufWriter::new(File::open("/dev/null").unwrap());
            {
                let emits = EmitOptions::new(&vec!["vm".to_string()]);
                let mut engine = CompilerEngine::new(reader, dev_null_writer, vm_writer, emits);
                engine.compile_class();
            }

            let mut cmd = Command::new("diff");
            cmd.args(["-w", &outputs[i], comps[i]]);
            assert!(cmd.status().unwrap().success());
        }
    }
}

// Square
// OK main.main (xml test is broken??)
// NOT main.others.
// NOT Square.jack
// NOT squareGame.jack
