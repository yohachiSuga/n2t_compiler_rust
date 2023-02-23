use std::collections::HashMap;

use log::{debug, info};

use crate::{
    error::{CompilerError, ALREADY_DEFINED, NOT_EXIST_TABLE},
    keyword::KeyWord,
};

const CLASS_TABLE_KEY: &str = "class";
const SUBROUTINE_TABLE_KEY: &str = "subroutine";

#[derive(PartialEq)]
pub enum Kind {
    Static,
    Field,
    Argument,
    Var,
}

impl From<&KeyWord> for Kind {
    fn from(keyword: &KeyWord) -> Self {
        match keyword {
            KeyWord::STATIC => Kind::Static,
            KeyWord::FIELD => Kind::Field,
            KeyWord::VAR => Kind::Var,
            _ => {
                panic!("not convert keyword {:?}", keyword);
            }
        }
    }
}

struct SymbolElement {
    index: usize,
    kind: Kind,
    // TODO: better name to use?
    _type: String,
}

// key is variable name
type SubroutineSymbolTable = HashMap<String, SymbolElement>;

pub struct SymbolTable {
    table: HashMap<String, SubroutineSymbolTable>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        let mut table = HashMap::new();
        table.insert(CLASS_TABLE_KEY.to_string(), SubroutineSymbolTable::new());
        SymbolTable { table }
    }

    pub fn start_subroutine(&mut self) {
        match self.table.remove(SUBROUTINE_TABLE_KEY) {
            Some(_) => {
                info!("remove existing subroutine table");
            }
            None => {
                info!("remove existing subroutine table");
            }
        }
        self.table.insert(
            SUBROUTINE_TABLE_KEY.to_string(),
            SubroutineSymbolTable::new(),
        );
    }

    pub fn define_wrap(
        &mut self,
        name: String,
        kind: &KeyWord,
        type_keyword: &KeyWord,
    ) -> Result<(), CompilerError> {
        let _type = match type_keyword {
            KeyWord::INT | KeyWord::BOOLEAN | KeyWord::CHAR => type_keyword.to_string(),
            _ => {
                panic!("only int, boolean or char is acceptable.");
            }
        };
        self.define(name, Kind::from(kind), _type)
    }

    pub fn define(&mut self, name: String, kind: Kind, _type: String) -> Result<(), CompilerError> {
        let index = self.kind_count(&kind);
        match kind {
            Kind::Static | Kind::Field => match self.table.get_mut(CLASS_TABLE_KEY) {
                Some(class_table) => match class_table.get(&name) {
                    Some(_) => {
                        debug!("already exist key:{}", name);
                        return Err(CompilerError::new("", ALREADY_DEFINED));
                    }
                    None => {
                        debug!("find new class variable:{}", name);
                        class_table.insert(name, SymbolElement { index, kind, _type });
                        return Ok(());
                    }
                },
                None => {
                    return Err(CompilerError::new("no class table", NOT_EXIST_TABLE));
                }
            },
            Kind::Argument | Kind::Var => match self.table.get_mut(SUBROUTINE_TABLE_KEY) {
                Some(subroutine_table) => match subroutine_table.get(&name) {
                    Some(_) => {
                        debug!("already exist key:{}", name);
                        return Err(CompilerError::new("", ALREADY_DEFINED));
                    }
                    None => {
                        debug!("find new subroutine variable:{}", name);
                        subroutine_table.insert(name, SymbolElement { index, kind, _type });
                        return Ok(());
                    }
                },
                None => {
                    return Err(CompilerError::new("no subroutine table", NOT_EXIST_TABLE));
                }
            },
        }
    }

    fn kind_count(&self, kind: &Kind) -> usize {
        match kind {
            Kind::Static | Kind::Field => match &self.table.get(CLASS_TABLE_KEY) {
                Some(class_table) => {
                    return class_table
                        .iter()
                        .filter(|(_, val)| &val.kind == kind)
                        .count();
                }
                None => 0,
            },
            Kind::Argument | Kind::Var => match &self.table.get(SUBROUTINE_TABLE_KEY) {
                Some(subroutine_table) => {
                    return subroutine_table
                        .iter()
                        .filter(|(_, val)| &val.kind == kind)
                        .count();
                }
                None => 0,
            },
        }
    }

    pub fn find(&self, name: &str) -> Option<&SymbolElement> {
        match &self.table.get(CLASS_TABLE_KEY) {
            Some(class_table) => match class_table.get(name) {
                Some(entry) => Some(entry),
                None => match &self.table.get(SUBROUTINE_TABLE_KEY) {
                    Some(subroutine_table) => match subroutine_table.get(name) {
                        Some(entry) => Some(entry),
                        None => None,
                    },
                    None => None,
                },
            },
            None => None,
        }
    }

    pub fn generate_xml_presence(&self, name: &str) -> String {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::SymbolTable;

    #[test]
    fn test_symbol_table() {
        let mut table = SymbolTable::new();
        table.start_subroutine();

        table.start_subroutine();
        assert!(table.find("test").is_none());
        assert!(table
            .define(
                "test".to_string(),
                crate::symbolTable::Kind::Argument,
                "string".to_string(),
            )
            .is_ok());
        assert!(table
            .define(
                "test2".to_string(),
                crate::symbolTable::Kind::Argument,
                "string".to_string(),
            )
            .is_ok());
        let entry = table.find("test");
        assert!(entry.is_some());
        assert_eq!(entry.as_ref().unwrap().index, 0);

        let entry = table.find("test2");
        assert!(entry.is_some());
        assert_eq!(entry.as_ref().unwrap().index, 1);

        table.start_subroutine();
        assert!(table.find("test").is_none());

        // test class variables are not removed.
        table.start_subroutine();
        assert!(table.find("test").is_none());
        assert!(table
            .define(
                "test".to_string(),
                crate::symbolTable::Kind::Static,
                "string".to_string(),
            )
            .is_ok());
        assert!(table.find("test").is_some());

        table.start_subroutine();
        assert!(table.find("test").is_some());
    }
}
