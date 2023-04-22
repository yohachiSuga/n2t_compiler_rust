use std::io::Write;

use crate::{symbol::Symbol, symbolTable::Kind};

#[derive(
    strum_macros::EnumString,
    strum_macros::Display,
    strum_macros::IntoStaticStr,
    strum_macros::EnumIter,
)]
pub enum Segment {
    #[strum(serialize = "constant")]
    CONST,
    #[strum(serialize = "argument")]
    ARG,
    #[strum(serialize = "local")]
    LOCAL,
    #[strum(serialize = "static")]
    STATIC,
    #[strum(serialize = "this")]
    THIS,
    #[strum(serialize = "that")]
    THAT,
    #[strum(serialize = "pointer")]
    POINTER,
    #[strum(serialize = "temp")]
    TEMP,
}

impl From<&Kind> for Segment {
    fn from(value: &Kind) -> Self {
        match value {
            Kind::Static => Segment::STATIC,
            Kind::Field => Segment::THIS,
            Kind::Argument => Segment::ARG,
            Kind::Var => Segment::LOCAL,
        }
    }
}

#[derive(
    strum_macros::EnumString,
    strum_macros::Display,
    strum_macros::IntoStaticStr,
    strum_macros::EnumIter,
)]
pub enum Command {
    #[strum(serialize = "add")]
    ADD,
    #[strum(serialize = "sub")]
    SUB,
    #[strum(serialize = "neg")]
    NEG,
    #[strum(serialize = "eq")]
    EQ,
    #[strum(serialize = "gt")]
    GT,
    #[strum(serialize = "lt")]
    LT,
    #[strum(serialize = "and")]
    AND,
    #[strum(serialize = "or")]
    OR,
    #[strum(serialize = "not")]
    NOT,
}

impl From<Symbol> for Command {
    fn from(value: Symbol) -> Self {
        match value {
            Symbol::plus => Command::ADD,
            Symbol::minus => Command::SUB,
            // call multiply
            // Symbol::star =>
            // call devide
            // Symbol::slash => todo!(),
            Symbol::ampersand => Command::AND,
            Symbol::pipe => Command::OR,
            Symbol::lt => Command::LT,
            Symbol::bt => Command::GT,
            Symbol::equal => Command::EQ,
            Symbol::tilde => Command::NOT,
            _ => {
                panic!("cannot convert to command")
            }
        }
    }
}

pub struct VMWriter<W> {
    writer: W,
}

impl<W: Write> VMWriter<W> {
    pub fn new(writer: W) -> VMWriter<W> {
        VMWriter { writer }
    }

    pub fn write_push(&mut self, segment: Segment, idx: u32) {
        writeln!(self.writer, "push {} {}", segment, idx).unwrap();
    }
    pub fn write_pop(&mut self, segment: Segment, idx: u32) {
        writeln!(self.writer, "pop {} {}", segment, idx).unwrap();
    }
    pub fn write_arithmetic(&mut self, cmd: Command) {
        writeln!(self.writer, "{}", cmd).unwrap();
    }
    pub fn write_label(&mut self, label: &str) {
        writeln!(self.writer, "label {}", label).unwrap();
    }
    pub fn write_goto(&mut self, label: &str) {
        writeln!(self.writer, "goto {}", label).unwrap();
    }
    pub fn write_if(&mut self, label: &str) {
        writeln!(self.writer, "if-goto {}", label).unwrap();
    }
    pub fn write_call(&mut self, name: &str, nargs: u32) {
        writeln!(self.writer, "call {} {}", name, nargs).unwrap()
    }
    pub fn write_func(&mut self, name: &str, nlocals: u32) {
        writeln!(self.writer, "function {} {}", name, nlocals).unwrap()
    }
    pub fn write_return(&mut self) {
        writeln!(self.writer, "return").unwrap()
    }

    pub fn close(&mut self) {
        self.writer.flush().unwrap();
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::vmWriter::{Command, Segment};

    use super::VMWriter;

    #[test]
    fn test_write_push_pop() {
        let mut data = vec![];
        let writer = Cursor::new(&mut data);
        let mut writer = VMWriter::new(writer);
        writer.write_push(Segment::CONST, 10);
        writer.write_pop(Segment::ARG, 100);
        writer.write_push(Segment::LOCAL, 10);
        writer.write_push(Segment::STATIC, 10);
        writer.write_pop(Segment::THIS, 100);
        writer.write_push(Segment::THAT, 10);
        writer.write_pop(Segment::POINTER, 100);
        writer.write_pop(Segment::TEMP, 100);
        writer.close();
        let expected_out = r"push constant 10
pop argument 100
push local 10
push static 10
pop this 100
push that 10
pop pointer 100
pop temp 100
";
        assert_eq!(String::from_utf8(data).unwrap(), expected_out);
    }

    #[test]
    fn test_write_arithmetic() {
        let mut data = vec![];
        let writer = Cursor::new(&mut data);
        let mut writer = VMWriter::new(writer);
        writer.write_arithmetic(Command::ADD);
        writer.write_arithmetic(Command::SUB);
        writer.write_arithmetic(Command::NEG);
        writer.write_arithmetic(Command::EQ);
        writer.write_arithmetic(Command::GT);
        writer.write_arithmetic(Command::LT);
        writer.write_arithmetic(Command::AND);
        writer.write_arithmetic(Command::OR);
        writer.write_arithmetic(Command::NOT);
        writer.close();
        let expected_out = r"add
sub
neg
eq
gt
lt
and
or
not
";
        assert_eq!(String::from_utf8(data).unwrap(), expected_out);
    }

    #[test]
    fn test_write_labels() {
        let mut data = vec![];
        let writer = Cursor::new(&mut data);
        let mut writer = VMWriter::new(writer);
        writer.write_label("TEST_LABEL");
        writer.write_goto("TEST_GOTO");
        writer.write_if("TEST_IF");
        writer.close();
        let expected_out = r"label TEST_LABEL
goto TEST_GOTO
if-goto TEST_IF
";
        assert_eq!(String::from_utf8(data).unwrap(), expected_out);
    }

    #[test]
    fn test_write_funcs() {
        let mut data = vec![];
        let writer = Cursor::new(&mut data);
        let mut writer = VMWriter::new(writer);
        let test_call = "TEST_CALL";
        let test_call_nargs = 1;
        let test_func = "TEST_FUNC";
        let test_func_nlocals = 2;
        writer.write_call(test_call, test_call_nargs);
        writer.write_func(test_func, test_func_nlocals);
        writer.write_return();
        writer.close();
        let expected_out = format!(
            r"call {test_call} {test_call_nargs}
function {test_func} {test_func_nlocals}
return
"
        );
        assert_eq!(String::from_utf8(data).unwrap(), expected_out);
    }
}
