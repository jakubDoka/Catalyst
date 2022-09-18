use std::{fmt::Display, path::PathBuf};

use crate::tokens::{Lexer, TokKind};

pub struct SourceCode {
    pub calls: Vec<Call>,
    pub files: Vec<File>,
}

impl SourceCode {
    pub fn new(lexer: &mut Lexer, path: PathBuf) -> Self {
        let mut files = Vec::new();
        let mut calls = Vec::new();

        lexer.skip_newlines();
        while lexer.current().kind != TokKind::Eof {
            match lexer.current().kind {
                TokKind::File => files.push(File::new(lexer)),
                TokKind::Call => calls.push(Call::new(lexer)),
                kind => lexer.error_many(kind, &[TokKind::File, TokKind::Call]),
            }
            lexer.skip_newlines();
        }

        let parent = path.parent().unwrap();
        files
            .iter_mut()
            .for_each(|file| file.path = parent.join(file.path.clone()));
        calls
            .iter_mut()
            .for_each(|call| call.path = parent.join(call.path.clone()));

        SourceCode { files, calls }
    }
}

pub struct Call {
    pub path: PathBuf,
}

impl Call {
    pub fn new(lexer: &mut Lexer) -> Self {
        lexer.expect(TokKind::Call);
        lexer.advance();

        lexer.expect(TokKind::String);
        let path = lexer.current().span;
        lexer.advance();

        Call {
            path: PathBuf::from(lexer.show(path)),
        }
    }
}

pub struct File {
    pub path: PathBuf,
    pub imports: Vec<Import>,
    pub structs: Vec<Struct>,
}

impl File {
    pub fn new(lexer: &mut Lexer) -> File {
        lexer.expect(TokKind::File);
        lexer.advance();

        lexer.expect(TokKind::String);
        let path = lexer.current().span;
        lexer.advance();

        let (structs, imports) = Self::parse_content(lexer);

        Self {
            path: PathBuf::from(lexer.show(path)),
            structs,
            imports,
        }
    }

    fn parse_content(lexer: &mut Lexer) -> (Vec<Struct>, Vec<Import>) {
        lexer.expect(TokKind::LBrace);
        lexer.advance();

        lexer.skip_newlines();

        let mut structs = Vec::new();
        let mut imports = Vec::new();
        while lexer.current().kind != TokKind::RBrace && lexer.current().kind != TokKind::Eof {
            match lexer.current().kind {
                TokKind::Use => imports.push(Import::new(lexer)),
                TokKind::Struct => structs.push(Struct::new(lexer)),
                kind => lexer.error_many(kind, &[TokKind::Struct, TokKind::Use]),
            };
            lexer.skip_newlines();
        }
        lexer.advance();

        (structs, imports)
    }
}

impl Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "//! This file is generated, do not edit!")?;

        for import in &self.imports {
            writeln!(f, "{};", import)?;
        }

        writeln!(f)?;

        for structure in &self.structs {
            writeln!(f, "{}", structure)?;

            structure.write_constructor(f)?;
            writeln!(f)?;

            structure.write_macro(f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

pub struct Import {
    pub path: String,
}

impl Import {
    pub fn new(lexer: &mut Lexer) -> Import {
        lexer.expect(TokKind::Use);
        lexer.advance();

        lexer.expect(TokKind::String);
        let path = lexer.current().span;
        lexer.advance();

        Self {
            path: lexer.show(path).to_string(),
        }
    }
}

impl Display for Import {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "use {}::*", self.path)
    }
}

pub struct Struct {
    pub name: String,
    pub fields: Vec<Field>,
}

impl Struct {
    pub fn new(lexer: &mut Lexer) -> Self {
        lexer.expect(TokKind::Struct);
        lexer.advance();
        lexer.expect(TokKind::Ident);
        let name = lexer.current().span;
        lexer.advance();
        let fields = Struct::parse_fields(lexer);
        Struct {
            name: lexer.show(name).to_string(),
            fields,
        }
    }

    pub fn parse_fields(lexer: &mut Lexer) -> Vec<Field> {
        let mut fields = Vec::new();
        lexer.expect(TokKind::LBrace);
        lexer.advance();
        lexer.skip_newlines();
        while lexer.current().kind != TokKind::RBrace && lexer.current().kind != TokKind::Eof {
            fields.push(Field::new(lexer));
            lexer.skip_newlines();
        }
        lexer.advance();
        fields
    }

    fn write_constructor(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "impl<'a> {}<'a> {{", &self.name)?;
        writeln!(f, "\tpub fn new(")?;
        for field in self.fields.iter().filter(|f| f.is_argument()) {
            writeln!(f, "\t\t{},", field)?;
        }
        writeln!(f, "\t) -> Self {{")?;
        writeln!(f, "\t\tSelf {{")?;
        for field in &self.fields {
            write!(f, "\t\t\t{}", &field.name)?;
            if let Some(ref default) = field.default {
                write!(f, ": {}", default)?;
            }
            writeln!(f, ",")?;
        }
        writeln!(f, "\t\t}}")?;
        writeln!(f, "\t}}")?;
        writeln!(f, "}}")
    }

    fn write_macro(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "#[macro_export]")?;
        writeln!(f, "macro_rules! {} {{", pascal_to_snake(&self.name))?;
        write!(f, "\t($self:expr")?;
        for field in self.fields.iter().filter(|field| field.is_macro_argument()) {
            write!(f, ", ${}:expr", field.name)?;
        }
        writeln!(f, ") => {{")?;
        writeln!(f, "\t\t{}::new(", &self.name)?;
        for field in self.fields.iter().filter(|field| field.is_argument()) {
            write!(f, "\t\t\t")?;
            field.write_type_prefix(false, f)?;
            if field.owned || field.passed {
                writeln!(f, "${},", &field.name)?;
            } else {
                writeln!(f, "$self.{},", pascal_to_snake(&field.name))?;
            }
        }
        writeln!(f, "\t\t)")?;
        writeln!(f, "\t}};")?;
        writeln!(f, "}}")
    }
}

impl Display for Struct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "pub struct {}<'a> {{", self.name)?;
        for field in &self.fields {
            writeln!(f, "\tpub {},", field)?;
        }
        writeln!(f, "}}")
    }
}

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Field {
    pub passed: bool,
    pub mutable: bool,
    pub owned: bool,
    pub ty: String,
    pub name: String,
    pub default: Option<String>,
}

impl Field {
    pub fn new(lexer: &mut Lexer) -> Self {
        let passed = lexer.current().kind == TokKind::Passed;
        if passed {
            lexer.advance();
        }
        let owned = lexer.current().kind == TokKind::Owned;
        let mutable = if owned {
            lexer.advance();
            false
        } else {
            let mutable = lexer.current().kind == TokKind::Mut;
            if mutable {
                lexer.advance();
            }
            mutable
        };

        lexer.expect(TokKind::Ident);
        let name = lexer.show(lexer.current().span).to_string();
        lexer.advance();

        let (name, ty) = if lexer.current().kind == TokKind::Colon {
            lexer.advance();
            lexer.expect(TokKind::Ident);
            let ty = lexer.show(lexer.current().span).to_string();
            lexer.advance();
            (name, ty)
        } else {
            (pascal_to_snake(&name), name)
        };

        let default = if lexer.current().kind == TokKind::Equals {
            lexer.advance();
            lexer.expect(TokKind::String);
            let default = lexer.show(lexer.current().span).to_string();
            lexer.advance();
            Some(default)
        } else {
            None
        };

        Field {
            passed,
            mutable,
            owned,
            ty,
            name,
            default,
        }
    }

    fn write_type_prefix(&self, def: bool, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if !self.owned {
            write!(f, "&")?;
            if def {
                write!(f, "'a ")?;
            }
            if self.mutable {
                write!(f, "mut ")?;
            }
        }
        Ok(())
    }

    fn write_type(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.write_type_prefix(true, f)?;
        write!(f, "{}", self.ty)
    }

    fn is_argument(&self) -> bool {
        self.default.is_none()
    }

    fn is_macro_argument(&self) -> bool {
        self.is_argument() && (self.passed || self.owned)
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: ", &self.name)?;
        self.write_type(f)
    }
}

fn pascal_to_snake(s: &str) -> String {
    // credit to copilot
    let mut result = String::new();
    let mut start = true;
    for c in s.chars() {
        if c.is_uppercase() && !start {
            result.push('_');
        }
        result.push(c.to_ascii_lowercase());
        start = false;
    }
    result
}
