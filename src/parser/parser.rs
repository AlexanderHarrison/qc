use crate::lexer::{Token, Span};
use crate::alloc::StaticBumpAllocator;

pub struct Parser<'a> {
    position: usize,
    tokens: &'a [(Token, Span)],
    file: &'a str,
}

/// Coincides with a base type without references.
/// The actual data is stored at 'index' in ParseData.types
/// E.g. I32, (), bool
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct TypeRef { index: usize, }
impl TypeRef { pub fn index(self) -> usize { self.index } }

pub const UNIT_TYPE: TypeRef = TypeRef { index: 0 };
pub const INT_TYPE: TypeRef = TypeRef { index: 1 };
pub const FLOAT_TYPE: TypeRef = TypeRef { index: 2 };
pub const BOOL_TYPE: TypeRef = TypeRef { index: 3 };

/// Comparing strings is expensive and done a lot in compilers.
/// Using this type allows us to compare pointers rather than data
#[derive(Copy, Clone, Debug, Hash)]
pub struct StrRef { s: &'static str, }

impl std::cmp::PartialEq<&str> for StrRef {
    fn eq(&self, other: &&str) -> bool {
        &self.s == other
    }
}

impl std::cmp::PartialEq<str> for StrRef {
    fn eq(&self, other: &str) -> bool { self.s == other }
}

impl std::cmp::PartialEq for StrRef {
    fn eq(&self, other: &StrRef) -> bool { self.s.as_ptr() == other.s.as_ptr() }
}
impl std::cmp::Eq for StrRef {}

impl std::fmt::Display for StrRef {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "{}", self.s)
    }
}

#[derive(Clone, Debug)]
pub struct TypeNames {
    names: Vec<StrRef>,
}

pub struct Strings {
    string_allocator: StaticBumpAllocator,
    strings: Vec<StrRef>,
}

pub struct ParseData {
    pub type_names: TypeNames,
    pub strings: Strings,
}

impl TypeNames {
    pub const BUILTIN_TYPE_COUNT: usize = 4;
    pub fn new(strings: &mut Strings) -> Self {
        let names = vec![
            strings.lookup_string("()"),
            strings.lookup_string("I64"),
            strings.lookup_string("F64"),
            strings.lookup_string("Bool"),
        ];

        assert_eq!(names.len(), Self::BUILTIN_TYPE_COUNT);

        Self {
            names
        }
    }

    pub fn is_builtin_type(&self, type_ref: TypeRef) -> bool {
        type_ref.index < Self::BUILTIN_TYPE_COUNT
    }

    pub fn iter_type_refs(&self) -> impl Iterator<Item=TypeRef> {
        (0..self.names.len())
            .map(|i| TypeRef { index: i })
    }

    pub fn get_type_name(&self, type_ref: TypeRef) -> StrRef {
        self.names[type_ref.index]
    }

    pub fn types_len(&self) -> usize {
        self.names.len()
    }

    pub fn lookup_type(&mut self, str_ref: StrRef) -> TypeRef {
        match self.names.iter().position(|t| *t == str_ref) {
            Some(index) => TypeRef { index },
            None => {
                let index = self.names.len();
                self.names.push(str_ref);
                TypeRef { index }
            }
        }
    }
}

impl Strings {
    pub fn new() -> Self {
        let mut string_allocator = StaticBumpAllocator::new();

        Strings {
            string_allocator,
            strings: Vec::new(),
        }
    }

    pub fn alloc_str(&mut self, string: &str) -> StrRef {
        StrRef { s: self.string_allocator.alloc_str(string) }
    }

    pub fn lookup_string(&mut self, string: &str) -> StrRef {
        match self.strings.iter().find(|s| *s == string) {
            Some(&s) => s,
            None => {
                let s = self.alloc_str(string);
                self.strings.push(s);
                s
            }
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [(Token, Span)], file: &'a str) -> Self {
        Self {
            position: 0,
            tokens,
            file,
        }
    }

    pub fn finished(&self) -> bool {
        self.position >= self.tokens.len()
    }

    pub fn span_string<'b>(&'b self, span: Span) -> &'a str {
        &self.file[span.range()]
    }

    pub fn take_next(&mut self) -> (Token, Span) {
        let t = self.tokens[self.position];
        self.position += 1;
        t
    }

    pub fn peek(&self) -> (Token, Span) {
        self.tokens[self.position]
    }

    pub fn try_peek(&self) -> Option<(Token, Span)> {
        if self.finished() {
            None
        } else {
            Some(self.tokens[self.position])
        }
    }

    pub fn expect_token(&mut self, token: Token) -> Span {
        let (tok, span) = self.peek();
        if tok != token {
            eprint!("expected {:?}, found", token);
            self.debug();
            assert!(tok == token);
        }
        self.take_next();
        span
    }

    pub fn try_expect_token(&mut self, token: Token) -> Option<Span> {
        match self.try_peek() {
            Some((tok, span)) if tok == token => {
                self.position += 1;
                Some(span)
            }
            _ => None
        }
    }

    pub fn span_line(&self, span: Span) -> usize {
        self.file.chars()
            .take(span.start)
            .filter(|&c| c == '\n')
            .count() + 1
    }

    #[allow(unused)]
    pub fn debug(&self) {
        let (tok, span) = self.peek();
        eprintln!("{:?} on line {}, '{}'", tok, self.span_line(span), self.span_string(span));
    }
}

pub trait Parsable: Sized {
    type Output;

    fn parse<'a, 'b>(parser: &'b mut Parser<'a>, data: &'b mut ParseData) -> Self::Output;
}
