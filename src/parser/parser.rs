use super::*;
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
#[derive(Copy, Clone, Debug)]
pub struct TypeRef { index: usize, }

/// Comparing strings is expensive and done a lot in compilers.
/// Using this type allows us to compare pointers rather than data
#[derive(Copy, Clone, Debug)]
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

impl StrRef {
    //pub fn inner(self) -> &'static str { self.s }
}

pub struct ParseData {
    string_allocator: StaticBumpAllocator,
    types: Vec<TypeData>,
    strings: Vec<StrRef>,
}

impl ParseData {
    pub fn new() -> Self {
        let mut string_allocator = StaticBumpAllocator::new();
        let types = vec![
            TypeData::new(StrRef { s: string_allocator.alloc_str("()") }),
        ];

        ParseData {
            string_allocator,
            strings: vec![],
            types,
        }
    }

    // todo refactor. kinda wack. reduce to one lookup? check if bottleneck
    pub fn lookup_type<'b>(&'b mut self, type_string: &'b str) -> TypeRef {
        let str_ref = self.lookup_string(type_string);
        
        match self.types.iter().position(|t| t == type_string) {
            Some(index) => TypeRef { index },
            None => {
                let index = self.types.len();
                self.types.push(TypeData::new(str_ref));
                TypeRef { index }
            }
        }
    }
    
    pub fn lookup_string<'b>(&'b mut self, string: &'b str) -> StrRef {
        match self.strings.iter().find(|s| *s == string) {
            Some(&s) => s,
            None => {
                let s = StrRef { s: self.string_allocator.alloc_str(string) };
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

    pub fn take_next<'b>(&'b mut self) -> (Token, Span) {
        let t = self.tokens[self.position];
        self.position += 1;
        t
    }

    pub fn peek<'b>(&'b self) -> (Token, Span) {
        self.tokens[self.position]
    }

    pub fn try_peek<'b>(&'b self) -> Option<(Token, Span)> {
        if self.finished() {
            None
        } else {
            Some(self.tokens[self.position])
        }
    }

    pub fn expect_token<'b>(&'b mut self, token: Token) -> Span {
        let (tok, span) = self.peek();
        if tok != token {
            eprint!("expected {:?}, found", token);
            self.debug();
            assert!(tok == token);
        }
        self.take_next();
        span
    }

    pub fn try_expect_token<'b>(&'b mut self, token: Token) -> Option<Span> {
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
