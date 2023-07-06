#![allow(unused)]

use crate::parser::*;
use super::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub type_ref: TypeRef,
    pub references: References,
}

impl Type {
    pub fn from_ref(type_ref: TypeRef) -> Self {
        Type {
            type_ref,
            references: References::new(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TypingResult {
    Finished,
    Unfinished
}

#[derive(Clone, Debug)]
pub enum Binding {
    Type(Type),
    Function {
        arg_types: Box<[Type]>,
        return_type: Type,
    }
}

pub struct Context<'a> {
    pub typing_data: &'a TypingData,
    pub names: Vec<StrRef>,
    pub types: Vec<Binding>,
    scopes: Vec<usize>,
}

impl<'a> Context<'a> {
    pub fn new(typing_data: &'a TypingData) -> Self {
        Context {
            typing_data,
            names: Vec::new(),
            types: Vec::new(),
            scopes: Vec::new(),
        }
    }

    pub fn push(&mut self, name: StrRef, bind: Binding) {
        self.names.push(name);
        self.types.push(bind);
    }

    pub fn search(&self, var: StrRef) -> Option<&Binding> {
        let len = self.names.len();
        self.names.iter()
            .rev()
            .position(|&s| s == var)
            .map(|i| &self.types[len - i - 1])
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(self.names.len())
    }

    pub fn pop_scope(&mut self) {
        let var_count = self.names.len();
        let scope_end_var_count = self.scopes.pop().expect("popped empty scope");
        assert!(scope_end_var_count <= var_count);
        self.names.truncate(scope_end_var_count);
        self.types.truncate(scope_end_var_count);
    }
}

