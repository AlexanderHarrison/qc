use crate::lexer::Token;
use crate::Parsable;
use crate::parser::{Parser, ParseData};

pub type ParsedParenList<P> = ParsedList<Comma, Parens, P>;
pub type ParsedCurlyBracketList<P> = ParsedList<Comma, CurlyBrackets, P>;

pub trait ListSeparator {
    const SEPARATOR: Token;
}

pub trait ListEnds {
    const START: Token;
    const END: Token;
}

pub struct ParsedList<Sep: ListSeparator, Ends: ListEnds, P: Parsable> {
    _marker: std::marker::PhantomData<(Sep, Ends, P)>
}

pub struct Comma;
impl ListSeparator for Comma { const SEPARATOR: Token = Token::Comma; }

pub struct Parens;
impl ListEnds for Parens { 
    const START: Token = Token::LeftParen; 
    const END: Token = Token::RightParen;
}

pub struct CurlyBrackets;
impl ListEnds for CurlyBrackets { 
    const START: Token = Token::LeftCurlyBracket; 
    const END: Token = Token::RightCurlyBracket;
}

impl<Sep, Ends, P> Parsable for ParsedList<Sep, Ends, P> where
    Sep: ListSeparator,
    Ends: ListEnds,
    P: Parsable
{
    type Output = Box<[<P as Parsable>::Output]>;

    fn parse(parser: &mut Parser<'_>, data: &mut ParseData) -> Self::Output {
        parser.expect_token(Ends::START);
        let mut list = Vec::new();

        if parser.try_expect_token(Ends::END).is_none() {
            loop {
                list.push(P::parse(parser, data));
                if parser.try_expect_token(Ends::END).is_some() {
                    break;
                } else {
                    parser.expect_token(Sep::SEPARATOR);
                }
            }
        }

        list.into_boxed_slice()
    }
}
