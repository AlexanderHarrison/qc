use logos::Logos;

#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn range(self) -> std::ops::Range<usize> {
        self.start..self.end
    }

    pub fn new(span: std::ops::Range<usize>) -> Self {
        Span { start: span.start, end: span.end }
    }
}

#[derive(Logos, Clone, Copy, Debug, PartialEq)]
pub enum Token {
    #[token(",")] Comma,
    #[token(".")] Period,
    #[token(";")] Semicolon,
    #[token("==")] DoubleEquals,
    #[token("=")] SingleEquals,
    #[token("(")] LeftParen,
    #[token(")")] RightParen,
    #[token("[")] LeftSquareBracket,
    #[token("]")] RightSquareBracket,
    #[token("{")] LeftCurlyBracket,
    #[token("}")] RightCurlyBracket,
    #[token(":")] Colon,
    #[token("*")] Asterisk,
    #[token("&")] Reference,
    #[token("?")] QuestionMark,
    #[token("#[")] DirectiveStart,

    #[token("->")] RightArrow,
    #[token("fn")] FnKeyword,
    #[token("let")] LetKeyword,
    #[token("return")] ReturnKeyword,
    #[token("struct")] StructKeyword,
    #[token("if")] IfKeyword,
    #[token("else")] ElseKeyword,
    #[token("for")] ForKeyword,

    #[regex(r"[\t \n]+", logos::skip)] Whitespace,
    #[regex(r"//.*", logos::skip)] Comment,

    // TODO merge capstring + lowstring
    #[regex("[A-Z][a-zA-Z0-9_]*")]
    CapString,

    #[regex("[a-z][a-zA-Z0-9_]*")]
    LowString,

    #[regex(r#""[^"]"#)]
    StringLiteral,

    #[regex(r"-?[0-9]+\.[0-9]+")]
    Float,

    #[regex(r"-?[0-9]+")]
    Integer,

    // placed at bottom to avoid collisions with float regex
    #[token("-")] Minus,
    #[token("+")] Plus,
}

pub fn tokenize(stream: &str) -> Option<Box<[(Token, Span)]>> {
    Token::lexer(stream)
        .spanned()
        .map(|(tok, span)| tok
             .map(|t| (t, Span::new(span)))
             //.map_err(|_| println!("{}", &stream[span.start..span.end]))
             .ok())
        .collect::<Option<Vec<(Token, Span)>>>()
        .map(|t| t.into_boxed_slice())
}
