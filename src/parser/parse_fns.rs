use crate::lexer::Token;
use super::*;

impl Parsable for ParsedFile {
    type Output = Self;
    fn parse(parser: &mut Parser<'_>, data: &mut ParseData) -> ParsedFile {
        let mut fns = Vec::new();
        let mut structs = Vec::new();

        while !parser.finished() {
            match parser.peek().0 {
                Token::FnKeyword => fns.push(ParsedFn::parse(parser, data)),
                Token::StructKeyword => structs.push(ParsedStruct::parse(parser, data)),
                _ => panic!("Unexpected top-level declaration")
            }
        }

        ParsedFile { 
            fns: fns.into_boxed_slice(),
            structs: structs.into_boxed_slice(),
        }
    }
}

impl Parsable for ParsedStruct {
    type Output = Self;
    fn parse(parser: &mut Parser<'_>, data: &mut ParseData) -> ParsedStruct {
        parser.expect_token(Token::StructKeyword);

        let name_span = parser.expect_token(Token::CapString);
        let name = data.lookup_string(parser.span_string(name_span));

        let fields = ParsedCurlyBracketList::<ParsedField>::parse(parser, data);

        ParsedStruct {
            name,
            fields,
        }
    }
}

impl Parsable for ParsedField {
    type Output = Self;
    fn parse(parser: &mut Parser<'_>, data: &mut ParseData) -> ParsedField {
        let name_span = parser.expect_token(Token::LowString);
        let name = data.lookup_string(parser.span_string(name_span));

        parser.expect_token(Token::Colon);

        let field_type = ParsedType::parse(parser, data);

        ParsedField {
            name,
            field_type
        }
    }
}

impl Parsable for ParsedFn {
    type Output = Self;
    fn parse(parser: &mut Parser<'_>, data: &mut ParseData) -> ParsedFn {
        parser.expect_token(Token::FnKeyword);
        let name_span = parser.expect_token(Token::LowString);
        let name = data.lookup_string(parser.span_string(name_span));

        let input_args = ParsedParenList::<ParsedArgument>::parse(parser, data);

        let return_type = match parser.try_expect_token(Token::RightArrow) {
            Some(_) => ParsedType::parse(parser, data),
            None => ParsedType {
                type_ref: data.lookup_type("()"),
                references: Box::new([])
            },
        };

        let body = ParsedBracedExpr::parse(parser, data);

        ParsedFn {
            name,
            input_args,
            return_type,
            body,
        }
    }
}

impl Parsable for ParsedArgument {
    type Output = Self;
    fn parse(parser: &mut Parser<'_>, data: &mut ParseData) -> ParsedArgument {
        let name_span = parser.expect_token(Token::LowString);
        let name = data.lookup_string(parser.span_string(name_span));

        parser.expect_token(Token::Colon);

        let arg_type = ParsedType::parse(parser, data);

        ParsedArgument {
            name,
            arg_type,
        }
    }
}

impl Parsable for ParsedType {
    type Output = Self;

    fn parse(parser: &mut Parser<'_>, data: &mut ParseData) -> ParsedType {
        let mut references = Vec::new();

        while parser.peek().0 == Token::Reference {
            references.push(ParsedReference::parse(parser, data))
        }

        let name_span = parser.expect_token(Token::CapString);
        let name = parser.span_string(name_span);
        let type_ref = data.lookup_type(name);

        ParsedType {
            type_ref,
            references: references.into_boxed_slice(),
        }
    }
}

impl Parsable for ParsedReference {
    type Output = Self;

    fn parse(parser: &mut Parser<'_>, _data: &mut ParseData) -> ParsedReference {
        parser.expect_token(Token::Reference);
        
        let mut flags = 0;
        loop {
            match parser.peek() {
                (Token::LowString, s) if parser.span_string(s) == "m" => {
                    assert!(flags & ReferenceFlags::Mutable as u8 == 0);
                    flags |= ReferenceFlags::Mutable as u8;
                    parser.take_next();
                }
                (Token::Integer, s) if parser.span_string(s) == "1" => {
                    assert!(flags & ReferenceFlags::Unique as u8 == 0);
                    flags |= ReferenceFlags::Unique as u8;
                    parser.take_next();
                }
                (Token::QuestionMark, _) => {
                    assert!(flags & ReferenceFlags::Nullable as u8 == 0);
                    flags |= ReferenceFlags::Nullable as u8;
                    parser.take_next();
                }
                _ => break,
            }
        }

        ParsedReference { flags }
    }
}

impl Parsable for ParsedExpr {
    type Output = ParsedExpr;

    fn parse(parser: &mut Parser<'_>, data: &mut ParseData) -> Self::Output {
        let mut simple_exprs = Vec::new();
        let mut operators = Vec::new();

        loop {
            let expr = if parser.peek().0 == Token::LeftCurlyBracket {
                let braced_expr = ParsedBracedExpr::parse(parser, data);
                ParsedSimpleExpr::BracedExpr(braced_expr)
            } else {
                let atom = ParsedAtom::parse(parser, data);
                ParsedSimpleExpr::InlineExpr(atom)
            };
            simple_exprs.push(expr);

            let token = parser.peek().0;
            match token {
                Token::Plus | Token::Minus | Token::DoubleEquals | Token::Asterisk => {
                    parser.take_next();
                    operators.push(InfixOperator::from_token(token).unwrap());
                }
                _ => break
            }
        }

        ParsedExpr {
            simple_exprs: simple_exprs.into_boxed_slice(),
            operators: operators.into_boxed_slice(),
        }
    }
}

impl Parsable for ParsedBracedExpr {
    type Output = Self;

    fn parse(parser: &mut Parser<'_>, data: &mut ParseData) -> Self::Output {
        parser.expect_token(Token::LeftCurlyBracket);

        let mut statements = Vec::new();
        loop {
            match parser.peek() {
                // TODO rest of statements here
                (Token::LetKeyword | Token::ReturnKeyword, _) => {
                    statements.push(ParsedStatement::parse(parser, data))
                }

                (Token::RightCurlyBracket, _) => {
                    parser.take_next();
                    return ParsedBracedExpr {
                        statements: statements.into_boxed_slice(),
                        return_expr: None,
                    }
                }
                _ => {
                    let return_expr = ParsedExpr::parse(parser, data);
                    parser.expect_token(Token::RightCurlyBracket);
                    return ParsedBracedExpr {
                        statements: statements.into_boxed_slice(),
                        return_expr: Some(Box::new(return_expr)),
                    }
                }
            }
        }
    }
}

impl Parsable for ParsedAtom {
    type Output = Self;

    fn parse(parser: &mut Parser<'_>, data: &mut ParseData) -> Self::Output {
        match parser.peek() {
            (Token::LowString, span) => {
                parser.take_next();
                let name = data.lookup_string(parser.span_string(span));

                if matches!(parser.try_peek(), Some((Token::LeftParen, _))) {
                    let arguments = ParsedParenList::<ParsedExpr>::parse(parser, data);
                    ParsedAtom::FunctionCall {
                        name,
                        arguments,
                    }
                } else {
                    ParsedAtom::Variable { name }
                }
            }
            (Token::LeftParen, _) => {
                parser.take_next();
                // either unit type or precedence specifier 
                // TODO

                assert!(parser.take_next().0 == Token::RightParen); // tuples?
                ParsedAtom::Unit
            }
            (Token::Integer, span) => {
                parser.take_next();
                let n = parser.span_string(span).parse::<isize>().unwrap();

                ParsedAtom::IntegerLiteral(n)
            }
            (Token::Float, span) => {
                parser.take_next();
                let n = parser.span_string(span).parse::<f64>().unwrap();

                ParsedAtom::FloatLiteral(n)
            }
            _ => {
                parser.debug();
                todo!()
            }
        }
    }
}

impl Parsable for ParsedStatement {
    type Output = ParsedStatement;

    fn parse(parser: &mut Parser<'_>, data: &mut ParseData) -> Self::Output {
        match parser.peek().0 {
            Token::LetKeyword => {
                parser.take_next();
                let name_span = parser.expect_token(Token::LowString);
                let name = data.lookup_string(parser.span_string(name_span));

                parser.expect_token(Token::SingleEquals);

                let expr = ParsedExpr::parse(parser, data);
                parser.expect_token(Token::Semicolon);

                ParsedStatement::Let {
                    name,
                    expr,
                }
            },
            Token::ReturnKeyword => {
                parser.take_next();

                let expr = ParsedExpr::parse(parser, data);
                parser.expect_token(Token::Semicolon);

                ParsedStatement::Return {
                    expr,
                }
            },
            _ => todo!(),
        }
    }
}

