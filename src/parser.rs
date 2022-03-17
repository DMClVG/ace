use std::rc::Rc;

use crate::lexer::{Token, TokenKind};

use crate::ast::*;

pub struct Parser<'a, 'b> {
    tokens: &'a [Token<'b>],
    current: usize,
    errors: Vec<ParserError<'b>>,
}

#[derive(Debug)]
pub struct ParserError<'a> {
    pub cause: String,
    pub responsible: &'a Token<'a>,
}

type Expr<'b> = Result<Expression, ParserError<'b>>;
// type Stmt<'b> = Result<Statement, ParserError<'b>>;

impl<'a: 'b, 'b> Parser<'a, 'b> {
    pub fn new(tokens: &'a [Token<'b>]) -> Self {
        Self {
            tokens,
            current: 0,
            errors: vec![],
        }
    }

    fn next(&mut self) -> &'b Token<'b> {
        self.advance();
        self.previous()
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn back(&mut self) {
        self.current -= 1;
    }

    fn peek(&self) -> &'b Token<'b> {
        &self.tokens[self.current + 1]
    }

    fn current(&self) -> &'b Token<'b> {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &'b Token<'b> {
        &self.tokens[self.current - 1]
    }

    fn is_eof(&self) -> bool {
        self.current >= self.tokens.len() || self.current().kind == TokenKind::EOF
    }

    fn exit_statement(&mut self) {
        while !self.is_eof() {
            match self.next().kind {
                TokenKind::If
                | TokenKind::While
                | TokenKind::RBraces
                | TokenKind::Else
                | TokenKind::Identifier(_) => {
                    self.back();
                    break;
                }
                _ => {}
            }
        }
    }

    fn error(&mut self, err: ParserError<'b>) {
        self.errors.push(err);
        self.exit_statement();
    }

    pub fn parse(mut self) -> Result<Statement, Vec<ParserError<'b>>> {
        while !self.is_eof() {
            match self.current().kind {
                TokenKind::LBraces => {
                    // skip tokens until it finds an {
                    let code = self.block(true);
                    if self.errors.len() != 0 {
                        return Err(self.errors);
                    } else {
                        return Ok(code);
                    }
                }
                _ => self.advance(),
            }
        }

        return Ok(Statement::Block(vec![], true)); // return empty block if script is empty
    }

    fn statement(&mut self) -> Statement {
        match self.current().kind {
            TokenKind::LBraces => {
                return self.block(true);
            }
            TokenKind::If => {
                self.advance();
                return Statement::If(
                    Box::new(self.statement()),
                    Box::new(self.block(false)),
                    self.else_block().map(|stmt| Box::new(stmt)),
                );
            }
            TokenKind::While => {
                self.advance();
                return Statement::While(Box::new(self.statement()), Box::new(self.block(false)));
            }
            TokenKind::Fun => {
                self.advance();
                if TokenKind::Colon == self.current().kind {
                    self.advance();
                    return Statement::FunDecl(self.params(), Box::new(self.block(true)));
                } else {
                    return Statement::FunDecl(vec![], Box::new(self.block(true)))
                }
            },
            TokenKind::EOF => {
                self.error(ParserError {
                    cause: "Expected a statement here".to_owned(),
                    responsible: self.previous(),
                });
            }
            _ => {}
        }
        let expr = self.expression();
        if let Err(err) = expr {
            self.error(err);
            Statement::Block(vec![], false)
        } else if let Ok(expr) = expr {
            if self.current().kind == TokenKind::Assign {
                self.advance();
                Statement::Assign(expr, Box::new(self.statement()))
            } else {
                Statement::Expr(expr)
            }
        } else {
            unreachable!();
        }
    }

    fn else_block(&mut self) -> Option<Statement> {
        if TokenKind::Else == self.current().kind {
            self.advance();
            Some(self.block(false))
        } else {
            None
        }
    }

    fn block(&mut self, scoped: bool) -> Statement {
        match self.current().kind {
            TokenKind::LBraces => {
                self.advance();

                let mut block = vec![];
                loop {
                    match self.current().kind {
                        TokenKind::RBraces => {
                            self.advance();
                            break;
                        }
                        TokenKind::EOF => self.error(ParserError {
                            cause: "Unclosed block".to_string(),
                            responsible: &self.previous(),
                        }),
                        _ => {
                            block.push(self.statement());
                        }
                    }
                }
                Statement::Block(block, scoped)
            }
            _ => self.statement(),
        }
    }

    // fn stmt_expression(&mut self) -> Statement {
    //     match self.expression() {
    //         Ok(expr) => Statement::Expr(expr),
    //         Err(err) => {
    //             self.error(err);
    //             Statement::Block(vec![])
    //         }
    //     }
    // }

    fn expression(&mut self) -> Expr<'b> {
        self.composition()
    }

    fn composition(&mut self) -> Expr<'b> {
        let mut expr = self.equality()?;
        loop {
            match self.current().kind {
                TokenKind::And => {
                    self.advance();
                    expr = Expression::Binary(
                        BinaryOperator::And,
                        Box::new(expr),
                        Box::new(self.equality()?),
                    )
                }
                TokenKind::Or => {
                    self.advance();
                    expr = Expression::Binary(
                        BinaryOperator::Or,
                        Box::new(expr),
                        Box::new(self.equality()?),
                    )
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Expr<'b> {
        // != or ==
        let mut expr = self.comparison()?;

        loop {
            match self.current().kind {
                TokenKind::Equal => {
                    self.advance();
                    expr = Expression::Binary(
                        BinaryOperator::IsEqual,
                        Box::new(expr),
                        Box::new(self.comparison()?),
                    );
                }
                TokenKind::Unequal => {
                    self.advance();
                    expr = Expression::Binary(
                        BinaryOperator::IsUnequal,
                        Box::new(expr),
                        Box::new(self.comparison()?),
                    );
                }
                _ => {
                    break;
                }
            }
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Expr<'b> {
        // > / < / >= / <=
        let mut expr = self.term()?;

        loop {
            match self.current().kind {
                TokenKind::Greater => {
                    self.advance();
                    expr = Expression::Binary(
                        BinaryOperator::IsGreater,
                        Box::new(expr),
                        Box::new(self.term()?),
                    );
                }
                TokenKind::Lesser => {
                    self.advance();
                    expr = Expression::Binary(
                        BinaryOperator::IsLesser,
                        Box::new(expr),
                        Box::new(self.term()?),
                    );
                }
                TokenKind::LesserOrEq => {
                    self.advance();
                    expr = Expression::Binary(
                        BinaryOperator::IsLesserOrEqual,
                        Box::new(expr),
                        Box::new(self.term()?),
                    );
                }
                TokenKind::GreaterOrEq => {
                    self.advance();
                    expr = Expression::Binary(
                        BinaryOperator::IsGreaterOrEqual,
                        Box::new(expr),
                        Box::new(self.term()?),
                    );
                }
                _ => {
                    break;
                }
            }
        }
        Ok(expr)
    }

    fn term(&mut self) -> Expr<'b> {
        // - and +
        let mut expr = self.factor()?;

        loop {
            match self.current().kind {
                TokenKind::Plus => {
                    self.advance();
                    expr = Expression::Binary(
                        BinaryOperator::Add,
                        Box::new(expr),
                        Box::new(self.factor()?),
                    );
                }
                TokenKind::Minus => {
                    self.advance();
                    expr = Expression::Binary(
                        BinaryOperator::Subtract,
                        Box::new(expr),
                        Box::new(self.factor()?),
                    );
                }
                _ => {
                    break;
                }
            }
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Expr<'b> {
        // * and /
        let mut expr = self.unary()?;

        loop {
            match self.current().kind {
                TokenKind::Asterisk => {
                    self.advance();
                    expr = Expression::Binary(
                        BinaryOperator::Multiply,
                        Box::new(expr),
                        Box::new(self.unary()?),
                    );
                }
                TokenKind::Slash => {
                    self.advance();
                    expr = Expression::Binary(
                        BinaryOperator::Divide,
                        Box::new(expr),
                        Box::new(self.unary()?),
                    );
                }
                TokenKind::Percent => {
                    self.advance();
                    expr = Expression::Binary(
                        BinaryOperator::Modulo,
                        Box::new(expr),
                        Box::new(self.unary()?),
                    );
                }
                _ => {
                    break;
                }
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Expr<'b> {
        match self.current().kind {
            TokenKind::Minus => {
                self.advance();
                Ok(Expression::Unary(
                    UnaryOperator::Negate,
                    Box::new(self.unary()?),
                ))
            }
            TokenKind::Exclamation => {
                self.advance();
                Ok(Expression::Unary(
                    UnaryOperator::Inverse,
                    Box::new(self.unary()?),
                ))
            }
            _ => self.post(),
        }
    }

    fn post(&mut self) -> Expr<'b> {
        let mut expr = self.primary()?;
        loop {
            match self.current().kind {
                TokenKind::LParenthese => {
                    self.advance();
                    expr = Expression::Binary(
                        BinaryOperator::Call,
                        Box::new(expr),
                        Box::new(Expression::List(self.series(
                            TokenKind::RParenthese,
                            TokenKind::Comma,
                            None,
                        )?)),
                    )
                }
                TokenKind::LBrackets => {
                    self.advance();
                    let i = self
                        .series(TokenKind::RBrackets, TokenKind::Comma, Some(1))?
                        .into_iter()
                        .nth(0)
                        .unwrap();
    
                    expr = Expression::Binary(
                        BinaryOperator::Index,
                        Box::new(expr),
                        Box::new(i),
                    )
                }
                TokenKind::Period => {
                    self.advance();
                    let field_name = self.next();
                    if let TokenKind::Identifier(name) = field_name.kind {
                        expr = Expression::Binary(
                            BinaryOperator::Index,
                            Box::new(expr),
                            Box::new(Expression::Primary(Leaf::Literal(Value::String(Rc::new(name.to_owned())))))
                        )
                    } else {
                        return Err(ParserError { cause: "Expected a field name after period".to_owned(), responsible: field_name });
                    }
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn primary(&mut self) -> Expr<'b> {
        match self.next().kind {
            TokenKind::Number(val) => Ok(Expression::Primary(Leaf::Literal(Value::Number(val)))),
            // TokenKind::Coefficient(x, ref y) => Ok(Expression::Binary(
            //     BinaryOperator::Multiply,
            //     Box::new(Expression::Primary(Leaf::Literal(Value::Number(x)))),
            //     Box::new(Expression::Primary(Leaf::Var(y.to_owned()))),
            // )),
            TokenKind::String(ref val) => Ok(Expression::Primary(Leaf::Literal(Value::String(
                Rc::new(val.to_string()),
            )))),
            TokenKind::LBrackets => Ok(Expression::List(self.series(
                TokenKind::RBrackets,
                TokenKind::Comma,
                None,
            )?)),
            TokenKind::Colon if self.current().kind == TokenKind::LBraces => {
                self.advance();
                Ok(Expression::Pairs(self.key_value(TokenKind::RBraces, TokenKind::Comma, TokenKind::Colon)?))
            },
            TokenKind::Identifier(ref id) => Ok(Expression::Primary(Leaf::Var(id.to_string()))),
            TokenKind::True => Ok(Expression::Primary(Leaf::Literal(Value::Bool(true)))),
            TokenKind::False => Ok(Expression::Primary(Leaf::Literal(Value::Bool(false)))),
            TokenKind::Nil => Ok(Expression::Primary(Leaf::Literal(Value::Nil))),
            TokenKind::LParenthese => {
                let expr = self.expression()?;
                match self.next() {
                    Token {
                        kind: TokenKind::RParenthese,
                        ..
                    } => Ok(Expression::Grouping(Box::new(expr))),
                    n @ Token {
                        kind: TokenKind::EOF,
                        ..
                    } => Err(ParserError {
                        cause: "Missing right parenthese".to_string(),
                        responsible: n,
                    }),
                    n => Err(ParserError {
                        cause: "Unexpected token".to_string(),
                        responsible: n,
                    }),
                }
            }
            TokenKind::EOF => Err(ParserError {
                cause: "Ended unexpectedly".to_string(),
                responsible: self.previous(),
            }),
            _ => Err(ParserError {
                cause: "Unexpected token".to_string(),
                responsible: self.previous(),
            }),
        }
    }

    fn series(
        &mut self,
        terminator: TokenKind,
        separator: TokenKind,
        max_elements: Option<usize>,
    ) -> Result<Vec<Expression>, ParserError<'b>> {
        let mut exprs = vec![];
        if terminator != self.current().kind {
            exprs.push(self.expression()?);

            loop {
                match self.current().kind {
                    ref t if *t == terminator => {
                        self.advance();
                        break;
                    }
                    ref t if *t == separator && self.peek().kind == terminator => {
                        self.advance();
                        self.advance();
                        break;
                    }
                    ref t
                        if *t == separator
                            && !(max_elements.is_some()
                                && exprs.len() == max_elements.unwrap()) =>
                    {
                        self.advance();
                        exprs.push(self.expression()?);
                    }
                    _ => Err(ParserError {
                        cause: format!("Expected a {:?} or a {:?}", separator, terminator),
                        responsible: self.current(),
                    })?,
                }
            }
        } else {
            self.advance();
        }
        Ok(exprs)
    }

    fn params(
        &mut self
    ) -> Vec<String> {
        let mut params = vec![];

        loop {
            if let TokenKind::Identifier(name) = self.next().kind {
                params.push(name.to_owned());
            } else {
                self.error(ParserError { cause: "Invalid parameter name".to_owned(), responsible: self.previous() });
                break;
            }

            match self.current().kind {
                TokenKind::Comma => {
                    self.advance();
                    continue;
                }
                _ => {
                    break;
                }
            }
        }

        params
    }

    fn key_value(
        &mut self,
        terminator: TokenKind,
        separator: TokenKind,
        assigner: TokenKind,
    ) -> Result<Vec<(String, Expression)>, ParserError<'b>> {
        let mut exprs = vec![];

        let read_keyval = |parser: &mut Parser<'a, 'b>| -> Result<(String, Expression), ParserError<'b>>{
            let key = match parser.current().kind {
                TokenKind::String(k) => k.to_owned(),
                TokenKind::Identifier(k) => k.to_owned(),
                _ => Err(ParserError { cause: "Key must be an identifier or a string".to_owned(), responsible: parser.current() })?
            };
            parser.advance();
            if assigner != parser.current().kind {
                Err(ParserError { cause: format!("Expected a {:?}", assigner), responsible: parser.current() })
            } else {
                parser.advance();
                let value = parser.expression()?;
                Ok((key, value))
            }
        };

        if terminator != self.current().kind {
            exprs.push(read_keyval(self)?);
            loop {
                match self.current().kind {
                    ref t if *t == terminator => {
                        self.advance();
                        break;
                    }
                    ref t if *t == separator && self.peek().kind == terminator => {
                        self.advance();
                        self.advance();
                        break;
                    }
                    ref t if *t == separator => {
                        self.advance();
                        exprs.push(read_keyval(self)?);
                    }
                    _ => Err(ParserError {
                        cause: format!("Expected a {:?} or a {:?}", separator, terminator),
                        responsible: self.current(),
                    })?,
                }
            }
        } else {
            self.advance();
        }
        Ok(exprs)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{Lexer, TokenKind},
        parser::Parser,
    };

    #[test]
    fn test_expression1() {
        let mut lexer = Lexer::new("5 == 5");
        let tokens = lexer.lex();
        println!(
            "{:?}",
            tokens
                .iter()
                .map(|token| { token.kind.clone() })
                .collect::<Vec<TokenKind>>()
        );
        let mut parser = Parser::new(&tokens);
        println!("RESULT: {:?}", parser.expression());
    }

    #[test]
    fn test_parse1() {
        let mut lexer = Lexer::new("{(y = 5+3) == 5}");
        let tokens = lexer.lex();
        println!(
            "{:?}",
            tokens
                .iter()
                .map(|token| { token.kind.clone() })
                .collect::<Vec<TokenKind>>()
        );
        let parser = Parser::new(&tokens);
        println!("RESULT: {:?}", parser.parse().unwrap());
    }
}
