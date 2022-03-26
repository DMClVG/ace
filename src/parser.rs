use crate::lexer::{Token, TokenKind, TokenKind::*};

use crate::ast::{BinaryOperator::*, Expression::*, Expression, UnaryOperator::*, Statement, Value};

pub struct Parser<'a, 'b> {
    tokens: &'a [Token<'b>],
    current: usize,
}

#[derive(Debug)]
pub struct ParserError<'a> {
    pub cause: std::string::String,
    pub responsible: &'a Token<'a>,
}

type Expr<'b> = Result<Expression, ParserError<'b>>;
type Stmt<'b> = Result<Statement, Vec<ParserError<'b>>>;

macro_rules! match_next {
    ($obj:expr, {$($matcher:pat $(if $pred:expr)? => $result:expr),* $(,)?})  => {
        match $obj.peek().kind {
            $($matcher $(if $pred)? => {
                $obj.next();
                Some($result)
            }),*
            _ => None,
        }
    }
}

impl<'a: 'b, 'b> Parser<'a, 'b> {
    pub fn new(tokens: &'a [Token<'b>]) -> Self {
        Self {
            tokens,
            current: 0,
        }
    }

    fn next(&mut self) -> &'b Token<'b> {
        self.current += 1;
        return self.peekb();
    }

    fn back(&mut self) -> &'b Token<'b> {
        self.current -= 1;
        return self.peekf();
    }

    fn match_adv(&mut self, tk: &TokenKind) -> bool {
        if self.peek().kind == *tk {
            self.next();
            true
        } else {
            false
        }
    }

    fn peekf(&self) -> &'b Token<'b> {
        &self.tokens[self.current + 1]
    }

    fn peek(&self) -> &'b Token<'b> {
        &self.tokens[self.current]
    }

    fn peekb(&self) -> &'b Token<'b> {
        &self.tokens[self.current - 1]
    }

    fn is_eof(&self) -> bool {
        self.peek().kind == EOF
    }

    pub fn parse(mut self) -> Stmt<'b> {
        // self.statements(EOF).map(|stmts| Statement::Block(stmts, true))
        self.block(true)
    }

    fn statement(&mut self) -> Stmt<'b> {
        if self.is_eof() {
            return Err(vec![ParserError { cause: "Expected a statement here".to_owned(), responsible: self.peekb() }]);
        }

        match_next!(self, {
            If => {
                let condition = self.expression_stmt()?;
                let body = self.block(false)?;
                let elseb = if let Some(elseb) = self.else_block() {
                    Some(Box::new(elseb?))
                } else {
                    None
                };
                Ok(Statement::If(Box::new(condition), Box::new(body), elseb))
            },
            While => {
                let condition = self.expression_stmt()?;
                let body = self.block(false)?;
                Ok(Statement::While(Box::new(condition), Box::new(body)))
            },
            Return => Ok(Statement::Return(Box::new(self.statement()?))),
            Break => Ok(Statement::Break),
            Continue => Ok(Statement::Continue),
            LBraces => {
                self.statements(RBraces).map(|stmts| Statement::Block(stmts, true))
            },
            Fun => {
                if self.match_adv(&Colon) {
                    Ok(Statement::FunDecl(self.params().map_err(|e| vec![e])?, Box::new(self.block(true)?)))
                } else {
                    Ok(Statement::FunDecl(vec![], Box::new(self.block(true)?)))
                }
            },
        })
        .unwrap_or_else(|| {
            if let Statement::Expr(expr) = self.expression_stmt()? {
                match_next!(self, {
                    Assign => {
                        let to = self.statement()?;
                        Ok(Statement::Assign(expr.clone(), Box::new(to))) // TODO: remove this clone
                    }
                })
                .unwrap_or_else(|| Ok(Statement::Expr(expr)))
            } else {
                unreachable!()
            }
        })
    }

    fn block(&mut self, scoped: bool) -> Stmt<'b> {
        if self.match_adv(&LBraces) {
            self.statements(RBraces)
                .map(|stmts| Statement::Block(stmts, scoped))
        } else {
            self.statement()
        }
    }

    fn else_block(&mut self) -> Option<Stmt<'b>> {
        match_next!(self, {
            Else => self.block(false)
        })
    }

    fn expression_stmt(&mut self) -> Stmt<'b> {
        Ok(Statement::Expr(self.expression().map_err(|err| vec![err])?))
    }

    fn exit_error(&mut self) {
        while !self.is_eof() {
            match self.next().kind {
                If
                | While
                | LBraces | Return => {
                    self.back();
                    break;
                },
                Assign | TokenKind::RBraces => {
                    break;
                }
                _ => {}
            }
        }
    }

    fn statements(
        &mut self,
        terminator: TokenKind,
    ) -> Result<Vec<Statement>, Vec<ParserError<'b>>> {
        let mut res = Ok(vec![]);

        while !self.match_adv(&terminator) {
            if self.is_eof() {
                if res.is_ok() { res = Err(vec![]) }
                res.as_mut().unwrap_err().push(ParserError { cause: "Block never closed".to_owned(), responsible: self.peekb() });
                break;
            }
            match self.statement() {
                Ok(s) if res.is_ok() => res.as_mut().unwrap().push(s),
                Err(mut e) => {
                    if res.is_ok() {
                        res = Err(vec![])
                    }
                    res.as_mut().unwrap_err().append(&mut e);
                    self.exit_error();
                }
                _ => {}
            }
        }
        res
    }
    
    fn params(
        &mut self
    ) -> Result<Vec<std::string::String>, ParserError<'b>> {
        let mut params = vec![];
        loop {
            match_next!(self, {
                Identifier(name) => params.push(name.to_owned()),
            }).ok_or_else(||
                ParserError {
                    cause: "Expected an indentifier".to_owned(),
                    responsible: self.peek(),
                }
            )?;

            if self.match_adv(&Comma) {
                continue;
            } else {
                break;
            }
        }
        Ok(params)
    }

    fn expression(&mut self) -> Expr<'b> {
        self.composition()
    }

    fn composition(&mut self) -> Expr<'b> {
        let mut expr = self.equality()?;

        while match_next! (self, {
            And => {
                expr = Binary(
                    DoAnd,
                    Box::new(expr),
                    Box::new(self.equality()?),
                )
            },
            Or => {
                expr = Binary(
                    DoOr,
                    Box::new(expr),
                    Box::new(self.equality()?),
                )
            },
        })
        .is_some()
        {}

        Ok(expr)
    }

    fn equality(&mut self) -> Expr<'b> {
        let mut expr = self.comparison()?;

        match_next! (self, {
            Equal => {
                expr = Binary(
                    IsEqual,
                    Box::new(expr),
                    Box::new(self.comparison()?),
                )
            },
            Unequal => {
                expr = Binary(
                    IsUnequal,
                    Box::new(expr),
                    Box::new(self.comparison()?),
                )
            },
        });

        Ok(expr)
    }

    fn comparison(&mut self) -> Expr<'b> {
        let mut expr = self.term()?;

        while match_next!(self, {
            Greater => {
                expr = Binary(
                    IsGreater,
                    Box::new(expr),
                    Box::new(self.term()?),
                );
            },
            Lesser => {
                expr = Binary(
                    IsLesser,
                    Box::new(expr),
                    Box::new(self.term()?),
                );
            },
            LesserOrEq => {
                expr = Binary(
                    IsLesserOrEqual,
                    Box::new(expr),
                    Box::new(self.term()?),
                );
            },
            GreaterOrEq => {
                expr = Binary(
                    IsGreaterOrEqual,
                    Box::new(expr),
                    Box::new(self.term()?),
                );
            },
        })
        .is_some()
        {}
        Ok(expr)
    }

    fn term(&mut self) -> Expr<'b> {
        let mut expr = self.factor()?;

        while match_next!(self, {
             Plus => {
                 expr = Binary(
                     Add,
                     Box::new(expr),
                     Box::new(self.factor()?),
                 );
             },
             Minus => {
                 expr = Binary(
                     Subtract,
                     Box::new(expr),
                     Box::new(self.factor()?),
                 );
             },
        })
        .is_some()
        {}
        Ok(expr)
    }

    fn factor(&mut self) -> Expr<'b> {
        let mut expr = self.unary()?;

        while match_next!(self, {
             Asterisk => {
                 expr = Binary(
                     Multiply,
                     Box::new(expr),
                     Box::new(self.unary()?),
                 );
             },
             Slash => {
                 expr = Binary(
                     Divide,
                     Box::new(expr),
                     Box::new(self.unary()?),
                 );
             },
             Percent => {
                expr = Binary(
                    Modulo,
                    Box::new(expr),
                    Box::new(self.unary()?),
                );
            },
        })
        .is_some()
        {}
        Ok(expr)
    }

    fn unary(&mut self) -> Expr<'b> {
        let expr = match_next! (self, {
            Exclamation => {
                Unary(
                    Inverse,
                    Box::new(self.unary()?),
                )
            },
            Minus => {
                Unary(
                    Negate,
                    Box::new(self.unary()?),
                )
            },
        });
        if let Some(expr) = expr {
            Ok(expr)
        } else {
            self.post()
        }
    }

    fn post(&mut self) -> Expr<'b> {
        let mut expr = self.primary()?;

        while match_next!(self, {
            LParenthese => {
                expr = Binary(
                    Call,
                    Box::new(expr),
                    Box::new(List(self.series_exprs(
                        RParenthese,
                        Comma,
                        None,
                    )?)),
                )
            },
            LBrackets => {
                let i = self
                .series_exprs(RBrackets, Comma, Some(1))?
                .into_iter()
                .nth(0)
                .ok_or_else(|| ParserError { cause: "Index is empty".to_owned(), responsible: self.peekb() })?;

                expr = Binary(
                    Index,
                    Box::new(expr),
                    Box::new(i),
                )
            },
            Period => {
                match_next!(self, {
                    Identifier(name) => {
                        expr = Binary(
                            Index,
                            Box::new(expr),
                            Box::new(Literal(name.into()))
                        )
                    },
                }).ok_or_else(|| {
                    ParserError { cause: "Expected a field name after period".to_owned(), responsible: self.peek() }
                })?;
            }
        }).is_some() {};

        Ok(expr)
    }

    fn primary(&mut self) -> Expr<'b> {
        if self.is_eof() {
            return Err(ParserError { cause: "Expression ended prematurely".to_owned(), responsible: self.peekb() });
        }

        match_next!(self, {
            Number(val) => Literal(val.into()),
            String(val) => Literal(val.into()),
            Identifier(id) => Var(id.to_owned()),

            True => Literal(true.into()),
            False => Literal(false.into()),
            Nil => Literal(Value::Nil),

            LParenthese => {
                let expr = self.expression()?;
                if !self.match_adv(&RParenthese) {
                    return Err(ParserError { cause: "Missing RParenthese".to_owned(), responsible: self.peekb() });
                }
                Grouping(Box::new(expr))
            },
            LBrackets => List(self.series_exprs(
                RBrackets,
                Comma,
                None,
            )?),
            Colon if self.peekf().kind == LBraces => {
                self.match_adv(&LBraces);
                Pairs(self.key_value(RBraces, Comma, Colon)?)
            },
        })
        .ok_or_else(|| ParserError {
            cause: "Unexpected token".to_string(),
            responsible: self.peek(),
        })
    }

    fn series_exprs(
        &mut self,
        terminator: TokenKind,
        separator: TokenKind,
        max_elements: Option<usize>,
    ) -> Result<Vec<Expression>, ParserError<'b>> {
        let mut exprs = vec![];
        loop {
            if self.match_adv(&terminator) {
                break;
            } else  {
                exprs.push(self.expression()?);

                if self.match_adv(&separator) {
                    if exprs.len() <= max_elements.unwrap_or(usize::MAX) {
                        continue;
                    } else {
                        return Err(ParserError { cause: "Too many elements".to_owned(), responsible: self.peek() });
                    }
                } else if self.match_adv(&terminator) {
                    break;
                } else {
                    return Err(ParserError { cause: format!("Expected a {:?} or a {:?}", &separator, &terminator), responsible: self.peek() });
                }
            }
        }
        Ok(exprs)
    }

    fn key_value(
        &mut self,
        terminator: TokenKind,
        separator: TokenKind,
        assigner: TokenKind,
    ) -> Result<Vec<(std::string::String, Expression)>, ParserError<'b>> {
        let mut keyvals = vec![];

        loop {
            if self.match_adv(&terminator) {
                break;
            } else  {
                let name = match_next!(self, {
                    Identifier(name) => name.to_owned(),
                }).ok_or_else(|| ParserError {
                    cause: "Expected an identifier".to_string(),
                    responsible: self.peek(),
                })?;

                if !self.match_adv(&assigner) {
                    return Err(ParserError {
                        cause: format!("Expected {:?}", assigner),
                        responsible: self.peek(),
                    });
                }

                let expr = self.expression()?;

                keyvals.push((name, expr));

                if self.match_adv(&separator) {
                    continue;
                } else if self.match_adv(&terminator) {
                    break;
                } else {
                    return Err(ParserError { cause: format!("Expected a {:?} or a {:?}", &separator, &terminator), responsible: self.peek() });
                }
            }
        }

        Ok(keyvals)
    }
}

#[cfg(test)]
mod tests {
}
