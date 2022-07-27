use crate::value::Value;

pub type Block = Vec<Statement>;
pub type VarName = String;

#[derive(Debug, Clone)]
pub enum Expression {
    Grouping(Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    List(Vec<Expression>),
    Pairs(Vec<(String, Expression)>),
    Literal(Value),
    Var(VarName),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Block, bool),
    Assign(Expression, Box<Statement>),
    FunDecl(Vec<String>, Box<Statement>),
    If(Box<Statement>, Box<Statement>, Option<Box<Statement>>),
    While(Box<Statement>, Box<Statement>),
    Expr(Expression),
    Return(Option<Box<Statement>>),
    // Break,
    // Continue
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    IsEqual,
    IsUnequal,
    IsLesser,
    IsGreater,
    IsLesserOrEqual,
    IsGreaterOrEqual,

    DoAnd,
    DoOr,

    Index,
    Call,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Negate,
    Inverse,
}