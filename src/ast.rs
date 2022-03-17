use std::{fmt::Display, rc::Rc, cell::RefCell, collections::HashMap};

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

    And,
    Or,

    Index,
    Call,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Negate,
    Inverse,
}

pub struct Fun(pub Box<dyn Fn(Value) -> Value>);

impl std::fmt::Debug for Fun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Fun()")
    }
}

#[derive(Debug, Clone, Default)]
pub struct Object {
    pub fields: HashMap<String, Value>,
}

impl Object {
    pub fn set(&mut self, name: String, val: Value) {
        if !val.is_nil() {
            self.fields.insert(name, val);
        } else {
            self.fields.remove(&name);
        }
    }

    pub fn get(&self, var: &str) -> Value {
        match self.fields.get(var) {
            Some(val) => val.clone(),
            None => Value::Nil,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(Rc<String>),
    Bool(bool),
    List(Rc<RefCell<Vec<Value>>>),
    Fun(Rc<Fun>),
    Object(Rc<RefCell<Object>>),
    Nil,
}

impl Value {
    pub fn new_list() -> Value {
        Value::List(Rc::new(RefCell::new(vec![])))
    }

    pub fn new_object() -> Value {
        Value::Object(Rc::new(RefCell::new(Object::default())))
    }

    pub fn is_nil(&self) -> bool {
        if let Self::Nil = self {
            true
        } else {
            false
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(val) => write!(f, "{}", val),
            Self::String(val) => write!(f, "{}", val),
            Self::Bool(val) => write!(f, "{}", val),
            Self::List(l) => write!(
                f,
                "[{}]",
                l.borrow().iter().map(|v| format!("{}", v)).collect::<Vec<String>>().join(", ")
            ),
            Self::Nil => write!(f, "nil"),
            Self::Object(obj) => write!(
                f,
                "{{ {} }}",
                obj.borrow()
                    .fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Fun(_) => write!(f, "Fun"),
        }
    }
}

pub type VarName = String;

pub type Block = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Leaf {
    Literal(Value),
    Var(VarName),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Grouping(Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    List(Vec<Expression>),
    Pairs(Vec<(String, Expression)>),
    Primary(Leaf),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Block, bool),
    Assign(Expression, Box<Statement>),
    FunDecl(Vec<String>, Box<Statement>),
    If(Box<Statement>, Box<Statement>, Option<Box<Statement>>),
    While(Box<Statement>, Box<Statement>),
    Expr(Expression),
}
