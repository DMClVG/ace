use std::{fmt::Display, rc::Rc, cell::RefCell, collections::HashMap};
use fxhash::{FxBuildHasher};

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

pub type FunDef = Box<dyn Fn(Value) -> Value>;

pub struct Fun(pub FunDef);


impl std::fmt::Debug for Fun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Fun()")
    }
}

#[derive(Debug, Clone, Default)]
pub struct Object {
    pub fields: HashMap<String, Value, FxBuildHasher>,
}

impl FromIterator<(String, Value)> for Object {
    fn from_iter<T: IntoIterator<Item = (String, Value)>>(iter: T) -> Self {
        Self { fields: iter.into_iter().collect::<_>() }
    }
}

impl Object {
    pub fn new() -> Self {
        Self::default()
    }

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

impl From<&str> for Value {
    fn from(x: &str) -> Self {
        Value::String(Rc::new(x.to_owned()))
    }
} 

impl From<f64> for Value {
    fn from(x: f64) -> Self {
        Value::Number(x)
    }
}

impl From<bool> for Value {
    fn from(x: bool) -> Self {
        Value::Bool(x)
    }
}

impl From<Object> for Value {
    fn from(o: Object) -> Self {
        Value::Object(Rc::new(RefCell::new(o)))
    }
}

impl From<Vec<Value>> for Value {
    fn from(l: Vec<Value>) -> Self {
        Value::List(Rc::new(RefCell::new(l)))
    }
}

impl From<FunDef> for Value {
    fn from(x: Box<dyn Fn(Value) -> Value>) -> Self {
        Value::Fun(Rc::new(Fun(x)))
    }
}

impl Value {
    pub fn is_nil(&self) -> bool {
        if let Self::Nil = self {
            true
        } else {
            false
        }
    }

    pub fn is_true(&self) -> bool {
        use Value::*;
        match self {
            Bool(false) => false,
            Number(a) if *a <= f64::EPSILON && *a >= -f64::EPSILON => false,
            String(a) if a.len() == 0 => false,
            List(a) if a.borrow().len() == 0 => false,
            Object(a) if a.borrow().fields.len() == 0 => false,
            Nil => false,
            _ => true,
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

#[macro_export]
macro_rules! obj {
    {$($a:ident: $b:expr),* $(,)?} => {
        {
            let mut object = self::Object::default();

            $(
                object.set(stringify!($a).to_owned(), ($b).into());
            )*

            object
        }
    };
}

#[macro_export]
macro_rules! list {
    [$($b:expr),* $(,)?] => {
        {
            let mut list = Vec::<Value>::default();

            $(
                list.push(($b).into());
            )*

            list
        }
    };
}