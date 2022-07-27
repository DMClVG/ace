use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Not, Rem, Sub},
    rc::Rc,
};

use fxhash::FxBuildHasher;

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
        Self {
            fields: iter.into_iter().collect::<_>(),
        }
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

impl From<i64> for Value {
    fn from(x: i64) -> Self {
        Value::Number(x as f64)
    }
}

impl From<i32> for Value {
    fn from(x: i32) -> Self {
        Value::Number(x as f64)
    }
}

impl From<usize> for Value {
    fn from(x: usize) -> Self {
        Value::Number(x as f64)
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

impl Into<bool> for Value {
    fn into(self) -> bool {
        self.is_true()
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

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Number(a), Number(b)) => Number(a + b),

            (String(a), String(b)) => String(Rc::new((*a).clone() + &(*b))),
            (String(a), b) => String(Rc::new((*a).clone() + &b.to_string())),
            (a, String(b)) => String(Rc::new(a.to_string() + &*b)),

            (List(a), List(b)) => {
                let mut new = (*a).borrow().clone();
                new.extend((*b).borrow().iter().cloned());
                List(Rc::new(RefCell::new(new)))
            }
            (_, _) => Nil,
        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Number(a), Number(b)) => Number(a - b),
            (_, _) => Nil,
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Number(a), Number(b)) => Number(a * b),
            (_, _) => Nil,
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Number(a), Number(b)) => Number(a / b),
            (_, _) => Nil,
        }
    }
}

impl Rem for Value {
    type Output = Value;

    fn rem(self, rhs: Self) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Number(a), Number(b)) => Number(a.rem_euclid(b)),
            (_, _) => Nil,
        }
    }
}

impl Not for Value {
    type Output = Value;

    fn not(self) -> Self::Output {
        Self::Bool(!Into::<bool>::into(self))
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(val) => Value::Number(-val),
            _ => Value::Nil,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Value::*;
        match (self, other) {
            (Number(a), Number(b)) => a.partial_cmp(b),
            (String(a), String(b)) => a.len().partial_cmp(&b.len()),
            (_, _) => None,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (&Number(a), &Number(b)) => a == b,
            (&Bool(a), &Bool(b)) => a == b,
            (String(a), String(b)) => *a == *b,
            (List(a), List(b)) if a.borrow().len() != (*b).borrow().len() => false,
            (List(a), List(b)) => (*a)
                .borrow()
                .iter()
                .zip((*b).borrow().iter())
                .all(|(a, b)| a == b),
            (Object(a), Object(b)) if (*a).borrow().fields.len() != (*b).borrow().fields.len() => {
                false
            }
            (Object(a), Object(b)) => {
                let a: _ = &(*a).borrow().fields;
                let b: _ = &(*b).borrow().fields;
                a.iter()
                    .all(|(ka, va)| b.get(ka).map_or(false, |vb| vb == va))
            }
            (Nil, Nil) => true,
            (_, _) => false,
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
                (*l).borrow()
                    .iter()
                    .map(|v| format!("{}", v))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Nil => write!(f, "nil"),
            Self::Object(obj) => write!(
                f,
                "{{ {} }}",
                (*obj)
                    .borrow()
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

impl Value {
    pub fn call(&self, args: Value) -> Value {
        use Value::*;
        match (self, args) {
            (Fun(body), args) => body.0(args),
            _ => Value::Nil,
        }
    }

    pub fn index(&self, i: impl Into<Value>) -> Value {
        use Value::*;
        match (self, i.into()) {
            (Object(obj), _) if obj.borrow().fields.len() == 0 => Nil,
            (Object(obj), String(s)) => {
                let obj = obj.borrow();
                obj.get(&s).clone()
            },
            (Object(obj), Number(i)) => {
                let obj = obj.borrow();
                let len = obj.fields.len();
                obj.fields.values().nth(i.to_index(len)).unwrap().clone()
            },
            (List(a), _) if a.borrow().len() == 0 => Value::Nil,
            (List(a), Number(i)) => {
                let a = (*a).borrow();
                a[i.to_index(a.len())].clone()
            }
            (String(a), Number(i)) => {
                Value::String(Rc::new(
                    a.chars().nth(i.to_index(a.len())).unwrap().to_string(),
                )) // TODO: Implement char type?
            }
            _ => Value::Nil,
        }
    }

    pub fn index_set(&mut self, i: Value, n: Value) {
        use Value::*;
        match (self, i) {
            (List(a), Number(i)) if a.borrow().len() != 0 => {
                let mut a = a.borrow_mut();
                let len = a.len();
                a[i.to_index(len)] = n;
            }
            (Object(obj), String(s)) => {
                let mut obj = obj.borrow_mut();
                obj.set((*s).to_owned(), n);
            }
            _ => {}
        }
    }
}

trait ToIndex {
    fn to_index(self, len: usize) -> usize;
}

impl ToIndex for f64 {
    fn to_index(self, len: usize) -> usize {
        (self.round() as isize).rem_euclid(len as isize) as usize
    }
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
