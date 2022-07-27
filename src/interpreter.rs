use std::{cell::RefCell, rc::Rc, time::SystemTime, ops::{Deref, DerefMut}};

use crate::{ast::*, obj, list};

#[derive(Default)]
pub struct Scope {
    enclosing: Option<Rc<RefCell<Scope>>>,
    inner: Object,
}

impl From<Object> for Scope {
    fn from(o: Object) -> Self {
        Self {
            enclosing: None,
            inner: o,
        }
    }
}

impl Scope {
    pub fn get(&self, name: &str) -> Value {
        match Object::get(self, name) {
            Value::Nil => {
                if let Some(ref enclosing) = self.enclosing {
                    Self::get(&enclosing.borrow(), name)
                } else {
                    Value::Nil
                }
            },
            v => v,
        }
    }
}

impl Deref for Scope {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Scope {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}


pub fn new() -> Rc<RefCell<Scope>> {
    use Value::*;

    let print: FunDef = Box::new(move |args: Value| {
        println!("{}", args.index(Number(0.0)));
        Nil
    });

    let clock: FunDef = Box::new(|_| {
        Number(
            SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_secs_f64(),
        )
    });

    let push: FunDef = Box::new(|args: Value| {
        if let List(list) = args.index(Number(0.0)) {
            list.borrow_mut().push(args.index(Number(1.0)));
        }
        Nil
    });

    let input: FunDef = Box::new(|_| {
        let mut res = std::string::String::new();
        if let Ok(_) = std::io::stdin().read_line(&mut res) {
            res.trim_end().into()
        } else {
            Nil
        }
    });

    let parse: FunDef = Box::new(|args: Value| {
        if let String(s) = args.index(Number(0.0)) {
            if let Ok(num) = s.parse::<f64>() {
                Number(num)
            } else {
                Nil
            }
        } else {
            Nil
        }
    });

    let random: FunDef = Box::new(|_| {
        Number(rand::random())
    });

    let global: Scope = obj! {
        io: obj! {
            print: print,
            input: input,
        },
        std: obj! {
            parse: parse,
            random: random,
        },
        os: obj! {
            clock: clock,
        },
        table: obj! {
            push: push,
        },
        cat: obj! {
            hey: 12.0,
            yo: "baby",
            house: list![0.0, "cat", obj! { lol: "bob" }],
        },
    }.into();

    Rc::new(RefCell::new(global))
}

impl Statement {
    pub fn execute(&self, s: Rc<RefCell<Scope>>) -> Result<Value, Value> {
        match self {
            Statement::Expr(expr) => {
                return Ok(expr.evaluate(&s.borrow()));
            }
            Statement::Assign(dest, stmt) => {
                let to = stmt.execute(s.clone())?;
                dest.evaluate_deref_assign(&mut s.borrow_mut(), to);
            }
            Statement::If(condition, block, else_block) => {
                let condition = condition.execute(s.clone())?;
                if condition.is_true() {
                    return block.execute(s);
                } else if let Some(e) = else_block {
                    return e.execute(s);
                }
            }
            Statement::While(condition, block) => {
                while condition.execute(s.clone())?.is_true() {
                    // collect in a list??
                    block.execute(s.clone())?;
                }
            }
            Statement::Block(stmts, scoped) => { // have bool to tell if scoped or not
                let s = if *scoped {
                    Rc::new(RefCell::new(Scope { enclosing: Some(s.clone()), ..Default::default()}))
                } else {
                    s
                };
                for (i, stmt) in stmts.iter().enumerate() {
                    if i == stmts.len() - 1 {
                        return stmt.execute(s.clone());
                    }
                    stmt.execute(s.clone())?;
                }
            }
            Statement::FunDecl(params, block) => {
                let block = block.to_owned();
                let params = params.to_owned();
                
                let fun = Fun(Box::new(move |args| {
                    let args  = if let Value::List(list) = args { list } else { unreachable!() };
                    let args = args.borrow();
                    
                    let s = Rc::new(RefCell::new(Scope { enclosing: Some(s.clone()), ..Default::default()}));
                    {
                        let mut s = s.borrow_mut();
                        for (i, param) in params.iter().take(args.len()).enumerate() {
                            s.set(param.to_owned(), args[i].clone());
                        }
                    }
                    match block.execute(s) {
                        Ok(v) => v,
                        Err(v) => v
                    }
                }));

                return Ok(Value::Fun(Rc::new(fun)));
            }
            Statement::Return(val) => {
                return Err(val.as_ref().map_or(Value::Nil, |stmt| stmt.execute(s).unwrap_or_else(|v| v)))
            }
            _ => { unreachable!() }
        }
        return Ok(Value::Nil)
    }
}

impl Expression {
    pub fn evaluate(&self, s: &Scope) -> Value {
        match self {
            Self::Literal(val) => val.clone(),
            Self::Var(var) => Scope::get(s, var),
            Self::Unary(op, expr) => match op {
                UnaryOperator::Inverse => expr.evaluate(s).opposite(),
                UnaryOperator::Negate => expr.evaluate(s).negate(),
            },
            Self::Binary(op, left, right) => {
                use BinaryOperator::*;
                let a = left.evaluate(s);
                let b = right.evaluate(s);
                match op {
                    IsGreater => a.grt(b),
                    IsGreaterOrEqual => a.grt_or_eq(b),
                    IsLesser => a.lsr(b),
                    IsLesserOrEqual => a.lsr_or_eq(b),
                    IsEqual => a.eq(b),
                    IsUnequal => a.neq(b),
                    Add => a.add(b),
                    Subtract => a.sub(b),
                    Multiply => a.mult(b),
                    Divide => a.div(b),
                    Modulo => a.rem(b),
                    DoAnd => Value::Bool(a.is_true() && b.is_true()),
                    DoOr => Value::Bool(a.is_true() || b.is_true()),
                    Index => a.index(b),
                    Call => a.call(b),
                }
            }
            Self::List(exprs) => exprs
                    .iter()
                    .map(|expr| expr.evaluate(s))
                    .collect::<Vec<Value>>().into(),
            Self::Pairs(pairs) => pairs
                    .iter()
                    .map(|(k, v)| (k.to_owned(), v.evaluate(s)))
                    .filter(|(_, v)| !v.is_nil())
                    .collect::<Object>().into(),
            Self::Grouping(expr) => expr.evaluate(s),
        }
    }

    pub fn evaluate_deref_assign(&self, s: &mut Scope, n: Value) {
        match self {
            Self::Var(var) => {
                s.set(var.to_owned(), n);
                return;
            },
            Self::Binary(op, left, right) => {
                use BinaryOperator::*;
                let mut a = left.evaluate(s);
                let b = right.evaluate(s);
                match op {
                    Index => a.index_set(b, n),
                    _ => {}
                }
                return;
            }
            _ => {}
        }
        self.evaluate(s); // if isn't dereferencable, just evaluate
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

    pub fn index(&self, i: Value) -> Value {
        use Value::*;
        match (self, i) {
            (Object(obj), String(s)) => {
                let obj = obj.borrow();
                obj.get(&s).clone()
            }
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

    pub fn opposite(self) -> Value {
        Value::Bool(!self.is_true())
    }

    pub fn negate(self) -> Value {
        match self {
            Value::Number(val) => Value::Number(-val),
            _ => Value::Nil,
        }
    }

    pub fn add(self, r: Value) -> Value {
        use Value::*;
        match (self, r) {
            (Number(a), Number(b)) => Number(a + b),

            (String(a), String(b)) => String(Rc::new((*a).clone() + &(*b))),
            (String(a), b) => String(Rc::new((*a).clone() + &b.to_string())),
            (a, String(b)) => String(Rc::new(a.to_string() + &*b)),

            (List(a), List(b)) => {
                let mut new = (*a).borrow().clone();
                new.extend(b.borrow().clone());
                List(Rc::new(RefCell::new(new)))
            }
            (_, _) => Nil,
        }
    }

    pub fn sub(self, r: Value) -> Value {
        use Value::*;
        match (self, r) {
            (Number(a), Number(b)) => Number(a - b),
            (_, _) => Nil,
        }
    }

    pub fn mult(self, r: Value) -> Value {
        use Value::*;
        match (self, r) {
            (Number(a), Number(b)) => Number(a * b),
            (_, _) => Nil,
        }
    }

    pub fn div(self, r: Value) -> Value {
        use Value::*;
        match (self, r) {
            (Number(a), Number(b)) => Number(a / b),
            (_, _) => Nil,
        }
    }

    pub fn rem(self, r: Value) -> Value {
        use Value::*;
        match (self, r) {
            (Number(a), Number(b)) => Number(a.rem_euclid(b)),
            (_, _) => Nil,
        }
    }

    pub fn eq(self, r: Value) -> Value {
        use Value::*;
        match (self, r) {
            (Number(a), Number(b)) => Bool(a == b),
            (Bool(a), Bool(b)) => Bool(a == b),
            (String(a), String(b)) => Bool(a == b),
            (List(a), List(b)) if a.borrow().len() != b.borrow().len() => Bool(false),
            (List(a), List(b)) => Bool(a.borrow().iter().zip(b.borrow().iter()).all(|(a, b)| {
                if let Value::Bool(true) = a.clone().eq(b.clone()) {
                    true
                } else {
                    false
                }
            })),
            (Object(a), Object(b)) if a.borrow().fields.len() != b.borrow().fields.len() => {
                Bool(false)
            }
            (Object(a), Object(b)) => {
                let a = &a.borrow().fields;
                let b = &b.borrow().fields;
                Bool(a.iter().all(|(ka, va)| {
                    b.get(ka).map_or(false, |vb| {
                        if let Bool(true) = vb.clone().eq(va.clone()) {
                            true
                        } else {
                            false
                        }
                    })
                }))
            }
            (Nil, Nil) => Bool(true),
            (_, _) => Bool(false),
        }
    }

    pub fn neq(self, r: Value) -> Value {
        self.eq(r).opposite()
    }

    pub fn grt(self, r: Value) -> Value {
        use Value::*;
        match (self, r) {
            (Number(a), Number(b)) => Bool(a > b),
            (String(a), String(b)) => Bool(a > b),
            (_, _) => Bool(false),
        }
    }

    pub fn lsr(self, r: Value) -> Value {
        use Value::*;
        match (self, r) {
            (Number(a), Number(b)) => Bool(a < b),
            (String(a), String(b)) => Bool(a < b),
            (_, _) => Bool(false),
        }
    }

    pub fn grt_or_eq(self, r: Value) -> Value {
        use Value::*;
        match (self, r) {
            (Number(a), Number(b)) => Bool(a >= b),
            (String(a), String(b)) => Bool(a >= b),
            (_, _) => Bool(false),
        }
    }

    pub fn lsr_or_eq(self, r: Value) -> Value {
        use Value::*;
        match (self, r) {
            (Number(a), Number(b)) => Bool(a <= b),
            (String(a), String(b)) => Bool(a <= b),
            (_, _) => Bool(false),
        }
    }

    pub fn to_string(self) -> String {
        format!("{}", self)
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