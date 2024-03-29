use std::{
    cell::RefCell,
    ops::{Deref, DerefMut},
    rc::Rc,
    time::SystemTime,
};

use crate::{ast::*, list, obj};

use crate::value::*;

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

pub struct ScopeWalker {
    current: Rc<RefCell<Scope>>
}


impl Scope {

    pub fn get(&self, name: &str) -> Value {
        let ret = self.inner.get(name);
        if !ret.is_nil() {
            return ret;
        }

        let mut scope = self.enclosing.clone();
        while let Some(s) = scope {
            let s = s.borrow();
            let value = s.inner.get(name);
            if !value.is_nil()  {
                return value;
            } else {
                scope = s.enclosing.clone();
            }
        }
        Value::Nil
    }

    pub fn assign(&mut self, name: &str, val: Value) -> bool {
        if let Some(field) = self.inner.fields.get_mut(name) {
            *field = val;
            return true;
        }
        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow_mut().assign(name, val);
        } else {
            return false;
        }
    }

    pub fn declare(&mut self, name: String, val: Value) {
        self.inner.set(name, val);
    }
}

pub fn new() -> Rc<RefCell<Scope>> {
    use Value::*;

    let print: FunDef = Box::new(move |args: Value| {
        println!("{}", args.index(&0.into()));
        Nil
    });

    let clock: FunDef = Box::new(|_| {
        SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64()
            .into()
    });

    let push: FunDef = Box::new(|args: Value| {
        if let List(list) = args.index(&0.into()) {
            list.borrow_mut().push(args.index(&1.into()));
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
        if let String(s) = args.index(&0.into()) {
            if let Ok(num) = s.parse::<f64>() {
                Number(num)
            } else {
                Nil
            }
        } else {
            Nil
        }
    });

    let random: FunDef = Box::new(|_| Number(rand::random()));

    let global: Scope = obj! {
        print: print,
        io: obj! {
            stdout: Value::Nil,
            stdin: Value::Nil,
        },
        std: obj! {
            parse: parse,
            random: random,
        },
        os: obj! {
            clock: clock,
        },
        array: obj! {
            push: push,
        },
    }
    .into();

    Rc::new(RefCell::new(global))
}

impl Statement {
    pub fn execute(&self, s: Rc<RefCell<Scope>>) -> Result<Value, Value> {
        match self {
            Statement::Expr(expr) => {
                return Ok(expr.evaluate(s));
            },
            Statement::Declare(id, stmt) => {
                let to = stmt.execute(s.clone())?;
                s.borrow_mut().declare(id.to_owned(), to);
            },
            Statement::Assign(dest, stmt) => {
                let to = stmt.execute(s.clone())?;
                dest.evaluate_deref_assign(s, to, None);
            },
            Statement::AddAssign(dest, stmt) => {
                let add = stmt.execute(s.clone())?;
                dest.evaluate_deref_assign(s, add, Some(|a, b| *a += b));
            },
            Statement::SubAssign(dest, stmt) => {
                let sub = stmt.execute(s.clone())?;
                dest.evaluate_deref_assign(s, sub, Some(|a, b| *a = a.to_owned() - b));
            },
            Statement::MulAssign(dest, stmt) => {
                let mul = stmt.execute(s.clone())?;
                dest.evaluate_deref_assign(s, mul, Some(|a, b| *a = a.to_owned() * b));
            },
            Statement::DivAssign(dest, stmt) => {
                let div = stmt.execute(s.clone())?;
                dest.evaluate_deref_assign(s, div, Some(|a, b| *a = a.to_owned() / b));
            },
            Statement::ModAssign(dest, stmt) => {
                let modulo = stmt.execute(s.clone())?;
                dest.evaluate_deref_assign(s, modulo, Some(|a, b| *a = a.to_owned() % b));
            },
            Statement::If(condition, block, else_block) => {
                let condition = condition.execute(s.clone())?;
                if condition.is_true() {
                    return block.execute(s);
                } else if let Some(e) = else_block {
                    return e.execute(s);
                }
            },
            Statement::For(ids, iterated, block) =>  {
                let iter = iterated.execute(s.clone())?;
                for value in iter {
                    match value {
                        Value::Nil => break,
                        Value::List(list) => {
                            for (id, v) in ids.iter().zip(list.borrow().iter()) {
                                s.borrow_mut().declare(id.to_owned(), v.clone())
                            }
                        },
                        _ => {
                            s.borrow_mut().declare(ids[0].to_owned(), value);
                        }
                    }
                    block.execute(s.clone())?;
                }
            },
            Statement::While(condition, block) => {
                while condition.execute(s.clone())?.is_true() {
                    // collect in a list??
                    block.execute(s.clone())?;
                }
            }
            Statement::Block(stmts, scoped) => {
                // have bool to tell if scoped or not
                let s = if *scoped {
                    Rc::new(RefCell::new(Scope {
                        enclosing: Some(s.clone()),
                        ..Default::default()
                    }))
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
                    let args = if let Value::List(list) = args {
                        list
                    } else {
                        unreachable!()
                    };
                    let args = args.borrow();

                    let s = Rc::new(RefCell::new(Scope {
                        enclosing: Some(s.clone()),
                        ..Default::default()
                    }));
                    {
                        let mut s = s.borrow_mut();
                        for (i, param) in params.iter().take(args.len()).enumerate() {
                            s.declare(param.to_owned(), args[i].clone());
                        }
                    }
                    match block.execute(s) {
                        Ok(v) => v,
                        Err(v) => v,
                    }
                }));

                return Ok(Value::Fun(Rc::new(fun)));
            }
            Statement::Return(val) => {
                return Err(val
                    .as_ref()
                    .map_or(Value::Nil, |stmt| stmt.execute(s).unwrap_or_else(|v| v)))
            },
            _ => { unimplemented!() }
        }
        return Ok(Value::Nil);
    }
}

impl Expression {
    pub fn evaluate(&self, s: Rc<RefCell<Scope>>) -> Value {
        match self {
            Self::Literal(val) => val.clone(),
            Self::Var(var) => Scope::get(&s.borrow(), var),
            Self::Unary(op, expr) => match op {
                UnaryOperator::Inverse => !expr.evaluate(s),
                UnaryOperator::Negate => -expr.evaluate(s),
            },
            Self::Binary(op, left, right) => {
                use BinaryOperator::*;
                use Value::Bool;
                let a = left.evaluate(s.clone());
                let b = right.evaluate(s);
                match op {
                    IsGreater => Bool(a > b),
                    IsGreaterOrEqual => Bool(a >= b),
                    IsLesser => Bool(a < b),
                    IsLesserOrEqual => Bool(a <= b),
                    IsEqual => Bool(a == b),
                    IsUnequal => Bool(a != b),
                    Add => a + b,
                    Subtract => a - b,
                    Multiply => a * b,
                    Divide => a / b,
                    Modulo => a % b,
                    DoAnd => Value::Bool(a.into() && b.into()),
                    DoOr => Value::Bool(a.into() || b.into()),
                    Index => a.index(&b),
                    Call => a.call(b),
                }
            }
            Self::List(exprs) => exprs
                .iter()
                .map(|expr| expr.evaluate(s.clone()))
                .collect::<Vec<Value>>()
                .into(),
            Self::Pairs(pairs) => pairs
                .iter()
                .map(|(k, v)| (k.to_owned(), v.evaluate(s.clone())))
                .filter(|(_, v)| !v.is_nil())
                .collect::<Object>()
                .into(),
            Self::Grouping(expr) => expr.evaluate(s),
        }
    }

    pub fn evaluate_deref_assign(&self, s: Rc<RefCell<Scope>>, n: Value, procedure: Option<fn(a: &mut Value, b: Value)>) {
        match self {
            Self::Var(var) => {
                let mut s = s.borrow_mut();
                if let Some(procedure) = procedure {
                    let mut a = s.get(var);
                    let b = n;
                    procedure(&mut a, b);
                    s.assign(var.as_str(), a);
                } else {
                    s.assign(var.as_str(), n);
                }
                return;
            }
            Self::Binary(op, left, right) => {
                use BinaryOperator::*;
                match op {
                    Index => {
                        let mut p = left.evaluate(s.clone());
                        let i = right.evaluate(s);
                        if let Some(procedure) = procedure {
                            let mut a = p.index(&i);
                            let b = n;
                            procedure(&mut a, b);
                            p.index_set(i, a)
                        } else {
                            p.index_set(i, n)
                        }
                    },
                    _ => {}
                }
                return;
            }
            _ => {}
        }
        self.evaluate(s); // if isn't dereferencable, just evaluate
    }
    
}
