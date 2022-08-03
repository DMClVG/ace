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

impl Scope {
    pub fn get(&self, name: &str) -> Value {
        match Object::get(self, name) {
            Value::Nil => {
                if let Some(ref enclosing) = self.enclosing {
                    Self::get(&enclosing.borrow(), name)
                } else {
                    Value::Nil
                }
            }
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
    }
    .into();

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
                dest.evaluate_deref_assign(&mut s.borrow_mut(), to, None);
            },
            Statement::AddAssign(dest, stmt) => {
                let add = stmt.execute(s.clone())?;
                dest.evaluate_deref_assign(&mut s.borrow_mut(), add, Some(|a, b| a + b));
            },
            Statement::SubAssign(dest, stmt) => {
                let sub = stmt.execute(s.clone())?;
                dest.evaluate_deref_assign(&mut s.borrow_mut(), sub, Some(|a, b| a - b));
            },
            Statement::MulAssign(dest, stmt) => {
                let mul = stmt.execute(s.clone())?;
                dest.evaluate_deref_assign(&mut s.borrow_mut(), mul, Some(|a, b| a * b));
            },
            Statement::DivAssign(dest, stmt) => {
                let div = stmt.execute(s.clone())?;
                dest.evaluate_deref_assign(&mut s.borrow_mut(), div, Some(|a, b| a / b));
            },
            Statement::ModAssign(dest, stmt) => {
                let modulo = stmt.execute(s.clone())?;
                dest.evaluate_deref_assign(&mut s.borrow_mut(), modulo, Some(|a, b| a % b));
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
                                s.borrow_mut().set(id.to_owned(), v.clone())
                            }
                        },
                        _ => {
                            s.borrow_mut().set(ids[0].to_owned(), value);
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
                            s.set(param.to_owned(), args[i].clone());
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
    pub fn evaluate(&self, s: &Scope) -> Value {
        match self {
            Self::Literal(val) => val.clone(),
            Self::Var(var) => Scope::get(s, var),
            Self::Unary(op, expr) => match op {
                UnaryOperator::Inverse => !expr.evaluate(s),
                UnaryOperator::Negate => -expr.evaluate(s),
            },
            Self::Binary(op, left, right) => {
                use BinaryOperator::*;
                use Value::Bool;
                let a = left.evaluate(s);
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
                .map(|expr| expr.evaluate(s))
                .collect::<Vec<Value>>()
                .into(),
            Self::Pairs(pairs) => pairs
                .iter()
                .map(|(k, v)| (k.to_owned(), v.evaluate(s)))
                .filter(|(_, v)| !v.is_nil())
                .collect::<Object>()
                .into(),
            Self::Grouping(expr) => expr.evaluate(s),
        }
    }

    pub fn evaluate_deref_assign(&self, s: &mut Scope, n: Value, procedure: Option<fn(a: Value, b: Value) -> Value>) {
        match self {
            Self::Var(var) => {
                if let Some(procedure) = procedure {
                    let a = s.get(var);
                    let b = n;
                    s.set(var.to_owned(), procedure(a, b))
                } else {
                    s.set(var.to_owned(), n);
                }
                return;
            }
            Self::Binary(op, left, right) => {
                use BinaryOperator::*;
                match op {
                    Index => {
                        let mut p = left.evaluate(s);
                        let i = right.evaluate(s);
                        if let Some(procedure) = procedure {
                            let a = p.index(&i);
                            let b = n;
                            p.index_set(i, procedure(a, b))
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
