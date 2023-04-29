#![allow(dead_code)]

#[derive(Debug)]
struct Program {
    funcs: Vec<FuncDecl>,
}

#[derive(Debug)]
struct FuncDecl {
    name: String,
    args: Vec<Arg>,
    body: Vec<Stmt>,
    returns: Option<Type>,
}

#[derive(Debug)]
enum Type {
    Integer,
    String,
}

#[derive(Debug)]
struct Arg {
    name: String,
    kind: Type,
}

#[derive(Debug)]
enum Stmt {
    IfStmt(IfStmt),
    ReturnStmt(ReturnStmt),
    WhileLoop(WhileLoop),
}

#[derive(Debug)]
enum Operation {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
struct Expr {
    value: ExprValue,
    left: Option<Box<Expr>>,
    right: Option<Box<Expr>>,
}

#[derive(Debug)]
enum ExprValue {
    Integer(i64),
    String(String),
    Operation(Operation),
}

#[derive(Debug)]
struct IfStmt {
    condition: Expr,
    if_true: Vec<Stmt>,
    if_false: Option<Vec<Stmt>>,
}

#[derive(Debug)]
struct ReturnStmt {
    returning: Expr,
}

#[derive(Debug)]
struct WhileLoop {
    condition: Expr,
    body: Vec<Stmt>,
}

macro_rules! return_stmt {
    ( $e:expr ) => {
        ReturnStmt { returning: $e }
    };
}

macro_rules! expr {
    ( $v:expr, $a:expr, $b:expr ) => {
        Expr {
            value: $v,
            left: Some(Box::new(expr!($a))),
            right: Some(Box::new(expr!($b))),
        }
    };
    ( $v:expr ) => {
        Expr {
            value: $v,
            left: None,
            right: None,
        }
    };
}

macro_rules! expr_value_int {
    ( $x:expr ) => {
        ExprValue::Integer($x)
    };
}

macro_rules! expr_value_op {
    ( $o:expr ) => {
        ExprValue::Operation($o)
    };
}

fn main() {
    println!(
        "{:#?}",
        return_stmt!(expr!(
            expr_value_op!(Operation::Add),
            expr_value_int!(7),
            expr_value_int!(14)
        ))
    );
}
