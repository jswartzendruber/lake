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
    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    Neq,
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

macro_rules! if_stmt {
    ($condition:expr, { $( $elem:expr; )* }) => {{
        let if_true = vec![ $( Stmt::ReturnStmt($elem), ), * ];
        IfStmt {
            condition: $condition,
            if_true,
            if_false: None,
        }
    }};
	($condition:expr, { $( $elem1:expr; )* }, else { $( $elem2:expr; )* }) => {{
        let if_true = vec![ $( Stmt::ReturnStmt($elem1), ), * ];
		let if_false = vec![ $( Stmt::ReturnStmt($elem2), ), * ];
        IfStmt {
            condition: $condition,
            if_true,
            if_false: Some(if_false),
        }
    }};
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

// IR

type Register = u32;

#[derive(Debug)]
enum IRROp3 {
    Add,
    Compare,
}

#[derive(Debug)]
enum IRInstr {
    IRR3(IRR3),
    IRI2(IRI2),
    IRJ2(IRJ2),
    IRJ(IRJ),
}

#[derive(Debug)]
struct IRR3 {
    opcode: IRROp3,
    dest: Register,
    op1: Register,
    op2: Register,
}

#[derive(Debug)]
enum IRIOp2 {
    Load,
    Return,
}

#[derive(Debug)]
struct IRI2 {
    opcode: IRIOp2,
    dest: Register,
    immediate: i64,
}

#[derive(Debug)]
enum IRJOp {
    Jump,
    JumpNotEq,
}

#[derive(Debug)]
struct IRJ {
    opcode: IRJOp,
    skip_instrs: u32,
}

#[derive(Debug)]
struct IRJ2 {
    opcode: IRJOp,
    skip_instrs: u32,
    op1: Register,
    op2: Register,
}

#[derive(Debug)]
struct IRGen {
    reg_counter: u32,
    ir: Vec<IRInstr>,
}

impl IRGen {
    fn new() -> Self {
        IRGen {
            reg_counter: 0,
            ir: vec![],
        }
    }

    fn new_reg(&mut self) -> u32 {
        self.reg_counter += 1;
        self.reg_counter - 1
    }

    fn gen_expr(&mut self, expr: &Expr) -> Register {
        let dest = self.new_reg();

        match &expr.value {
            ExprValue::Integer(i) => {
                self.ir.push(IRInstr::IRI2(IRI2 {
                    opcode: IRIOp2::Load,
                    dest,
                    immediate: *i,
                }));

                dest
            }
            ExprValue::Operation(o) => {
                let op1 = self.gen_expr(expr.left.as_ref().unwrap());
                let op2 = self.gen_expr(expr.right.as_ref().unwrap());

                let opcode = match o {
                    Operation::Add => IRROp3::Add,
                    Operation::Eq => IRROp3::Compare,
                    _ => unimplemented!(),
                };

                self.ir.push(IRInstr::IRR3(IRR3 {
                    opcode,
                    dest,
                    op1,
                    op2,
                }));

                dest
            }
            _ => unimplemented!(),
        }
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::ReturnStmt(return_stmt) => {
                let expr = self.gen_expr(&return_stmt.returning);
                self.ir.push(IRInstr::IRI2(IRI2 {
                    opcode: IRIOp2::Return,
                    dest: expr,
                    immediate: 69,
                }));
            }
            _ => unimplemented!(),
        }
    }

    fn lower(mut self, if_stmt: IfStmt) -> IRGen {
        let condition = self.gen_expr(&if_stmt.condition);

        let zero = self.new_reg();
        self.ir.push(IRInstr::IRI2(IRI2 {
            opcode: IRIOp2::Load,
            dest: zero,
            immediate: 0,
        }));
        self.ir.push(IRInstr::IRJ2(IRJ2 {
            opcode: IRJOp::JumpNotEq,
            skip_instrs: 0,
            op1: condition,
            op2: zero,
        }));
        let jump_if_false_loc = self.ir.len() - 1;

        for stmt in &if_stmt.if_true {
            self.gen_stmt(stmt);
        }
        self.ir.push(IRInstr::IRJ(IRJ {
            opcode: IRJOp::Jump,
            skip_instrs: 0,
        }));
        let jump_if_true_loc = self.ir.len() - 1;

        let if_false_skip_count = self.ir.len() - jump_if_false_loc - 1;
        let jump_if_false = self.ir.get_mut(jump_if_false_loc);
        match jump_if_false.unwrap() {
            IRInstr::IRJ2(instr) => {
                instr.skip_instrs = if_false_skip_count as u32;
            }
            _ => unreachable!(),
        }

        if let Some(if_false) = if_stmt.if_false {
            for stmt in &if_false {
                self.gen_stmt(stmt);
            }
        }

        let if_true_skip_count = self.ir.len() - jump_if_true_loc - 1;
        if if_true_skip_count > 0 {
            let jump_if_true = self.ir.get_mut(jump_if_true_loc);
            match jump_if_true.unwrap() {
                IRInstr::IRJ(instr) => {
                    instr.skip_instrs = if_true_skip_count as u32;
                }
                _ => unreachable!(),
            }
        }

        self
    }
}

// End IR

fn main() {
    let prog = if_stmt!(
        expr!(
            expr_value_op!(Operation::Eq),
            expr_value_int!(7),
            expr_value_int!(7)
        ),
        {
            return_stmt!(expr!(expr_value_int!(1)));
        }, else {
            return_stmt!(expr!(expr_value_int!(0)));
        }
    );

    // println!("{:#?}", prog);

    let ir = IRGen::new().lower(prog);
    for instr in ir.ir {
        println!("{:?}", instr);
    }
}
