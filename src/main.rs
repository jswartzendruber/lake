#![allow(dead_code)]

use std::{collections::HashMap, fs::File, io::Write, process::Command};

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

type Label = u32;
type Register = u32;

#[derive(Debug)]
enum IRInstr {
    Label(Label),
    IR2(IR2),
    IRJ(IRJ),
    Ret,
}

#[derive(Debug, Copy, Clone)]
enum IROp2 {
    Add,
    Move,
    Compare,
}

#[derive(Debug)]
enum IR2Source {
    Reg(Register),
    Imm(i64),
}

#[derive(Debug)]
struct IR2 {
    opcode: IROp2,
    source: IR2Source,
    dest: Register,
}

#[derive(Debug)]
enum IRJOp {
    Jump,
    JumpNotEq,
}

#[derive(Debug)]
struct IRJ {
    opcode: IRJOp,
    label: Label,
}

#[derive(Debug)]
struct IRGen {
    label_counter: u32,
    reg_counter: u32,
    ir: Vec<IRInstr>,
}

impl IRGen {
    fn new() -> Self {
        IRGen {
            label_counter: 0,
            reg_counter: 0,
            ir: vec![],
        }
    }

    fn new_reg(&mut self) -> u32 {
        self.reg_counter += 1;
        self.reg_counter - 1
    }

    fn new_label(&mut self) -> u32 {
        self.label_counter += 1;
        self.label_counter - 1
    }

    fn gen_expr(&mut self, expr: &Expr) -> Register {
        let dest = self.new_reg();

        match &expr.value {
            ExprValue::Integer(i) => {
                self.ir.push(IRInstr::IR2(IR2 {
                    opcode: IROp2::Move,
                    source: IR2Source::Imm(*i),
                    dest,
                }));

                dest
            }
            ExprValue::Operation(o) => {
                let op1 = self.gen_expr(expr.left.as_ref().unwrap());
                let op2 = self.gen_expr(expr.right.as_ref().unwrap());

                match o {
                    Operation::Add => {
                        self.ir.push(IRInstr::IR2(IR2 {
                            opcode: IROp2::Add,
                            source: IR2Source::Reg(op1),
                            dest,
                        }));
                        self.ir.push(IRInstr::IR2(IR2 {
                            opcode: IROp2::Add,
                            source: IR2Source::Reg(op2),
                            dest,
                        }));
                    }
                    Operation::Eq => {
                        self.ir.push(IRInstr::IR2(IR2 {
                            opcode: IROp2::Compare,
                            source: IR2Source::Reg(op1),
                            dest: op2,
                        }));
                    }
                    _ => unimplemented!(),
                };

                dest
            }
            _ => unimplemented!(),
        }
    }

    fn gen_if_stmt(&mut self, if_stmt: &IfStmt) {
        self.gen_expr(&if_stmt.condition);

        let jump_if_false_label = self.new_label();
        self.ir.push(IRInstr::IRJ(IRJ {
            opcode: IRJOp::JumpNotEq,
            label: jump_if_false_label,
        }));

        for stmt in &if_stmt.if_true {
            self.gen_stmt(stmt);
        }
        let jump_if_true_label = self.new_label();
        self.ir.push(IRInstr::IRJ(IRJ {
            opcode: IRJOp::Jump,
            label: jump_if_true_label,
        }));
        self.ir.push(IRInstr::Label(jump_if_false_label));

        if let Some(if_false) = &if_stmt.if_false {
            for stmt in if_false {
                self.gen_stmt(stmt);
            }
            self.ir.push(IRInstr::Label(jump_if_true_label));
        }
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::ReturnStmt(return_stmt) => {
                self.gen_expr(&return_stmt.returning);
                self.ir.push(IRInstr::Ret);
            }
            Stmt::IfStmt(if_stmt) => self.gen_if_stmt(if_stmt),
            _ => unimplemented!(),
        }
    }

    fn lower(mut self, if_stmt: IfStmt) -> IRGen {
        self.gen_stmt(&Stmt::IfStmt(if_stmt));

        self
    }
}

// End IR

// Compile

struct Compiler {
    instrs: Vec<String>,
    needs_patching: HashMap<usize, u32>,
}

impl Compiler {
    fn new() -> Self {
        Self {
            instrs: vec![],
            needs_patching: HashMap::new(),
        }
    }

    fn compile(mut self, ir: &Vec<IRInstr>) -> Self {
        self.instrs.push(format!("\t.global _start"));
        self.instrs.push(format!("\t.text"));
        self.instrs.push(format!("_start:"));

        for instr in ir {
            self.emit_instr(instr);
        }

        self.instrs.push(format!("\tmov $0, %rdi")); // exit code
        self.instrs.push(format!("\tmov $60, %rax")); // sys_exit
        self.instrs.push(format!("\tsyscall"));

        self
    }

    // Need some register allcation
    fn emit_instr(&mut self, instr: &IRInstr) {
        match instr {
            IRInstr::IR2(instr) => {
                let op = match instr.opcode {
                    IROp2::Add => "add",
                    IROp2::Move => "mov",
                    IROp2::Compare => "cmp",
                };

                let src = match instr.source {
                    IR2Source::Reg(r) => format!("%{}", r),
                    IR2Source::Imm(i) => format!("${}", i),
                };

                let dest = format!("%{}", instr.dest);

                self.instrs.push(format!("\t{} {}, {}", op, src, dest));
            }
            IRInstr::IRJ(instr) => match instr.opcode {
                IRJOp::Jump => {
                    self.instrs.push(format!("\tjmp .L_{}", instr.label));
                }
                IRJOp::JumpNotEq => {
                    self.instrs.push(format!("\tjne .L_{}", instr.label));
                }
            },
            IRInstr::Label(label) => self.instrs.push(format!(".L_{}:", label)),
            IRInstr::Ret => self.instrs.push(format!("\tret")),
        }
    }

    fn run(&self) {
        let mut file = File::create("out.s").unwrap();
        for s in &self.instrs {
            file.write_all(s.as_bytes()).unwrap();
            file.write_all(b"\n").unwrap();
        }

        let assembler = Command::new("as")
            .arg("out.s")
            .arg("-o")
            .arg("out.o")
            .output()
            .expect("Error assembling");
        if assembler.status.success() {
            print!("{}", String::from_utf8(assembler.stdout).unwrap());
        } else {
            print!("{}", String::from_utf8(assembler.stderr).unwrap());
        }

        let linker = Command::new("ld")
            .arg("-m")
            .arg("elf_x86_64")
            .arg("out.o")
            .arg("-o")
            .arg("out")
            .output()
            .expect("Error linking");
        if linker.status.success() {
            print!("{}", String::from_utf8(linker.stdout).unwrap());
        } else {
            print!("{}", String::from_utf8(linker.stderr).unwrap());
        }

        let result = Command::new("./out").output().expect("Error running code");
        print!("{}", String::from_utf8(result.stdout).unwrap());
        println!("Exit code: {}", result.status.code().unwrap());
    }
}

// End Compile

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
    for instr in &ir.ir {
        println!("{:?}", instr);
    }
    println!();
    let compiler = Compiler::new().compile(&ir.ir);
    for instr in &compiler.instrs {
        println!("{}", instr);
    }
    println!();
    compiler.run();
}
