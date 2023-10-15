use crate::{
    lexer::Comparator,
    parser::{Command, Expr, AST},
};

pub fn compile(ast: &AST<'_>, no_comments: bool) -> String {
    let mut compiler = Compiler {
        registers: vec![],
        output: vec![],
        real_out: 0,
    };

    compiler.compile_sub_code(&ast.root, None);
    compiler.push_output("END".into());
    let mut out = String::from("");

    for str in compiler.output {
        if no_comments && str.starts_with("//") {
            continue;
        }
        out.push_str(&str);
    }

    out
}

struct Compiler<'a> {
    registers: Vec<&'a str>,
    output: Vec<String>,
    real_out: usize,
}
impl<'a> Compiler<'a> {
    fn reg_alloc(&mut self, ident: &'a str) -> usize {
        if self.registers.len() == 128 {
            panic!("All registers are occupied. Fix you code.")
        }
        self.push_output(format!("//Allocating {ident} as R{}", self.registers.len()));
        self.registers.push(ident);
        self.registers.len() - 1
    }

    fn find_reg(&self, ident: &'a str) -> usize {
        self.registers.len()
            - self
                .registers
                .iter()
                .rev()
                .position(|r| *r == ident)
                .unwrap()
            - 1
    }

    fn reg_dealloc_until(&mut self, index: usize) {
        while self.registers.len() != index {
            println!("--Deallocated: {}", self.registers.pop().unwrap());
        }
    }
    fn push_output(&mut self, mut line: String) -> usize {
        println!("{line}");
        if !line.starts_with("//") {
            self.real_out += 1;
        }
        line.push('\n');
        self.output.push(line);
        if self.real_out != 0 {
            self.real_out
        } else {
            usize::MAX
        }
    }

    fn compile_expr(&mut self, expr: &Expr<'a>, into: usize) {
        let presize = self.registers.len();
        match expr {
            Expr::Cmp(left, right, cmp) => self.complile_cmp(left, right, *cmp, into),
            Expr::Add(left, right) => self.compile_add(left, right, into),
            Expr::Sub(left, right) => self.compile_sub(left, right, into),
            Expr::Mult(left, right) => self.compile_mult(left, right, into),
            Expr::Ident(_) => panic!("This should not be implemented lol"),
            Expr::Raw(number) => {
                self.push_output(format!("SET R{into} {number}"));
            }
            Expr::ArrIndex(ident, index_expr) => self.compile_arr_index(ident, index_expr, into),
            Expr::Or(left, right) => self.compile_or(left, right, into),
            Expr::And(_, _) => todo!(),
        }
        self.reg_dealloc_until(presize);
    }

    fn complile_cmp(&mut self, left: &Expr<'a>, right: &Expr<'a>, cmp: Comparator, into: usize) {
        let left = if let Expr::Ident(ident) = left {
            self.find_reg(ident)
        } else {
            let index = self.reg_alloc("CMP_LEFT");
            self.compile_expr(left, index);
            index
        };

        let right = if let Expr::Ident(ident) = right {
            self.find_reg(ident)
        } else {
            let index = self.reg_alloc("CMP_RIGHT");
            self.compile_expr(right, index);
            index
        };

        match cmp {
            Comparator::Eq => {
                self.push_output(format!("SUB R{left} R{right} R{into}"));
            }
            Comparator::Gt => todo!(),
            Comparator::Lt => todo!(),
        };
    }

    fn compile_arr_index(&mut self, ident: &'a str, index_expr: &Expr<'a>, into: usize) {
        self.push_output(format!("//Calculating mem address of {ident} + offset"));
        let index_reg = if let Expr::Ident(index_ident) = index_expr {
            self.find_reg(index_ident)
        } else {
            let index_reg = self.reg_alloc("INDEX");
            self.compile_expr(index_expr, index_reg);
            index_reg
        };
        let addr_reg = self.reg_alloc("FINAL_ADDR");
        self.push_output(format!(
            "ADD R{} R{index_reg} R{addr_reg}",
            self.find_reg(ident)
        ));

        self.push_output(format!("FETCH R{addr_reg} R{into}"));
    }

    fn compile_sub_code(&mut self, code: &[Command<'a>], prev_loop: Option<usize>) {
        let presize = self.registers.len();
        for cmd in code {
            match cmd {
                Command::RawExpr(_) => todo!(),
                Command::If(expr, sub, else_sub) => self.compile_if(expr, sub, else_sub, prev_loop),
                Command::For(ident, range, sub) => self.compile_for(ident, *range, sub),
                Command::Init(ident, expr) => self.compile_init(ident, expr),
                Command::VarReassign(_, _) => todo!(),
                Command::ArrIndexReassign(ident, index_expr, val) => {
                    self.compile_arr_reassign(ident, index_expr, val)
                }
                Command::Continue => {
                    let Some(prev_loop) = prev_loop else {
                        panic!("Cannot 'continue' outside loop!");
                    };
                    self.push_output(format!("JMP {prev_loop}"));
                }
            }
        }

        self.reg_dealloc_until(presize);
    }

    fn compile_add(&mut self, left: &Expr<'a>, right: &Expr<'a>, into: usize) {
        let left = if let Expr::Ident(left) = left {
            self.find_reg(left)
        } else {
            let index = self.reg_alloc("LEFT_RES");
            self.compile_expr(left, index);
            index
        };
        let right = if let Expr::Ident(right) = right {
            self.find_reg(right)
        } else {
            let index = self.reg_alloc("RIGHT_RES");
            self.compile_expr(right, index);
            index
        };
        self.push_output(format!("ADD R{left} R{right} R{into}"));
    }
    fn compile_sub(&mut self, left: &Expr<'a>, right: &Expr<'a>, into: usize) {
        let left = if let Expr::Ident(left) = left {
            self.find_reg(left)
        } else {
            let index = self.reg_alloc("LEFT_RES");
            self.compile_expr(left, index);
            index
        };
        let right = if let Expr::Ident(right) = right {
            self.find_reg(right)
        } else {
            let index = self.reg_alloc("RIGHT_RES");
            self.compile_expr(right, index);
            index
        };
        self.push_output(format!("SUB R{left} R{right} R{into}"));
    }

    fn compile_mult(&mut self, left: &Expr<'a>, right: &Expr<'a>, into: usize) {
        let left = if let Expr::Ident(left) = left {
            self.find_reg(left)
        } else {
            let index = self.reg_alloc("LEFT_RES");
            self.compile_expr(left, index);
            index
        };

        let right = if let Expr::Ident(right) = right {
            self.find_reg(right)
        } else {
            let index = self.reg_alloc("RIGHT_RES");
            self.compile_expr(right, index);
            index
        };
        self.push_output(format!("MULT R{left} R{right} R{into}"));
    }

    fn compile_init(&mut self, ident: &'a str, expr: &Expr<'a>) {
        self.reg_alloc(ident);

        self.push_output(format!("//Evaluating {ident}"));
        self.compile_expr(expr, self.find_reg(ident));
    }

    fn compile_or(&mut self, left: &Expr<'a>, right: &Expr<'a>, into: usize) {
        println!("--DOES NOT SHORT CIRCUIT!!");
        let left_reg = if let Expr::Ident(left) = left {
            self.find_reg(left)
        } else {
            let left_reg = self.reg_alloc("LEFT_RES");
            self.compile_expr(left, left_reg);
            left_reg
        };
        let right_reg = if let Expr::Ident(right) = right {
            self.find_reg(right)
        } else {
            let right_reg = self.reg_alloc("RIGHT_RES");
            self.compile_expr(right, right_reg);
            right_reg
        };
        self.push_output(format!("ADD R{left_reg} R{right_reg} R{into}"));
    }

    fn compile_for(&mut self, ident: &'a str, range: (i32, i32), sub: &[Command<'a>]) {
        let presize = self.registers.len();

        let ident_reg = self.reg_alloc(ident);
        let for_end_reg = self.reg_alloc("FOR_END");
        let (start, end) = range;
        self.push_output(format!(
            "//Initializing for loop of '{ident}' from {start} to {end}"
        ));
        self.push_output(format!("SET R{ident_reg} {start}"));
        self.push_output(format!("SET R{for_end_reg} {end}"));

        let for_cond_reg = self.reg_alloc("FOR_COND");
        self.push_output(format!("//For loop condition with R{for_cond_reg} as cond"));
        let for_cond_start =
            self.push_output(format!("SUB R{for_end_reg} R{ident_reg} R{for_cond_reg}"));

        self.push_output(format!("JUMPZ R{for_cond_reg} {{END OF FOR}}"));
        let for_end_index = self.output.len() - 1;

        self.compile_sub_code(sub, Some(for_cond_start));

        self.push_output(format!("INC R{ident_reg}"));
        self.push_output(format!("//Jumping to for '{ident}' condition"));
        let end_of_for = self.push_output(format!("JMP {for_cond_start}")) + 1;

        //Mutate the {YET UNKNOWN} to be the correct address
        let mut_line = &mut self.output[for_end_index];
        while mut_line.pop() != Some('{') {}
        mut_line.push_str(&format!("{end_of_for}\n"));
        println!("--Mutated '{{END OF FOR}}' of '{ident}' to {end_of_for}");

        self.reg_dealloc_until(presize);
    }

    fn compile_if(
        &mut self,
        expr: &Expr<'a>,
        sub: &[Command<'a>],
        else_sub: &[Command<'a>],
        prev_loop: Option<usize>,
    ) {
        let presize = self.registers.len();
        let if_cond_reg = self.reg_alloc("IF_COND");
        self.compile_expr(expr, if_cond_reg);

        self.push_output(format!("JUMPNZ R{if_cond_reg} {{END OF IF}}"));
        self.reg_dealloc_until(if_cond_reg);
        let if_end_index = self.output.len() - 1;

        self.compile_sub_code(sub, prev_loop);
        if else_sub.len() != 0 {
            self.push_output(format!("JMP {{END OF ELSE}}"));
            let else_end_index = self.output.len() - 1;

            let else_index = self.real_out + 1;
            let mut_line = &mut self.output[if_end_index];
            while mut_line.pop() != Some('{') {}
            mut_line.push_str(&format!("{else_index}\n"));
            println!("--Mutated '{{END OF IF}}' to 'else' at {else_index}");

            self.compile_sub_code(else_sub, prev_loop);

            let end_of_else = self.real_out + 1;

            let mut_line = &mut self.output[else_end_index];
            while mut_line.pop() != Some('{') {}
            mut_line.push_str(&format!("{end_of_else}\n"));
            println!("--Mutated '{{END OF ELSE}}' to {end_of_else}");
        } else {
            let after_if_index = self.real_out + 1;
            let mut_line = &mut self.output[if_end_index];
            while mut_line.pop() != Some('{') {}
            mut_line.push_str(&format!("{after_if_index}\n"));
            println!("--Mutated '{{END OF IF}}' to {after_if_index}");
        }

        self.reg_dealloc_until(presize);
    }

    fn compile_arr_reassign(&mut self, ident: &'a str, index_expr: &Expr<'a>, val: &Expr<'a>) {
        let presize = self.registers.len();

        self.push_output(format!("//Calculating mem address of {ident} + offset"));
        let index_reg = if let Expr::Ident(index_ident) = index_expr {
            self.find_reg(index_ident)
        } else {
            let index_reg = self.reg_alloc("INDEX");
            self.compile_expr(index_expr, index_reg);
            index_reg
        };
        let addr_reg = self.reg_alloc("FINAL_ADDR");
        self.push_output(format!(
            "ADD R{} R{index_reg} R{addr_reg}",
            self.find_reg(ident)
        ));

        let val_reg = if let Expr::Ident(ident) = val {
            self.find_reg(ident)
        } else {
            let reassign_val_reg = self.reg_alloc("REASSIGN_VAL");
            self.compile_expr(val, reassign_val_reg);
            reassign_val_reg
        };

        self.push_output(format!("STORE R{addr_reg} R{val_reg}"));
        self.reg_dealloc_until(presize);
    }
}
