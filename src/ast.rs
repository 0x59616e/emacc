use crate::irbuilder::IRBuilder;
use crate::symtab::*;
use crate::parser::*;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub trait Stmt {
  fn emit_ir(&self, ir_builder: &mut IRBuilder);
  fn print_ast(&self, prefix: String);
}

pub trait Expr {
  fn emit_ir(&self, ir_builder: &mut IRBuilder) -> Option<Value>;
  fn get_address(&self) -> Option<Value> {
    // only valid for `Var`
    None
  }
  fn print_ast(&self, prefix: String);
}

//--------------
pub struct TranslationUnit {
  pub func_or_decl_list: Vec<FunctionDef>,
}

impl Stmt for TranslationUnit {
  fn emit_ir(&self, ir_builder: &mut IRBuilder) {
    for func in self.func_or_decl_list.iter() {
      func.emit_ir(ir_builder);
    }
  }

  fn print_ast(&self, prefix: String) {
    println!("-TranslationUnit");
    let mut s1 = prefix.clone();
    let mut s2 = prefix.clone();
    s1.push_str(" |");
    s2.push_str("  ");

    for (i, item) in self.func_or_decl_list.iter().enumerate() {
      print!("{}", prefix);
      if i != self.func_or_decl_list.len() - 1 {
        print!(" |");
        item.print_ast(s1.clone());
      } else {
        print!(" `");
        item.print_ast(s2.clone());
      }
    }
  }
}

//--------------
pub struct FunctionDef {
  pub func_prototype: Rc<RefCell<SymTabEntry>>,
  pub body: Box<CompoundStmt>,
  pub local_var_list: Vec<HashMap<String, Rc<RefCell<SymTabEntry>>>>,
}

impl FunctionDef {
  pub fn param_list(
    &self,
  ) -> std::collections::hash_map::Values<String, Rc<RefCell<SymTabEntry>>> {
    self.local_var_list[0].values()
  }

  pub fn local_var_list(
    &self
  ) -> std::iter::Flatten<std::slice::Iter<HashMap<String, Rc<RefCell<SymTabEntry>>>>> {
    self.local_var_list.iter().flatten()
  }
}

impl Stmt for FunctionDef {
  fn emit_ir(&self, ir_builder: &mut IRBuilder) {
    ir_builder.enter_new_func_scope(&self.func_prototype);

    // Generate virtual register for function parameter.
    for parm in self.param_list() {
      ir_builder.gen_new_vreg(parm.borrow().get_type());
    }

    ir_builder.enter_new_basicblock_scope();

    // Generate alloca instruction for all local variable.
    for var in self.local_var_list().map(|(_, var)| var) {
      let inst = ir_builder.gen_alloca_inst(var.borrow().get_type());
      {
        var.borrow_mut().set_address(inst.get_address().unwrap());
      }
      ir_builder.insert_inst(inst);
    }

    // generate store instruction for function parameter
    for (src_reg, param) in self.param_list()
                                .enumerate()
                                .map(|(reg_num, param)|
                                    (ir_builder.gen_vreg(reg_num, param.borrow().get_type()), param)) {
      let inst = ir_builder.gen_store_inst(src_reg, param.borrow().get_address().unwrap());
      ir_builder.insert_inst(inst);
    }

    // then, we can emit ir
    self.body.emit_ir(ir_builder);
    ir_builder.reset();
  }

  fn print_ast(&self, mut prefix: String) {
    println!("-FunctionDefinition {}", &self.func_prototype.borrow());
    print!("{} `", prefix);
    prefix.push_str("  ");
    self.body.print_ast(prefix);
  }
}

//--------------
pub struct CompoundStmt {
  pub body: Vec<Box<dyn Stmt>>,
}

impl Stmt for CompoundStmt {
  fn emit_ir(&self, ir_builder: &mut IRBuilder) {
    for stmt in self.body.iter() {
      stmt.emit_ir(ir_builder);
    }
  }

  fn print_ast(&self, prefix: String) {
    println!("-CompoundStmt");
    let mut s1 = prefix.clone();
    let mut s2 = prefix.clone();
    s1.push_str(" |");
    s2.push_str("  ");

    for (i, item) in self.body.iter().enumerate() {
      print!("{}", prefix);
      if i != self.body.len() - 1 {
        print!(" |");
        item.print_ast(s1.clone());
      } else {
        print!(" `");
        item.print_ast(s2.clone());
      }
    }
  }

}

//--------------
pub struct Initializer {
  pub init_expr: Box<dyn Expr>,
}

impl Stmt for Initializer {
  fn emit_ir(&self, _: &mut IRBuilder) {
    panic!("You should've not been here");
  }

  fn print_ast(&self, prefix: String) {
    self.init_expr.print_ast(prefix);
  }
}

//--------------
pub struct VarDeclStmt {
  pub object: Rc<RefCell<SymTabEntry>>,
  pub init: Option<Box<Initializer>>,
}

impl Stmt for VarDeclStmt {
  fn emit_ir(&self, ir_builder: &mut IRBuilder) {
    if let Some(initializer) = &self.init {
      let src = initializer.init_expr.emit_ir(ir_builder).expect("No Value");
      let dest = self.object.borrow().get_address()
                                    .expect("There should've been an address");
      let inst = ir_builder.gen_store_inst(src, dest);
      ir_builder.insert_inst(inst);
      // That's it
    }
  }

  fn print_ast(&self, mut prefix: String) {
    println!("-VarDecl {}", self.object.borrow());
    if let Some(init) = &self.init {
      print!("{} `", prefix);
      prefix.push_str("  ");
      init.print_ast(prefix);
    }
  }

}

pub struct IfStmt {
  pub cond_expr: Box<dyn Expr>,
  pub then_stmt: Box<dyn Stmt>,
  pub else_stmt: Option<Box<dyn Stmt>>,
}

impl IfStmt {
  fn has_else(&self) -> bool {
    if let Some(_) = self.else_stmt {
      return true;
    } 
    false
  }
}

impl Stmt for IfStmt {
  fn emit_ir(&self, ir_builder: &mut IRBuilder) {
    // First, evaluate the conditional expression
    let src = self.cond_expr.emit_ir(ir_builder).expect("No Value");
    
    let then_block = ir_builder.gen_new_basicblock();
    let cont_block = ir_builder.gen_new_basicblock();
    let else_block = if self.has_else() {
      ir_builder.gen_new_basicblock()
    } else {
      Rc::clone(&cont_block)
    };

    let br_inst = ir_builder.gen_condi_br_inst(src,
                                               &then_block,
                                               &else_block);
    ir_builder.insert_inst(br_inst);

    let curr_bb = ir_builder.get_curr_bb();

    ir_builder.enter_basicblock_scope(Rc::clone(&then_block));
    self.then_stmt.emit_ir(ir_builder);

    if !then_block.borrow().is_terminate() {
      let br_inst = ir_builder.gen_uncondi_br_inst(&cont_block);
      ir_builder.insert_inst(br_inst);
    }

    if self.has_else() {
      ir_builder.enter_basicblock_scope(Rc::clone(&else_block));
      self.else_stmt.as_ref().unwrap().emit_ir(ir_builder);

      if !else_block.borrow().is_terminate() {
        let br_inst = ir_builder.gen_uncondi_br_inst(&cont_block);
        ir_builder.insert_inst(br_inst);
      }
    }

    // let's keep on our journey...
    ir_builder.enter_basicblock_scope(cont_block);
  }

  fn print_ast(&self, prefix: String) {
    println!("-IfStmt");
    let mut s1 = prefix.clone();
    let mut s2 = prefix.clone();
    s1.push_str(" |");
    s2.push_str("  ");

    print!("{} |", prefix);
    self.cond_expr.print_ast(s1.clone());
    if let None = self.else_stmt {
      print!("{} `", prefix);
      self.then_stmt.print_ast(s2);
    } else {
      print!("{} |", prefix);
      self.then_stmt.print_ast(s1);
      print!("{} `", prefix);
      self.else_stmt.as_ref().unwrap().print_ast(s2);
    }
  }
}


pub struct ExprStmt {
  pub expr: Box<dyn Expr>,
}

impl Stmt for ExprStmt {
  fn emit_ir(&self, ir_builder: &mut IRBuilder) {
    self.expr.emit_ir(ir_builder);
  }

  fn print_ast(&self, prefix: String) {
    self.expr.print_ast(prefix);
  }
}

pub struct ReturnStmt {
  pub expr: Option<Box<dyn Expr>>,
}

impl Stmt for ReturnStmt {
  fn emit_ir(&self, ir_builder: &mut IRBuilder) {
    let inst = if let Some(expr) = &self.expr {
      let retval = expr.emit_ir(ir_builder).expect("No Value");
      ir_builder.gen_ret_inst_with_retval(retval)
    } else {
      ir_builder.gen_ret_inst_with_no_retval()
    };

    ir_builder.insert_inst(inst);
    ir_builder.leave_basicblock_scope();
  }

  fn print_ast(&self, mut prefix: String) {
    println!("-ReturnStmt");
    if let Some(expr) = &self.expr {
      print!("{} `", prefix);
      prefix.push_str("  ");
      expr.print_ast(prefix);
    }
  }
}

pub struct CallExpr {
  pub func: Rc<RefCell<SymTabEntry>>,
  pub arg_list: Vec<Box<dyn Expr>>,
}

impl Expr for CallExpr {
  fn emit_ir(&self, ir_builder: &mut IRBuilder) -> Option<Value> {
    let arg_list: Vec<Value> = self.arg_list
                                    .iter()
                                    .map(|expr| expr.emit_ir(ir_builder).expect("No Value"))
                                    .collect();

    let (dest, inst) = ir_builder.gen_call_inst(&self.func, arg_list);
    ir_builder.insert_inst(inst);
    dest
  }

  fn print_ast(&self, prefix: String) {
    println!("-CallExpr '{}'", self.func.borrow());
    let mut s1 = prefix.clone();
    let mut s2 = prefix.clone();
    s1.push_str(" |");
    s2.push_str("  ");

    for (i, item) in self.arg_list.iter().enumerate() {
      print!("{}", prefix);
      if i != self.arg_list.len() - 1 {
        print!(" |");
        item.print_ast(s1.clone());
      } else {
        print!(" `");
        item.print_ast(s2.clone());
      }
    }
  }
}

pub struct BinOpExpr {
  pub op: BinOpType,
  pub lhs: Box<dyn Expr>,
  pub rhs: Box<dyn Expr>,
}

impl BinOpExpr {
  fn is_op_comma(&self) -> bool {
    self.op == BinOpType::Comma
  }

  fn is_op_assignment(&self) -> bool {
    self.op == BinOpType::Assignment
  }

  fn is_op_relational(&self) -> bool {
    self.op == BinOpType::Equal ||
    self.op == BinOpType::NotEqual ||
    self.op == BinOpType::GreaterEqual ||
    self.op == BinOpType::Greater ||
    self.op == BinOpType::LessEqual ||
    self.op == BinOpType::Less
  }

  fn is_op_logical(&self) -> bool {
    self.is_op_logical_and() ||
    self.is_op_logical_or()
  }

  fn is_op_logical_and(&self) -> bool {
    self.op == BinOpType::LogicalAnd
  }

  fn is_op_logical_or(&self) -> bool {
    self.op == BinOpType::LogicalOr
  }
}

impl Expr for BinOpExpr {
  fn emit_ir(&self, ir_builder: &mut IRBuilder) -> Option<Value> {
    if self.is_op_comma() {
      self.lhs.emit_ir(ir_builder);
      self.rhs.emit_ir(ir_builder);
      return None;
    } else if self.is_op_assignment() {
      let dest = self.lhs.get_address().expect("No Address");
      let src = self.rhs.emit_ir(ir_builder).expect("No Value");
      let inst = ir_builder.gen_store_inst(src, dest);
      ir_builder.insert_inst(inst);
      return Some(dest);
    } else if self.is_op_logical() {
      // Oh No...
      let block0 = ir_builder.get_curr_bb();
      let block1 = ir_builder.gen_new_basicblock();
      let block2 = ir_builder.gen_new_basicblock();

      let src1 = self.lhs.emit_ir(ir_builder).expect("No Value");

      let inst = if self.is_op_logical_or() {
        ir_builder.gen_condi_br_inst(src1, &block2, &block1)
      } else {
        ir_builder.gen_condi_br_inst(src1, &block1, &block2)
      };
      ir_builder.insert_inst(inst);

      ir_builder.enter_basicblock_scope(Rc::clone(&block1));
      let src2 = self.rhs.emit_ir(ir_builder).expect("No Value");
      let inst = ir_builder.gen_uncondi_br_inst(&block2);
      ir_builder.insert_inst(inst);


      let block1 = ir_builder.get_curr_bb();
      ir_builder.enter_basicblock_scope(Rc::clone(&block2));

      let dest = ir_builder.gen_new_vreg_with_data_ty(src1.get_data_ty());
      let src1 = ir_builder.gen_i1_const(self.is_op_logical_or());
      let inst = ir_builder.gen_phi_inst(
        dest,
        vec![
              (src1, block0.borrow().get_label()),
              (src2, block1.borrow().get_label())
            ]
      );
      ir_builder.insert_inst(inst);
      Some(dest)
    } else {
      let src1 = self.lhs.emit_ir(ir_builder).expect("No Value");
      let src2 = self.rhs.emit_ir(ir_builder).expect("No Value");
      let dest = ir_builder.gen_new_vreg_with_data_ty(src1.get_data_ty());
      // cmp instruction
      let inst = if self.is_op_relational() {
        ir_builder.gen_cmp_inst(src1, src2, dest, self.op)
      } else {
        ir_builder.gen_binary_inst(src1, src2, dest, self.op)
      };
      ir_builder.insert_inst(inst);
      return Some(dest);
    }
  }

  fn print_ast(&self, prefix: String) {
    println!("-BinOpExpr '{}'", self.op);
    print!("{} |", prefix);
    let mut s1 = prefix.clone();
    let mut s2 = prefix.clone();
    s1.push_str(" |");
    self.lhs.print_ast(s1);
    print!("{} `", prefix);
    s2.push_str("  ");
    self.rhs.print_ast(s2);
  }
}

pub struct Constant {
  pub value: i32,
}

impl Expr for Constant {
  fn emit_ir(&self, ir_builder: &mut IRBuilder) -> Option<Value> {
    Some(ir_builder.gen_i32_const(self.value))
  }

  fn print_ast(&self, _prefix: String) {
    println!("-Constant '{}'", self.value);
  }
}

pub struct Var {
  pub object: Rc<RefCell<SymTabEntry>>,
}

impl Expr for Var {
  fn emit_ir(&self, ir_builder: &mut IRBuilder) -> Option<Value> {
    let dest = ir_builder.gen_new_vreg(self.object.borrow().get_type());
    let src = self.object.borrow().get_address().expect("No Address");
    let inst = ir_builder.gen_load_inst(src, dest);
    ir_builder.insert_inst(inst);
    Some(dest)
  }

  fn get_address(&self) -> Option<Value> {
    self.object.borrow().get_address()
  }

  fn print_ast(&self, _prefix: String) {
    println!("-Var: {}", self.object.borrow());
  }
}