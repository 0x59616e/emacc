use crate::ast::*;
use crate::basicblock::*;
use crate::function::*;
use crate::instruction::*;
use crate::module::*;
use crate::parser::BinOpType;
use crate::symtab::{SymTabEntry, Type};
use crate::value::{Value, DataTy};
use std::rc::Rc;
use std::cell::RefCell;
use std::mem;

pub struct IRBuilder {
  next_vreg: usize,
  module: Module,
  curr_func: Option<Rc<RefCell<Function>>>,
  curr_bb: Option<Rc<RefCell<BasicBlock>>>,
}

impl IRBuilder {
  pub fn new() -> IRBuilder {
    IRBuilder {
      next_vreg: 0,
      module: Module::new(),
      curr_func: None,
      curr_bb: None,
    }
  }

  pub fn change_language_ty_to_data_ty(ty: Type) -> DataTy {
    match ty {
      Type::Int => DataTy::I32,
      _ => panic!("only int suppported, found: {:?}", ty),
    }
  }

  pub fn run(mut self, node: TranslationUnit) -> Module {
    node.emit_ir(&mut self);
    self.module
  }

  fn gen_new_num(&mut self) -> usize {
    let result = self.next_vreg;
    self.next_vreg += 1;
    result
  }

  pub fn reset(&mut self) {
    self.next_vreg = 0;
    self.leave_basicblock_scope();
    self.leave_func_scope();
  }
  pub fn gen_new_vreg(&mut self, ty: Type) -> Value {
    Value::new_register(self.gen_new_num(), IRBuilder::change_language_ty_to_data_ty(ty))
  }

  pub fn gen_new_vreg_with_data_ty(&mut self, ty: DataTy) -> Value {
    Value::new_register(self.gen_new_num(), ty)
  }

  pub fn gen_vreg(&self, reg_num: usize, ty: Type) -> Value {
    Value::new_register(reg_num, IRBuilder::change_language_ty_to_data_ty(ty))
  }

  pub fn gen_i32_const(&self, value: i32) -> Value {
    Value::new_const_i32(value)
  }

  pub fn gen_i1_const(&self, value: bool) -> Value {
    Value::new_const_i1(value)
  }

  pub fn gen_phi_inst(&self, dest: Value, pair: Vec<(Value, Value)>) -> Instruction {
    Instruction::gen_phi_inst(dest, pair, self.get_curr_bb())
  }

  pub fn gen_call_inst(
    &mut self,
    func: &Rc<RefCell<SymTabEntry>>,
    arg_list: Vec<Value>
  ) -> (Option<Value>, Instruction)
  {
    match func.borrow().get_return_type() {
      Type::Void => (None, self.gen_call_inst_with_no_retval(func.borrow().get_name(), arg_list)),
      _ => {
        let dest = self.gen_new_vreg(func.borrow().get_return_type());
        (Some(dest), self.gen_call_inst_with_retval(dest, func.borrow().get_name(), arg_list))
      }
    }
  }

  pub fn gen_call_inst_with_retval(
    &self,
    dest: Value,
    func_name: String,
    arg_list: Vec<Value>
  ) -> Instruction
  {
    Instruction::gen_call_inst(Some(dest), func_name, arg_list, self.get_curr_bb())
  }

  pub fn gen_call_inst_with_no_retval(&self, func_name: String, arg_list: Vec<Value>) -> Instruction {
    Instruction::gen_call_inst(None, func_name, arg_list, self.get_curr_bb())
  }

  pub fn gen_ret_inst_with_retval(&self, retval: Value) -> Instruction {
    Instruction::gen_ret_inst(Some(retval), self.get_curr_bb())
  }

  pub fn gen_ret_inst_with_no_retval(&self) -> Instruction {
    Instruction::gen_ret_inst(None, self.get_curr_bb())
  }

  pub fn gen_binary_inst(
    &self,
    src1: Value,
    src2: Value,
    dest: Value,
    binop: BinOpType
  ) -> Instruction
  {
    let ty = match binop {
      BinOpType::Plus  => BinaryInstTy::Add,
      BinOpType::Minus => BinaryInstTy::Sub,
      BinOpType::Mul   => BinaryInstTy::Mul,
      BinOpType::Div   => BinaryInstTy::Div,
      _ => panic!("What the fuck is this operator ????"),
    };
    Instruction::gen_binary_inst(src1, src2, dest, ty, self.get_curr_bb())
  }
  pub fn gen_cmp_inst(
    &self,
    src1: Value,
    src2: Value,
    dest: Value,
    binop: BinOpType,
  ) -> Instruction
  {
    let ty = match binop {
      BinOpType::Equal        => CmpTy::Eq,
      BinOpType::NotEqual     => CmpTy::Ne,
      BinOpType::GreaterEqual => CmpTy::Ge,
      BinOpType::Greater      => CmpTy::Gt,
      BinOpType::LessEqual    => CmpTy::Le,
      BinOpType::Less         => CmpTy::Lt,
      _ => panic!("What the fuck is this operator ????"),
    };
    Instruction::gen_cmp_inst(src1, src2, dest, ty, self.get_curr_bb())
  }

  pub fn gen_br_inst(
    &self,
    condi_br: bool,
    src: Option<Value>,
    true_bb_label: Value,
    false_bb_label: Option<Value>,
  ) -> Instruction 
  {
    Instruction::gen_br_inst(
      condi_br,
      src,
      true_bb_label,
      false_bb_label,
      self.get_curr_bb()
    )
  }

  pub fn gen_condi_br_inst(
    &self,
    src: Value,
    true_bb: &Rc<RefCell<BasicBlock>>,
    false_bb: &Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    self.gen_br_inst(
      true,
      Some(src),
      true_bb.borrow().get_label(),
      Some(false_bb.borrow().get_label())
    )
  }

  pub fn gen_uncondi_br_inst(&self, dest: &Rc<RefCell<BasicBlock>>) -> Instruction {
    self.gen_br_inst(false, None, dest.borrow().get_label(), None)
  }

  pub fn gen_store_inst(
    &self,
    src: Value,
    dest: Value
  ) -> Instruction
  {
    Instruction::gen_store_inst(src, dest, self.get_curr_bb())
  }

  pub fn gen_load_inst(&self, src: Value, dest: Value) -> Instruction {
    Instruction::gen_load_inst(src,dest, self.get_curr_bb())
  }

  pub fn gen_alloca_inst(&mut self, ty: Type) -> Instruction {
    let addr = self.gen_new_vreg(ty);
    Instruction::gen_alloca_inst(addr, self.get_curr_bb())
  }

  pub fn insert_inst(&self, inst: Instruction) {
    self.curr_bb.as_ref().unwrap().borrow_mut().insert(inst);
  }

  fn get_curr_func(&self) -> Rc<RefCell<Function>> {
    Rc::clone(self.curr_func.as_ref().unwrap())
  }

  pub fn enter_new_func_scope(&mut self, func: &Rc<RefCell<SymTabEntry>>) {
    self.leave_func_scope();
    self.curr_func = Some(Rc::new(RefCell::new(Function::new(func))));
  }

  pub fn leave_func_scope(&mut self) {
    if let Some(_) = self.curr_func {
      let func = mem::take(&mut self.curr_func).unwrap();
      self.module.insert(func);
    }
  }

  pub fn construct_edge_between_bb(
    &self,
    from: &Rc<RefCell<BasicBlock>>,
    to: &Rc<RefCell<BasicBlock>>
  )
  {
    from.borrow_mut().insert_succ(Rc::clone(&to));
    to.borrow_mut().insert_pred(Rc::clone(&from));
  }

  pub fn gen_new_basicblock(&mut self) -> Rc<RefCell<BasicBlock>> {
    let mut bb = BasicBlock::new();
    bb.set_label(Value::new_label(self.gen_new_num()));
    bb.set_parent(self.get_curr_func());
    Rc::new(RefCell::new(bb))
  }

  pub fn get_curr_bb(&self) -> Rc<RefCell<BasicBlock>> {
    Rc::clone(self.curr_bb.as_ref().unwrap())
  }

  pub fn enter_basicblock_scope(&mut self, bb: Rc<RefCell<BasicBlock>>) {
    self.leave_basicblock_scope();
    self.curr_bb = Some(bb);
  }

  pub fn enter_new_basicblock_scope(&mut self) {
    self.leave_basicblock_scope();
    self.curr_bb = Some(self.gen_new_basicblock());
  }

  pub fn leave_basicblock_scope(&mut self) {
    if let Some(_) = self.curr_bb {
      let bb = mem::take(&mut self.curr_bb).unwrap();
      self.curr_func.as_ref().unwrap().borrow_mut().insert(bb);
    }
  }
}
