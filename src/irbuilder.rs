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
  module: Rc<RefCell<Module>>,
  curr_func: Option<Rc<RefCell<Function>>>,
  curr_bb: Option<Rc<RefCell<BasicBlock>>>,
}

impl IRBuilder {
  pub fn new() -> IRBuilder {
    IRBuilder {
      next_vreg: 0,
      module: Rc::new(RefCell::new(Module::new())),
      curr_func: None,
      curr_bb: None,
    }
  }

  pub fn run(mut self, node: TranslationUnit) -> Rc<RefCell<Module>> {
    node.emit_ir(&mut self);
    self.module
  }

  fn new_num(&mut self) -> usize {
    let result = self.next_vreg;
    self.next_vreg += 1;
    result
  }

  pub fn reset(&mut self) {
    self.next_vreg = 0;
    self.leave_basicblock_scope();
    self.leave_func_scope();
  }

  pub fn new_vreg(&mut self, ty: Type, is_ptr: bool) -> Rc<RefCell<Value>> {
    Rc::new(RefCell::new(Value::new_register(self.new_num(), DataTy::from(ty), is_ptr)))
  }

  pub fn new_vreg_with_data_ty(&mut self, ty: DataTy, is_ptr: bool) -> Rc<RefCell<Value>> {
    Rc::new(RefCell::new(Value::new_register(self.new_num(), ty, is_ptr)))
  }

  pub fn new_const_i32(&self, value: i32) -> Rc<RefCell<Value>> {
    Rc::new(RefCell::new(Value::new_const_i32(value)))
  }

  pub fn new_const_i1(&self, value: bool) -> Rc<RefCell<Value>> {
    Rc::new(RefCell::new(Value::new_const_i1(value)))
  }

  pub fn new_phi_inst(
    &self,
    dest: &Rc<RefCell<Value>>,
    pair: Vec<(Rc<RefCell<Value>>, Rc<RefCell<BasicBlock>>)>
  ) -> Instruction {
    Instruction::new_phi_inst(Rc::clone(dest), pair, self.get_curr_bb())
  }

  pub fn new_call_inst(
    &mut self,
    func: &Rc<RefCell<SymTabEntry>>,
    arg_list: Vec<Rc<RefCell<Value>>>
  ) -> (Option<Rc<RefCell<Value>>>, Instruction)
  {
    match func.borrow().get_return_type() {
      Type::Void => (None, self.new_call_inst_with_no_retval(func.borrow().get_name(), arg_list)),
      _ => {
        let dest = self.new_vreg(func.borrow().get_return_type(), false);
        (Some(Rc::clone(&dest)), self.new_call_inst_with_retval(&dest, func.borrow().get_name(), arg_list))
      }
    }
  }

  pub fn new_call_inst_with_retval(
    &self,
    dest: &Rc<RefCell<Value>>,
    func_name: String,
    arg_list: Vec<Rc<RefCell<Value>>>
  ) -> Instruction
  {
    Instruction::new_call_inst(Some(Rc::clone(dest)), func_name, arg_list, self.get_curr_bb())
  }

  pub fn new_call_inst_with_no_retval(
    &self,
    func_name: String,
    arg_list: Vec<Rc<RefCell<Value>>>
  ) -> Instruction
  {
    Instruction::new_call_inst(None, func_name, arg_list, self.get_curr_bb())
  }

  pub fn new_ret_inst_with_retval(&self, retval: &Rc<RefCell<Value>>) -> Instruction {
    Instruction::new_ret_inst(Some(Rc::clone(retval)), self.get_curr_bb())
  }

  pub fn new_ret_inst_with_no_retval(&self) -> Instruction {
    Instruction::new_ret_inst(None, self.get_curr_bb())
  }

  pub fn new_binary_inst(
    &self,
    src1: &Rc<RefCell<Value>>,
    src2: &Rc<RefCell<Value>>,
    dest: &Rc<RefCell<Value>>,
    binop: BinOpType
  ) -> Instruction
  {
    let ty = BinaryTy::from(binop);
    Instruction::new_binary_inst(Rc::clone(src1), Rc::clone(src2), Rc::clone(dest), ty, self.get_curr_bb())
  }
  pub fn new_cmp_inst(
    &self,
    src1: &Rc<RefCell<Value>>,
    src2: &Rc<RefCell<Value>>,
    dest: &Rc<RefCell<Value>>,
    binop: BinOpType,
  ) -> Instruction
  {
    let ty = CmpTy::from(binop);
    Instruction::new_cmp_inst(Rc::clone(src1), Rc::clone(src2), Rc::clone(dest), ty, self.get_curr_bb())
  }

  fn new_br_inst(
    &self,
    condi_br: bool,
    src: Option<Rc<RefCell<Value>>>,
    true_bb_label: Rc<RefCell<BasicBlock>>,
    false_bb_label: Option<Rc<RefCell<BasicBlock>>>,
  ) -> Instruction 
  {
    Instruction::new_br_inst(
      condi_br,
      src,
      true_bb_label,
      false_bb_label,
      self.get_curr_bb()
    )
  }

  pub fn new_condi_br_inst(
    &self,
    src: &Rc<RefCell<Value>>,
    true_bb: &Rc<RefCell<BasicBlock>>,
    false_bb: &Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    self.construct_edge_between_bb(&self.get_curr_bb(), true_bb);
    self.construct_edge_between_bb(&self.get_curr_bb(), false_bb);
    self.new_br_inst(
      true,
      Some(Rc::clone(src)),
      Rc::clone(true_bb),
      Some(Rc::clone(false_bb))
    )
  }

  pub fn new_uncondi_br_inst(&self, dest: &Rc<RefCell<BasicBlock>>) -> Instruction {
    self.construct_edge_between_bb(&self.get_curr_bb(), dest);
    self.new_br_inst(false, None, Rc::clone(dest), None)
  }

  pub fn new_store_inst(
    &self,
    src: &Rc<RefCell<Value>>,
    dest: &Rc<RefCell<Value>>
  ) -> Instruction
  {
    Instruction::new_store_inst(Rc::clone(src), Rc::clone(dest), self.get_curr_bb())
  }

  pub fn new_load_inst(&self, src: &Rc<RefCell<Value>>, dest: &Rc<RefCell<Value>>) -> Instruction {
    Instruction::new_load_inst(Rc::clone(src), Rc::clone(dest), self.get_curr_bb())
  }

  pub fn new_alloca_inst(&mut self, ty: Type) -> Instruction {
    let addr = self.new_vreg(ty, true);
    Instruction::new_alloca_inst(addr, self.get_curr_bb())
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
      self.module.borrow_mut().insert(func);
    }
  }

  pub fn construct_edge_between_bb(
    &self,
    from: &Rc<RefCell<BasicBlock>>,
    to: &Rc<RefCell<BasicBlock>>
  )
  {
    from.borrow_mut().insert_succ(Rc::clone(to));
    to.borrow_mut().insert_pred(Rc::clone(from));
  }

  pub fn new_basicblock(&mut self) -> Rc<RefCell<BasicBlock>> {
    let mut bb = BasicBlock::new();
    bb.set_label(Value::new_label(self.new_num()));
    bb.set_parent(self.get_curr_func());
    Rc::new(RefCell::new(bb))
  }

  pub fn get_curr_bb(&self) -> Rc<RefCell<BasicBlock>> {
    Rc::clone(self.curr_bb.as_ref().unwrap())
  }

  pub fn enter_basicblock_scope(&mut self, bb: &Rc<RefCell<BasicBlock>>) {
    self.leave_basicblock_scope();
    self.curr_bb = Some(Rc::clone(bb));
  }

  pub fn enter_new_basicblock_scope(&mut self) {
    self.leave_basicblock_scope();
    self.curr_bb = Some(self.new_basicblock());
  }

  pub fn leave_basicblock_scope(&mut self) {
    if let Some(_) = self.curr_bb {
      let bb = mem::take(&mut self.curr_bb).unwrap();
      self.curr_func.as_ref().unwrap().borrow_mut().insert(bb);
    }
  }
}
