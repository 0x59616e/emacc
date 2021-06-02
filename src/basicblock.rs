use crate::function::*;
use crate::instruction::*;
use crate::value::Value;
use std::cell::RefCell;
use std::rc::Rc;

pub struct BasicBlock {
  parent: Option<Rc<RefCell<Function>>>,
  label: Option<Value>,
  succ: Vec<Value>,
  pred: Vec<Value>,
  inst_list: Vec<Instruction>,
}

impl BasicBlock {
  pub fn new() -> BasicBlock {
    BasicBlock {
      parent: None,
      label: None,
      succ: vec![],
      pred: vec![],
      inst_list: vec![],
    }
  }

  pub fn print(&self) {
    println!("\n{}:", self.get_label());
    for inst in self.inst_list.iter() {
      print!("  ");
      inst.print();
    }
  }

  pub fn is_terminate(&self) -> bool {
    if let Some(last_inst) = self.inst_list.last() {
      last_inst.is_terminate_inst()
    } else {
      false
    }
  }

  pub fn insert_succ(&mut self, succ: Rc<RefCell<BasicBlock>>) {
    self.succ.push(succ.borrow().get_label());
  }

  pub fn insert_pred(&mut self, pred: Rc<RefCell<BasicBlock>>) {
    self.pred.push(pred.borrow().get_label());
  }

  pub fn set_parent(&mut self, parent: Rc<RefCell<Function>>) {
    self.parent = Some(parent);
  }

  pub fn set_label(&mut self, label: Value) {
    self.label = Some(label);
  }

  pub fn get_label(&self) -> Value {
    self.label.expect("There should've been a label...")
  }

  pub fn insert(&mut self, inst: Instruction) {
    self.inst_list.push(inst);
  }
}