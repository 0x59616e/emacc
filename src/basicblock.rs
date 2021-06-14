use crate::function::*;
use crate::instruction::*;
use crate::value::Value;
use std::cell::RefCell;
use std::rc::Rc;

pub struct BasicBlock {
  parent: Option<Rc<RefCell<Function>>>,
  label: Option<Value>,
  succs: Vec<Rc<RefCell<BasicBlock>>>,
  preds: Vec<Rc<RefCell<BasicBlock>>>,
  inst_list: Vec<Rc<RefCell<Instruction>>>,
}

impl BasicBlock {
  pub fn new() -> BasicBlock {
    BasicBlock {
      parent: None,
      label: None,
      succs: vec![],
      preds: vec![],
      inst_list: vec![],
    }
  }

  pub fn print(&self) {
    print!("\n{}:\t\t\t\t\t;preds: ", self.get_label());
    for pred in self.preds.iter() {
      print!("{} ", pred.borrow().get_label());
    }
    println!("");
  
    for inst in self.inst_list.iter() {
      print!("  ");
      inst.borrow().print();
    }
  }

  pub fn remove_inst(&mut self, inst: &Rc<RefCell<Instruction>>) {
    let i = self.inst_list.iter()
                          .position(|x| Rc::ptr_eq(x, inst))
                          .expect("No inst");
    self.inst_list.remove(i);
  }

  pub fn is_terminated(&self) -> bool {
    self.inst_list.last().map_or(false, |inst| inst.borrow().is_terminator())
  }

  pub fn inst_list(&self) -> impl Iterator<Item = &Rc<RefCell<Instruction>>> {
    self.inst_list.iter()
  }

  pub fn preds_list(&self) -> impl Iterator<Item = &Rc<RefCell<BasicBlock>>> {
    self.preds.iter()
  }

  pub fn succs_list(&self) -> impl Iterator<Item = &Rc<RefCell<BasicBlock>>> {
    self.succs.iter()
  }

  pub fn is_join_points(&self) -> bool {
    self.preds.len() > 1
  }

  pub fn is_root(&self) -> bool {
    // TODO: This is not a good idea, change it.
    self.preds.len() == 0
  }

  pub fn insert_succ(&mut self, succ: Rc<RefCell<BasicBlock>>) {
    self.succs.push(succ);
  }

  pub fn insert_pred(&mut self, pred: Rc<RefCell<BasicBlock>>) {
    self.preds.push(pred);
  }

  pub fn set_parent(&mut self, parent: Rc<RefCell<Function>>) {
    self.parent = Some(parent);
  }

  pub fn get_pred(&self) -> Option<Rc<RefCell<BasicBlock>>> {
    // return arbitrary predessor
    self.preds.get(0).cloned()
  }

  pub fn set_label(&mut self, label: Value) {
    self.label = Some(label);
  }

  pub fn get_label(&self) -> Value {
    self.label.expect("There should've been a label...")
  }

  pub fn insert(&mut self, inst: Instruction) {
    self.inst_list.push(Rc::new(RefCell::new(inst)));
  }

  pub fn insert_at_head(&mut self, inst: Instruction) {
    self.inst_list.insert(0, Rc::new(RefCell::new(inst)));
  }

  pub fn insert_before_terminator(&mut self, inst: Rc<RefCell<Instruction>>) {
    let pos = self.inst_list.iter().position(|inst| inst.borrow().is_terminator())
                                   .expect("No terminator ??");

    self.inst_list.insert(pos, inst);
  }

  pub fn remove_all_phi(&mut self) {
    let pos = self.inst_list()
                  .position(|inst| ! inst.borrow().is_phi_inst())
                  .unwrap_or(0);
    self.inst_list.drain(..pos);
  }

  pub fn update_inst_list(&mut self, list: Vec<Rc<RefCell<Instruction>>>) {
    self.inst_list = list;
  }
}