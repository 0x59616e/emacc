use crate::ir::function::*;
use std::rc::Rc;
use std::cell::RefCell;

pub struct Module {
  func_list: Vec<Rc<RefCell<Function>>>,
}

impl Module {
  pub fn new() -> Module {
    Module {
      func_list: vec![],
    }
  }
}

impl Module {
  pub fn print(&self) {
    for func in self.func_list.iter() {
      func.borrow().print();
    }
  }

  pub fn func_list(&self) -> impl Iterator<Item = &Rc<RefCell<Function>>> {
    self.func_list.iter()
  }
  pub fn insert(&mut self, func: Rc<RefCell<Function>>) {
    self.func_list.push(func);
  }
}