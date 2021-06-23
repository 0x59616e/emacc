use std::cell::RefCell;
use std::rc::Rc;
use super::function::Function;
use super::value::{Value, ValueTy};

pub struct SSAValueCounter {
  num: usize,
}

impl SSAValueCounter {
  pub fn new(func: Option<&Rc<RefCell<Function>>>) -> Self {
    let mut result = SSAValueCounter{num: 0};
    if let Some(func) = func {
      for _ in func.borrow().param_list() {
        result.new_num();
      }

      for bb in func.borrow().bb_list() {
        result.new_num();

        for inst in bb.borrow().inst_list() {
          if let Some(_) = inst.borrow().get_dest() {
            result.new_num();
          }
        }
      }
    }
    result
  }

  pub fn new_num(&mut self) -> usize {
    self.num += 1;
    self.num
  }
}