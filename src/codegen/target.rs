pub mod riscv;

use std::cell::RefCell;
use std::rc::Rc;
use crate::ir::function::Function;


pub trait Target {
  // lower IR for register allocation
  fn instruction_lowering(&self, inst: &Rc<RefCell<Function>>);
}