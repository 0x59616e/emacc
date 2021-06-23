pub mod dominator;
pub mod domfrontier;
pub mod liveout;

use crate::ir::function::Function;
use std::cell::RefCell;
use std::rc::Rc;

pub trait Analysis {
  fn run(&mut self);
  fn new(_: &Rc<RefCell<Function>>) -> Self;
}

pub fn run_analysis<T: Analysis>(func: &Rc<RefCell<Function>>) -> T {
  let mut t = T::new(func);
  t.run();
  t
}