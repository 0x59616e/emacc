pub mod riscv;

use crate::ir::function::Function;
use crate::ir::value::Value;
use std::cell::RefCell;
use std::collections::BTreeSet;
use std::rc::Rc;
use super::liveinterval::LiveIntervalInfo;

pub trait Target {
  // Lower IR for register allocation.
  fn instruction_lowering(&self, _: &Rc<RefCell<Function>>);
  // Return registers available for allocation.
  fn get_avail_regs(&self) -> BTreeSet<Value>;

  fn calc_register_binding(&self, _: &Rc<RefCell<Function>>, _: &mut LiveIntervalInfo);
  // Allocate stack slot, emit asm.
  fn allocate_stack_and_emit_asm(&self, _: &Rc<RefCell<Function>>);
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub enum RegisterSaver {
  Caller,
  Callee,
}
