use crate::value::Value;
use crate::basicblock::BasicBlock;
use std::cell::RefCell;
use std::rc::Rc;

pub struct MachineInstr {
  // instruction type, defined by target
  ty: u8,
  op: Vec<Rc<RefCell<Value>>>,
  // use for phi and br
  bb: Vec<Rc<RefCell<BasicBlock>>>,
}
