pub mod target;

use crate::function::Function;
use crate::instruction::{Instruction, BinaryTy};
use crate::module::Module;
use crate::value::Value;
use std::cell::RefCell;
use std::rc::Rc;
use target::Target;

pub fn code_gen<T: Target>(module: &Rc<RefCell<Module>>, target: T) {
  // for func in module.borrow().func_list() {
  //   eliminate_phi(func);
  // }

  // println!("-----------after phi elimination-------------");
  // module.borrow().print();

  for func in module.borrow().func_list() {
    target.instruction_lowering(func);
  }

  println!("-----------after instruction lowering--------");
  module.borrow().print();
  // Time to register allocation...
}

pub fn eliminate_phi(func: &Rc<RefCell<Function>>) {
  for bb in func.borrow().bb_list() {
    for inst in bb.borrow().inst_list() {

      if ! inst.borrow().is_phi_inst() {
        break;
      }

      let dest = inst.borrow().get_dest().unwrap();
      for (&val, bb) in inst.borrow_mut().get_pairs_list_in_phi() {
        let zero = Value::new_const_i32(0);
        let mov_inst = Rc::new(RefCell::new(Instruction::new_binary_inst(zero,
                                                                         val,
                                                                         dest,
                                                                         BinaryTy::Add,
                                                                         Rc::clone(bb))));
        bb.borrow_mut().insert_before_terminator(mov_inst);
      }
    }
    bb.borrow_mut().remove_all_phi();
  }
}