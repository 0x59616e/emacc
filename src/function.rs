use crate::basicblock::*;
use crate::symtab::SymTabEntry;
use crate::value::{Value, DataTy};
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

pub struct Function {
  name: String,
  ret_ty: DataTy,
  arg_ty: Vec<DataTy>,
  bb_list: Vec<Rc<RefCell<BasicBlock>>>,
  bb_map: HashMap<Value, Rc<RefCell<BasicBlock>>>,
}

impl Function {
  pub fn new(func: &Rc<RefCell<SymTabEntry>>) -> Function {
    Function {
      name: func.borrow().get_name(),
      ret_ty: DataTy::from(func.borrow().get_return_type()),
      arg_ty: func.borrow()
                      .get_prototype()
                      .iter()
                      .map(|&ty| DataTy::from(ty))
                      .collect(),
      bb_list: vec![],
      bb_map: HashMap::new(),
    }
  }

  pub fn print(&self) {
    print!("\n{} {}(", self.ret_ty, self.name);
    for (i, ty) in self.arg_ty.iter().enumerate() {
      print!("{} %{}", ty, i);
      if i != self.arg_ty.len() - 1 {
        print!(", ");
      }
    }
    println!(")");

    for bb in self.bb_list.iter() {
      bb.borrow().print();
    }
  }

  pub fn insert(&mut self, bb: Rc<RefCell<BasicBlock>>) {
    self.bb_list.push(Rc::clone(&bb));
    let label = {bb.borrow().get_label()};
    self.bb_map.insert(label, bb);
  }
}