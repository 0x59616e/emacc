use crate::basicblock::*;
use crate::module::Module;
use crate::symtab::SymTabEntry;
use crate::value::{DataTy};
use std::rc::Rc;
use std::cell::RefCell;

pub struct Function {
  name: String,
  ret_ty: DataTy,
  arg_ty: Vec<DataTy>,
  bb_list: Vec<Rc<RefCell<BasicBlock>>>,
  parent: Option<Rc<RefCell<Module>>>,
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
      parent: None,
    }
  }

  pub fn bb_list_mut(&mut self) -> impl Iterator<Item = &mut Rc<RefCell<BasicBlock>>> {
    self.bb_list.iter_mut()
  }

  pub fn bb_list(&self) -> impl Iterator<Item = &Rc<RefCell<BasicBlock>>> {
    self.bb_list.iter()
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
  }

  pub fn set_parent(&mut self, parent: &Rc<RefCell<Module>>) {
    self.parent = Some(Rc::clone(parent));
  }

  pub fn get_parent(&self) -> Rc<RefCell<Module>> {
    Rc::clone(self.parent.as_ref().expect("No parent"))
  }
}