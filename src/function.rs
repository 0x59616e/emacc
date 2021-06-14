use crate::basicblock::*;
use crate::instruction::Instruction;
use crate::module::Module;
use crate::symtab::SymTabEntry;
use crate::value::{DataTy, Value};
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

pub struct Function {
  name: String,
  ret_ty: DataTy,
  param_ty: Vec<DataTy>,
  bb_list: Vec<Rc<RefCell<BasicBlock>>>,
  parent: Option<Rc<RefCell<Module>>>,
}

impl Function {
  pub fn new(func: &Rc<RefCell<SymTabEntry>>) -> Function {
    Function {
      name: func.borrow().get_name(),
      ret_ty: DataTy::from(func.borrow().get_return_type()),
      param_ty: func.borrow()
                      .get_prototype()
                      .iter()
                      .map(|&ty| DataTy::from(ty))
                      .collect(),
      bb_list: vec![],
      parent: None,
    }
  }
  // FIXME: remove this function, it seems useless
  pub fn bb_list_mut(&mut self) -> impl Iterator<Item = &mut Rc<RefCell<BasicBlock>>> {
    self.bb_list.iter_mut()
  }

  pub fn bb_list(&self) -> impl Iterator<Item = &Rc<RefCell<BasicBlock>>> {
    self.bb_list.iter()
  }

  pub fn print(&self) {
    print!("\n{} {}(", self.ret_ty, self.name);
    for (i, ty) in self.param_ty.iter().enumerate() {
      print!("{} %{}", ty, i);
      if i != self.param_ty.len() - 1 {
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
  }

  pub fn set_parent(&mut self, parent: &Rc<RefCell<Module>>) {
    self.parent = Some(Rc::clone(parent));
  }

  pub fn get_parent(&self) -> Rc<RefCell<Module>> {
    Rc::clone(self.parent.as_ref().expect("No parent"))
  }

  pub fn param_count(&self) -> usize {
    self.param_ty.len()
  }

  pub fn param_data_ty_list(&self) -> impl Iterator<Item = &DataTy> {
    self.param_ty.iter()
  }

  pub fn analyze_use_list(&self) {
    // FIXME: This can only be used on SSA form.

    let mut map: HashMap<Value, Rc<RefCell<Instruction>>> = HashMap::new();
    // let root = self.bb_list().nth(0).unwrap();
    // self.analyze_use_list_impl(root, map);
    for bb in self.bb_list() {
      for inst in bb.borrow().inst_list() {
        // clear use list
        inst.borrow_mut().clear_use_list();
        for op in inst.borrow().src_operand_list() {
          if let Some(def) = map.get(&op) {
            def.borrow_mut().add_to_use_list(inst);
          }
        }

        if let Some(dest) = inst.borrow().get_dest() {
          map.insert(dest, Rc::clone(inst));
        }
      }
    }
  }

  // pub fn analyze_use_list_impl(
  //   &self,
  //   bb: &Rc<RefCell<BasicBlock>>,
  //   mut map: HashMap<Value, Rc<RefCell<Instruction>>>
  // )
  // {
  //   for inst in bb.borrow().inst_list() {
  //     // clear use list
  //     inst.borrow_mut().clear_use_list();
  //     for op in inst.borrow().src_operand_list() {
  //       if let Some(def) = map.get(&op) {
  //         def.borrow_mut().add_to_use_list(inst);
  //       }
  //     }

  //     if let Some(dest) = inst.borrow().get_dest() {
  //       map.insert(dest, Rc::clone(inst));
  //     }
  //   }

  //   for succ in bb.borrow().succs_list() {
  //     self.analyze_use_list_impl(succ, map.clone());
  //   }
  // }
}