use crate::basicblock::BasicBlock;
use crate::domfrontier::DomFrontierInfo;
use crate::function::Function;
use crate::instruction::Instruction;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

pub struct PromoteMemoryToReg {
  func: Rc<RefCell<Function>>,
  dfinfo: DomFrontierInfo,
  visited: Vec<Rc<RefCell<BasicBlock>>>,
}

impl PromoteMemoryToReg {

  pub fn run(&mut self) {
    let allocas = self.pick_alloca();
    
    let mut curr_val = vec![];
    let root = self.dfinfo.get_dominator_info_mut().get_root();

    allocas.into_iter().for_each(|alloca|{
      Self::add_new_mapping(
        &mut curr_val,
        &alloca.borrow().get_ptr().expect("No ptr"),
        &Rc::new(RefCell::new(Value::new_undef()))
      );
      self.insert_phi_function(&alloca);
      root.borrow_mut().remove_inst(&alloca);
    });

    self.rename(root, curr_val, &mut 0);
  }

  fn rename(
    &mut self,
    bb: Rc<RefCell<BasicBlock>>,
    mut curr_val: Vec<(Rc<RefCell<Value>>, Rc<RefCell<Value>>)>, // (old, new)
    i: &mut usize,
  )
  {
    if self.has_visited(&bb) {
      return;
    }

    // First, rename the basic block
    bb.borrow_mut().set_label(Value::new_label(*i));
    *i += 1;

    self.add_to_visited_set(&bb);
    // For each instruction...rename it...
    let mut remove_list = vec![];
    for inst in bb.borrow().inst_list() {
      if inst.borrow().is_phi_inst() {
        if let Some(ptr) = Self::get_ptr_this_inst_is_defining(inst, &mut curr_val) {
          let new_reg = Rc::new(RefCell::new(Value::new_register(*i, ptr.borrow().get_data_ty(), false)));
          *i += 1;
          inst.borrow_mut().set_dest(&new_reg);
          Self::update_curr_mapping(ptr, new_reg);
        }
      } else if inst.borrow().is_store_inst() {
        if let Some(ptr) = Self::get_ptr_this_inst_is_defining(inst, &mut curr_val) {
          Self::update_curr_mapping(ptr, inst.borrow().get_operand(0).unwrap());
          remove_list.push(Rc::clone(inst));
        }
      } else if inst.borrow().is_load_inst() {
        if let Some(ptr) = Self::get_ptr_this_inst_is_using(inst, &mut curr_val) {
          let ptr = Rc::clone(ptr);
          Self::add_new_mapping(&mut curr_val, &mut inst.borrow().get_dest().expect("No dest"), &ptr);
          remove_list.push(Rc::clone(inst));
        }
      } else {
        for op in inst.borrow_mut().input_operand_list_mut() {
          if let Some((_, new)) = curr_val.iter().rev().find(|(old, _)| *op.borrow() == *old.borrow()) {
            *op = Rc::clone(new);
          }
        }

        if let Some(dest) = inst.borrow().get_dest() {
          let new_reg = Value::new_register(*i, dest.borrow().get_data_ty(), false);
          *i += 1;
          dest.replace(new_reg);
        }
      }
    }
    // Saigo, phi in successor...
    Self::update_phi_in_successor(&bb, &mut curr_val);

    for inst in remove_list {
      bb.borrow_mut().remove_inst(&inst);
    }

    // move on...
    for succ in bb.borrow().succs_list() {
      self.rename(Rc::clone(succ), curr_val.iter().cloned().collect(), i);
    }
  }

  fn add_new_mapping(
    curr_val: &mut Vec<(Rc<RefCell<Value>>, Rc<RefCell<Value>>)>,
    old: &Rc<RefCell<Value>>,
    new: &Rc<RefCell<Value>>)
  {
    curr_val.push((Rc::clone(old), Rc::clone(new)));
  }

  fn has_visited(&self, bb: &Rc<RefCell<BasicBlock>>) -> bool {
    self.visited.iter().any(|x| Rc::ptr_eq(bb, x))
  }

  fn add_to_visited_set(&mut self, bb: &Rc<RefCell<BasicBlock>>) {
    self.visited.push(Rc::clone(bb));
  }

  fn update_phi_in_successor(
    bb: &Rc<RefCell<BasicBlock>>,
    curr_val: &mut [(Rc<RefCell<Value>>, Rc<RefCell<Value>>)]
  )
  {
    for succ in bb.borrow().succs_list() {
      for inst in succ.borrow().inst_list() {
        if !inst.borrow().is_phi_inst() {
          break;
        }

        inst.borrow_mut().get_pairs_list_in_phi_mut()
                          .into_iter()
                          .filter(|(_, x)| Rc::ptr_eq(bb, x))
                          .for_each(|(ptr1, _)| {
                            if let Some((_, new)) = curr_val.iter().find(|(ptr2, _)| Rc::ptr_eq(ptr1, ptr2)) {
                              *ptr1 = Rc::clone(new);
                            }
                          });
      }
    }
  }

  fn update_curr_mapping(ptr: &mut Rc<RefCell<Value>>, new: Rc<RefCell<Value>>) {
    *ptr = new;
  }

  fn get_ptr_this_inst_is_using<'a> (
    inst: &Rc<RefCell<Instruction>>,
    curr_val: &'a mut [(Rc<RefCell<Value>>, Rc<RefCell<Value>>)]
  ) -> Option<&'a mut Rc<RefCell<Value>>>
  {
    curr_val.iter_mut().find(|(ptr, _)| inst.borrow().is_using(ptr)).map(|(_, x)| x)
  }

  fn get_ptr_this_inst_is_defining<'a>(
    inst: &Rc<RefCell<Instruction>>,
    curr_val: &'a mut [(Rc<RefCell<Value>>,Rc<RefCell<Value>>)]
  ) -> Option<&'a mut Rc<RefCell<Value>>>
  {
    curr_val.iter_mut().find(|(ptr, _)| inst.borrow().is_defining(ptr)).map(|(_, x)| x)
  }

  fn insert_phi_function(&self, alloca: &Rc<RefCell<Instruction>>) {
    // we need to get the defining blocks
    let mut defblocks = vec![];
    let ptr = alloca.borrow().get_ptr().expect("No ptr in alloca?");
    for bb in self.func.borrow().bb_list() {
      for inst in bb.borrow().inst_list() {
        if inst.borrow().is_store_inst() && inst.borrow().is_defining(&ptr) {
          defblocks.push(Rc::clone(bb));
          break;
        }
      }
    }
    // insert phi function...
    let mut done: HashSet<Value> = HashSet::new();
    while !defblocks.is_empty() {
      let mut new_defblocks = vec![];
      defblocks.into_iter()
                .map(|bb| self.dfinfo.get_domfrontier(&bb).into_iter())
                .flatten()
                .for_each(|bb| {
                  if !done.contains(&bb.borrow().get_label()) {
                    let pairs = bb.borrow()
                                  .preds_list()
                                  .map(|bb|(Rc::clone(&ptr), Rc::clone(bb)))
                                  .collect::<Vec<_>>();
                    let phi = Instruction::new_phi_inst(Rc::clone(&ptr),
                                                        pairs,
                                                        Rc::clone(&bb));
                    bb.borrow_mut().insert_at_head(phi);
                    done.insert(bb.borrow().get_label());
                    new_defblocks.push(bb);
                  }
                });
      defblocks = new_defblocks;
    }
  }

  fn pick_alloca(&mut self) -> Vec<Rc<RefCell<Instruction>>> {
    // just pick all of them...
    let root_bb = self.dfinfo.get_dominator_info_mut().get_root();
    return root_bb.borrow().inst_list()
                            .filter(|inst| inst.borrow().is_alloca_inst())
                            .cloned()
                            .collect::<Vec<_>>();
  }

  pub fn new(func: &Rc<RefCell<Function>>, dfinfo: DomFrontierInfo) -> PromoteMemoryToReg {
    PromoteMemoryToReg {
      func: Rc::clone(func),
      dfinfo,
      visited: vec![],
    }
  }
}
