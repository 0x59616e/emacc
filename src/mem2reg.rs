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
    let allocas = self.pick_allocas();
    
    let mut curr_def: Vec<(Value, Value)> = vec![];
    let root = self.dfinfo.get_dominator_info().get_root();

    allocas.into_iter().for_each(|alloca|{
      Self::add_new_mapping(
        &mut curr_def,
        alloca.borrow().get_ptr().expect("No ptr"),
        Value::new_undef(),
      );
      self.insert_phi_function(&alloca);
      root.borrow_mut().remove_inst(&alloca);
    });
    let mut i = self.func.borrow().param_count();
    self.rename(root, curr_def, &mut i);
    self.func.borrow().analyze_use_list();
  }

  fn rename(
    &mut self,
    bb: Rc<RefCell<BasicBlock>>,
    mut curr_map: Vec<(Value, Value)>, // (old, new)
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
        let ptr = inst.borrow().get_dest().unwrap();
        let new_reg = Value::new_vreg(*i, inst.borrow().get_dest().unwrap().get_data_ty(), false);
        *i += 1;
        inst.borrow_mut().set_dest(new_reg);
        Self::update_curr_mapping(ptr, new_reg, &mut curr_map);
        inst.borrow_mut().replace_all_use_with(ptr, new_reg);
      } else if inst.borrow().is_store_inst() {
        let ptr = inst.borrow().get_ptr().unwrap();
        let new_val = inst.borrow().get_operand(0).unwrap();
        Self::update_curr_mapping(ptr, new_val, &mut curr_map);
        remove_list.push(Rc::clone(inst));
      } else if inst.borrow().is_load_inst() {
        let ptr = inst.borrow().get_ptr().unwrap();
        let curr_val = Self::get_curr_val_inst_of_ptr(ptr, &curr_map);
        let old_val = inst.borrow().get_dest().unwrap();
        inst.borrow_mut().replace_all_use_with(old_val, curr_val);
        remove_list.push(Rc::clone(inst));
      } else {
        let dest = inst.borrow().get_dest();
        if let Some(dest) = dest {
          let new_reg = Value::new_vreg(*i, dest.get_data_ty(), false);
          *i += 1;
          inst.borrow_mut().set_dest(new_reg);
          inst.borrow_mut().replace_all_use_with(dest, new_reg);
        }
      }
    }

    for inst in remove_list {
      bb.borrow_mut().remove_inst(&inst);
    }
    
    // Saigo, phi in successor...
    Self::update_phi_in_successor(&bb, &mut curr_map);

    // move on...
    for succ in bb.borrow().succs_list() {
      self.rename(Rc::clone(succ), curr_map.iter().cloned().collect(), i);
    }
  }

  fn add_new_mapping(
    curr_val: &mut Vec<(Value, Value)>,
    old: Value,
    new: Value
  )
  {
    curr_val.push((old, new));
  }

  fn has_visited(&self, bb: &Rc<RefCell<BasicBlock>>) -> bool {
    self.visited.iter().any(|x| Rc::ptr_eq(bb, x))
  }

  fn add_to_visited_set(&mut self, bb: &Rc<RefCell<BasicBlock>>) {
    self.visited.push(Rc::clone(bb));
  }

  fn update_phi_in_successor(
    bb: &Rc<RefCell<BasicBlock>>,
    curr_val: &mut [(Value, Value)]
  )
  {
    for succ in bb.borrow().succs_list() {
      for inst in succ.borrow().inst_list() {
        if !inst.borrow().is_phi_inst() {
          break;
        }

        let mut inst = inst.borrow_mut();
        let ptr = inst.get_pairs_list_in_phi_mut()
                          .into_iter()
                          .find(|(_, x)| Rc::ptr_eq(bb, x)).unwrap().0;

        if let Some((_, new_val)) = curr_val.iter().find(|(ptr2, _)| *ptr == *ptr2) {
          *ptr = *new_val;
        }
      }
    }
  }

  fn update_curr_mapping(
    ptr: Value,
    new_val: Value,
    curr_def: &mut [(Value, Value)]
  )
  {
    if let Some((_, curr_val)) = curr_def.iter_mut().find(|(v, _)| *v == ptr) {
      *curr_val = new_val;
    }
  }

  fn get_curr_val_inst_of_ptr(
    ptr: Value,
    curr_def: &[(Value, Value)]
  ) -> Value
  {
    curr_def.iter().find(|(v, _curr_val)| *v == ptr).unwrap().1
  }

  fn insert_phi_function(&self, alloca: &Rc<RefCell<Instruction>>) {
    // we need to get the defining blocks
    let mut defblocks = vec![];
    let ptr = alloca.borrow().get_ptr().expect("No ptr in alloca?");
    for bb in self.func.borrow().bb_list() {
      for inst in bb.borrow().inst_list() {
        if inst.borrow().is_store_inst() && inst.borrow().is_defining(ptr) {
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
                                  .map(|bb|(ptr, Rc::clone(bb)))
                                  .collect::<Vec<_>>();
                    let phi = Instruction::new_phi_inst(ptr,
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

  fn pick_allocas(&mut self) -> Vec<Rc<RefCell<Instruction>>> {
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
