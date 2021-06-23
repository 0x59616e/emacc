use crate::ir::basicblock::BasicBlock;
use crate::ir::function::Function;
use crate::ir::value::Value;
use std::cell::RefCell;
use std::collections::{HashSet, HashMap};
use std::rc::Rc;
use super::Analysis;

pub struct LiveOutInfo {
  func: Rc<RefCell<Function>>,
  liveout: HashMap<Value, HashSet<Value>>,
  livein: HashMap<Value, HashSet<Value>>,
  varkill: HashMap<Value, HashSet<Value>>,
}

impl Analysis for LiveOutInfo {
  fn new(func: &Rc<RefCell<Function>>) -> Self {
    LiveOutInfo {
      func: Rc::clone(func),
      liveout: HashMap::new(),
      livein: HashMap::new(),
      varkill: HashMap::new(),
    }
  }

  fn run(&mut self) {
    let mut changed = true;
    self.initialize();

    while changed {
      changed = false;

      let func = Rc::clone(&self.func);

      for bb in func.borrow().bb_list() {
        changed |= self.compute_liveout(bb);
      }
    }

    println!("--------------LiveOut------------");
    for (bb, liveoutset) in self.liveout.iter() {
      print!("{}:", bb);
      for val in liveoutset.iter() {
        print!(" {} |", val);
      }
      println!("");
    }
    println!("--------------tuOeviL------------");
  }
}

impl LiveOutInfo {
  fn compute_liveout(&mut self, bb: &Rc<RefCell<BasicBlock>>) -> bool {
    let mut result = HashSet::new();
    for succ in bb.borrow().succs_list() {
      let tmp = self.get_livethrough(succ);
      let tmp = self.get_livein(succ, Some(bb)).union(&tmp).cloned().collect::<HashSet<_>>();

      result = result.union(&tmp).cloned().collect();
    }

    self.update_liveout(bb, result)
  }

  fn update_liveout(&mut self, bb: &Rc<RefCell<BasicBlock>>, new: HashSet<Value>) -> bool {
    let old = self.liveout.get_mut(&bb.borrow().get_label()).unwrap();
    let changed = !old.is_subset(&new) || !new.is_subset(old);
    *old = new;
    changed
  }

  fn initialize(&mut self) {
    for bb in self.func.borrow().bb_list() {
      self.liveout.insert(bb.borrow().get_label(), HashSet::new());
      self.livein.insert(bb.borrow().get_label(), HashSet::new());
      self.varkill.insert(bb.borrow().get_label(), HashSet::new());
    }
    self.calc_livein_and_varkill();
  }

  fn get_varkill(&self, bb: &Rc<RefCell<BasicBlock>>) -> &HashSet<Value> {
    self.varkill.get(&bb.borrow().get_label()).unwrap()
  }

  pub fn get_liveout(&self, bb: &Rc<RefCell<BasicBlock>>) -> &HashSet<Value> {
    self.liveout.get(&bb.borrow().get_label()).unwrap()
  }

  pub fn get_livein_and_livethrough(&self, bb: &Rc<RefCell<BasicBlock>>) -> HashSet<Value> {
    self.get_livein(bb, None).union(&self.get_livethrough(bb)).cloned().collect()
  }

  pub fn get_livethrough(&self, bb: &Rc<RefCell<BasicBlock>>) -> HashSet<Value> {
    let varkill = self.get_varkill(bb);
    self.get_liveout(bb).iter()
        .filter(|&op| !varkill.contains(op))
        .cloned()
        .collect::<HashSet<_>>()
  }

  pub fn get_livein(
    &self,
    succ: &Rc<RefCell<BasicBlock>>,
    bb: Option<&Rc<RefCell<BasicBlock>>>,
  ) -> HashSet<Value>
  {
    let mut result = self.livein.get(&succ.borrow().get_label()).unwrap().clone();
    for inst in succ.borrow().inst_list() {
      if !inst.borrow().is_phi_inst() {
        break;
      }

      // use in phi only lives out of the bb where it comes from
      if let Some(bb) = bb {
        for (&op, pred) in inst.borrow().get_pairs_list_in_phi() {
          if op.is_vreg() && pred.borrow().get_label() == bb.borrow().get_label() {
            result.insert(op);
            break;
          }
        }
      }
    }
    result
  }

  fn calc_livein_and_varkill(&mut self) {
    let func = Rc::clone(&self.func);

    for bb in func.borrow().bb_list() {
      for inst in bb.borrow().inst_list() {

        if !inst.borrow().is_phi_inst() {
          for op in inst.borrow().src_operand_list() {
            if op.is_reg() && !self.is_killed(bb, op) {
              self.add_to_livein_set(bb, op);
            }
          }
        }

        if let Some(dest) = inst.borrow().get_dest() {
          self.kill(bb, dest);
        }
      }
    }
  }

  fn add_to_livein_set(&mut self, bb: &Rc<RefCell<BasicBlock>>, op: Value) {
    if !op.is_reg() {
      return;
    }
    self.livein.get_mut(&bb.borrow().get_label()).unwrap().insert(op);
  }

  fn is_killed(&self, bb: &Rc<RefCell<BasicBlock>>, op: Value) -> bool {
    if !op.is_reg() {
      panic!("Not a register");
    }
    self.varkill.get(&bb.borrow().get_label()).unwrap().contains(&op)
  }

  fn kill(&mut self, bb: &Rc<RefCell<BasicBlock>>, op: Value) {
    if !op.is_reg() {
      return;
    }
    self.varkill.get_mut(&bb.borrow().get_label()).unwrap().insert(op);
  }
}