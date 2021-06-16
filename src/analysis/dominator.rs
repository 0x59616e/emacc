use crate::ir::basicblock::BasicBlock;
use crate::ir::function::Function;
use crate::ir::value::Value;
use std::cell::RefCell;
use std::collections::hash_set::HashSet;
use std::collections::HashMap;
use std::rc::Rc;
use super::Analysis;

//------------DominatorInfo------------
pub struct DominatorInfo {
  func: Rc<RefCell<Function>>,
  dominator: HashMap<Value, HashSet<Value>>,
  child: HashMap<Value, Vec<Rc<RefCell<BasicBlock>>>>,
  parent: HashMap<Value, Rc<RefCell<BasicBlock>>>,
}

impl Analysis for DominatorInfo {
  fn run(&mut self) {

    self.initialize();

    let mut changed = true;
    let func = Rc::clone(&self.func);

    while changed {
      changed = false;

      func.borrow().bb_list().for_each(|bb| {
        changed |= self.update(bb);
      });
    }
    self.construct_dominator_tree();
  }

  fn new(func: &Rc<RefCell<Function>>) -> Self {
    DominatorInfo {
      func: Rc::clone(func),
      dominator: HashMap::new(),
      child: HashMap::new(),
      parent: HashMap::new(),
    }
  }
}


impl DominatorInfo {
  fn construct_dominator_tree(&mut self) {
    let func = Rc::clone(&self.func);
    for bb in func.borrow().bb_list() {

      let parent = self.find_imm_dominator(bb, bb.borrow().preds_list().nth(0));
      if let Some(parent) = parent {
        self.construct_tree_edge(bb, &parent);
      }
    }
  }

  fn find_imm_dominator(
    &self,
    bb: &Rc<RefCell<BasicBlock>>,
    runner: Option<&Rc<RefCell<BasicBlock>>>
  ) -> Option<Rc<RefCell<BasicBlock>>>
  {
    if let Some(runner) = runner {
      if self.is_dominated_by(bb, runner) {
        Some(Rc::clone(runner))
      } else {
        self.find_imm_dominator(bb, runner.borrow().preds_list().nth(0))
      }
    } else {
      None
    }
  }

  fn construct_tree_edge(
    &mut self,
    child: &Rc<RefCell<BasicBlock>>,
    parent: &Rc<RefCell<BasicBlock>>
  )
  {
    self.parent.insert(child.borrow().get_label(), Rc::clone(parent));
    self.child.get_mut(&parent.borrow().get_label()).unwrap().push(Rc::clone(child));
  }

  fn is_dominated_by(&self, bb: &Rc<RefCell<BasicBlock>>, pred: &Rc<RefCell<BasicBlock>>) -> bool {
    self.dominator.get(&bb.borrow().get_label()).unwrap().contains(&pred.borrow().get_label())
  }

  fn update(&mut self, bb: &Rc<RefCell<BasicBlock>>) -> bool {
    // intersection of all predessors
    let (changed, new) = {
      let old = self.dominator.get(&bb.borrow().get_label()).unwrap();
      let mut new = bb.borrow().preds_list()
                          .map(|bb| self.dominator.get(&bb.borrow().get_label()).unwrap())
                          .fold(old.clone(), |acc, dom| {
                              acc.intersection(&dom).cloned().collect()
                          });
      new.insert(bb.borrow().get_label());
      let changed = !old.is_subset(&new) || !new.is_subset(old);
      (changed, new)
    };
    self.dominator.insert(bb.borrow().get_label(), new);
    changed
  }

  fn initialize(&mut self) {
    for bb in self.func.borrow().bb_list() {
      let init_dominator_set = if bb.borrow().preds_list().count() == 0 {
        let mut tmp = HashSet::new();
        tmp.insert(bb.borrow().get_label());
        tmp
      } else {
        self.func.borrow().bb_list()
                          .map(|bb| bb.borrow().get_label())
                          .collect::<HashSet<_>>()
      };
      self.dominator.insert(bb.borrow().get_label(), init_dominator_set);
      self.child.insert(bb.borrow().get_label(), Vec::new());
    }
  }

  pub fn get_root(&self) -> Rc<RefCell<BasicBlock>> {
    for bb in self.func.borrow().bb_list() {
      if let None = self.parent.get(&bb.borrow().get_label()) {
        return Rc::clone(bb);
      }
    }
    panic!("Can't find root");
  }

  pub fn get_idom(&self, bb: &Rc<RefCell<BasicBlock>>) -> Option<Rc<RefCell<BasicBlock>>> {
    self.parent.get(&bb.borrow().get_label()).cloned()
  }
}