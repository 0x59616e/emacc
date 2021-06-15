use crate::basicblock::BasicBlock;
use crate::function::Function;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::hash_set::HashSet;
use std::collections::HashMap;
use std::rc::Rc;
use super::Analysis;

pub trait DominatorInfoImpl {
  fn new() -> Self;

  fn initialize<'a, T>(&'a mut self, bb_list: T)
    where T: Iterator<Item = &'a mut Rc<RefCell<BasicBlock>>>;

  fn update<'a, T>(&mut self, bb: &Rc<RefCell<BasicBlock>>, preds_list: T) -> bool
    where T: Iterator<Item = &'a Rc<RefCell<BasicBlock>>>;
  
  fn get_idom(&self, bb: &Rc<RefCell<BasicBlock>>) -> Option<Rc<RefCell<BasicBlock>>>;
  fn get_root(&self) -> Option<Rc<RefCell<BasicBlock>>>;
  fn calc_root(&mut self);
}

//------------DominatorInfo------------
pub struct DominatorInfo<T = DominatorInfoBase> 
where T: DominatorInfoImpl {
  func: Rc<RefCell<Function>>,
  info: Option<T>,
}

impl DominatorInfo {
  pub fn get_root(&self) -> Rc<RefCell<BasicBlock>> {
    DominatorInfoImpl::get_root(self.info.as_ref().unwrap())
                      .unwrap_or(Rc::clone(self.func.borrow().bb_list().collect::<Vec<_>>()[0]))
  }

  pub fn get_idom(&self, bb: &Rc<RefCell<BasicBlock>>) -> Option<Rc<RefCell<BasicBlock>>> {
    DominatorInfoImpl::get_idom(self.info.as_ref().unwrap(), bb)
  }
}

impl<T: DominatorInfoImpl> Analysis for DominatorInfo<T> {
  fn run(&mut self) {
    let mut dom = T::new();

    DominatorInfoImpl::initialize(&mut dom, self.func.borrow_mut().bb_list_mut());

    let mut changed = true;

    while changed {
      changed = false;

      self.func.borrow_mut().bb_list_mut().for_each(|bb| {
        changed = DominatorInfoImpl::update(&mut dom, bb, bb.borrow().preds_list());
      });
    }

    dom.calc_root();
    self.info = Some(dom);
  }

  fn new(func: &Rc<RefCell<Function>>) -> Self {
    DominatorInfo {
      func: Rc::clone(func),
      info: None,
    }
  }
}

//------------DominatorInfoBase------------
pub struct DominatorInfoBase {
  dom: HashMap<Value, HashSet<Value>>,
  tree: HashMap<Value, Rc<RefCell<BasicBlock>>>,
  root: Option<Rc<RefCell<BasicBlock>>>,
}

impl DominatorInfoImpl for DominatorInfoBase {
  fn calc_root(&mut self) {
    for (_, parent) in self.tree.iter() {
      if let None = self.tree.get(&parent.borrow().get_label()) {
        self.root = Some(Rc::clone(parent));
        break;
      }
    }
  }

  fn get_root(&self) -> Option<Rc<RefCell<BasicBlock>>> {
    self.root.as_ref().cloned()
  }

  fn get_idom(&self, bb: &Rc<RefCell<BasicBlock>>) -> Option<Rc<RefCell<BasicBlock>>> {
    self.tree.get(&bb.borrow().get_label()).cloned()
  }

  fn initialize<'a, T>(&'a mut self, bb_list: T)
  where 
    T: Iterator<Item = &'a mut Rc<RefCell<BasicBlock>>>
  {
    let mut bb_list: Vec<_> = bb_list.map(|bb| Rc::clone(bb)).collect();
    let uset = Self::get_universe_set(bb_list.iter_mut());
    bb_list.iter().for_each(|bb| {
      if bb.borrow().is_root() {
        self.insert(bb, Self::turn_single_bb_into_set(bb))
      } else {
        self.insert(bb, uset.clone());
        self.construct_edge(bb, bb.borrow().get_pred().as_ref().expect("No predessor??"));
      }
    });
  }


  fn update<'a, T>(&mut self, bb: &Rc<RefCell<BasicBlock>>, preds_list: T) -> bool
  where
    T: Iterator<Item = &'a Rc<RefCell<BasicBlock>>>
  {
    let (changed, new) = {
      let old = self.get_dominator(bb);
      let mut new = preds_list.map(|bb| self.get_dominator(bb))
                            .fold(old.clone(), |acc, bb| {
                              acc.intersection(&bb).cloned().collect::<HashSet<Value>>()
                            });
      new.insert(bb.borrow().get_label());

      (!new.is_subset(old) || !old.is_subset(&new), new)
    }; 
    
    if let Some(mut parent) = self.get_idom(&bb) {
      while !new.contains(&parent.borrow().get_label()) {
        parent = self.get_idom(&parent).expect("Something goes wrong...");
      }
      self.construct_edge(&bb, &parent);
    }

    self.insert(bb, new);

    changed
  }

  fn new() -> Self {
    Self::new()
  }
}

impl DominatorInfoBase {
  pub fn new() -> DominatorInfoBase {
    DominatorInfoBase {
      dom: HashMap::new(),
      tree: HashMap::new(),
      root: None,
    }
  }

  fn get_dominator(&self, bb: &Rc<RefCell<BasicBlock>>) -> &HashSet<Value> {
    self.dom.get(&bb.borrow().get_label()).expect("No set")
  }

  fn insert(&mut self, key: &Rc<RefCell<BasicBlock>>, val: HashSet<Value>) {
    self.dom.insert(key.borrow().get_label(), val);
  }

  fn construct_edge(&mut self, child: &Rc<RefCell<BasicBlock>>, parent: &Rc<RefCell<BasicBlock>>) {
    self.tree.insert(child.borrow().get_label(), Rc::clone(parent));
  }

  fn get_universe_set<'a, T>(bb_list: T) -> HashSet<Value>
  where
    T: Iterator<Item = &'a mut Rc<RefCell<BasicBlock>>>
  {
    bb_list.map(|bb| bb.borrow().get_label()).collect()
  }

  fn turn_single_bb_into_set(bb: &Rc<RefCell<BasicBlock>>) -> HashSet<Value> {
    vec![bb.borrow().get_label()].into_iter().collect()
  }
}







