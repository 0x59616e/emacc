use crate::basicblock::BasicBlock;
use crate::dominator::DominatorInfo;
use crate::function::Function;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub trait DomFrontierInfoImpl {
  fn new() -> Self;

  fn add_frontier(&mut self, frontier: &Rc<RefCell<BasicBlock>>, bb: &Rc<RefCell<BasicBlock>>);
  fn get_domfrontier(&self, bb: &Rc<RefCell<BasicBlock>>) -> Vec<Rc<RefCell<BasicBlock>>>;
}

pub struct DomFrontierInfo<T = DomFrontierInfoBase>
where T: DomFrontierInfoImpl {
  func: Rc<RefCell<Function>>,
  dom: DominatorInfo,
  info: Option<T>,
}

impl<T> DomFrontierInfo<T>
where T: DomFrontierInfoImpl {
  pub fn get_dominator(&self, bb: &Rc<RefCell<BasicBlock>>) -> Vec<Rc<RefCell<BasicBlock>>> {
    DomFrontierInfoImpl::get_domfrontier(self.info.as_ref().unwrap(), bb)
  }

  pub fn run(&mut self) {
    /* every join points must be one of dominator frontier 
     * of its predessor and its idom of its predessor
     */
    let mut dfinfo = T::new();

    self.func.borrow().bb_list()
                      .filter(|&bb| bb.borrow()
                      .is_join_points())
                      .for_each(|bb| {
                        bb.borrow().preds_list()
                                    .map(|pred| Rc::clone(pred))
                                    .for_each(|mut pred| {
                                      let idom = self.dom.get_idom(bb).expect("Join points must have idom");
                                      while !Rc::ptr_eq(&idom, &pred) {
                                        dfinfo.add_frontier(bb, &pred);
                                        pred = self.dom.get_idom(&pred).expect("No idom ? weird");
                                      }
                                    });
                      });
    self.info = Some(dfinfo);
  }

  pub fn new(func: &Rc<RefCell<Function>>, dom: DominatorInfo) -> DomFrontierInfo<T> {
    DomFrontierInfo {
      func: Rc::clone(func),
      dom,
      info: None,
    }
  }
}

//------------------
pub struct DomFrontierInfoBase {
  df: HashMap<Value, Vec<Rc<RefCell<BasicBlock>>>>,
}

impl DomFrontierInfoImpl for DomFrontierInfoBase {
  fn new() -> Self {
    DomFrontierInfoBase {
      df: HashMap::new(),
    }
  }

  fn add_frontier(&mut self, frontier: &Rc<RefCell<BasicBlock>>, bb: &Rc<RefCell<BasicBlock>>) {
    if let Some(set) = self.df.get_mut(&bb.borrow().get_label()) {
      set.push(Rc::clone(frontier));
    } else {
      self.df.insert(bb.borrow().get_label(), vec![Rc::clone(frontier)]);
    }
  }

  fn get_domfrontier(&self, bb: &Rc<RefCell<BasicBlock>>) -> Vec<Rc<RefCell<BasicBlock>>> {
    if let Some(res) = self.df.get(&bb.borrow().get_label()) {
      res.iter().cloned().collect()
    } else {
      vec![]
    }
  }
}
