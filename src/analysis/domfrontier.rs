use crate::basicblock::BasicBlock;
use crate::function::Function;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use super::Analysis;
use super::dominator::DominatorInfo;
use super::get_analysis;

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

  pub fn get_dominator_info_mut(&mut self) -> &mut DominatorInfo {
    &mut self.dom
  }

  pub fn get_dominator_info(&self) -> &DominatorInfo {
    &self.dom
  }

  pub fn get_domfrontier(&self, bb: &Rc<RefCell<BasicBlock>>) -> Vec<Rc<RefCell<BasicBlock>>> {
    self.info.as_ref().expect("No info").get_domfrontier(bb)
  }
}
impl<T> Analysis for DomFrontierInfo<T>
where T: DomFrontierInfoImpl {
  fn run(&mut self) {
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

  fn new(func: &Rc<RefCell<Function>>) -> Self {
    let dom = get_analysis(func);
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
  fn add_frontier(&mut self, frontier: &Rc<RefCell<BasicBlock>>, bb: &Rc<RefCell<BasicBlock>>) {
    if let Some(set) = self.df.get_mut(&bb.borrow().get_label()) {
      set.push(Rc::clone(frontier));
    } else {
      self.df.insert(bb.borrow().get_label(), vec![Rc::clone(frontier)]);
    }
  }

  fn get_domfrontier(&self, bb: &Rc<RefCell<BasicBlock>>) -> Vec<Rc<RefCell<BasicBlock>>> {
    self.df.get(&bb.borrow().get_label()).unwrap_or(&Vec::new()).iter().cloned().collect()
  }
  
  fn new() -> Self {
    DomFrontierInfoBase {
      df: HashMap::new(),
    }
  }

}
