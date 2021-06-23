use crate::analysis::{run_analysis, liveout::LiveOutInfo};
use crate::ir::function::Function;
use crate::ir::instruction::Instruction;
use crate::ir::value::Value;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;
use super::index::{Index, IndexBuilder};
use super::liverange::LiveRange;
use super::target::RegisterSaver;


pub struct LiveInterval {
  liveranges: Vec<LiveRange>,
  def_point: Vec<Index>,
  // registers in this live interval
  values: Vec<Value>,
  // The register to which this live interval must be binded.
  // If failed, stop register allocation.
  bind: Option<Value>,
  // register preference
  reg_class_prefer: RegisterSaver,
  // The final result
  alloc: Value,
}

impl LiveInterval {
  pub fn get_reg_class_preference(&self) -> RegisterSaver {
    self.reg_class_prefer
  }

  pub fn set_reg_class_preference(&mut self, prefer: RegisterSaver) {
    self.reg_class_prefer = prefer;
  }
  pub fn set_binding(&mut self, bind: Value) {
    self.bind = Some(bind);
  }

  pub fn get_binding(&self) -> Option<Value> {
    self.bind
  }

  pub fn get_alloc(&self) -> Value {
    self.alloc
  }

  pub fn alloc(&mut self, val: Value) {
    self.alloc = val;
  }

  pub fn merge(&mut self, mut li: &Rc<RefCell<LiveInterval>>) {
    // FIXME: check conflict
    self.liveranges.append(&mut li.borrow_mut().liveranges);
    self.def_point.append(&mut li.borrow_mut().def_point);
    self.values.append(&mut li.borrow_mut().values);
  }

  pub fn create_empty_live_range(&mut self, idx: Index) {
    // create a empty live range [idx, idx)
    self.def_point.push(idx);
    self.liveranges.push(LiveRange::new(idx, idx));
  }

  pub fn extend_live_range(&mut self, idx: Index) {
    // extend the last live range to idx)
    self.liveranges.last_mut().unwrap().extend_to(idx);
  }

  pub fn new(val: Value) -> Self {
    let alloc = if val.is_phy_reg() {
      val
    } else {
      Value::new_undef()
    };

    LiveInterval {
      liveranges: Vec::new(),
      def_point: Vec::new(),
      values: vec![val],
      bind: None,
      reg_class_prefer: RegisterSaver::Caller,
      alloc,
    }
  }

  pub fn include(&self, idx: Index) -> bool {
    for lr in self.lr_list() {
      if lr.include(idx) {
        return true;
      }
    }
    return false;
  }

  pub fn conflict_with(&self, other: &Self) -> bool {
    for lr in self.lr_list() {
      if lr.conflict_with(other) {
        return true;
      }
    }

    return false;
  }

  pub fn def_point_list(&self) -> impl Iterator<Item = &Index> {
    self.def_point.iter()
  }

  pub fn lr_list(&self) -> impl Iterator<Item = &LiveRange> {
    self.liveranges.iter()
  }

  pub fn print(&self) {
    println!("LiveInterval:");
    print!("reg:");
    for vreg in self.values.iter() {
      print!(" {} |", vreg);
    }

    print!("\ndef point:");
    for defp in self.def_point.iter() {
      print!(" {} |", defp);
    }

    print!("\nlive range:");
    for lr in self.liveranges.iter() {
      print!(" {}", lr);
    }

    print!("\nbind:");
    if let Some(bind) = self.bind {
      print!(" {}", bind);
    }

    print!("\nalloc:");
    println!("{}", self.alloc);
    println!("\n");
  }
}

pub struct LiveIntervalBuilder {
  func: Rc<RefCell<Function>>,
  li_info: LiveIntervalInfo,
}

impl LiveIntervalBuilder {
  pub fn run(mut self) -> LiveIntervalInfo {

    let func = Rc::clone(&self.func);
  
    let liveout_info = run_analysis::<LiveOutInfo>(&func);
    let mut idxbuilder = IndexBuilder::new();

    for bb in func.borrow().bb_list() {
print!("{}: ", bb.borrow().get_label());
      // create a new empty live range from the start for every live-in variables
      // and live-through variables
      let start_idx = idxbuilder.create_new_index();
print!("[{}, ", start_idx);
      for op in liveout_info.get_livein_and_livethrough(bb) {
        self.create_empty_live_range(op, start_idx);
      }

      let mut current_idx = start_idx;
      for inst in bb.borrow().inst_list() {
        // --> Phi instruction are all at the start of the block semantically.
        // --> We don't care about the uses in phi.
        //     They are all deeeeeeeeeaaaaaaaaad.
        if !inst.borrow().is_phi_inst() {
          current_idx = idxbuilder.create_new_index();

          for op in inst.borrow().src_operand_list() {
            self.extend_live_range(op, current_idx);
          }

          if inst.borrow().is_call_inst() {
            self.li_info.add_func_call_point(current_idx, inst);
          }
        }

        if let Some(dest) = inst.borrow().get_dest() {
          // create a new empty range from here for dest
          self.create_empty_live_range(dest, current_idx);
        }
      }

      // extend the op's live range in live out set to
      // the end of the block
      let end_idx = idxbuilder.create_new_index();
println!("{})", end_idx);
      for &op in liveout_info.get_liveout(bb) {
        self.extend_live_range(op, end_idx);
      }
    }

    // merge live interval for phi's uses and def
    for bb in func.borrow().bb_list() {
      for inst in bb.borrow().inst_list() {
        if !inst.borrow().is_phi_inst() {
          break;
        }

        inst.borrow().src_operand_list()
                      .into_iter()
                      .zip(inst.borrow().src_operand_list().into_iter().skip(1))
                      .for_each(|(op1, op2)|
        {
          self.merge_live_interval(op1, op2);
        });
        self.merge_live_interval(inst.borrow().get_dest().unwrap(),
                                 inst.borrow().get_operand(0).unwrap());
      }
    }
    self.li_info.calc_register_preference();
    self.li_info
  }

  fn merge_live_interval(&mut self, op1: Value, op2: Value) {
    if !op1.is_reg() || !op2.is_reg() {
      return;
    }

    let mut li1 = self.remove_live_interval(op1);
    let li2 = self.remove_live_interval(op2);
    li1.borrow_mut().merge(&li2);
    self.li_info.map.insert(op2, op1);
    self.li_info.li.insert(op1, Rc::clone(&li1));
  }

  fn remove_live_interval(&mut self, op: Value) -> Rc<RefCell<LiveInterval>> {
    assert!(op.is_reg());

    let op2 = *self.li_info.map.get(&op).unwrap();
    if op == op2 {
      self.li_info.li.remove(&op).unwrap()
    } else {
      self.remove_live_interval(op2)
    }
  }

  fn extend_live_range(&mut self, op: Value, idx: Index) {
    if !op.is_reg() {
      return;
    }
    // extend the live range of op to idx)
    self.li_info.li.get_mut(&op).unwrap().borrow_mut().extend_live_range(idx);
  }

  fn create_empty_live_range(&mut self, op: Value, idx: Index) {
    if !op.is_reg() {
      return;
    }

    // create a [idx, idx) range for op
    if let None = self.li_info.li.get(&op) {
      self.li_info.li.insert(op, Rc::new(RefCell::new(LiveInterval::new(op))));
      self.li_info.map.insert(op, op);
    }

    self.li_info.li.get_mut(&op).unwrap().borrow_mut().create_empty_live_range(idx);
  }

  pub fn new(func: &Rc<RefCell<Function>>) -> Self {
    LiveIntervalBuilder {
      func: Rc::clone(func),
      li_info: LiveIntervalInfo::new(),
    }
  }
}

pub struct LiveIntervalInfo {
  pub(super) li: BTreeMap<Value, Rc<RefCell<LiveInterval>>>,
  pub(super) map: BTreeMap<Value, Value>,
  // Used for generating store and load for caller-saved registers
  pub(super) func_call_points: Vec<(Index, Rc<RefCell<Instruction>>)>,
}

impl LiveIntervalInfo {
  pub fn print(&self) {
    for li in self.li.values() {
      li.borrow().print();
    }
  }

  fn add_func_call_point(&mut self, point: Index, inst: &Rc<RefCell<Instruction>>) {
    self.func_call_points.push((point, Rc::clone(inst)));
  }

  pub fn calc_register_preference(&mut self) {
    let fcps = self.func_call_points.iter().map(|(i, _)| *i).collect::<Vec<_>>();

    for li in self.li.values_mut() {
      for fcp in fcps.iter() {
        let tmp = li.borrow().include(*fcp);
        if tmp {
          li.borrow_mut().set_reg_class_preference(RegisterSaver::Callee);
        }
      }
    }
  }

  pub fn li_list(&self) -> impl Iterator<Item = &Rc<RefCell<LiveInterval>>> {
    self.li.values()
  }

  pub fn get_li(&self, op: &Value) -> Rc<RefCell<LiveInterval>> {
    let op2 = self.map.get(op).unwrap();
    if *op2 == *op {
      return Rc::clone(self.li.get(op).unwrap());
    } else {
      return self.get_li(op2);
    }
  }

  pub fn new() -> Self {
    Self {
      li: BTreeMap::new(),
      map: BTreeMap::new(),
      func_call_points: Vec::new(),
    }
  }
}