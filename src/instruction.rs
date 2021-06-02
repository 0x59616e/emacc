use crate::basicblock::*;
use crate::value::Value;
use std::cell::RefCell;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

pub trait Inst {
  fn get_address(&self) -> Option<Rc<RefCell<Value>>> {
    None
  }

  fn is_terminate_inst(&self) -> bool {
    false
  }

  fn print(&self);
}

// allocate an address on stack, store that address into %{dest}
// %{dest} = alloca i32, align 4
pub struct AllocaInst {
  dest: Rc<RefCell<Value>>,
}

impl Inst for AllocaInst {
  fn print(&self) {
    println!("{} = alloca {}", self.dest.borrow(), self.dest.borrow().get_data_ty())
  }

  fn get_address(&self) -> Option<Rc<RefCell<Value>>> {
    Some(Rc::clone(&self.dest))
  }
}



// store the Value's value or constant to memory
// store i32 (%{src} | const), i32* %{dest}, align 4
pub struct StoreInst {
  src : Rc<RefCell<Value>>,
  dest: Rc<RefCell<Value>>,
}

impl Inst for StoreInst {
  fn print(&self) {
    println!("store {} {}, {}* {}", self.src.borrow().get_data_ty(),
                                    self.src.borrow(),
                                    self.dest.borrow().get_data_ty(),
                                    self.dest.borrow());
  }

  fn get_address(&self) -> Option<Rc<RefCell<Value>>> {
    Some(Rc::clone(&self.dest))
  }
}

// memory to reister
// %{dest} = load i32, i32* %{src}, align 4
pub struct LoadInst {
  src : Rc<RefCell<Value>>,
  dest: Rc<RefCell<Value>>,
}

impl Inst for LoadInst {
  fn print(&self) {
    println!("{} = load {}, {}* {}",self.dest.borrow(),
                                    self.dest.borrow().get_data_ty(),
                                    self.src.borrow().get_data_ty(),
                                    self.src.borrow())
  }
  fn get_address(&self) -> Option<Rc<RefCell<Value>>> {
    Some(Rc::clone(&self.src))
  }
}

pub struct BranchInst {
  condi_br: bool,
  src: Option<Rc<RefCell<Value>>>,
  true_bb_label: Rc<RefCell<Value>>,
  false_bb_label: Option<Rc<RefCell<Value>>>,
}

impl Inst for BranchInst {
  fn print(&self) {
    print!("br ");
    if let Some(src) = &self.src {
      print!("{} {}, ", src.borrow().get_data_ty(), src.borrow());
    }
    print!("label {}", self.true_bb_label.borrow());
    if let Some(false_bb_label) = &self.false_bb_label {
      print!(", label {}", false_bb_label.borrow());
    }
    println!("");
  }

  fn is_terminate_inst(&self) -> bool {
    true
  }
}

pub struct ReturnInst {
  retval: Option<Rc<RefCell<Value>>>,
}

impl Inst for ReturnInst {
  fn print(&self) {
    print!("ret");
    if let Some(retval) = &self.retval {
      println!(" {} {}", retval.borrow().get_data_ty(), retval.borrow());
    } else {
      println!("");
    }
  }

  fn is_terminate_inst(&self) -> bool {
    true
  }
}

// compare
// %{result} = cmp condi op1, op2
#[derive(Debug)]
pub enum CmpTy {
  Eq, // equal
  Ne, // not equal
  Gt, // greater than
  Ge, // greater than or equal
  Lt, // less than
  Le  // less than or equal
}

impl fmt::Display for CmpTy {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Eq => write!(f, "eq"),
      Self::Ne => write!(f, "ne"),
      Self::Gt => write!(f, "gt"),
      Self::Ge => write!(f, "ge"),
      Self::Lt => write!(f, "lt"),
      Self::Le => write!(f, "le"),
    }
  }
}

// dest = icmp ty op1, op2
pub struct CmpInst {
  ty: CmpTy,
  dest: Rc<RefCell<Value>>,
  op1: Rc<RefCell<Value>>,
  op2: Rc<RefCell<Value>>,
}

impl Inst for CmpInst {
  fn print(&self) {
    println!("{} = cmp {} {}, {}, {}",self.dest.borrow(),
                                      self.ty,
                                      self.dest.borrow().get_data_ty(),
                                      self.op1.borrow(),
                                      self.op2.borrow());
  }
}

pub enum BinaryInstTy {
  Add, // add
  Sub, // sub
  Div, // div
  Mul, // mul
  Shl, // <<
  Shr, // >>
  And, // bitwise &
  Or,  // bitwise |
  Xor, // bitwise ^
}

impl fmt::Display for BinaryInstTy {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Add => write!(f, "add"),
      Self::Sub => write!(f, "sub"),
      Self::Div => write!(f, "div"),
      Self::Mul => write!(f, "mul"),
      Self::Shl => write!(f, "shl"),
      Self::Shr => write!(f, "shr"),
      Self::And => write!(f, "and"),
      Self::Or  => write!(f, "or"),
      Self::Xor => write!(f, "xor"),
    }
  }
}

pub struct BinaryInst {
  ty: BinaryInstTy,
  dest: Rc<RefCell<Value>>,
  src1: Rc<RefCell<Value>>,
  src2: Rc<RefCell<Value>>,
}

impl Inst for BinaryInst {
  fn print(&self) {
    println!("{} = {} {} {}, {}", self.dest.borrow(),
                                  self.ty,
                                  self.dest.borrow().get_data_ty(),
                                  self.src1.borrow(),
                                  self.src2.borrow());
  }
}

pub struct CallInst {
  dest: Option<Rc<RefCell<Value>>>, // return value
  func_name: String, // function name
  arg_list: Vec<Rc<RefCell<Value>>>,
}

impl Inst for CallInst {
  fn print(&self) {
    if let Some(dest) = &self.dest {
      print!("{} = ", dest.borrow());
    }
    print!("call {} @{}(", self.dest.as_ref().unwrap().borrow().get_data_ty(),
                            self.func_name);
    for (i, arg) in self.arg_list.iter().enumerate() {
      print!("{} {}", arg.borrow().get_data_ty(), arg.borrow());
      if i != self.arg_list.len() - 1 {
        print!(", ");
      }
    }
    println!(")");
  }
}


pub struct PHIInst {
  dest: Rc<RefCell<Value>>,
  // (value, label)
  pair: Vec<(Rc<RefCell<Value>>, Rc<RefCell<Value>>)>,
}

impl Inst for PHIInst {
  fn print(&self) {
    print!("{} = phi {}, ", self.dest.borrow(), self.dest.borrow().get_data_ty());
    for (i, (value, label)) in self.pair.iter().enumerate() {
      print!("[{} {}]", value.borrow(), label.borrow());
      if i != self.pair.len() - 1 {
        print!(", ");
      }
    }
    println!("");
  }
}

pub struct Instruction {
  pub parent: Rc<RefCell<BasicBlock>>,
  pub kind: Box<dyn Inst>,
}

impl Instruction {
  pub fn gen_phi_inst(
    dest: Rc<RefCell<Value>>,
    pair: Vec<(Rc<RefCell<Value>>, Rc<RefCell<Value>>)>,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      kind: Box::new(
        PHIInst {
          dest,
          pair,
        }
      )
    }
  }

  pub fn gen_call_inst(
    dest: Option<Rc<RefCell<Value>>>,
    func_name: String,
    arg_list: Vec<Rc<RefCell<Value>>>,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      kind: Box::new(
        CallInst {
          dest,
          func_name,
          arg_list,
        }
      )
    }
  }
  pub fn gen_alloca_inst(
    dest: Rc<RefCell<Value>>,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent: parent,
      kind: Box::new(
        AllocaInst {
          dest,
        }
      ),
    }
  }

  pub fn gen_store_inst(
    src: Rc<RefCell<Value>>,
    dest: Rc<RefCell<Value>>,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      kind: Box::new(
        StoreInst {
          src,
          dest,
        }
      )
    }
  }

  pub fn gen_load_inst(
    src: Rc<RefCell<Value>>,
    dest: Rc<RefCell<Value>>,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      kind: Box::new(
        LoadInst {
          src,
          dest,
        }
      )
    }
  }

  pub fn gen_binary_inst(
    src1: Rc<RefCell<Value>>,
    src2: Rc<RefCell<Value>>,
    dest: Rc<RefCell<Value>>,
    ty: BinaryInstTy,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      kind: Box::new(
        BinaryInst {
          src1,
          src2,
          dest,
          ty,
        }
      )
    }
  }
  pub fn gen_cmp_inst(
    op1: Rc<RefCell<Value>>,
    op2: Rc<RefCell<Value>>,
    dest: Rc<RefCell<Value>>,
    ty: CmpTy,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      kind: Box::new (
        CmpInst {
          ty,
          dest,
          op1,
          op2,
        }
      )
    }
  }

  pub fn gen_br_inst(
    condi_br: bool,
    src: Option<Rc<RefCell<Value>>>,
    true_bb_label: Rc<RefCell<Value>>,
    false_bb_label: Option<Rc<RefCell<Value>>>,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      kind: Box::new(BranchInst {
        condi_br,
        src,
        true_bb_label,
        false_bb_label,
      })
    }
  }

  pub fn gen_ret_inst(
    retval: Option<Rc<RefCell<Value>>>,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction 
  {
    Instruction {
      parent,
      kind: Box::new(ReturnInst {
        retval
      })
    }
  }
}

impl Deref for Instruction {
  type Target = Box<dyn Inst>;

  fn deref(&self) -> &Self::Target {
    &self.kind
  }
}