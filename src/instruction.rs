use crate::basicblock::*;
use crate::value::Value;
use std::cell::RefCell;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

pub trait Inst {
  fn get_address(&self) -> Option<Value> {
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
  dest: Value,
}

impl Inst for AllocaInst {
  fn print(&self) {
    println!("{} = alloca {}", self.dest, self.dest.get_data_ty())
  }

  fn get_address(&self) -> Option<Value> {
    Some(self.dest)
  }
}



// store the Value's value or constant to memory
// store i32 (%{src} | const), i32* %{dest}, align 4
pub struct StoreInst {
  src : Value,
  dest: Value,
}

impl Inst for StoreInst {
  fn print(&self) {
    println!("store {} {}, {}* {}", self.src.get_data_ty(),
                                    self.src,
                                    self.dest.get_data_ty(),
                                    self.dest);
  }

  fn get_address(&self) -> Option<Value> {
    Some(self.dest)
  }
}

// memory to reister
// %{dest} = load i32, i32* %{src}, align 4
pub struct LoadInst {
  src : Value,
  dest: Value,
}

impl Inst for LoadInst {
  fn print(&self) {
    println!("{} = load {}, {}* {}",self.dest,
                                    self.dest.get_data_ty(),
                                    self.src.get_data_ty(),
                                    self.src)
  }
  fn get_address(&self) -> Option<Value> {
    Some(self.src)
  }
}

pub struct BranchInst {
  condi_br: bool,
  src: Option<Value>,
  true_bb_label: Value,
  false_bb_label: Option<Value>,
}

impl Inst for BranchInst {
  fn print(&self) {
    print!("br ");
    if let Some(src) = self.src {
      print!("{} {}, ", src.get_data_ty(), src);
    }
    print!("label {}", self.true_bb_label);
    if let Some(false_bb_label) = self.false_bb_label {
      print!(", label {}", false_bb_label);
    }
    println!("");
  }

  fn is_terminate_inst(&self) -> bool {
    true
  }
}

pub struct ReturnInst {
  retval: Option<Value>,
}

impl Inst for ReturnInst {
  fn print(&self) {
    print!("ret");
    if let Some(retval) = self.retval {
      println!(" {} {}", retval.get_data_ty(), retval);
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
  dest: Value,
  op1: Value,
  op2: Value,
}

impl Inst for CmpInst {
  fn print(&self) {
    println!("{} = cmp {} {}, {}, {}",self.dest,
                                      self.ty,
                                      self.dest.get_data_ty(),
                                      self.op1,
                                      self.op2);
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
  dest: Value,
  src1: Value,
  src2: Value,
}

impl Inst for BinaryInst {
  fn print(&self) {
    println!("{} = {} {} {}, {}", self.dest,
                                  self.ty,
                                  self.dest.get_data_ty(),
                                  self.src1,
                                  self.src2);
  }
}

pub struct CallInst {
  dest: Option<Value>, // return value
  func_name: String, // function name
  arg_list: Vec<Value>,
}

impl Inst for CallInst {
  fn print(&self) {
    if let Some(dest) = self.dest {
      print!("{} = ", dest);
    }
    print!("call {} @{}(",self.dest.unwrap().get_data_ty(),
                            self.func_name);
    for (i, arg) in self.arg_list.iter().enumerate() {
      print!("{} {}", arg.get_data_ty(), arg);
      if i != self.arg_list.len() - 1 {
        print!(", ");
      }
    }
    println!(")");
  }
}


pub struct PHIInst {
  dest: Value,
  // (value, label)
  pair: Vec<(Value, Value)>,
}

impl Inst for PHIInst {
  fn print(&self) {
    print!("{} = phi {}, ", self.dest, self.dest.get_data_ty());
    for (i, (value, label)) in self.pair.iter().enumerate() {
      print!("[{} {}]", value, label);
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
    dest: Value,
    pair: Vec<(Value, Value)>,
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
    dest: Option<Value>,
    func_name: String,
    arg_list: Vec<Value>,
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
    dest: Value,
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
    src: Value,
    dest: Value,
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
    src: Value,
    dest: Value,
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
    src1: Value,
    src2: Value,
    dest: Value,
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
    op1: Value,
    op2: Value,
    dest: Value,
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
    src: Option<Value>,
    true_bb_label: Value,
    false_bb_label: Option<Value>,
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
    retval: Option<Value>,
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