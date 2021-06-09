use crate::basicblock::*;
use crate::parser::BinOpType;
use crate::value::Value;
use std::cell::RefCell;
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

pub trait Inst {
  fn is_terminate_inst(&self) -> bool {
    false
  }
  fn is_alloca_inst(&self) -> bool {
    false
  }
  fn is_phi_inst(&self) -> bool {
    false
  }
  fn is_store_inst(&self) -> bool {
    false
  }
  fn is_load_inst(&self) -> bool {
    false
  }
  fn is_branch_inst(&self) -> bool {
    false
  }
  fn is_return_inst(&self) -> bool {
    false
  }
  fn is_defining(&self, _: &Rc<RefCell<Value>>) -> bool;
  fn is_using(&self, _: &Rc<RefCell<Value>>) -> bool;
  fn get_operand(&self, _: usize) -> Option<Rc<RefCell<Value>>>;
  fn get_dest(&self) -> Option<Rc<RefCell<Value>>>;
  fn set_dest(&mut self, _: &Rc<RefCell<Value>>);
  fn get_ptr(&self) -> Option<Rc<RefCell<Value>>> {
    panic!("Only valid for alloca & load");
  }

  fn get_pairs_list_in_phi_mut(&mut self) -> Vec<&mut (Rc<RefCell<Value>>, Rc<RefCell<BasicBlock>>)> {
    panic!("Only for phi");
  }

  fn input_operand_list_mut(&mut self) -> Vec<&mut Rc<RefCell<Value>>>;

  fn print(&self);
}

// allocate an address on stack, store that address into %{dest}
// %{dest} = alloca i32, align 4
pub struct AllocaInst {
  dest: Rc<RefCell<Value>>,
}

impl Inst for AllocaInst {
  fn is_alloca_inst(&self) -> bool {
    return true;
  }

  fn get_ptr(&self) -> Option<Rc<RefCell<Value>>> {
    Some(Rc::clone(&self.dest))
  }

  fn get_operand(&self, _: usize) -> Option<Rc<RefCell<Value>>> {
    None
  }

  fn get_dest(&self) -> Option<Rc<RefCell<Value>>> {
    None
  }

  fn is_defining(&self, _: &Rc<RefCell<Value>>) -> bool {
    false
  }

  fn is_using(&self, _: &Rc<RefCell<Value>>) -> bool {
    false
  }

  fn set_dest(&mut self, _: &Rc<RefCell<Value>>) {
    unimplemented!();
  }

  fn input_operand_list_mut(&mut self) -> Vec<&mut Rc<RefCell<Value>>> {
    vec![]
  }

  fn print(&self) {
    println!("{} = alloca {}", self.dest.borrow(), self.dest.borrow().get_data_ty())
  }
}



// store the Value's value or constant to memory
// store i32 (%{src} | const), i32* %{dest}, align 4
pub struct StoreInst {
  src : Rc<RefCell<Value>>,
  dest: Rc<RefCell<Value>>,
}

impl Inst for StoreInst {
  fn is_store_inst(&self) -> bool {
    true
  }

  fn is_defining(&self, ptr: &Rc<RefCell<Value>>) -> bool {
    *self.dest.borrow() == *ptr.borrow()
  }

  fn is_using(&self, v: &Rc<RefCell<Value>>) -> bool {
    *self.src.borrow() == *v.borrow()
  }

  fn get_operand(&self, i: usize) -> Option<Rc<RefCell<Value>>> {
    if i == 0 {Some(Rc::clone(&self.src))} else {None}
  }

  fn get_dest(&self) -> Option<Rc<RefCell<Value>>> {
    Some(Rc::clone(&self.dest))
  }

  fn set_dest(&mut self, _: &Rc<RefCell<Value>>) {
    unimplemented!();
  }

  fn input_operand_list_mut(&mut self) -> Vec<&mut Rc<RefCell<Value>>> {
    vec![&mut self.src]
  }

  fn print(&self) {
    println!("store {} {}, {}* {}", self.src.borrow().get_data_ty(),
                                    self.src.borrow(),
                                    self.dest.borrow().get_data_ty(),
                                    self.dest.borrow());
  }

  fn get_ptr(&self) -> Option<Rc<RefCell<Value>>> {
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
  fn is_load_inst(&self) -> bool {
    true
  }

  fn is_defining(&self, v: &Rc<RefCell<Value>>) -> bool {
    *v.borrow() == *self.dest.borrow()
  }

  fn is_using(&self, v: &Rc<RefCell<Value>>) -> bool {
    *self.src.borrow() == *v.borrow()
  }

  fn get_operand(&self, i: usize) -> Option<Rc<RefCell<Value>>> {
    if i == 0 {Some(Rc::clone(&self.src))} else {None}
  }

  fn get_dest(&self) -> Option<Rc<RefCell<Value>>> {
    Some(Rc::clone(&self.dest))
  }

  fn get_ptr(&self) -> Option<Rc<RefCell<Value>>> {
    Some(Rc::clone(&self.src))
  }

  fn set_dest(&mut self, _: &Rc<RefCell<Value>>) {
    unimplemented!();
  }

  fn input_operand_list_mut(&mut self) -> Vec<&mut Rc<RefCell<Value>>> {
    vec![&mut self.src]
  }

  fn print(&self) {
    println!("{} = load {}, {}* {}",self.dest.borrow(),
                                    self.dest.borrow().get_data_ty(),
                                    self.src.borrow().get_data_ty(),
                                    self.src.borrow())
  }
}

pub struct BranchInst {
  condi_br: bool,
  src: Option<Rc<RefCell<Value>>>,
  true_bb: Rc<RefCell<BasicBlock>>,
  false_bb: Option<Rc<RefCell<BasicBlock>>>,
}

impl Inst for BranchInst {
  fn is_branch_inst(&self) -> bool {
    true
  }

  fn is_defining(&self, _: &Rc<RefCell<Value>>) -> bool {
    false
  }

  fn is_using(&self, v: &Rc<RefCell<Value>>) -> bool {
    self.condi_br && *v.borrow() == *self.src.as_ref().unwrap().borrow()
  }

  fn get_operand(&self, i: usize) -> Option<Rc<RefCell<Value>>> {
    if i == 0 {self.src.as_ref().cloned()} else {None}
  }

  fn get_dest(&self) -> Option<Rc<RefCell<Value>>> {
    None
  }

  fn set_dest(&mut self, _: &Rc<RefCell<Value>>) {
    panic!("Branch has no output register");
  }

  fn print(&self) {
    print!("br ");
    if let Some(src) = &self.src {
      print!("{} {}, ", src.borrow().get_data_ty(), src.borrow());
    }
    print!("label {}", self.true_bb.borrow().get_label());
    if let Some(false_bb) = &self.false_bb {
      print!(", label {}", false_bb.borrow().get_label());
    }
    println!("");
  }

  fn input_operand_list_mut(&mut self) -> Vec<&mut Rc<RefCell<Value>>> {
    self.src.as_mut().map_or(Vec::new(), |src| vec![src])
  }

  fn is_terminate_inst(&self) -> bool {
    true
  }
}

pub struct ReturnInst {
  retval: Option<Rc<RefCell<Value>>>,
}

impl Inst for ReturnInst {
  fn is_return_inst(&self) -> bool {
    true
  }

  fn is_defining(&self, _: &Rc<RefCell<Value>>) -> bool {
    false
  }

  fn is_using(&self, v: &Rc<RefCell<Value>>) -> bool {
    self.retval.as_ref().map_or(false, |retval| *v.borrow() == *retval.borrow())
  }

  fn get_operand(&self, i: usize) -> Option<Rc<RefCell<Value>>> {
    self.retval.as_ref().map_or(None, |retval| {
      if i == 1 {
        Some(Rc::clone(retval))
      } else {
        None
      }
    })
  }

  fn get_dest(&self) -> Option<Rc<RefCell<Value>>> {
    None
  }

  fn set_dest(&mut self, _: &Rc<RefCell<Value>>) {
    panic!("Return has no output register");
  }

  fn input_operand_list_mut(&mut self) -> Vec<&mut Rc<RefCell<Value>>> {
    self.retval.as_mut().map_or(Vec::new(), |retval| vec![retval])
  }

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

impl From<BinOpType> for CmpTy {
  fn from(binop: BinOpType) -> CmpTy {
    match binop {
      BinOpType::Equal        => CmpTy::Eq,
      BinOpType::NotEqual     => CmpTy::Ne,
      BinOpType::GreaterEqual => CmpTy::Ge,
      BinOpType::Greater      => CmpTy::Gt,
      BinOpType::LessEqual    => CmpTy::Le,
      BinOpType::Less         => CmpTy::Lt,
      _ => panic!("What the fuck is this operator ????"),
    }
  }
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
  fn is_defining(&self, v: &Rc<RefCell<Value>>) -> bool {
    *self.dest.borrow() == *v.borrow()
  }

  fn is_using(&self, v: &Rc<RefCell<Value>>) -> bool {
    *self.op1.borrow() == *v.borrow() ||
    *self.op2.borrow() == *v.borrow()
  }

  fn get_operand(&self, i: usize) -> Option<Rc<RefCell<Value>>> {
    match i {
      0 => Some(Rc::clone(&self.op1)),
      1 => Some(Rc::clone(&self.op2)),
      _ => None,
    }
  }

  fn get_dest(&self) -> Option<Rc<RefCell<Value>>> {
    Some(Rc::clone(&self.dest))
  }

  fn set_dest(&mut self, new_dest: &Rc<RefCell<Value>>) {
    self.dest = Rc::clone(new_dest);
  }

  fn input_operand_list_mut(&mut self) -> Vec<&mut Rc<RefCell<Value>>> {
    vec![&mut self.op1, &mut self.op2]
  }

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

impl From<BinOpType> for BinaryInstTy {
  fn from(binop: BinOpType) -> BinaryInstTy {
    match binop {
      BinOpType::Plus  => BinaryInstTy::Add,
      BinOpType::Minus => BinaryInstTy::Sub,
      BinOpType::Mul   => BinaryInstTy::Mul,
      BinOpType::Div   => BinaryInstTy::Div,
      _ => panic!("What the fuck is this operator ????"),
    }
  }
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
  fn is_defining(&self, v: &Rc<RefCell<Value>>) -> bool {
    *self.dest.borrow() == *v.borrow()
  }

  fn is_using(&self, v: &Rc<RefCell<Value>>) -> bool {
    *self.src1.borrow() == *v.borrow() ||
    *self.src2.borrow() == *v.borrow()
  }

  fn get_operand(&self, i: usize) -> Option<Rc<RefCell<Value>>> {
    match i {
      0 => Some(Rc::clone(&self.src1)),
      1 => Some(Rc::clone(&self.src2)),
      _ => None
    }
  }

  fn get_dest(&self) -> Option<Rc<RefCell<Value>>> {
    Some(Rc::clone(&self.dest))
  }

  fn set_dest(&mut self, new_dest: &Rc<RefCell<Value>>) {
    self.dest = Rc::clone(new_dest);
  }

  fn input_operand_list_mut(&mut self) -> Vec<&mut Rc<RefCell<Value>>> {
    vec![&mut self.src1, &mut self.src2]
  }

  fn print(&self) {
    println!("{} = {} {}, {}, {}", self.dest.borrow(),
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
  fn is_defining(&self, v: &Rc<RefCell<Value>>) -> bool {
    self.dest.as_ref().map_or(false, |dest| *dest.borrow() == *v.borrow())
  }

  fn is_using(&self, v: &Rc<RefCell<Value>>) -> bool {
    self.arg_list.iter().any(|arg| *arg.borrow() == *v.borrow())
  }

  fn get_operand(&self, i: usize) -> Option<Rc<RefCell<Value>>> {
    self.arg_list.get(i).cloned()
  }

  fn get_dest(&self) -> Option<Rc<RefCell<Value>>> {
    self.dest.as_ref().cloned()
  }

  fn set_dest(&mut self, new_dest: &Rc<RefCell<Value>>) {
    if let None = self.dest {
      return;
    }
    self.dest = Some(Rc::clone(new_dest));
  }

  fn input_operand_list_mut(&mut self) -> Vec<&mut Rc<RefCell<Value>>> {
    self.arg_list.iter_mut().collect()
  }

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
  // (Register | Constant, BB)
  pairs: Vec<(Rc<RefCell<Value>>, Rc<RefCell<BasicBlock>>)>,
}

impl Inst for PHIInst {
  fn is_phi_inst(&self) -> bool {
    true
  }

  fn is_defining(&self, v: &Rc<RefCell<Value>>) -> bool {
    *self.dest.borrow() == *v.borrow()
  }

  fn is_using(&self, v: &Rc<RefCell<Value>>) -> bool {
    self.pairs.iter().any(|(p, _)| *v.borrow() == *p.borrow())
  }

  fn get_operand(&self, i: usize) -> Option<Rc<RefCell<Value>>> {
    self.pairs.iter().map(|(v, _)| v).nth(i).cloned()
  }

  fn get_dest(&self) -> Option<Rc<RefCell<Value>>> {
    Some(Rc::clone(&self.dest))
  }

  fn set_dest(&mut self, new_dest: &Rc<RefCell<Value>>) {
    self.dest = Rc::clone(new_dest);
  }

  fn input_operand_list_mut(&mut self) -> Vec<&mut Rc<RefCell<Value>>> {
    self.pairs.iter_mut().map(|(x, _)| x).collect()
  }

  fn get_pairs_list_in_phi_mut<'a>(&mut self) -> Vec<&mut (Rc<RefCell<Value>>, Rc<RefCell<BasicBlock>>)> {
    self.pairs.iter_mut().collect::<Vec<_>>()
  }

  fn print(&self) {
    print!("{} = phi {}, ", self.dest.borrow(), self.dest.borrow().get_data_ty());
    for (i, (value, bb)) in self.pairs.iter().enumerate() {
      print!("[{} {}]", value.borrow(), bb.borrow().get_label());
      if i != self.pairs.len() - 1 {
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
  pub fn get_parent(&self) -> Rc<RefCell<BasicBlock>> {
    Rc::clone(&self.parent)
  }

  pub fn new_phi_inst(
    dest: Rc<RefCell<Value>>,
    pairs: Vec<(Rc<RefCell<Value>>, Rc<RefCell<BasicBlock>>)>,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      kind: Box::new(
        PHIInst {
          dest,
          pairs,
        }
      )
    }
  }

  pub fn new_call_inst(
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
  pub fn new_alloca_inst(
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

  pub fn new_store_inst(
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

  pub fn new_load_inst(
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

  pub fn new_binary_inst(
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
  pub fn new_cmp_inst(
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

  pub fn new_br_inst(
    condi_br: bool,
    src: Option<Rc<RefCell<Value>>>,
    true_bb: Rc<RefCell<BasicBlock>>,
    false_bb: Option<Rc<RefCell<BasicBlock>>>,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      kind: Box::new(BranchInst {
        condi_br,
        src,
        true_bb,
        false_bb,
      })
    }
  }

  pub fn new_ret_inst(
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

impl DerefMut for Instruction {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.kind
  }
}