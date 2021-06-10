use crate::basicblock::*;
use crate::parser::BinOpType;
use crate::value::Value;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

pub trait Inst {
  fn is_terminate_inst(&self) -> bool;
  fn is_alloca_inst(&self) -> bool;
  fn is_phi_inst(&self) -> bool;
  fn is_store_inst(&self) -> bool;
  fn is_load_inst(&self) -> bool;
  fn is_branch_inst(&self) -> bool;
  fn is_return_inst(&self) -> bool;
  fn is_defining(&self, _: &Rc<RefCell<Value>>) -> bool;
  fn is_using(&self, _: &Rc<RefCell<Value>>) -> bool;
  fn get_operand(&self, _: usize) -> Option<Rc<RefCell<Value>>>;
  fn get_dest(&self) -> Option<Rc<RefCell<Value>>>;
  fn set_dest(&mut self, _: &Rc<RefCell<Value>>);
  fn get_ptr(&self) -> Option<Rc<RefCell<Value>>>;
  fn get_pairs_list_in_phi_mut(&mut self) -> Vec<(&mut Rc<RefCell<Value>>, &mut Rc<RefCell<BasicBlock>>)>;
  fn input_operand_list_mut(&mut self) -> Vec<&mut Rc<RefCell<Value>>>;
  fn print(&self);
}

#[derive(PartialEq, Eq)]
enum InstrTy {
  Alloca,
  Store,
  Load,
  Return,
  Branch,
  Cmp(CmpTy),
  Binary(BinaryTy),
  Call(String),
  Phi,
}

impl fmt::Display for InstrTy {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Alloca     => write!(f, "alloca"),
      Self::Store      => write!(f, "store"),
      Self::Load       => write!(f, "load"),
      Self::Return     => write!(f, "return"),
      Self::Branch     => write!(f, "br"),
      Self::Cmp(ty)    => write!(f, "cmp {}", ty),
      Self::Binary(ty) => write!(f, "{}", ty),
      Self::Call(name) => write!(f, "call @{}", name),
      Self::Phi        => write!(f, "phi"),
    }
  }
}

pub struct Instruction {
  pub parent: Rc<RefCell<BasicBlock>>,
  ty: InstrTy,
  dest: Option<Rc<RefCell<Value>>>,
  op: Vec<Rc<RefCell<Value>>>,
  // used for phi and br
  bb: Vec<Rc<RefCell<BasicBlock>>>,
}

impl Inst for Instruction {
  fn is_terminate_inst(&self) -> bool {
    match self.ty {
      InstrTy::Branch | InstrTy::Return => true,
      _ => false,
    }
  }

  fn is_alloca_inst(&self) -> bool {
    return self.ty == InstrTy::Alloca;
  }

  fn is_phi_inst(&self) -> bool {
    return self.ty == InstrTy::Phi;
  }

  fn is_store_inst(&self) -> bool {
    return self.ty == InstrTy::Store;
  }

  fn is_load_inst(&self) -> bool {
    return self.ty == InstrTy::Load;
  }

  fn is_branch_inst(&self) -> bool {
    return self.ty == InstrTy::Branch;
  }

  fn is_return_inst(&self) -> bool {
    return self.ty == InstrTy::Return;
  }

  fn is_defining(&self, u: &Rc<RefCell<Value>>) -> bool {
    self.dest.as_ref().map_or(false, |v| Rc::ptr_eq(u, v))
  }

  fn is_using(&self, u: &Rc<RefCell<Value>>) -> bool {
    self.op.iter().any(|v| Rc::ptr_eq(u, v))
  }

  fn get_operand(&self, i: usize) -> Option<Rc<RefCell<Value>>> {
    self.op.get(i).cloned()
  }

  fn get_dest(&self) -> Option<Rc<RefCell<Value>>> {
    self.dest.as_ref().cloned()
  }

  fn set_dest(&mut self, u: &Rc<RefCell<Value>>) {
    self.dest = Some(Rc::clone(u));
  }

  fn get_ptr(&self) -> Option<Rc<RefCell<Value>>> {
    if !self.is_alloca_inst() && !self.is_store_inst() {
      panic!("Not alloca or store");
    }
    self.dest.as_ref().cloned()
  }

  fn get_pairs_list_in_phi_mut(&mut self) -> Vec<(&mut Rc<RefCell<Value>>, &mut Rc<RefCell<BasicBlock>>)> {
    if !self.is_phi_inst() {
      panic!("Not phi")
    }
    self.op.iter_mut().zip(self.bb.iter_mut()).collect()
  }

  fn input_operand_list_mut(&mut self) -> Vec<&mut Rc<RefCell<Value>>> {
    self.op.iter_mut().collect()
  }

  fn print(&self) {
    if let Some(dest) = self.dest.as_ref() {
      print!("{} = ", dest.borrow());
    }
    print!("{}", self.ty);
    if !self.is_phi_inst() {
      self.op.iter().for_each(|op| print!(", {}", op.borrow()));
      self.bb.iter().for_each(|bb| print!(", {}", bb.borrow().get_label()));
    } else {
      self.op.iter().zip(self.bb.iter()).for_each(|(op, bb)|{
        print!(", [{}, {}]", op.borrow(), bb.borrow().get_label());
      });
    }
    println!("");
  }
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
    let (op, bb): (Vec<_>, Vec<_>) = pairs.into_iter().unzip();
    Instruction {
      parent,
      ty: InstrTy::Phi,
      dest: Some(dest),
      op,
      bb,
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
      ty: InstrTy::Call(func_name),
      dest,
      op: arg_list,
      bb: Vec::new(),
    }
  }

  pub fn new_alloca_inst(
    dest: Rc<RefCell<Value>>,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent: parent,
      ty: InstrTy::Alloca,
      dest: Some(dest),
      op: vec![],
      bb: vec![],
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
      ty: InstrTy::Store,
      dest: Some(dest),
      op: vec![src],
      bb: vec![],
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
      ty: InstrTy::Load,
      dest: Some(dest),
      op: vec![src],
      bb: vec![],
    }
  }

  pub fn new_binary_inst(
    src1: Rc<RefCell<Value>>,
    src2: Rc<RefCell<Value>>,
    dest: Rc<RefCell<Value>>,
    ty: BinaryTy,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      ty: InstrTy::Binary(ty),
      dest: Some(dest),
      op: vec![src1, src2],
      bb: vec![],
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
      ty: InstrTy::Cmp(ty),
      dest: Some(dest),
      op: vec![op1, op2],
      bb: vec![],
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
    let bb = if condi_br {vec![true_bb, false_bb.unwrap()]} else {vec![true_bb]};
    let op = if condi_br {vec![src.unwrap()]} else {vec![]};
    Instruction {
      parent,
      ty: InstrTy::Branch,
      dest: None,
      op,
      bb,
    }
  }

  pub fn new_ret_inst(
    retval: Option<Rc<RefCell<Value>>>,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction 
  {
    Instruction {
      parent,
      ty: InstrTy::Return,
      op: retval.map_or(vec![], |retval| vec![retval]),
      dest: None,
      bb: vec![],
    }
  }
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(PartialEq, Eq)]
pub enum BinaryTy {
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

impl From<BinOpType> for BinaryTy {
  fn from(binop: BinOpType) -> BinaryTy {
    match binop {
      BinOpType::Plus  => BinaryTy::Add,
      BinOpType::Minus => BinaryTy::Sub,
      BinOpType::Mul   => BinaryTy::Mul,
      BinOpType::Div   => BinaryTy::Div,
      _ => panic!("What the fuck is this operator ????"),
    }
  }
}

impl fmt::Display for BinaryTy {
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
