use super::basicblock::*;
use crate::parser::BinOpType;
use crate::ir::value::Value;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(PartialEq, Eq)]
pub enum InstrTy {
  Asm(String),
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
      Self::Asm(name)        => write!(f, "{}", name),
    }
  }
}

pub struct Instruction {
  pub parent: Rc<RefCell<BasicBlock>>,
  ty: InstrTy,
  // use for code lowering
  mi_ty: u8,
  dest: Option<Value>,
  op: Vec<Value>,
  uselist: Vec<Rc<RefCell<Instruction>>>,
  // used for phi and br
  bb: Vec<Rc<RefCell<BasicBlock>>>,
}

impl Instruction {
  pub fn set_asm_ty(&mut self, (ty, name): (u8, String)) {
    self.ty = InstrTy::Asm(name);
    self.mi_ty = ty;
  }

  pub fn get_inst_ty(&self) -> &InstrTy {
    &self.ty
  }

  pub fn is_terminator(&self) -> bool {
    self.is_branch_inst() || self.is_return_inst()
  }

  pub fn is_alloca_inst(&self) -> bool {
    return self.ty == InstrTy::Alloca;
  }

  pub fn is_phi_inst(&self) -> bool {
    return self.ty == InstrTy::Phi;
  }

  pub fn is_store_inst(&self) -> bool {
    return self.ty == InstrTy::Store;
  }

  pub fn is_load_inst(&self) -> bool {
    return self.ty == InstrTy::Load;
  }

  pub fn is_branch_inst(&self) -> bool {
    return self.ty == InstrTy::Branch;
  }

  pub fn is_condi_br(&self) -> bool {
    self.is_branch_inst() && self.bb.len() == 2
  }

  pub fn is_return_inst(&self) -> bool {
    return self.ty == InstrTy::Return;
  }

  pub fn is_defining(&self, u: Value) -> bool {
    self.dest.map_or(false, |v| u == v)
  }

  pub fn is_using(&self, u: Value) -> bool {
    self.op.iter().any(|v| u == *v)
  }

  pub fn get_operand(&self, i: usize) -> Option<Value> {
    self.op.get(i).cloned()
  }

  pub fn get_bb(&self, i: usize) -> Option<Rc<RefCell<BasicBlock>>> {
    self.bb.get(i).cloned()
  }

  pub fn get_dest(&self) -> Option<Value> {
    self.dest
  }

  pub fn set_dest(&mut self, u: Value) {
    self.dest = Some(u);
  }

  pub fn get_ptr(&self) -> Option<Value> {
    if self.is_alloca_inst() || self.is_store_inst() {
      return self.dest;
    } else if self.is_load_inst() {
      return self.get_operand(0);
    }
    panic!("Not alloca, store or load");
  }

  pub fn get_pairs_list_in_phi_mut(&mut self) -> Vec<(&mut Value, &mut Rc<RefCell<BasicBlock>>)> {
    if !self.is_phi_inst() {
      panic!("Not phi")
    }
    self.op.iter_mut().zip(self.bb.iter_mut()).collect()
  }

  pub fn get_pairs_list_in_phi(&self) -> Vec<(&Value, &Rc<RefCell<BasicBlock>>)> {
    if !self.is_phi_inst() {
      panic!("Not phi");
    }

    self.op.iter().zip(self.bb.iter()).collect()
  }

  pub fn src_operand_list_mut(&mut self) -> impl Iterator<Item = &mut Value> {
    self.op.iter_mut()
  }

  pub fn src_operand_list(&self) -> Vec<Value> {
    self.op.iter().cloned().collect()
  }

  pub fn add_to_use_list(
    &mut self,
    inst: &Rc<RefCell<Instruction>>
  )
  {
    self.uselist.push(Rc::clone(inst));
  }

  fn replace_op(&mut self, old_op: Value, new_op: Value) {
    *self.op.iter_mut().find(|op| **op == old_op).expect("No op") = new_op;
  }

  pub fn replace_all_use_with(&mut self, old_op: Value, new_op: Value) {
    self.set_dest(new_op);
    for inst in self.uselist.iter() {
      inst.borrow_mut().replace_op(old_op, new_op);
    }
  }

  pub fn clear_use_list(&mut self) {
    self.uselist.clear();
  }

  pub fn print(&self) {
    if let Some(dest) = self.dest {
      print!("{} = ", dest);
    }
    print!("{}", self.ty);
    if !self.is_phi_inst() {
      self.op.iter().for_each(|op| print!(", {}", op));
      self.bb.iter().for_each(|bb| print!(", {}", bb.borrow().get_label()));
    } else {
      self.op.iter().zip(self.bb.iter()).for_each(|(op, bb)|{
        print!(", [{}, {}]", op, bb.borrow().get_label());
      });
    }
    println!("");
  }

  pub fn get_parent(&self) -> Rc<RefCell<BasicBlock>> {
    Rc::clone(&self.parent)
  }

  pub fn new_phi_inst(
    dest: Value,
    pairs: Vec<(Value, Rc<RefCell<BasicBlock>>)>,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    let (op, bb): (Vec<_>, Vec<_>) = pairs.into_iter().unzip();
    Instruction {
      parent,
      ty: InstrTy::Phi,
      mi_ty: 0,
      uselist: vec![],
      dest: Some(dest),
      op,
      bb,
    }
  }

  pub fn add_operand(mut self, op: Value) -> Instruction {
    self.op.push(op);
    self
  }

  pub fn add_bb(mut self, bb: Rc<RefCell<BasicBlock>>) -> Instruction {
    self.bb.push(bb);
    self
  }

  pub fn new_asm_inst((mi_ty, name): (u8, String), parent: Rc<RefCell<BasicBlock>>) -> Instruction {
    Instruction {
      parent,
      ty: InstrTy::Asm(name),
      mi_ty,
      dest: None,
      op: vec![],
      uselist: vec![],
      bb: vec![],
    }
  }

  pub fn new_call_inst(
    dest: Option<Value>,
    func_name: String,
    arg_list: Vec<Value>,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      ty: InstrTy::Call(func_name),
      mi_ty: 0,
      dest,
      op: arg_list,
      uselist: vec![],
      bb: Vec::new(),
    }
  }

  pub fn new_alloca_inst(
    dest: Value,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent: parent,
      ty: InstrTy::Alloca,
      mi_ty: 0,
      dest: Some(dest),
      uselist: vec![],
      op: vec![],
      bb: vec![],
    }
  }

  pub fn new_store_inst(
    src: Value,
    dest: Value,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      ty: InstrTy::Store,
      mi_ty: 0,
      dest: Some(dest),
      uselist: vec![],
      op: vec![src],
      bb: vec![],
    }
  }

  pub fn new_load_inst(
    src: Value,
    dest: Value,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      ty: InstrTy::Load,
      mi_ty: 0,
      dest: Some(dest),
      uselist: vec![],
      op: vec![src],
      bb: vec![],
    }
  }

  pub fn new_binary_inst(
    src1: Value,
    src2: Value,
    dest: Value,
    ty: BinaryTy,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      ty: InstrTy::Binary(ty),
      mi_ty: 0,
      dest: Some(dest),
      uselist: vec![],
      op: vec![src1, src2],
      bb: vec![],
    }
  }

  pub fn new_cmp_inst(
    op1: Value,
    op2: Value,
    dest: Value,
    ty: CmpTy,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction
  {
    Instruction {
      parent,
      ty: InstrTy::Cmp(ty),
      mi_ty: 0,
      dest: Some(dest),
      uselist: vec![],
      op: vec![op1, op2],
      bb: vec![],
    }
  }

  pub fn new_br_inst(
    condi_br: bool,
    src: Option<Value>,
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
      mi_ty: 0,
      dest: None,
      op,
      uselist: vec![],
      bb,
    }
  }

  pub fn new_ret_inst(
    retval: Option<Value>,
    parent: Rc<RefCell<BasicBlock>>
  ) -> Instruction 
  {
    Instruction {
      parent,
      ty: InstrTy::Return,
      mi_ty: 0,
      op: retval.map_or(vec![], |retval| vec![retval]),
      dest: None,
      bb: vec![],
      uselist: vec![],
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

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum BinaryTy {
  Add, // add
  Sub, // sub
  Div, // div
  Mul, // mul
  // Shl, // <<
  // Shr, // >>
  // And, // bitwise &
  // Or,  // bitwise |
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
      // Self::Shl => write!(f, "shl"),
      // Self::Shr => write!(f, "shr"),
      // Self::And => write!(f, "and"),
      // Self::Or  => write!(f, "or"),
      Self::Xor => write!(f, "xor"),
    }
  }
}
