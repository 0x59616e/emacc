use crate::codegen::target::RegisterSaver;
use crate::symtab::Type;
use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DataTy {
  I1,
  I32,
}

impl From<Type> for DataTy {
  fn from(ty: Type) -> DataTy {
    match ty {
      Type::Int => DataTy::I32,
      _ => panic!("only int suppported, found: {:?}", ty),
    }
  }
}

impl fmt::Display for DataTy {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::I1 => write!(f, "i1"),
      Self::I32 => write!(f, "i32"),
    }
  }
}

#[derive(Clone, Copy, Hash, PartialEq, PartialOrd, Ord, Eq)]
pub enum ValueTy {
  Label {
    label_num: usize,
  },
  VReg {
    reg_num: usize,
    ty: DataTy,
    is_stack_slot: bool,
    is_tmp: bool,
  },
  Const_I32(i32, DataTy),
  Const_I1(bool, DataTy),
  PhyReg(u8, Option<RegisterSaver>),
  StackMemAddr(u16),
  Undef,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Value {
  ty: ValueTy,
}

impl fmt::Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.ty {
      ValueTy::VReg{reg_num, ty, is_stack_slot, is_tmp} => {
        write!(f, "%")?;
        if is_tmp {write!(f, ".")?;}
        write!(f, "{} {}", reg_num, ty)?;
        if is_stack_slot {write!(f, "*")} else {write!(f, "")}
      },
      ValueTy::Const_I32(value, ty)         => write!(f, "{} {}", value, ty),
      ValueTy::Const_I1(value, ty)          => write!(f, "{} {}", value, ty),
      ValueTy::Label{label_num}             => write!(f, "label %{}", label_num),
      ValueTy::Undef                        => write!(f, "undef"),
      ValueTy::PhyReg(i, _)                 => write!(f, "x{}", i),
      _ => panic!("uh ??"),
    }
  }
}

impl Value {
  pub fn get_data_ty(&self) -> DataTy {
    match self.ty {
      ValueTy::VReg{ty, ..}     | 
      ValueTy::Const_I32(_, ty) | 
      ValueTy::Const_I1(_, ty)  => ty,
      _ => panic!("This is a label c'mon")
    }
  }

  pub fn neg(&mut self) {
    match &mut self.ty {
      ValueTy::Const_I32(value, _) => {
        *value = -*value;
      }
      _ => (),
    }
  }

  // FIXME: Is there any better way to do this ?
  pub fn plus(&mut self, add: i64) {
    match &mut self.ty {
      ValueTy::Const_I32(val,..) => {
        *val += add as i32;
      }
      _ => panic!(""),
    }
  }

  pub fn is_callee_saved(&self) -> bool {
    assert!(self.is_phy_reg());
    self.check_register_saver(RegisterSaver::Callee)
  }

  pub fn is_caller_saved(&self) -> bool {
    assert!(self.is_phy_reg());
    self.check_register_saver(RegisterSaver::Caller)
  }

  fn check_register_saver(&self, ty: RegisterSaver) -> bool {
    match self.ty {
      ValueTy::PhyReg(_, Some(saver)) => saver == ty,
      _ => false,
    }
  }

  pub fn is_reg(&self) -> bool {
    self.is_phy_reg() || self.is_vreg()
  }

  pub fn get_phyreg_num(&self) -> u8 {
    match self.ty {
      ValueTy::PhyReg(num, _) => num,
      _ => panic!("Not a physical register"),
    }
  }

  pub fn is_phy_reg(&self) -> bool {
    match self.ty {
      ValueTy::PhyReg(..) => true,
      _ => false,
    }
  }

  pub fn is_label(&self) -> bool {
    match self.ty {
      ValueTy::Label{..} => true,
      _ => false,
    }
  }

  pub fn is_vreg(&self) -> bool {
    match self.ty {
      ValueTy::VReg{is_stack_slot, ..} => !is_stack_slot,
      _ => false,
    }
  }
  // FIXME: We need a better way to manage constant value
  pub fn get_const_i32(&self) -> i32 {
    match self.ty {
      ValueTy::Const_I32(val, _) => val,
      _ => panic!("Not a i32 const")
    }
  }

  pub fn is_const(&self) -> bool {
    match self.ty {
      ValueTy::Const_I1(..) | ValueTy::Const_I32(..) => true,
      _ => false,
    }
  }
  // FIXME: Is there any better way to do this ?
  pub fn _new_const(ty: DataTy, value: i64) -> Value {
    match ty {
      DataTy::I32 => Value::new_const_i32(value as i32),
      DataTy::I1  => Value::new_const_i1(value != 0),
    }
  }

  pub fn new_const_i1(value: bool) -> Value {
    Value {
      ty: ValueTy::Const_I1(value, DataTy::I1),
    }
  }

  pub fn new_const_i32(value: i32) -> Value {
    Value {
      ty: ValueTy::Const_I32(value, DataTy::I32),
    }
  }

  pub fn new_label(label_num: usize) -> Value {
    Value {
      ty: ValueTy::Label{label_num}
    }
  }

  pub fn new_phyreg(reg_num: u8, saver: Option<RegisterSaver>) -> Value {
    Value {
      ty: ValueTy::PhyReg(reg_num, saver)
    }
  }

  pub fn new_tmp_vreg(mut reg: Value) -> Value {
    match &mut reg.ty {
      ValueTy::VReg {is_tmp, ..} => {
        assert!(!*is_tmp);
        *is_tmp = true;
        reg
      }
      _ => panic!("Not a register"),
    }
  }

  pub fn new_vreg(reg_num: usize, ty: DataTy, is_stack_slot: bool) -> Value {
    Value {
      ty: ValueTy::VReg {
        reg_num,
        ty, // currently only `int` supported
        is_stack_slot,
        is_tmp: false,
      }
    }
  }

  pub fn new_undef() -> Value {
    Value {
      ty: ValueTy::Undef,
    }
  }
}