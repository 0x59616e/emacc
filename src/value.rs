use crate::symtab::Type;
use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[derive()]
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

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub enum ValueTy {
  Label {
    label_num: usize,
  },
  Register {
    reg_num: usize,
    ty: DataTy,
    is_ptr: bool,
  },
  Const {
    value: u64,
    ty: DataTy,
  },
  Undef,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Value {
  ty: ValueTy,
}

impl fmt::Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.ty {
      ValueTy::Register{reg_num, ..} => write!(f, "%{}", reg_num),
      ValueTy::Const{value, ..} => write!(f, "{}", value),
      ValueTy::Label{label_num} => write!(f, "%{}", label_num),
      ValueTy::Undef => write!(f, "undef"),
    }
  }
}

impl Value {
  pub fn get_data_ty(&self) -> DataTy {
    match self.ty {
      ValueTy::Register{ty, ..} | ValueTy::Const{ty, ..} => ty,
      _ => panic!("This is a label c'mon")
    }
  }

  pub fn new_const_i1(value: bool) -> Value {
    Value {
      ty: ValueTy::Const {
        value: value as u64,
        ty: DataTy::I1,
      }
    }
  }

  pub fn new_const_i32(value: i32) -> Value {
    Value {
      ty: ValueTy::Const {
        value: value as u64,
        ty: DataTy::I32
      }
    }
  }

  pub fn new_label(label_num: usize) -> Value {
    Value {
      ty: ValueTy::Label{label_num}
    }
  }

  pub fn new_register(reg_num: usize, ty: DataTy, is_ptr: bool) -> Value {
    Value {
      ty: ValueTy::Register {
        reg_num,
        ty, // currently only `int` supported
        is_ptr,
      }
    }
  }

  pub fn new_undef() -> Value {
    Value {
      ty: ValueTy::Undef,
    }
  }
}