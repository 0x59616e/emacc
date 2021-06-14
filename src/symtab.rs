use crate::value::Value;
use std::collections::BTreeMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

pub type ScopeID = i32;

#[derive(Copy, Clone, Debug)]
pub enum Type {
  Function,
  Void,
  Int,
}

#[derive(Clone)]
pub struct SymTabEntry {
  pub name: String,
  pub entry_type: Type,
  pub scope_id: Option<ScopeID>,
  pub is_parm: Option<usize>,
  pub addr: Option<Value>,
  // function
  pub return_type: Option<Type>,
  pub prototype: Option<Vec<Type>>,
}

impl SymTabEntry {
  pub fn get_name(&self) -> String {
    return self.name.clone();
  }

  pub fn get_type(&self) -> Type {
    return self.entry_type;
  }

  pub fn get_prototype(&self) -> &Vec<Type> {
    self.prototype.as_ref().expect("No prototype")
  }

  pub fn get_address(&self) -> Option<Value> {
    self.addr
  }

  pub fn get_return_type(&self) -> Type {
    self.return_type.unwrap()
  }

  pub fn set_scope(&mut self, scope_id: ScopeID) {
    self.scope_id = Some(scope_id);
  }

  pub fn set_name(&mut self, name: String) {
    self.name = name;
  }

  pub fn set_as_parm(&mut self, order: usize) {
    self.is_parm = Some(order);
  }

  pub fn set_as_func(&mut self) {
    self.return_type = Some(self.entry_type);
    self.entry_type = Type::Function;
  }

  pub fn set_func_prototype(&mut self, prototype: Vec<Type>) {
    self.prototype = Some(prototype);
  }

  pub fn set_address(&mut self, dest: Value) {
    self.addr = Some(dest);
  }
}

pub struct SymTab {
  scope_stack: Vec<ScopeID>,
  pub decl_set: Vec<BTreeMap<String, Rc<RefCell<SymTabEntry>>>>,
}

impl SymTab {
  pub fn new() -> SymTab {
    SymTab {
      scope_stack: vec![0],
      decl_set: vec![BTreeMap::new()],
    }
  }

  pub fn get_curr_scope(&self) -> ScopeID {
    *self.scope_stack.last().unwrap()
  }

  pub fn insert_local_var(&mut self, local_var: Rc<RefCell<SymTabEntry>>) {
    local_var.borrow_mut().set_scope(self.get_curr_scope());
    let name = local_var.borrow().get_name();
    let scope = self.get_curr_scope();
    self.decl_set[scope as usize].insert(name, local_var);
  }

  pub fn insert_global_var(&mut self, global_var: Rc<RefCell<SymTabEntry>>) {
    let global_var_borrow = global_var.borrow();
    let name = global_var_borrow.get_name();
    drop(global_var_borrow);
    self.decl_set[0].insert(name, global_var);
  }

  pub fn find_var(&self, name: &String) -> Option<&Rc<RefCell<SymTabEntry>>> {
    for &scope_id in self.scope_stack.iter().rev() {
      let result = self.decl_set[scope_id as usize].get(name);
      if let Some(_) = result {
        return result;
      }
    }
    None
  }

  pub fn _find_global_def(&self, name: &String) -> Option<&Rc<RefCell<SymTabEntry>>> {
    self.decl_set[0].get(name)
  }

  pub fn enter_new_scope(&mut self) {
    self.scope_stack.push(self.decl_set.len() as ScopeID);
    self.decl_set.push(BTreeMap::new());
  }

  pub fn leave_scope(&mut self) {
    self.scope_stack.pop();
    // There must be a global scope there
    assert_ne!(self.scope_stack.len(), 0);
  }
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Type::Int => write!(f, "int"),
      _ => write!(f, ""),    
    }
  }
}

impl fmt::Display for SymTabEntry {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.get_name())?;
    match self.get_type() {
      Type::Int => write!(f, " 'int'"),
      Type::Function => {
        write!(f, " '{} ()(", self.get_return_type())?;
        let vec = self.prototype.as_ref().unwrap();
        let len = vec.len();
        for (i, item) in vec.iter().enumerate() {
          write!(f, "{}", item);
          if i != len - 1 {
            write!(f, ", ")?;
          }
        }
        write!(f, ")'")
      }
      _ => panic!("'void' currently not supported")
    }
  }
}
