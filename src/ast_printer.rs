use crate::parser::*;
use std::fmt;

impl fmt::Display for ObjectKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ObjectKind::Int => write!(f, "int"),
      _ => write!(f, "")
    }
  }
}

impl fmt::Display for Object {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} ", self.name);
    match self.kind {
      ObjectKind::Int => write!(f, "'{}'", self.kind),
      ObjectKind::Function => {
        write!(f, "{}", self.return_type.unwrap())?;
        write!(f, " (")?;
        let mut iter = self.para_list.as_ref().unwrap().iter().peekable();
        while let Some(object) = iter.next() {
          write!(f, "{}", object.kind)?;
          if let Some(_) = iter.peek() {
            write!(f, ", ")?;
          }
        }
        write!(f, ")")
      }
      _ => write!(f, "")
    }
  }
}

impl fmt::Display for BinOpType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      BinOpType::Comma => write!(f, ","),
      BinOpType::Assignment => write!(f, "="),
      BinOpType::Plus => write!(f, "+"),
      BinOpType::Minus => write!(f, "-"),
      BinOpType::Mul => write!(f, "*"),
      BinOpType::Div => write!(f, "/"),
    }
  }
}

pub fn print_ast(prefix: i32, node: &Node, level: i32) {
  let mut s: Vec<u8> = Vec::new();
  for i in 0..level {
    s.push(b' ');
    if (prefix >> i) & 1 == 1 {
      s.push(b'|');
    } else {
      s.push(b' ');
    }
  }
  let s = String::from_utf8(s).unwrap();

  match node.get_kind() {
    NodeKind::Program {func_or_decl_list} => {
      println!("-Program");
      for (i, node) in func_or_decl_list.iter().enumerate() {
        print!("{}", s);
        if i != func_or_decl_list.len() - 1 {
          print!(" |");
          print_ast(prefix | (1 << level), node, level + 1);
        } else {
          print!(" `");
          print_ast(prefix, node, level + 1);
        }
      }
    }
    NodeKind::FunctionDefinition{prototype, body} => {
      println!("-FunctionDecl: {}", prototype);
      print!("{} `", s);
      print_ast(prefix, &*body, level + 1);
    }
    NodeKind::VarDecl{object, init} => {
      println!("-VarDecl: {}", object);
      if let Some(expr) = init {
        print!("{} `", s);
        print_ast(prefix, &(*expr.init_expr), level + 1);
      }
    }
    NodeKind::CompoundStmt{body} => {
      println!("-CompoundStmt");
      for (i, node) in body.iter().enumerate() {
        print!("{}", s);
        if i != body.len() - 1 {
          print!(" |");
          print_ast(prefix | (1 << level), node, level + 1);
        } else {
          print!(" `");
          print_ast(prefix, node, level + 1);
        }
      }
    }
    NodeKind::ReturnStmt{expr} => {
      println!("-ReturnStmt");
      if let Some(expr) = expr {
        print!("{} `", s);
        print_ast(prefix, &*expr, level + 1);
      }
    }
    NodeKind::BinOpExpr {op, lhs, rhs} => {
      println!("-BinOpExpr: '{}'", op);
      print!("{} |", s);
      print_ast(prefix | (1 << level), &*lhs, level + 1);
      print!("{} `", s);
      print_ast(prefix, &*rhs, level + 1);
    }
    NodeKind::CallExpr{func, arg_list} => {
      println!("-CallExpr: '{}'", func);
      for (i, node) in arg_list.iter().enumerate() {
        print!("{}", s);
        if i != arg_list.len() - 1 {
          print!(" |");
          print_ast(prefix | (1 << level), node, level + 1);
        } else {
          print!(" `");
          print_ast(prefix, node, level + 1);
        }
      }
    }
    NodeKind::UnaryExpr(node) => {
      println!("-UnaryExpr");
      print!("{} `", s);
      print_ast(prefix, &*node, level + 1);
    }
    NodeKind::Variable(object) => {
      println!("-Var: {}", object);
    }
    NodeKind::Constant(v) => {
      println!("-Constant: {}", v);
    }
  }
}