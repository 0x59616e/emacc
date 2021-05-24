use crate::parser::*;

// The controlling expression of an if statement shall have scalar type
pub fn check_on_if_stmt(node: &Node) -> bool {
  if let NodeKind::IfStmt {cond_expr, ..} = node.get_kind() {
    is_scalar_type(cond_expr)
  } else {
    false
  }
}

fn is_scalar_type(node: &Node) -> bool {
  match node.get_kind() {
    NodeKind::BinOpExpr{op, lhs, rhs} => {
      if *op == BinOpType::Comma {
        return false;
      }

      return is_scalar_type(lhs) && is_scalar_type(rhs);
    }
    NodeKind::CallExpr{func, ..} => {
      if let Some(kind) = func.return_type {
        return kind == ObjectKind::Int;
      }
      return false;
    }
    NodeKind::Variable(object) => {
      return object.kind == ObjectKind::Int;
    }
    NodeKind::Constant(_) => {
      return true;
    }
    _ => false
  }
}