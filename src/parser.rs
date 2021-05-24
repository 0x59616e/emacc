use crate::lex::{Token, TokenKind};
use std::collections::HashMap;
type TokenIterator = std::iter::Peekable<std::vec::IntoIter<Token>>;

#[derive(PartialEq, Debug)]
pub enum BinOpType {
  Comma,         // ,
  Assignment,    // =
  LogicalOr,     // ||
  LogicalAnd,    // &&
  Equal,         // ==
  NotEqual,      // !=
  GreaterEqual,  // >=
  Greater,       // >
  LessEqual,     // <=
  Less,          // <
  Plus,          // +
  Minus,         // -
  Mul,           // *
  Div,           // /
}

enum Level {
  Unknown         = 0,    // Not binary operator.
  Comma           = 1,    // ,
  Assignment      = 2,    // =, *=, /=, %=, +=, -=, <<=, >>=, &=, ^=, |=
  _Conditional     = 3,    // ?
  LogicalOr       = 4,    // ||
  LogicalAnd      = 5,    // &&
  _InclusiveOr     = 6,    // |
  _ExclusiveOr     = 7,    // ^
  _And             = 8,    // &
  Equality        = 9,    // ==, !=
  Relational      = 10,   //  >=, <=, >, <
  _Spaceship       = 11,   // <=>
  _Shift           = 12,   // <<, >>
  Additive        = 13,   // -, +
  Multiplicative  = 14,   // *, /, %
  _PointerToMember = 15    // .*, ->*
}
type PrecLevel = i32;

pub struct Initializer {
  pub init_expr: Box<Node>,
}

pub enum NodeKind {
  Program {
    func_or_decl_list: Vec<Node>,
  },
  FunctionDefinition {
    prototype: Object,
    body: Box<Node>,
  },
  VarDecl {
    object: Object,
    init: Option<Box<Initializer>>,
  },
  CompoundStmt {
    body: Vec<Node>
  },
  IfStmt {
    cond_expr: Box<Node>,
    then_stmt: Vec<Node>,
    else_stmt: Option<Vec<Node>>,
  },
  ReturnStmt {
    expr: Option<Box<Node>>
  },
  BinOpExpr {
    op: BinOpType,
    lhs: Box<Node>,
    rhs: Box<Node>
  },
  CallExpr {
    func: Object,
    arg_list: Vec<Node>,
  },
  UnaryExpr(Box<Node>),
  Variable(Object),
  Constant(i32),
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum ObjectKind {
  Unknown,
  Int,
  Function,
}

#[derive(Clone, Debug)]
pub struct Object {
  pub name: String,
  pub kind: ObjectKind,
  // function
  pub return_type: Option<ObjectKind>,
  pub para_list: Option<Vec<Object>>,
}

pub struct Node {
  kind: NodeKind,
}

struct Scope {
  // The function in which this scope currently reside
  function: Option<Object>,
  // global variable or functio definition
  global_decl_set: HashMap<String, Object>,
  // local variable
  local_decl_set: Vec<HashMap<String, Object>>
}

impl Node {
  pub fn get_kind(&self) -> &NodeKind {
    return &self.kind;
  }
}

impl Scope {
  fn new() -> Scope {
    Scope {
      function: None,
      global_decl_set: HashMap::new(),
      local_decl_set: Vec::new(),
    }
  }

  fn enter_new_scope(&mut self) {
    self.local_decl_set.push(HashMap::new());
  }

  fn enter_new_function_scope(&mut self, function: Object) {
    self.function = Some(function);
    self.enter_new_scope();
  }

  fn leave_scope(&mut self) {
    self.local_decl_set.pop();
    if self.local_decl_set.is_empty() {
      self.function = None;
    }
  }

  fn insert_local_var(&mut self, object: Object) {
    self.local_decl_set.last_mut().unwrap().insert(object.name.clone(), object);
  }

  fn insert_global_var(&mut self, object: Object) {
    self.global_decl_set.insert(object.name.clone(), object);
  }

  fn find_var(&self, name: &String) -> Option<&Object> {
    for table in self.local_decl_set.iter().rev() {
      let res = table.get(name);
      if let Some(_) = res {
        return res;
      }
    }

    self.global_decl_set.get(name)
  }
}

impl Node {
  fn new_binary_node(op: BinOpType, lhs: Node, rhs: Node) -> Node {
    Node {
      kind: NodeKind::BinOpExpr {
        op: op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
      }
    }
  }
}

fn consume(tokiter: &mut TokenIterator, kind: TokenKind) -> Option<Token> {
  let tok = tokiter.next_if(|x| x.is(kind))?;
  Some(tok)
}

fn get_next_op_prec(tokiter: &mut TokenIterator) -> PrecLevel {
  match tokiter.peek().unwrap().kind {
    TokenKind::Comma => Level::Comma as PrecLevel,
    TokenKind::Assignment => Level::Assignment as PrecLevel,
    TokenKind::LogicalOr => Level::LogicalOr as PrecLevel,
    TokenKind::LogicalAnd => Level::LogicalAnd as PrecLevel,
    TokenKind::EqualEqual |
    TokenKind::NotEqual   => Level::Equality as PrecLevel,
    TokenKind::Less      |
    TokenKind::LessEqual |
    TokenKind::Greater   |
    TokenKind::GreaterEqual => Level::Relational as PrecLevel,
    TokenKind::Plus |
    TokenKind::Minus => Level::Additive as PrecLevel,
    TokenKind::Star |
    TokenKind::Div   => Level::Multiplicative as PrecLevel,
    _ => Level::Unknown as PrecLevel,
  }
}

fn get_next_op(tokiter: &mut TokenIterator) -> BinOpType {
  let tok = tokiter.next().unwrap();
  match tok.kind {
    TokenKind::Comma => BinOpType::Comma,
    TokenKind::Assignment => BinOpType::Assignment,
    TokenKind::Plus => BinOpType::Plus,
    TokenKind::Minus => BinOpType::Minus,
    TokenKind::Star => BinOpType::Mul,
    TokenKind::Div  => BinOpType::Div,
    TokenKind::EqualEqual => BinOpType::Equal,
    TokenKind::NotEqual => BinOpType::NotEqual,
    TokenKind::Greater => BinOpType::Greater,
    TokenKind::GreaterEqual => BinOpType::GreaterEqual,
    TokenKind::Less => BinOpType::Less,
    TokenKind::LessEqual => BinOpType::LessEqual,
    TokenKind::LogicalOr => BinOpType::LogicalOr,
    TokenKind::LogicalAnd => BinOpType::LogicalAnd,
    _ => panic!("Unregonizable operator: '{:?}'", tok),
  }
}

// cast-expression -> unary-exprssion
//                    (type-name) cast-exprssion
//-----------------------------------------------
// unary-expression -> postfix-expression
//                    ++ unary-expression
//                    -- unary-expression
//                    unary-operator cast-expression
//                    sizeof unary-expression
//                    sizeof ( type-name )
//                    _Alignof ( type-name )
// ----------------------------------------------
// postfix-expression -> primary-expression
//                       postfix-expression [ expression ]
//                       postfix-expression ( argument-expression-listopt )
//                       postfix-expression . identifier
//                       postfix-expression -> identifier
//                       postfix-expression ++
//                       postfix-expression --
//                       ( type-name ) { initializer-list }
//                       ( type-name ) { initializer-list , }
//-------------------------------------------------
// primary-exprssion -> primary-expression:
//                      identifier
//                      constant
//                      string-literal
//                      ( expression )

fn parse_cast_expr(tokiter: &mut TokenIterator, scope: &Scope) -> Node {
  let tok = tokiter.next().unwrap();

  let node = match tok.kind {
    TokenKind::Identifier(name) => {
      let object = scope.find_var(&name).expect("undeclared variable");
      match object.kind {
        ObjectKind::Int => Node {kind: NodeKind::Variable(object.clone())},
        ObjectKind::Function => {
          consume(tokiter, TokenKind::LParen);
          let mut arg_list: Vec<Node> = vec![];
          if let None = consume(tokiter, TokenKind::RParen) {
            loop {
              arg_list.push(parse_assign_expr(tokiter, scope));
              if let None = consume(tokiter, TokenKind::Comma) {
                consume(tokiter, TokenKind::RParen);
                break;
              }
            }
          }
          Node {kind: NodeKind::CallExpr {
            func: object.clone(),
            arg_list,
          }}
        },
        _ => panic!("????"),
      }
    },
    TokenKind::Num(num) => Node {kind: NodeKind::Constant(num)},
    TokenKind::LParen => {
      let node = parse_expr(tokiter, scope);
      consume(tokiter, TokenKind::RParen);
      node
    }
    _ => panic!("?????"),
  };
  node
}

fn parse_rhs_of_binary_op(tokiter: &mut TokenIterator,
                          scope: &Scope,
                          mut lhs: Node,
                          min_prec_level: PrecLevel) -> Node {
  loop {
    let op_prec = get_next_op_prec(tokiter);

    if op_prec < min_prec_level {
      return lhs;
    }

    let op = get_next_op(tokiter);
    let mut rhs = parse_cast_expr(tokiter, scope);

    let next_op_prec = get_next_op_prec(tokiter);

    let is_right_assoc = op == BinOpType::Assignment;
    if op_prec < next_op_prec || 
       (op_prec == next_op_prec && is_right_assoc) {
      rhs = parse_rhs_of_binary_op(tokiter,
                                   scope,
                                   rhs,
                                   op_prec + !(is_right_assoc) as PrecLevel);
    }

    lhs = Node::new_binary_node(op, lhs, rhs);
  }
}

fn parse_assign_expr(tokiter: &mut TokenIterator, scope: &Scope) -> Node {
  let lhs = parse_cast_expr(tokiter, scope);
  parse_rhs_of_binary_op(tokiter, scope, lhs, Level::Assignment as PrecLevel)
}
// expresstion-statement -> expression[opt] ;
//------------------------------------------
// expression -> assignment-expression
//               expression , assignment-expresion
fn parse_expr(tokiter: &mut TokenIterator, scope: &Scope) -> Node {
  let lhs = parse_assign_expr(tokiter, scope);
  parse_rhs_of_binary_op(tokiter, scope, lhs, Level::Comma as PrecLevel)
}

fn parse_expr_stmt(tokiter: &mut TokenIterator, scope: &Scope) -> Node {
  let node = parse_expr(tokiter, scope);
  consume(tokiter, TokenKind::Semicolon);
  return node;
}

// jump-statement -> return expression[opt] ';'
fn parse_jump_stmt(tokiter: &mut TokenIterator, scope: &mut Scope) -> Node {
  consume(tokiter, TokenKind::KwReturn);
  let mut node = Node {
    kind: NodeKind::ReturnStmt{expr: None},
  };
  if let None = consume(tokiter, TokenKind::Semicolon) {
    node = Node {
      kind: NodeKind::ReturnStmt{expr: Some(Box::new(parse_expr(tokiter, scope)))},
    };
    consume(tokiter, TokenKind::Semicolon);
  }
  node
}
// selection-statement -> if ( expression ) statement
//                        if ( expression ) statement else statement
fn parse_selection_stmt(tokiter: &mut TokenIterator, scope: &mut Scope) -> Node {
  consume(tokiter, TokenKind::KwIf);
  consume(tokiter, TokenKind::LParen);
  let cond_expr = Box::new(parse_expr(tokiter, scope));
  consume(tokiter, TokenKind::RParen);
  let then_stmt = parse_stmt(tokiter, scope);
  let mut else_stmt = None;
  if let Some(_) = consume(tokiter, TokenKind::KwElse) {
    else_stmt = Some(parse_stmt(tokiter, scope));
  }
  Node {
    kind: NodeKind::IfStmt {
      cond_expr,
      then_stmt,
      else_stmt,
    }
  }
}
// statement ->  compound-statement
//               expression-statement
//               selection-statement
//               jump-statement
//------------------------------------
fn parse_stmt(tokiter: &mut TokenIterator, scope: &mut Scope) -> Vec<Node> {
  let mut stmt_list: Vec<Node> = Vec::new();
  let tok = tokiter.peek().unwrap();
  match tok.kind {
    TokenKind::LBrace => stmt_list.push(parse_compound_stmt(tokiter, scope)),
    TokenKind::KwIf => stmt_list.push(parse_selection_stmt(tokiter, scope)),
    TokenKind::KwReturn => stmt_list.push(parse_jump_stmt(tokiter, scope)),
    _ => stmt_list.push(parse_expr_stmt(tokiter, scope)),
  }
  stmt_list
}
// compound-stmt -> { block-item-list[opt] }
//------------------------------------------
// block-item-list -> block-item
//                    block-item-list block-item
//------------------------------------------
// block-item -> declaration
//               statement
fn parse_compound_stmt(tokiter: &mut TokenIterator, scope: &mut Scope) -> Node {
  if let None = consume(tokiter, TokenKind::LBrace) {
    panic!("There should've been a left curly brace");
  }
  scope.enter_new_scope();

  let mut node_list: Vec<Node> = Vec::new();
  loop {
    let tok = tokiter.peek().unwrap();
    if tok.is(TokenKind::KwInt) {
      // declaration
      node_list.append(&mut parse_declaration(tokiter, scope));
    } else if !tok.is(TokenKind::RBrace) {
      // statement
      node_list.append(&mut parse_stmt(tokiter, scope));
    } else {
      break;
    }
  }

  if let None = consume(tokiter, TokenKind::RBrace) {
    panic!("There should've been a right curly brace");
  }
  scope.leave_scope();

  Node {
    kind: NodeKind::CompoundStmt{body: node_list,}
  }
}

// declarator -> pointer[opt] direct-declarator
//----------------------------------------------------
// direct-declarator -> identifier
//                      direct-declarator ( parameter-type-list )
//                      direct-declarator ( identifier-list[opt] )
//----------------------------------------------------
// parameter-type-list -> parameter-list
//----------------------------------------------------
// parameter-list -> parameter-declaration
//                   parameter-list , parameter-declaration
//----------------------------------------------------
// parameter-declaration -> declaration-specifiers declarator

fn parse_declarator(tokiter: &mut TokenIterator, mut object: Object) -> Object {
  let tok = tokiter.next().unwrap();

  if let TokenKind::Identifier(name) = tok.kind {
    object.name = name;
  } else {
    panic!("There should've beeen an identifier");
  }

  if let Some(_) = tokiter.next_if(|x| x.is(TokenKind::LParen)) {
    // This is a function
    object.return_type = Some(object.kind);
    object.kind = ObjectKind::Function;

    let mut para_list: Vec<Object> = Vec::new();

    loop {
      if let Some(_) = consume(tokiter, TokenKind::RParen) {
        break;
      }

      if para_list.len() > 0 {
        if let None = consume(tokiter, TokenKind::Comma) {
          panic!("There should've been a comma");
        }
      }

      let parameter_specifier = parse_declaration_specifiers(tokiter);
      let parameter = parse_declarator(tokiter, parameter_specifier);

      para_list.push(parameter);
    }
    object.para_list = Some(para_list);
  }
  object
}
// declaration-specifiers ->  storage-class-specifier
//                            type-specifier
//                            type-qualifier
//                            function-specifier
//                            alignment-specifier
//-----------------------------------------------------
// type-specifier -> int
fn parse_declaration_specifiers(tokiter: &mut TokenIterator) -> Object {
  consume(tokiter, TokenKind::KwInt);

  Object {
    name: String::new(),
    kind: ObjectKind::Int,
    return_type: None,
    para_list: None,
  }
}

// fn parse_declaration_specifiers(tokiter:)
// translation-unit -> external-declaration
//                     translation-unit external-declaration
//----------------------------------------------------------
// external-declaration -> function-definition
//                         declaration
//----------------------------------------------------------
// function-definition -> declaration-specifiers declarator compound-statement
//----------------------------------------------------------
// declaration -> declaration-specifiers init-declarator-list[opt] ;
//----------------------------------------------------------
// init-declarator-list -> init-declarator
//                          init-declarator-list , init-declarator
//----------------------------------------------------------
// init-declarator -> declarator
//                    declarator '=' initializer
//----------------------------------------------------------
// initializer -> assignment-expression
//                { initializer-list }
//                { initializer-list , }
fn parse_declaration(tokiter: &mut TokenIterator, scope: &mut Scope) -> Vec<Node> {
  let decl_specifier = parse_declaration_specifiers(tokiter);
  let mut node_list = Vec::new();
  if let None = consume(tokiter, TokenKind::Semicolon) {
    loop {
      let object = parse_declarator(tokiter, decl_specifier.clone());
      assert!(object.kind == ObjectKind::Int);
      scope.insert_local_var(object.clone());

      let mut init: Option<Box<Initializer>> = None;

      if let Some(_) = consume(tokiter, TokenKind::Assignment) {
        init = Some(Box::new(Initializer {
          init_expr: Box::new(parse_assign_expr(tokiter, scope)),
        }));
      }

      node_list.push(Node {
        kind: NodeKind::VarDecl {
          object,
          init,
        }
      });

      if let None = consume(tokiter, TokenKind::Comma) {
        break;
      }
    }
    consume(tokiter, TokenKind::Semicolon);
  }
  node_list
}

pub fn parse(toklist: Vec<Token>) -> Node {

  let mut tokiter = toklist.into_iter().peekable();
  let mut scope = Scope::new();
  let mut func_or_decl_list: Vec<Node> = vec![];

  while let Some(_) = tokiter.peek() {
    let function_specifier = parse_declaration_specifiers(&mut tokiter);
    let function = parse_declarator(&mut tokiter, function_specifier);
    scope.insert_global_var(function.clone());
    scope.enter_new_function_scope(function.clone());
    for parm in function.para_list.as_ref().unwrap().iter() {
      scope.insert_local_var(parm.clone());
    }

    let node = Node {
      kind: NodeKind::FunctionDefinition {
        prototype: function,
        body: Box::new(parse_compound_stmt(&mut tokiter, &mut scope)),
      }
    };
    func_or_decl_list.push(node);
    scope.leave_scope();
  }
  Node {
    kind: NodeKind::Program {
      func_or_decl_list,
    }
  }
}