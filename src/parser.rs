use crate::lex::{Token, TokenKind};
use std::collections::HashMap;
type TokenIterator = std::iter::Peekable<std::vec::IntoIter<Token>>;

#[derive(PartialEq, Debug)]
pub enum BinOpType {
  Comma,         // ,
  Assignment,    // =
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
  _LogicalOr       = 4,    // ||
  _LogicalAnd      = 5,    // &&
  _InclusiveOr     = 6,    // |
  _ExclusiveOr     = 7,    // ^
  _And             = 8,    // &
  _Equality        = 9,    // ==, !=
  _Relational      = 10,   //  >=, <=, >, <
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
  ReturnStmt {
    expr: Option<Box<Node>>
  },
  BinOpExpr {
    op: BinOpType,
    lhs: Box<Node>,
    rhs: Box<Node>
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

  fn insert_local_var(&mut self, object: Object) {
    self.local_decl_set.last_mut().unwrap().insert(object.name.clone(), object);
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
    TokenKind::Star => BinOpType::Mul,
    TokenKind::Div  => BinOpType::Div,
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

fn parse_cast_expr(tokiter: &mut TokenIterator, scope: &mut Scope) -> Node {
  let tok = tokiter.next().unwrap();

  let node = match tok.kind {
    TokenKind::Identifier(name) => {
      let object = scope.local_decl_set.last().unwrap()
                          .get(&name).expect("undeclared variable");
      Node {
        kind: NodeKind::Variable(object.clone())
      }
    },
    TokenKind::Num(num) => Node {kind: NodeKind::Constant(num)},
    TokenKind::LParen => {
      let node = parse_expr(tokiter, scope);
      consume(tokiter, TokenKind::RParen);
      node
    }
    _ => panic!("shi ran"),
  };
  node
}

fn parse_rhs_of_binary_op(tokiter: &mut TokenIterator,
                          scope: &mut Scope,
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

fn parse_assign_expr(tokiter: &mut TokenIterator, scope: &mut Scope) -> Node {
  let lhs = parse_cast_expr(tokiter, scope);
  parse_rhs_of_binary_op(tokiter, scope, lhs, Level::Assignment as PrecLevel)
}
// expresstion-statement -> expression[opt] ;
//------------------------------------------
// expression -> assignment-expression
//               expression , assignment-expresion
fn parse_expr(tokiter: &mut TokenIterator, scope: &mut Scope) -> Node {
  let lhs = parse_assign_expr(tokiter, scope);
  parse_rhs_of_binary_op(tokiter, scope, lhs, Level::Comma as PrecLevel)
}

fn parse_expr_stmt(tokiter: &mut TokenIterator, scope: &mut Scope) -> Node {
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
// statement ->  compound-statement
//               expression-statement
//               jump-statement
//------------------------------------
fn parse_stmt(tokiter: &mut TokenIterator, scope: &mut Scope) -> Vec<Node> {
  let mut stmt_list: Vec<Node> = Vec::new();
  let tok = tokiter.peek().unwrap();
  match tok.kind {
    TokenKind::LBrace => stmt_list.push(parse_compound_stmt(tokiter, scope)),
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

  let function_specifier = parse_declaration_specifiers(&mut tokiter);
  let function = parse_declarator(&mut tokiter, function_specifier);
  scope.enter_new_function_scope(function.clone());
  let node = Node {
    kind: NodeKind::FunctionDefinition {
      prototype: function,
      body: Box::new(parse_compound_stmt(&mut tokiter, &mut scope)),
    }
  };
  node
}