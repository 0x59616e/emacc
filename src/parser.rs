use crate::ast::*;
use crate::symtab::*;
use crate::lex::*;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

#[derive(PartialEq, Debug, Clone, Copy)]
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
  _BitwiseAnd,    // &
  _BitwiseOr,     // |
  _BitwiseXor,    // ^
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
  _Shift           = 11,   // <<, >>
  Additive        = 12,   // -, +
  Multiplicative  = 13,   // *, /, %
  _PointerToMember = 14    // .*, ->*
}
type PrecLevel = i32;

pub struct Parser {
  symtab: SymTab,
  toklist: std::iter::Peekable<std::vec::IntoIter<Token>>,
}

impl Parser {
  pub fn new(toklist: Vec<Token>) -> Parser {
    Parser {
      symtab: SymTab::new(),
      toklist: toklist.into_iter().peekable(),
    }
  }

  fn have_token(&mut self) -> bool {
    if let None = self.toklist.peek() {
      return false;
    }
    true
  }

  fn peek_token(&mut self) -> &Token {
    self.toklist.peek().unwrap()
  }

  fn skip_token(&mut self) -> Token {
    self.toklist.next().unwrap()
  }

  fn consume_token(&mut self, kind: TokenKind) -> Option<Token> {
    let tok = self.toklist.next_if(|x| x.is(kind))?;
    Some(tok)
  }

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
  pub fn run(&mut self) -> TranslationUnit {
    let mut func_or_decl_list = Vec::new();

    while self.have_token() {
      let declspec = self.parse_declaration_specifiers();
      if let (func_prototype, Some(para_list)) = self.parse_declarator(declspec) {
        let func_prototype = Rc::new(RefCell::new(func_prototype));
        self.symtab.insert_global_var(Rc::clone(&func_prototype));
        self.symtab.enter_new_scope();

        for (i, mut parm) in para_list.into_iter().enumerate() {
          parm.set_as_parm(i);
          self.symtab.insert_local_var(Rc::new(RefCell::new(parm)));
        }

        func_or_decl_list.push(FunctionDef {
          func_prototype: func_prototype,
          body: self.parse_compound_stmt(),
          local_var_list: self.symtab.decl_set.drain(1..).collect(),
        });

        self.symtab.leave_scope();
      } else {
        panic!("Parser::run: We haven't supported global var decl");
      }

    }
    TranslationUnit {
      func_or_decl_list
    }
  }

  fn parse_declaration(&mut self) -> Vec<Box<dyn Stmt>> {
    let declspec = self.parse_declaration_specifiers();
    let mut var_decl_stmt_list: Vec<Box<dyn Stmt>> = Vec::new();

    if let None = self.consume_token(TokenKind::Semicolon) {
      loop {
        if let (object, None) = self.parse_declarator(declspec.clone()) {
          let object = Rc::new(RefCell::new(object));
          self.symtab.insert_local_var(Rc::clone(&object));

          let mut init = None;

          if let Some(_) = self.consume_token(TokenKind::Assignment) {
            init = Some(Box::new(Initializer {
              init_expr: self.parse_assign_expr(),
            }));
          }

          var_decl_stmt_list.push(Box::new(VarDeclStmt {
            object: Rc::clone(&object),
            init: init,
          }));

          if let None = self.consume_token(TokenKind::Comma) {
            self.consume_token(TokenKind::Semicolon);
            break;
          }
        } else {
          panic!("parse_declaration: Function definition??");
        }
      }
    }
    var_decl_stmt_list
  }

  // declaration-specifiers ->  storage-class-specifier
  //                            type-specifier
  //                            type-qualifier
  //                            function-specifier
  //                            alignment-specifier
  //-----------------------------------------------------
  // type-specifier -> int
  fn parse_declaration_specifiers(&mut self) -> SymTabEntry {
    self.consume_token(TokenKind::KwInt);

    SymTabEntry {
      name: String::new(),
      entry_type: Type::Int,
      scope_id: None,
      addr: None,
      return_type: None,
      prototype: None,
      is_parm: None,
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
  fn parse_declarator(&mut self, mut object: SymTabEntry) -> (SymTabEntry, Option<Vec<SymTabEntry>>) {
    let tok = self.skip_token();

    if let TokenKind::Identifier(name) = tok.kind {
      object.set_name(name);
    } else {
      panic!("parse_declarator: No Identifier");
    }

    let mut prototype: Vec<Type> = Vec::new();
    let mut para_list: Vec<SymTabEntry> = Vec::new();

    if let Some(_) = self.consume_token(TokenKind::LParen) {
      // This is a function
      object.set_as_func();

      loop {
        if let Some(_) = self.consume_token(TokenKind::RParen) {
          break;
        }

        if para_list.len() > 0 {
          if let None = self.consume_token(TokenKind::Comma) {
            panic!("parse_declarator: There should've been a comma")
          }
        }

        let declspec = self.parse_declaration_specifiers();
        match self.parse_declarator(declspec) {
          (_, Some(_)) => panic!("parse_declarator: We haven't supported function pointer..."),
          (parameter, None) => {
            prototype.push(parameter.get_type());
            para_list.push(parameter);
          }
        }
      }

      object.set_func_prototype(prototype);

      return (object, Some(para_list));
    }
    (object, None)
  }

  // compound-stmt -> { block-item-list[opt] }
  //------------------------------------------
  // block-item-list -> block-item
  //                    block-item-list block-item
  //------------------------------------------
  // block-item -> declaration
  //               statement
  fn parse_compound_stmt(&mut self) -> Box<CompoundStmt> {
    if let None = self.consume_token(TokenKind::LBrace) {
      panic!("parse_compound_stmt: No left brace");
    }
    self.symtab.enter_new_scope();

    let mut stmt_list: Vec<Box<dyn Stmt>> = Vec::new();

    loop {
      let tok = self.peek_token();
      if tok.is(TokenKind::KwInt) {
        // declaration
        stmt_list.append(&mut self.parse_declaration());
      } else if !tok.is(TokenKind::RBrace) {
        // statement
        stmt_list.push(self.parse_stmt());
      } else {
        break;
      }
    }

    if let None = self.consume_token(TokenKind::RBrace) {
      panic!("parse_compound_stmt: No right brace");
    }

    self.symtab.leave_scope();
    Box::new(CompoundStmt{
      body: stmt_list,
    })
  }
  // statement ->  compound-statement
  //               expression-statement
  //               selection-statement
  //               jump-statement
  //------------------------------------
  fn parse_stmt(&mut self) -> Box<dyn Stmt> {
    let tok = self.peek_token();
    match tok.kind {
      TokenKind::LBrace => self.parse_compound_stmt(),
      TokenKind::KwIf => self.parse_selection_stmt(),
      TokenKind::KwReturn => self.parse_jump_stmt(),
      _ => self.parse_expr_stmt(),
    }
  }
  // jump-statement -> return expression[opt] ';'
  fn parse_jump_stmt(&mut self) -> Box<dyn Stmt> {
    self.consume_token(TokenKind::KwReturn);
    let node: Box<ReturnStmt>;

    if let None = self.consume_token(TokenKind::Semicolon) {
      node = Box::new(ReturnStmt{expr: Some(self.parse_expr())});
      self.consume_token(TokenKind::Semicolon);
    } else {
      node = Box::new(ReturnStmt{expr: None});
    }
    node
  }
  // selection-statement -> if ( expression ) statement
  //                        if ( expression ) statement else statement
  fn parse_selection_stmt(&mut self) -> Box<dyn Stmt> {
    self.consume_token(TokenKind::KwIf);
    self.consume_token(TokenKind::LParen);
    let cond_expr = self.parse_expr();
    self.consume_token(TokenKind::RParen);
    let then_stmt = self.parse_stmt();
    let mut else_stmt = None;
    if let Some(_) = self.consume_token(TokenKind::KwElse) {
      else_stmt = Some(self.parse_stmt());
    }

    Box::new(IfStmt {
      cond_expr,
      then_stmt,
      else_stmt,
    })
  }
  // expresstion-statement -> expression[opt] ;
  //------------------------------------------
  // expression -> assignment-expression
  //               expression , assignment-expresion
  fn parse_expr_stmt(&mut self) -> Box<dyn Stmt> {
    let node = self.parse_expr();
    self.consume_token(TokenKind::Semicolon);
    Box::new(ExprStmt {
      expr: node,
    })
  }

  fn parse_expr(&mut self) -> Box<dyn Expr> {
    let lhs = self.parse_assign_expr();
    self.parse_rhs_of_binary_op(lhs, Level::Comma as PrecLevel)
  }

  fn parse_assign_expr(&mut self) -> Box<dyn Expr> {
    let lhs = self.parse_cast_expr();
    self.parse_rhs_of_binary_op(lhs, Level::Assignment as PrecLevel)
  }

  fn parse_rhs_of_binary_op(
    &mut self,
    mut lhs: Box<dyn Expr>,
    min_prec_level: PrecLevel
  ) -> Box<dyn Expr> {
    loop {
      let op_prec = self.get_next_op_prec();

      if op_prec < min_prec_level {
        return lhs;
      }

      let op = self.get_next_op();
      let mut rhs = self.parse_cast_expr();

      let next_op_prec = self.get_next_op_prec();

      let is_right_assoc = op == BinOpType::Assignment;

      if op_prec < next_op_prec ||
        (op_prec == next_op_prec && is_right_assoc) {
        rhs = self.parse_rhs_of_binary_op(rhs,
                                          op_prec + !(is_right_assoc) as PrecLevel)
      }

      lhs = Box::new(BinOpExpr {
        op,
        lhs,
        rhs,
      });
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
  fn parse_cast_expr(&mut self) -> Box<dyn Expr> {
    let tok = self.skip_token();

    let node: Box<dyn Expr> = match tok.kind {
      TokenKind::Identifier(name) => {
        let object = self.symtab.find_var(&name).expect("undeclared variable").clone();
        let object_borrow = object.borrow();
        match object_borrow.get_type() {
          Type::Int => Box::new(Var {object: Rc::clone(&object)}),
          Type::Function => {
            self.consume_token(TokenKind::LParen);

            let mut arg_list = Vec::new();
            if let None = self.consume_token(TokenKind::RParen) {
              loop {
                arg_list.push(self.parse_assign_expr());
                if let None = self.consume_token(TokenKind::Comma) {
                  self.consume_token(TokenKind::RParen);
                  break;
                }
              }
            }

            Box::new(CallExpr {
              func: Rc::clone(&object),
              arg_list,
            })
          }
          _ => panic!("'void' currently not supported")
        }
      }
      TokenKind::Num(num) => Box::new(Constant{value: num}),
      TokenKind::LParen => {
        let node = self.parse_expr();
        self.consume_token(TokenKind::RParen);
        node
      }
      _ => panic!("cast_expr: {:?}", self.toklist),
    };
    node
  }

  fn get_next_op_prec(&mut self) -> PrecLevel {
    match self.peek_token().kind {
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
  
  fn get_next_op(&mut self) -> BinOpType {
    let tok = self.skip_token();
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
      BinOpType::Equal => write!(f, "=="),
      BinOpType::NotEqual => write!(f, "!="),
      BinOpType::Less => write!(f, "<"),
      BinOpType::LessEqual => write!(f, "<="),
      BinOpType::Greater => write!(f, ">"),
      BinOpType::GreaterEqual => write!(f, ">="),
      BinOpType::LogicalOr => write!(f, "||"),
      BinOpType::LogicalAnd => write!(f, "&&"),
      _ => panic!("curretly not support"),
    }
  }
}
