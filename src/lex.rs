#[derive(Debug, PartialEq)]

pub enum TokenKind {
  Useless,
  Identifier(String),
  Num(i32),       // numeric literal
  Plus,           // +
  Minus,          // -
  Star,           // *
  Div,            // /
  Assignment,     // =
  EqualEqual,     // ==
  NotEqual,       // !=
  Greater,        // >
  GreaterEqual,   // >=
  Less,           // <
  LessEqual,      // <=
  LogicalOr,      // ||
  LogicalAnd,     // &&
  BitwiseOr,      // |
  BitwiseAnd,     // &
  Not,            // !
  LParen,         // (
  RParen,         // )
  LBrace,         // {
  RBrace,         // }
  Semicolon,      // ;
  Comma,          // ,
  KwInt,          // int
  KwReturn,       // return
  KwIf,           // if
  KwElse,         // else
  KwWhile,        // while
}

#[derive(Debug)]
pub struct Token {
  pub kind: TokenKind,
}

impl Token {
  fn useless() -> Token {
    Token {
      kind: TokenKind::Useless,
    }
  }

  pub fn is(&self, kind: TokenKind) -> bool {
    return self.kind == kind;
  }
}

fn is_numeric(c: u8) -> bool {
  return b'0' <= c && c <= b'9';
}

fn is_eng_alphbet(c: u8) -> bool {
  return (b'a' <= c && c <= b'z') || 
         (b'A' <= c && c <= b'Z')
}

fn read_number(iter: &mut std::iter::Peekable<std::vec::IntoIter<u8>>) -> Token
{
  let mut res = 0i32;

  while let Some(c) = iter.next_if(|&x| is_numeric(x)) {
    res = res * 10 + (c - b'0') as i32;
  }

  Token {
    kind: TokenKind::Num(res),
  }
}

fn read_identifier_or_keyword(iter: &mut std::iter::Peekable<std::vec::IntoIter<u8>>) -> Token
{
  let mut res = String::new();

  while let Some(c) = iter.next_if(|&x| is_eng_alphbet(x) || 
                                        is_numeric(x)  ||
                                        x == b'_') {
    res.push(c as char);
  }

  // compare to keyword
  let mut kind = TokenKind::Useless;

  if res.eq("int") {
    kind = TokenKind::KwInt;
  } else if res.eq("return") {
    kind = TokenKind::KwReturn;
  } else if res.eq("if") {
    kind = TokenKind::KwIf;
  } else if res.eq("else") {
    kind = TokenKind::KwElse;
  } else if res.eq("while") {
    kind = TokenKind::KwWhile;
  } else {
    kind = TokenKind::Identifier(res);
  }

  Token {
    kind: kind,
  }
}

fn read_punc2(iter: &mut  std::iter::Peekable<std::vec::IntoIter<u8>>,
              second_char: u8,
              tok1: TokenKind,
              tok2: TokenKind) -> TokenKind {
  let &c = iter.peek().unwrap();
  if c == second_char {
    iter.next();
    tok1
  } else {
    tok2
  }
}

fn read_punctuator(iter: &mut std::iter::Peekable<std::vec::IntoIter<u8>>) -> Token
{
  let c = iter.next().unwrap();

  let kind = match c {
    b'(' => TokenKind::LParen,
    b')' => TokenKind::RParen,
    b'{' => TokenKind::LBrace,
    b'}' => TokenKind::RBrace,
    b'+' => TokenKind::Plus,
    b'-' => TokenKind::Minus,
    b'*' => TokenKind::Star,
    b'/' => TokenKind::Div,
    b';' => TokenKind::Semicolon,
    b',' => TokenKind::Comma,
    b'=' => read_punc2(iter, b'=', TokenKind::EqualEqual, TokenKind::Assignment),
    b'>' => read_punc2(iter, b'=', TokenKind::GreaterEqual, TokenKind::Greater),
    b'<' => read_punc2(iter, b'=', TokenKind::LessEqual, TokenKind::Less),
    b'!' => read_punc2(iter, b'=', TokenKind::NotEqual, TokenKind::Not),
    b'|' => read_punc2(iter, b'|', TokenKind::LogicalOr, TokenKind::BitwiseOr),
    b'&' => read_punc2(iter, b'&', TokenKind::LogicalAnd, TokenKind::BitwiseAnd),
     _  => panic!("What the fuck is this '{}' ?", c)
  };

  Token {
    kind: kind,
  }
}

pub fn tokenize(code: String) ->Vec<Token> {

  let mut toklist: Vec<Token> = Vec::new();

  let mut iter = code.into_bytes().into_iter().peekable();

  while let Some(&c) = iter.peek() {
    let tok: Token = match c {
      // identifier or keyword
      b'a'..=b'z' |
      b'A'..=b'Z' |
      b'_'       => read_identifier_or_keyword(&mut iter),
      // numeric literal
      b'0'..=b'9' => read_number(&mut iter),
      // punctuator
      b'(' | b')' |
      b'{' | b'}' |
      b'+' | b'-' |
      b'*' | b'/' |
      b';' | b',' |
      b'=' | b'>' |
      b'<' | b'!' |
      b'|' | b'&' => read_punctuator(&mut iter),
      // useless character
      b' ' | b'\n'| b'\t' => Token::useless(),
      _ => panic!("{} <-- unrecognizable character", c as char),
    };

    if !tok.is(TokenKind::Useless) {
      toklist.push(tok);
    } else {
      iter.next();
    }
  }

  toklist
}
