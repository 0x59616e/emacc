use std::env;
use std::fs::File;
use std::io::prelude::*;

mod lex;
mod parser

#[derive(Debug)]
struct Unit {
  a: char
}


fn main() {
  let args: Vec<String> = env::args().collect();
  let mut file = File::open(&args[1]).unwrap();

  let mut content = String::new();
  file.read_to_string(&mut content).unwrap();

  let toklist = lex::tokenize(content);

  println!("{:?}", toklist);
}
