use std::env;
use std::fs::File;
use std::io::prelude::*;

mod symtab;
mod ast;
mod lex;
mod parser;
mod irbuilder;
mod instruction;
mod basicblock;
mod function;
mod module;
mod value;

use ast::Stmt;

fn main() {
  let args: Vec<String> = env::args().collect();
  let mut file = File::open(&args[1]).unwrap();

  let mut content = String::new();
  file.read_to_string(&mut content).unwrap();

  let toklist = lex::tokenize(content);
  let mut parser = parser::Parser::new(toklist);
  let translation_unit = parser.run();

  translation_unit.print_ast("".to_string());

  let ir_builder = irbuilder::IRBuilder::new();

  let module = ir_builder.run(translation_unit);
  module.print();
}
