mod analysis;
mod ast;
mod basicblock;
mod codegen;
mod function;
mod lex;
mod instruction;
mod irbuilder;
mod mem2reg;
mod module;
mod parser;
mod symtab;
mod value;

use ast::Stmt;
use codegen::target::riscv::RISCV32;
use codegen::*;
use analysis::dominator::*;
use analysis::domfrontier::*;
use mem2reg::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() {
  let args: Vec<String> = env::args().collect();
  let mut file = File::open(&args[1]).unwrap();

  let mut content = String::new();
  file.read_to_string(&mut content).unwrap();

  let toklist = lex::tokenize(content);
  let mut parser = parser::Parser::new(toklist);
  let translation_unit = parser.run();

  // translation_unit.print_ast("".to_string());

  let ir_builder = irbuilder::IRBuilder::new();

  let module = ir_builder.run(translation_unit);

  module.borrow().print();
  module.borrow_mut().func_list().for_each(|func| {
    let mut mem2reg = PromoteMemoryToReg::new(func);
    mem2reg.run();
  });
  let riscv = RISCV32::new();
  println!("\n^before------v-after---------");
  module.borrow().print();
  code_gen(&module, riscv);
}
