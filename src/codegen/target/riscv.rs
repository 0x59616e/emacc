use crate::ir::basicblock::BasicBlock;
use crate::ir::function::Function;
use crate::ir::instruction::{Instruction, InstrTy, BinaryTy, CmpTy};
use crate::value::Value;
use std::cell::RefCell;
use std::rc::Rc;
use super::Target;



pub struct RISCV32 {
  inst_set: [&'static str; 20],
  reg:      [&'static str; 32],
}

impl Target for RISCV32 {
  fn instruction_lowering(&self, func: &Rc<RefCell<Function>>) {
    // lower instruction
    for bb in func.borrow().bb_list() {
      let mut new_inst_list = vec![];
      {
        let bbb = bb.borrow();
        let mut iter = bbb.inst_list().peekable();

        while let Some(inst) = iter.next() {
          // FIXME: The imm should be within -2048~2047. Currently I just ignore this restriction.
          match inst.borrow().get_inst_ty() {
            InstrTy::Alloca => panic!("Will be supported in the future"),
            InstrTy::Load   => panic!("Will be supported in the future"),
            InstrTy::Store  => panic!("Will be supported in the future"),
             // do nothing
            InstrTy::Phi    => new_inst_list.push(Rc::clone(inst)),
            InstrTy::Binary(binty) => {
              match binty {
                BinaryTy::Add => {
                  let src1 = inst.borrow().get_operand(0).unwrap();
                  let src2 = inst.borrow().get_operand(1).unwrap();
                  if src1.is_const() && src2.is_const() {
                    // we need to split them...
                    // This situation is due to phi-elimination.
                    let dest = inst.borrow().get_dest().unwrap();
                    let tmp_dest = Value::new_tmp_vreg(dest);
                    let zero = self.reg("x0");
                    let add = Instruction::new_binary_inst(src1, zero, tmp_dest, BinaryTy::Add, Rc::clone(bb));
                    new_inst_list.push(Rc::new(RefCell::new(add)));
                    let add = Instruction::new_binary_inst(src2, tmp_dest, dest, BinaryTy::Add, Rc::clone(bb));
                    new_inst_list.push(Rc::new(RefCell::new(add)));
                  } else {
                    new_inst_list.push(Rc::clone(inst));
                  }
                },
                BinaryTy::Sub |
                BinaryTy::Mul |
                BinaryTy::Div => {
                  // FIXME: The imm should be within -2048~2047. Currently I just ignore it.
                  // TODO: turn "%rd = sub %rs Imm" into "%rd = addi %rs -Imm"
                  //
                  // %rd = op %rs Imm 
                  //      |
                  //      v
                  // %.rd = addi x0 Imm
                  // %rd  =  op %rs %.rd
                  let src1 = inst.borrow().get_operand(0).unwrap();
                  let src2 = inst.borrow().get_operand(1).unwrap();
                  if src1.is_const() || src2.is_const() {
                    // This should'be been handled in ConstFold & ConstProp
                    // Although I have not implemented yet.
                    assert!(!src1.is_const() || !src2.is_const());

                    let (src1, src2) = if src2.is_const() {(src2, src1)} else {(src1, src2)};
                    let dest = inst.borrow().get_dest().unwrap();
                    let zero = self.reg("x0");
                    let tmp_dest = Value::new_tmp_vreg(dest);
                    let addi = Instruction::new_binary_inst(src1, zero, tmp_dest, BinaryTy::Add, Rc::clone(bb));
                    let op = Instruction::new_binary_inst(tmp_dest, src2, dest, *binty, Rc::clone(bb));
                    new_inst_list.push(Rc::new(RefCell::new(addi)));
                    new_inst_list.push(Rc::new(RefCell::new(op)));
                  }
                },
                BinaryTy::Xor => {
                  unimplemented!();
                }
              }
            },
            InstrTy::Cmp(cmpty) => {
              if iter.peek().unwrap().borrow().is_condi_br() {
                let br = iter.next().unwrap();
                // Turn cmp + br into branch-and-jump
                let mut src1 = inst.borrow().get_operand(0).unwrap();
                let mut src2 = inst.borrow().get_operand(1).unwrap();
                assert!(!src1.is_const() || !src2.is_const());
                // if one of them is constant, we need to move it into register first
                if src1.is_const() || src2.is_const() {
                  let constsrc = if src1.is_const() {src1} else {src2};
                  let dest = inst.borrow().get_dest().unwrap();
                  let addi = Instruction::new_binary_inst(constsrc, self.reg("x0"), dest, BinaryTy::Add, Rc::clone(bb));
                  new_inst_list.push(Rc::new(RefCell::new(addi)));
                  if src1.is_const() {src1 = dest} else {src2 = dest}
                }

                let bb1 = br.borrow().get_bb(0).unwrap();
                let bb2 = br.borrow().get_bb(1).unwrap();
                let id = match cmpty {
                  CmpTy::Lt | CmpTy::Gt => self.asm("blt"),
                  CmpTy::Ge | CmpTy::Le => self.asm("bge"),
                  CmpTy::Eq             => self.asm("beq"),
                  CmpTy::Ne             => self.asm("bne"),
                };
                let (src1, src2) = match cmpty {
                  CmpTy::Gt | CmpTy::Le => (src2, src1),
                  _ => (src1, src2)
                };
                let new_br = Instruction::new_asm_inst(id, Rc::clone(bb))
                                          .add_operand(src1)
                                          .add_operand(src2)
                                          .add_bb(bb1);
                let jmp = Instruction::new_br_inst(false, None, bb2, None, Rc::clone(bb));
                new_inst_list.push(Rc::new(RefCell::new(new_br)));
                new_inst_list.push(Rc::new(RefCell::new(jmp)));
              } else {
                // turn cmp into...slt / slti / seqz / snez
                let src1 = inst.borrow().get_operand(0).unwrap();
                let src2 = inst.borrow().get_operand(1).unwrap();
                let dest = inst.borrow().get_dest().unwrap();
                assert!(!src1.is_const() || !src2.is_const());
                // if compare type is CmpTy::Eq / CmpTy::Ne
                // then we need to use xor to test the two operand
                if *cmpty == CmpTy::Eq || *cmpty == CmpTy::Ne {
                  let tmp_dest = Value::new_tmp_vreg(dest);
                  let xor = Instruction::new_binary_inst(src1,
                                                        src2,
                                                        tmp_dest,
                                                        BinaryTy::Xor,
                                                        Rc::clone(bb));
                  let asmid = match cmpty {
                    CmpTy::Eq => self.asm("seqz"),
                    CmpTy::Ne => self.asm("snez"),
                    _ => panic!("??"),
                  };
                  let mut new_cmp = Instruction::new_asm_inst(asmid, Rc::clone(bb))
                                                .add_operand(tmp_dest);
                  new_cmp.set_dest(dest);
                  new_inst_list.push(Rc::new(RefCell::new(xor)));
                  new_inst_list.push(Rc::new(RefCell::new(new_cmp)));
                } else {
                  let (mut src1, mut src2) = match cmpty {
                    CmpTy::Ge | CmpTy::Gt => (src2, src1),
                    CmpTy::Le | CmpTy::Lt => (src1, src2),
                    _ => panic!(""),
                  };

                  let asmid = if src2.is_const() {self.asm("slti")} else {self.asm("slt")};

                  if src2.is_const() && *cmpty == CmpTy::Le {
                    src2.plus(1);
                  } else if src1.is_const() && *cmpty == CmpTy::Le {
                    src1.plus(-1);
                  }

                  let src1 = if *cmpty == CmpTy::Le && !src2.is_const() || src1.is_const() {
                    let tmp_dest = Value::new_tmp_vreg(dest);
                    let src2 = if src1.is_const() {self.reg("x0")} else {Value::new_const_i32(-1)};
                    let add = Instruction::new_binary_inst(src1, src2, tmp_dest, BinaryTy::Add, Rc::clone(bb));
                    new_inst_list.push(Rc::new(RefCell::new(add)));
                    tmp_dest
                  } else {
                    src1
                  };

                  let mut newcmp = Instruction::new_asm_inst(asmid, Rc::clone(bb))
                                            .add_operand(src1)
                                            .add_operand(src2);
                  newcmp.set_dest(dest);
                  new_inst_list.push(Rc::new(RefCell::new(newcmp)));
                }
              }
            }
            InstrTy::Branch => {
              if inst.borrow().is_condi_br() {
                let src = inst.borrow().get_operand(0).unwrap();
                let bb1 = inst.borrow().get_bb(0).unwrap();
                let bb2 = inst.borrow().get_bb(1).unwrap();

                let br_id = self.asm("bnez");

                let new_br = Instruction::new_asm_inst(br_id, Rc::clone(bb))
                                          .add_operand(src)
                                          .add_bb(bb1);
                let jmp = Instruction::new_br_inst(false, None, bb2, None, Rc::clone(bb));
                
                new_inst_list.push(Rc::new(RefCell::new(new_br)));
                new_inst_list.push(Rc::new(RefCell::new(jmp)));
              } else {
                new_inst_list.push(Rc::clone(inst));
              }
            }
            InstrTy::Call(_name) => {
              // TODO: support more than eight arguments
              assert!(inst.borrow().src_operand_list().len() <= 8);
              new_inst_list.push(Rc::clone(inst));
            }
            InstrTy::Return => {
              // TODO: support other return type
              new_inst_list.push(Rc::clone(inst));
            }
            InstrTy::Asm(_) => panic!(""),
          }
        }
      }
      bb.borrow_mut().update_inst_list(new_inst_list);
    }
  }
}

impl RISCV32 {
  fn asm(&self, name: &str) -> (u8, String) {
    self.inst_set.iter()
                  .enumerate()
                  .find(|(_, &asm)| asm.eq(name))
                  .map(|(id, name)| (id as u8, name.to_string()))
                  .unwrap()
  }

  fn reg(&self, name: &str) -> Value {
    let id = self.reg.iter().enumerate().find(|(_, &reg)| reg.eq(name)).map(|(id, _)| id as u8).unwrap();
    Value::new_phyreg(id)
  }

  pub fn new() -> RISCV32 {
    RISCV32 {
      inst_set: ["add",
                 "addi",
                 "xor",
                 "xori",
                 "sub",
                 "mul",
                 "div",
                 "beq",
                 "bne",
                 "beqz",
                 "bnez",
                 "blt",
                 "bge",
                 "j",
                 "ret",
                 "slt",
                 "slti",
                 "seqz",
                 "snez",
                 "call"],
      reg:      ["x0",
                 "ra",
                 "sp",
                 "gp",
                 "tp",
                 "t0",
                 "t1",
                 "t2",
                 "s0",
                 "s1",
                 "a0",
                 "a1",
                 "a2",
                 "a3",
                 "a4",
                 "a5",
                 "a6",
                 "a7",
                 "s2",
                 "s3",
                 "s4",
                 "s5",
                 "s6",
                 "s7",
                 "s8",
                 "s9",
                 "s10",
                 "s11",
                 "t3",
                 "t4",
                 "t5",
                 "t6",]
    }
  }
}