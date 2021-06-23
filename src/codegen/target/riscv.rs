use crate::ir::function::Function;
use crate::ir::instruction::{Instruction, InstrTy, BinaryTy, CmpTy};
use crate::ir::ssavaluecounter::SSAValueCounter;
use crate::ir::value::{Value, DataTy};
use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap};
use std::rc::Rc;
use super::Target;
use super::RegisterSaver;
use super::super::liveinterval::LiveIntervalInfo;



pub struct RISCV32 {
  inst_set: [&'static str; 20],
  reg:      [(&'static str, Option<RegisterSaver>); 32],
}


// TODO: emit asm with macro?
fn emit_asm(asm: &str, op: &str) {
  println!("{:8}{:<8}{}", " ", asm, op);
}

fn emit_prologue(func: &Rc<RefCell<Function>>) {
  println!(".globl   {}", func.borrow().get_name());
  println!("{}:", func.borrow().get_name());
}

impl Target for RISCV32 {
  fn allocate_stack_and_emit_asm(&self, func: &Rc<RefCell<Function>>) {
    emit_prologue(func);
    // allocate stack space for 'alloca' instruction
    // FIXME: We only support 4 bytes data at present.
    let mut stack_offset: HashMap<Value, usize> = HashMap::new();
    let mut bb_name: HashMap<Value, String> = HashMap::new();

    for (i, bb) in func.borrow().bb_list().enumerate() {
      bb_name.insert(bb.borrow().get_label(), format!(".BB_{}", i));
    }

    let root = func.borrow().get_root();
    let stack_slots = root.borrow().inst_list()
                                    .filter(|inst| inst.borrow().is_alloca_inst())
                                    .map(|inst| inst.borrow().get_dest().unwrap())
                                    .collect::<Vec<_>>();
    let offset_val = (stack_slots.len() * 4 + 0xf) & !0xf;
    emit_asm("addi", format!("sp, sp, -{}", offset_val).as_str());
    for (i, slot) in stack_slots.into_iter().enumerate() {
      stack_offset.insert(slot, offset_val - (i + 1) * 4);
    }

    for bb in func.borrow().bb_list() {
      if bb.borrow().preds_list().count() > 0 {
        println!("{}", bb_name.get(&bb.borrow().get_label()).unwrap());
      }

      for inst in bb.borrow().inst_list() {
        match inst.borrow().get_inst_ty() {
          InstrTy::Alloca => (),
          InstrTy::Store  => {
            let src = self.get_reg_abi_name(inst.borrow().get_operand(0).unwrap().get_phyreg_num());
            let offset = stack_offset.get(&inst.borrow().get_dest().unwrap()).unwrap();
            emit_asm("sw", format!("{}, {}(sp)", src, offset).as_str());
          }
          InstrTy::Load => {
            let dest = self.get_reg_abi_name(inst.borrow().get_dest().unwrap().get_phyreg_num());
            let offset = stack_offset.get(&inst.borrow().get_operand(0).unwrap()).unwrap();
            emit_asm("lw", format!("{}, {}(sp)", dest, offset).as_str());
          }
          InstrTy::Copy => {
            let dest = self.get_reg_abi_name(inst.borrow().get_dest().unwrap().get_phyreg_num());
            let src = inst.borrow().get_operand(0).unwrap();
            if src.is_phy_reg() {
              let src = self.get_reg_abi_name(inst.borrow().get_operand(0).unwrap().get_phyreg_num());
              emit_asm("mv", format!("{}, {}", dest, src).as_str());
            } else if src.is_const() {
              let val = src.get_const_i32();
              emit_asm("li", format!("{}, {}", dest, val).as_str());
            }
          }
          InstrTy::Return => {
            emit_asm("addi", format!("sp, sp, {}", offset_val).as_str());
            emit_asm("ret", "");
          }
          InstrTy::Asm(asm) => {
            // FIXME: It's a mess
            let mut s = String::new();
            let src1 = self.get_reg_abi_name(inst.borrow().get_operand(0).unwrap().get_phyreg_num());
            s.push_str(src1);
            if let Some(src2) = inst.borrow().get_operand(1) {
              s.push_str(format!(", {}", self.get_reg_abi_name(src2.get_phyreg_num())).as_str());
            }
            if let Some(src3) = inst.borrow().get_operand(2) {
              if src3.is_const() {
                s.push_str(format!(", {}", src3.get_const_i32()).as_str());
              } else {
                s.push_str(self.get_reg_abi_name(src3.get_phyreg_num()));
              }
            }
            if let Some(bb) = inst.borrow().get_bb(0) {
              s.push_str(format!(", {}", bb_name.get(&bb.borrow().get_label()).unwrap()).as_str());
            }
            emit_asm(asm, s.as_str());
          }
          InstrTy::Branch => {
            let bb = inst.borrow().get_bb(0).unwrap();
            let name = bb_name.get(&bb.borrow().get_label()).unwrap();
            emit_asm("j", format!("{}", name).as_str());
          }
          InstrTy::Cmp(_) => panic!("There should not have been a cmp"),
          InstrTy::Binary(binty) => {
            let rd = inst.borrow().get_dest().unwrap();
            let rs1 = inst.borrow().get_operand(0).unwrap();
            let rs2 = inst.borrow().get_operand(1).unwrap();
            match binty {
              BinaryTy::Add => {
                let (rs1, rs2) = if rs1.is_const() {
                  (rs2, rs1)
                } else {
                  (rs1, rs2)
                };
                let rd = self.get_reg_abi_name(rd.get_phyreg_num());
                let rs1 = self.get_reg_abi_name(rs1.get_phyreg_num());
                if rs2.is_const() {
                  emit_asm("addi", format!("{}, {}, {}", rd, rs1, rs2.get_const_i32()).as_str());
                } else {
                  let rs2 = self.get_reg_abi_name(rs2.get_phyreg_num());
                  emit_asm("add", format!("{}, {}, {}", rd, rs1, rs2).as_str());
                }
              }
              _ => {
                let rd = self.get_reg_abi_name(rd.get_phyreg_num());
                let rs1 = self.get_reg_abi_name(rs1.get_phyreg_num());
                let rs2 = self.get_reg_abi_name(rs2.get_phyreg_num());
                let asm = match binty {
                  BinaryTy::Sub => "sub",
                  BinaryTy::Mul => "mul",
                  _ => panic!("Unsuported"),
                };
                emit_asm(asm, format!("{}, {}, {}", rd, rs1, rs2).as_str());
              }
            }
           
          }
          InstrTy::Call(name) => emit_asm("call", name.as_str()),
          InstrTy::Phi => (),
        }
      }
    }
  }

  fn calc_register_binding(&self, func: &Rc<RefCell<Function>>, li_info: &mut LiveIntervalInfo) {
    for bb in func.borrow().bb_list() {
      for inst in bb.borrow().inst_list() {
        if inst.borrow().is_call_inst() {
          for (i, op) in inst.borrow().src_operand_list().into_iter().enumerate() {
            if op.is_vreg() {
              li_info.get_li(&op).borrow_mut().set_binding(self.reg(format!("a{}", i).as_str()));
            }
          }

          if let Some(dest) = inst.borrow().get_dest() {
            if dest.is_vreg() {
              li_info.get_li(&dest).borrow_mut().set_binding(self.reg("a0"));
            }
          }
        }

        if inst.borrow().is_return_inst() {
          if let Some(src) = inst.borrow().get_operand(0) {
            if src.is_vreg() {
              li_info.get_li(&src).borrow_mut().set_binding(self.reg("a0"));
            }
          }
        }
      }
    }
  }

  fn instruction_lowering(&self, func: &Rc<RefCell<Function>>) {
    // lower instruction

    let mut reg_stack_map = HashMap::new();
    let mut svb = SSAValueCounter::new(Some(func));

    let root = func.borrow().get_root();
    // move ra into stack
    let ra = self.reg("ra");
    let dest = Value::new_vreg(svb.new_num(), DataTy::I32, true);
    println!("{}", dest);
    let alloca = Instruction::new_alloca_inst(dest, Rc::clone(&root));
    let store = Instruction::new_store_inst(ra, dest, Rc::clone(&root));
    root.borrow_mut().insert_at_head(alloca);
    root.borrow_mut().insert_at_head(store);
    reg_stack_map.insert(ra, dest);
    // move function parameter into correspoing virtual register:
    // %0 = copy a0
    // %1 = copy a1
    // etc...
    for (i, param) in func.borrow().param_list().into_iter().enumerate() {
      let reg = self.reg(format!("a{}", i).as_str());
      let copy = Instruction::new_copy_inst(param, reg, Rc::clone(&root));
      root.borrow_mut().insert_at_head(copy);
    }

    for bb in func.borrow().bb_list() {
      let mut new_inst_list = vec![];
      {
        let bbb = bb.borrow();
        let mut iter = bbb.inst_list().peekable();

        while let Some(inst) = iter.next() {
          // FIXME: The imm should be within -2048~2047. Currently I just ignore this restriction
          match inst.borrow().get_inst_ty() {
            // do nothing
            InstrTy::Alloca => new_inst_list.push(Rc::clone(inst)),
            // do nothing
            InstrTy::Load   => new_inst_list.push(Rc::clone(inst)),
            // do nothing
            InstrTy::Store  => new_inst_list.push(Rc::clone(inst)),
            // do nothing
            InstrTy::Phi    => new_inst_list.push(Rc::clone(inst)),
            // do nothing
            InstrTy::Copy   => new_inst_list.push(Rc::clone(inst)),
            InstrTy::Binary(binty) => {
              match binty {
                BinaryTy::Add => {
                  let src1 = inst.borrow().get_operand(0).unwrap();
                  let src2 = inst.borrow().get_operand(1).unwrap();
                  if src1.is_const() && src2.is_const() {
                    // we need to split them...
                    // This situation is due to phi-elimination.
                    let (src1, src2) = if src1.is_const() {
                      (src1, src2)
                    } else {
                      (src2, src1)
                    };
                    let dest = inst.borrow().get_dest().unwrap();
                    let tmp_dest = Value::new_tmp_vreg(dest);
                    let zero = self.reg("x0");
                    let add = Instruction::new_binary_inst(zero, src1, tmp_dest, BinaryTy::Add, Rc::clone(bb));
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
                  } else {
                    new_inst_list.push(Rc::clone(inst));
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
            InstrTy::Call(name) => {
              // TODO: support more than eight arguments
              assert!(inst.borrow().src_operand_list().len() <= 8);
              // move function argument into a temporary register
              // %d = call(%a, %b)
              //  |
              //  V
              // %n1 = copy, %a
              // %n2 = copy, %b
              // %.d = call(%n1, %n2)
              // %d = copy, %.d
              let mut arg_list = Vec::new();
              for src in inst.borrow().src_operand_list() {
                if src.is_vreg() || src.is_const() {
                  let tmp_src = Value::new_vreg(svb.new_num(), src.get_data_ty(), false);
                  let copy = Instruction::new_copy_inst(tmp_src, src, Rc::clone(bb));
                  new_inst_list.push(Rc::new(RefCell::new(copy)));
                  arg_list.push(tmp_src);
                } else {
                  arg_list.push(src);
                }
              }
              let call = Instruction::new_call_inst(None, name.clone(), arg_list, Rc::clone(bb));
              let call = Rc::new(RefCell::new(call));
              new_inst_list.push(Rc::clone(&call));
              let dest = inst.borrow().get_dest();
              if let Some(dest) = dest {
                let tmp_dest = Value::new_tmp_vreg(dest);
                call.borrow_mut().set_dest(tmp_dest);
                let copy = Instruction::new_copy_inst(dest, tmp_dest, Rc::clone(bb));
                new_inst_list.push(Rc::new(RefCell::new(copy)));
              }
            }
            InstrTy::Return => {
              // move ra back
              let ra = self.reg("ra");
              let src = reg_stack_map.get(&ra).unwrap();
              let load = Instruction::new_load_inst(*src, ra, Rc::clone(bb));
              new_inst_list.push(Rc::new(RefCell::new(load)));
              new_inst_list.push(Rc::clone(inst));
            }
            InstrTy::Asm(_) => panic!(""),
          }
        }
      }
      bb.borrow_mut().update_inst_list(new_inst_list);
    }
  }

  fn get_avail_regs(&self) -> BTreeSet<Value> {
    self.reg[9..].iter().map(|(name, _)| self.reg(name)).collect()
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
    let id = self.reg.iter()
                     .enumerate()
                     .find(|(_, &(reg, _))| reg.eq(name)).map(|(id, (_, saver))| (id as u8, saver))
                     .unwrap();
    Value::new_phyreg(id.0, *id.1)
  }

  fn get_reg_abi_name(&self, id: u8) -> &str {
    self.reg[id as usize].0
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
      reg:      [("x0", None),                           // x0      
                 ("ra", Some(RegisterSaver::Caller)),    // x1
                 ("sp", Some(RegisterSaver::Callee)),    // x2
                 ("gp", None),                           // x3
                 ("tp", None),                           // x4
                 ("t0", Some(RegisterSaver::Caller)),    // x5
                 ("t1", Some(RegisterSaver::Caller)),    // x6
                 ("t2", Some(RegisterSaver::Caller)),    // x7
                 ("s0", Some(RegisterSaver::Callee)),    // x8
                 ("s1", Some(RegisterSaver::Callee)),    // x9
                 ("a0", Some(RegisterSaver::Caller)),    // x10
                 ("a1", Some(RegisterSaver::Caller)),    // x11
                 ("a2", Some(RegisterSaver::Caller)),    // x12
                 ("a3", Some(RegisterSaver::Caller)),    // x13
                 ("a4", Some(RegisterSaver::Caller)),    // x14
                 ("a5", Some(RegisterSaver::Caller)),    // x15
                 ("a6", Some(RegisterSaver::Caller)),    // x16
                 ("a7", Some(RegisterSaver::Caller)),    // x17
                 ("s2", Some(RegisterSaver::Callee)),    // x18
                 ("s3", Some(RegisterSaver::Callee)),    // x19
                 ("s4", Some(RegisterSaver::Callee)),    // x20
                 ("s5", Some(RegisterSaver::Callee)),    // x21
                 ("s6", Some(RegisterSaver::Callee)),    // x22
                 ("s7", Some(RegisterSaver::Callee)),    // x23
                 ("s8", Some(RegisterSaver::Callee)),    // x24
                 ("s9", Some(RegisterSaver::Callee)),    // x25
                 ("s10", Some(RegisterSaver::Callee)),   // x26
                 ("s11", Some(RegisterSaver::Callee)),   // x27
                 ("t3", Some(RegisterSaver::Caller)),    // x28
                 ("t4", Some(RegisterSaver::Caller)),    // x29
                 ("t5", Some(RegisterSaver::Caller)),    // x30
                 ("t6", Some(RegisterSaver::Caller)),]   // x31
    }
  }
}