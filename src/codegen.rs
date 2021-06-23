pub mod target;
mod liveinterval;
mod liverange;
mod index;

use crate::ir::function::Function;
use crate::ir::instruction::{Instruction, BinaryTy};
use crate::ir::module::Module;
use crate::ir::ssavaluecounter::SSAValueCounter;
use crate::ir::value::*;
use liveinterval::*;
use std::cell::RefCell;
use std::collections::BTreeSet;
use std::rc::Rc;
use target::RegisterSaver;
use target::Target;

pub fn code_gen<T: Target>(module: &Rc<RefCell<Module>>, target: T) {
  // for func in module.borrow().func_list() {
  //   eliminate_phi(func);
  // }

  // println!("-----------after phi elimination-------------");
  // module.borrow().print();

  for func in module.borrow().func_list() {
    target.instruction_lowering(func);
  }

  println!("-----------after instruction lowering--------");
  module.borrow().print();

  // There may be some constants in phi
  // We need to move those constants into virtual registers in order to 
  // to register allocation.
  for func in module.borrow().func_list() {
    for bb in func.borrow().bb_list() {
      for inst in bb.borrow().inst_list() {
        if !inst.borrow().is_phi_inst() {
          break;
        }

        let dest = inst.borrow().get_dest().unwrap();
        let parent_bb = inst.borrow().get_parent();
        for (op, bb) in inst.borrow_mut().get_pairs_list_in_phi_mut() {
          if op.is_const() {
            // FIXME: We can't guarantee there won't be more than one constant in phi.
            //        If occurs, then this IR won't be in SSA form.
            //        We need to change the way of creating tmp vreg.
            let tmp_dest = Value::new_tmp_vreg(dest);
            let copy = Instruction::new_copy_inst(tmp_dest, *op, Rc::clone(&parent_bb));
            bb.borrow_mut().insert_before_terminator(Rc::new(RefCell::new(copy)));
            *op = tmp_dest;
          }
        }
      }
    }
  }
  println!("---------------after constants mov-----------------");
  module.borrow().print();


  regalloc(module, &target);
  println!("---------------after register allocation-----------");
  module.borrow().print();

  // TODO: Determine basic block placement

  // Remove useless jmp
  // After remove useless jmp, there may be no "terminator instruction"
  // at the end of the basic block.
  for func in module.borrow().func_list() {
    let func = func.borrow();
    let mut iter = func.bb_list().collect::<Vec<_>>().into_iter().peekable();

    while let Some(bb) = iter.next() {
      if let Some(next_bb) = iter.peek() {
        let mut bb = bb.borrow_mut();
        let inst = Rc::clone(bb.inst_list().last().unwrap());

        if inst.borrow().is_uncondi_br() {
          let target_bb = inst.borrow().get_bb(0).unwrap();
          if target_bb.borrow().get_label() == next_bb.borrow().get_label() {
            bb.remove_inst(&inst);
          }
        }
      }
    }
  }
  println!("--------------after useless jmp removed------------");
  module.borrow().print();

  // TODO: (1) Spill virtual register to memory
  //           We need a unused register when we use it.
  //           We can use live interval to find a unused register.
  //       (2) Preserve caller-saved register before function call
  //           This is almost the same as (1): find a unused register,
  //           store before, and load after function all.

  println!("----------------------emit asm---------------------");
  for func in module.borrow().func_list() {
    target.allocate_stack_and_emit_asm(func);
  }
}

pub fn regalloc<T: Target>(module: &Rc<RefCell<Module>>, target: &T) {
  for func in module.borrow().func_list() {
    let libuilder = LiveIntervalBuilder::new(func);
    let mut li_info = libuilder.run();
    target.calc_register_binding(&func, &mut li_info);
    let regs = target.get_avail_regs();


    // push live interval of phyregs into 'done' first
    let mut done = li_info.li_list()
                      .filter(|li| li.borrow().get_alloc().is_phy_reg())
                      .map(|li| Rc::clone(li))
                      .collect::<Vec<_>>();

    for li in li_info.li_list().filter(|li| !li.borrow().get_alloc().is_phy_reg()) {

      let unavail = calc_unavail_regs(&done, li);

      let bind = li.borrow().get_binding();
      if let Some(bind) = bind {
        if unavail.contains(&bind) {
          panic!("Can't satisfy the binding demand")
        }
        li.borrow_mut().alloc(bind);
      } else {
        let avail = regs.difference(&unavail).collect::<Vec<_>>();

        let reg_class_prefer = li.borrow().get_reg_class_preference();
        if let RegisterSaver::Callee = reg_class_prefer {
          // try allocate callee-saved register
          // if failed, spill it.
          // FIXME: We may allocate it to a caller-saved register,
          //        save it to memory before calling
          //        and load it back to register after returning.
          if let Some(&&reg) = avail.iter().find(|reg| reg.is_callee_saved()) {
            li.borrow_mut().alloc(reg);
          }
        } else {
          // allocate caller-saved register first
          if let Some(&&reg) = avail.iter().find(|reg| reg.is_caller_saved()) {
            li.borrow_mut().alloc(reg);
          } else if let Some(&&reg) = avail.iter().find(|reg| reg.is_callee_saved()) {
            li.borrow_mut().alloc(reg);
          } else {
            panic!("Running out of register");
          }
        }
      }
      done.push(Rc::clone(li));
    }

    li_info.print();

    for bb in func.borrow().bb_list() {
      let mut remove_list = Vec::new();
      for inst in bb.borrow().inst_list() {
        let dest = inst.borrow().get_dest();
        if let Some(dest) = dest {
          if dest.is_vreg() {
            let reg = li_info.get_li(&dest).borrow().get_alloc();
            if reg.is_phy_reg() {
              inst.borrow_mut().set_dest(reg);
            }
          }
        }

        let src_operand_list = inst.borrow().src_operand_list();
        for src in src_operand_list {
          if !src.is_vreg() {
            continue;
          }

          let reg = li_info.get_li(&src).borrow().get_alloc();
          if reg.is_phy_reg() {
            inst.borrow_mut().replace_op(src, reg);
          }
        }

        // copy coalescing
        if inst.borrow().is_copy_inst() {
          let dest = inst.borrow().get_dest().unwrap();
          let src = inst.borrow().get_operand(0).unwrap();
          if dest == src {
            remove_list.push(Rc::clone(inst));
          }
        }
      }

      for inst in remove_list {
        bb.borrow_mut().remove_inst(&inst);
      }
    }
  
    // Move callee-saved register to stack
    let root = func.borrow().get_root();
    let mut reg_stack_map = Vec::new();
    let mut svb = SSAValueCounter::new(Some(func));
    for li in li_info.li_list() {
      let reg = li.borrow().get_alloc();
      if reg.is_callee_saved() {
        let dest = Value::new_vreg(svb.new_num(), DataTy::I32, true);
        let store = Instruction::new_store_inst(reg, dest, Rc::clone(&root));
        let alloca = Instruction::new_alloca_inst(dest, Rc::clone(&root));
        reg_stack_map.push((reg, dest));
        root.borrow_mut().insert_at_head(alloca);
        root.borrow_mut().insert_at_head(store);
      }
    }

    // move callee-saved register back from stack
    for bb in func.borrow().bb_list() {
      let mut insert_list = Vec::new();
      for inst in bb.borrow().inst_list() {
        if inst.borrow().is_return_inst() {
          for (reg, stack) in reg_stack_map.iter() {
            let load = Instruction::new_load_inst(*stack, *reg, Rc::clone(bb));
            insert_list.push(load);
          }
        }
      }

      for copy in insert_list {
        bb.borrow_mut().insert_before_terminator(Rc::new(RefCell::new(copy)));
      }
    }
  }
}

pub fn calc_unavail_regs(li_list: &Vec<Rc<RefCell<LiveInterval>>>, li: &Rc<RefCell<LiveInterval>>)-> BTreeSet<Value> {
  let mut result = BTreeSet::new();
  for li2 in li_list.iter() {
    if li.borrow().conflict_with(&*li2.borrow()) || li2.borrow().conflict_with(&*li.borrow()) {
      let alloc = li2.borrow().get_alloc();
      if alloc.is_phy_reg() {
        result.insert(alloc);
      }
    }
  }

  result
}

pub fn _eliminate_phi(func: &Rc<RefCell<Function>>) {
  for bb in func.borrow().bb_list() {
    for inst in bb.borrow().inst_list() {

      if ! inst.borrow().is_phi_inst() {
        break;
      }

      let dest = inst.borrow().get_dest().unwrap();
      for (&val, bb) in inst.borrow_mut().get_pairs_list_in_phi() {
        let zero = Value::new_const_i32(0);
        let mov_inst = Rc::new(RefCell::new(Instruction::new_binary_inst(zero,
                                                                         val,
                                                                         dest,
                                                                         BinaryTy::Add,
                                                                         Rc::clone(bb))));
        bb.borrow_mut().insert_before_terminator(mov_inst);
      }
    }
    bb.borrow_mut().remove_all_phi();
  }
}