enum MachineOperandTy {
  VReg(u32),
  PhyReg(u8),
  Imm(u16),
  StackMemAddr {
    base: u64,
    offset: u64,
  }
}

pub(super) struct MachineOperand {
  ty: MachineOperandTy,
}