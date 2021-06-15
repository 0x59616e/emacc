use crate::value::Value;

struct LiveRange {
  start: u32,
  end: u32,
}

impl LiveRange {
  fn new(start: u32, end: u32) -> LiveRange {
    LiveRange{start, end}
  }
}

struct LiveInterval {
  LiveRangeSet: Vec<LiveRange>,
  ValueSet: Vec<Value>,
}
