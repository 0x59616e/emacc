use super::index::Index;
use super::liveinterval::LiveInterval;

pub struct LiveRange {
  // [start, end)
  start: Index,
  end  : Index,
}

impl LiveRange {
  pub fn extend_to(&mut self, new_end: Index) {
    assert!(self.end <= new_end);
    self.end = new_end;
  }

  pub fn include(&self, idx: Index) -> bool {
    return self.start <= idx && idx < self.end
  }

  pub fn conflict_with(&self, other: &LiveInterval) -> bool {
    for defp in other.def_point_list() {
      if self.start <= *defp && *defp < self.end {
        return true;
      }
    }
    return false;
  }

  pub fn new(start: Index, end: Index) -> Self {
    assert!(start <= end);
    Self {
      start,
      end,
    }
  }
}

impl std::fmt::Display for LiveRange {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "[{}, {})", self.start, self.end)
  }
}