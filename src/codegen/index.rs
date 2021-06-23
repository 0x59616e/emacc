#[derive(Clone, Copy, PartialOrd, Ord, Eq, PartialEq)]
pub struct Index {
  index: u32,
}

impl Index {
  fn new(index: u32) -> Self {
    Index {index}
  }
}

impl std::fmt::Display for Index {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.index)
  }
}

pub(super) struct IndexBuilder {
  index: u32,
}

impl IndexBuilder {
  pub fn create_new_index(&mut self) -> Index {
    let result = Index::new(self.index);
    self.index += 1;
    result
  }

  pub fn new() -> Self {
    IndexBuilder{index: 0}
  }
}