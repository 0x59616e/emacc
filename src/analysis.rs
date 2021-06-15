pub mod dominator;
pub mod domfrontier;

trait Analysis {
  fn run(&mut self);
}