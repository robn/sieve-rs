extern crate sieve;

fn main() {
  let src: &'static str = include_str!("../../testdata/rfc5228-full.sieve");
  let script = sieve::compile(src).unwrap();
}
