extern crate sieve;

fn main() {
    let src: &'static str = include_str!("../../testdata/rfc5228-3.1-if.sieve");
    let message: &'static str = include_str!("../../testdata/rfc5228-sample1.eml");

    let script = sieve::compile(src).unwrap();
    println!("{:#?}", script);
    script.execute(message);
}
