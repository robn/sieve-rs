#[macro_use]
extern crate nom;

pub mod script;
pub mod error;

mod parser;

pub use self::script::compile;
