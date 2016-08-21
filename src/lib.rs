#[macro_use]
extern crate nom;
extern crate email;

mod types;
mod parser;
mod execution;

pub mod script;
pub mod error;

pub use self::script::compile;
