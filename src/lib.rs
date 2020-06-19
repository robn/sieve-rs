#[macro_use]
extern crate nom;
extern crate email;

mod execution;
mod parser;
mod types;

pub mod error;
pub mod script;

pub use self::script::compile;
