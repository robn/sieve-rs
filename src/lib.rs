#[macro_use]
extern crate nom;
extern crate email;

pub mod script;
pub mod error;
pub mod action;

mod parser;
mod execution;

pub use self::script::compile;
