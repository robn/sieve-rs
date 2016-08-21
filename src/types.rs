#[derive(Clone,PartialEq,Debug)]
pub enum Argument {
  StringList(Vec<String>),
  Number(usize),
  Tag(String),
}

#[derive(Clone,PartialEq,Debug)]
pub struct Arguments {
  pub arguments: Vec<Argument>,
  pub tests:     Vec<Test>,
}

#[derive(Clone,PartialEq,Debug)]
pub enum Command {
  // Control commands (RFC 5228 s3)
  If(Vec<Test>, Vec<Command>),
  ElsIf(Vec<Test>, Vec<Command>),
  Else(Vec<Command>),
  Require(Vec<Argument>),
  Stop,

  // Action commands (RFC 5228 s4)
  FileInto(Vec<Argument>),
  Redirect(Vec<Argument>),
  Keep,
  Discard,

  Unknown(String),
}

#[derive(Clone,PartialEq,Debug)]
pub struct Test {
  pub identifier: String,
  pub arguments:  Arguments,
}

#[derive(Clone,PartialEq,Debug)]
pub struct Script {
  pub commands: Vec<Command>,
}

#[derive(Clone,PartialEq,Debug)]
pub enum Action {
}
