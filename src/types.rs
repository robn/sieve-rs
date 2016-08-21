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
pub enum Test {
  // Test commands (RFC 5228 s5)
  Address(Vec<Argument>),
  AllOf(Vec<Test>),
  AnyOf(Vec<Test>),
  Envelope(Vec<Argument>),
  Exists(Vec<Argument>),
  False,
  Header(Vec<Argument>),
  Not(Vec<Test>),
  Size(Vec<Argument>),
  True,

  Unknown(String),
}

#[derive(Clone,PartialEq,Debug)]
pub struct Script {
  pub commands: Vec<Command>,
}

#[derive(Clone,PartialEq,Debug)]
pub enum Action {
}
