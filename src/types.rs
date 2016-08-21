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
pub struct Command {
  pub identifier: String,
  pub arguments:  Arguments,
  pub commands:   Vec<Command>,
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
