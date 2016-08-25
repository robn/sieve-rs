#[derive(Clone,PartialEq,Debug)]
pub enum Command {
  // Control commands (RFC 5228 s3)
  If(Test, Vec<Command>),
  ElsIf(Test, Vec<Command>),
  Else(Vec<Command>),
  Require(Vec<String>),
  Stop,

  // Action commands (RFC 5228 s4)
  FileInto(String),
  Redirect(String),
  Keep,
  Discard,
}

#[derive(Clone,PartialEq,Debug)]
pub enum Comparator {
  AsciiCaseMap, // i;ascii-casemap (RFC 4790 s9.2)
  Octet,        // i;octet (RFC 4790 s9.3)
}

#[derive(Clone,PartialEq,Debug)]
pub enum AddressPart {
  LocalPart,
  Domain,
  All,
}

#[derive(Clone,PartialEq,Debug)]
pub enum MatchType {
  Is,
  Contains,
  Matches,
}

#[derive(Clone,PartialEq,Debug)]
pub enum SizeComparator {
  Over,
  Under,
}

#[derive(Clone,PartialEq,Debug)]
pub enum Test {
  // Test commands (RFC 5228 s5)
  Address(Comparator, AddressPart, MatchType, Vec<String>, Vec<String>),
  AllOf(Vec<Test>),
  AnyOf(Vec<Test>),
  Envelope(Comparator, AddressPart, MatchType, Vec<String>, Vec<String>),
  Exists(Vec<String>),
  False,
  Header(Comparator, MatchType, Vec<String>, Vec<String>),
  Not(Box<Test>),
  Size(SizeComparator, usize),
  True,
}

#[derive(Clone,PartialEq,Debug)]
pub struct Script {
  pub commands: Vec<Command>,
}

#[derive(Clone,PartialEq,Debug)]
pub enum Action {
}
