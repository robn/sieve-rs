use parser;
use execution;
use types::{Script,Command,Action};
use error::CompileError;

use nom::IResult;
use email::MimeMessage;

pub fn compile(script: &str) -> Result<Script,CompileError> {
  let res = parser::start(script);
  match res {
    IResult::Incomplete(i) => Err(CompileError::Incomplete(i)),
    IResult::Error(_)      => Err(CompileError::Unknown(res)),
    IResult::Done(_,c)     => Ok(Script{ commands: c }),
  }
}

impl Script {
  pub fn execute(&self, message: &str) -> Vec<Action> {
    let email = MimeMessage::parse(message).unwrap();
    execution::run(self, &email)
  }
}

#[cfg(test)]
mod tests {
  use super::compile;
  use error::CompileError;
  use nom::{Needed,IResult,Err,ErrorKind};

  #[test]
  fn compile_test() {
    assert_eq!(compile("require \"fileinto\"").unwrap_err(),
      CompileError::Incomplete(Needed::Size(19)));
    assert_eq!(compile("require \"fileinto\"; 2ab xxx").unwrap_err(),
      CompileError::Unknown(IResult::Error(Err::Position(ErrorKind::Eof, " 2ab xxx"))));
    assert!(compile("require \"fileinto\";").is_ok());
  }
}
