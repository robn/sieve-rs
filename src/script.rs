use crate::error::CompileError;
use crate::execution;
use crate::parser;
use crate::types::{Action, Script};
use email::MimeMessage;
// use nom::IResult;

pub fn compile(script: &str) -> Result<Script, CompileError> {
    let res = parser::start(script);
    println!("{:#?}", res);
    Ok(Script { commands: vec![] })
    /*
    match res {
      IResult::Incomplete(i) => Err(CompileError::Incomplete(i)),
      IResult::Error(_)      => Err(CompileError::Unknown(res)),
      IResult::Done(_,c)     => Ok(Script{ commands: c }),
    }
    */
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
    use crate::error::CompileError;
    use nom::Needed;
    // use nom::{Err, ErrorKind, IResult};

    #[test]
    fn compile_test() {
        assert_eq!(
            compile("require \"fileinto\"").unwrap_err(),
            CompileError::Incomplete(Needed::Size(19))
        );
        //assert_eq!(compile("require \"fileinto\"; 2ab xxx").unwrap_err(),
        //  CompileError::Unknown(IResult::Error(Err::Position(ErrorKind::Eof, " 2ab xxx"))));
        assert!(compile("require \"fileinto\";").is_ok());
    }
}
