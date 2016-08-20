use script::Script;
use action::Action;
use parser::Command;

use email::MimeMessage;

pub fn run(script: &Script, email: &MimeMessage) -> Vec<Action> {
  run_commands(&script.commands, email)
}

fn run_commands(commands: &Vec<Command>, email: &MimeMessage) -> Vec<Action> {
  commands.iter().flat_map(|c|
    match &*c.identifier {
      "require" => cmd_require(&c, email),
      "if"      => cmd_if(&c, email),
      "elsif"   => cmd_elsif(&c, email),
      _         => vec!(), // XXX unknown command, throw exception
    }
  ).collect()
}

fn cmd_require(command: &Command, email: &MimeMessage) -> Vec<Action> {
  vec!()
}

fn cmd_if(command: &Command, email: &MimeMessage) -> Vec<Action> {
  vec!()
}

fn cmd_elsif(command: &Command, email: &MimeMessage) -> Vec<Action> {
  vec!()
}
