use types::{Script,Command,Action};

use email::MimeMessage;

pub fn run(script: &Script, email: &MimeMessage) -> Vec<Action> {
  run_commands(&script.commands, email)
}

fn run_commands(commands: &Vec<Command>, email: &MimeMessage) -> Vec<Action> {
  commands.iter().flat_map(|c| {
    println!("{}", c.identifier);
    match &*c.identifier {

      // controls
      "if"      => cmd_if(&c, email),
      "elsif"   => cmd_elsif(&c, email),
      "else"    => cmd_else(&c, email),
      "require" => cmd_require(&c, email),
      "stop"    => cmd_stop(&c, email),

      // actions
      "fileinto" => cmd_fileinto(&c, email),
      "redirect" => cmd_redirect(&c, email),
      "keep"     => cmd_keep(&c, email),
      "discard"  => cmd_discard(&c, email),

      // XXX unknown command, throw exception
      _         => vec!(),
    }
  } ).collect()
}

fn cmd_if(command: &Command, email: &MimeMessage) -> Vec<Action> {
  run_commands(&command.commands, email);
  vec!()
}

fn cmd_elsif(command: &Command, email: &MimeMessage) -> Vec<Action> {
  run_commands(&command.commands, email);
  vec!()
}

fn cmd_else(command: &Command, email: &MimeMessage) -> Vec<Action> {
  run_commands(&command.commands, email);
  vec!()
}

fn cmd_require(command: &Command, email: &MimeMessage) -> Vec<Action> {
  vec!()
}

fn cmd_stop(command: &Command, email: &MimeMessage) -> Vec<Action> {
  vec!()
}

fn cmd_fileinto(command: &Command, email: &MimeMessage) -> Vec<Action> {
  vec!()
}

fn cmd_redirect(command: &Command, email: &MimeMessage) -> Vec<Action> {
  vec!()
}

fn cmd_keep(command: &Command, email: &MimeMessage) -> Vec<Action> {
  vec!()
}

fn cmd_discard(command: &Command, email: &MimeMessage) -> Vec<Action> {
  vec!()
}
