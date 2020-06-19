use crate::types::{Action, Command, Script};
use email::MimeMessage;

pub fn run(script: &Script, email: &MimeMessage) -> Vec<Action> {
    run_commands(&script.commands, email)
}

fn run_commands(commands: &[Command], email: &MimeMessage) -> Vec<Action> {
    commands
        .iter()
        .flat_map(|c| {
            println!("{:#?}", c);
            match *c {
                // controls
                Command::If(_, _) => cmd_if(&c, email),
                Command::ElsIf(_, _) => cmd_elsif(&c, email),
                Command::Else(_) => cmd_else(&c, email),
                Command::Require(_) => cmd_require(&c, email),
                Command::Stop => cmd_stop(&c, email),

                // actions
                Command::FileInto(_) => cmd_fileinto(&c, email),
                Command::Redirect(_) => cmd_redirect(&c, email),
                Command::Keep => cmd_keep(&c, email),
                Command::Discard => cmd_discard(&c, email),
            }
        })
        .collect()
}

fn cmd_if(_command: &Command, _email: &MimeMessage) -> Vec<Action> {
    //run_commands(&command.commands, email);
    vec![]
}

fn cmd_elsif(_command: &Command, _email: &MimeMessage) -> Vec<Action> {
    //run_commands(&command.commands, email);
    vec![]
}

fn cmd_else(_command: &Command, _email: &MimeMessage) -> Vec<Action> {
    //run_commands(&command.commands, email);
    vec![]
}

fn cmd_require(_command: &Command, _email: &MimeMessage) -> Vec<Action> {
    vec![]
}

fn cmd_stop(_command: &Command, _email: &MimeMessage) -> Vec<Action> {
    vec![]
}

fn cmd_fileinto(_command: &Command, _email: &MimeMessage) -> Vec<Action> {
    vec![]
}

fn cmd_redirect(_command: &Command, _email: &MimeMessage) -> Vec<Action> {
    vec![]
}

fn cmd_keep(_command: &Command, _email: &MimeMessage) -> Vec<Action> {
    vec![]
}

fn cmd_discard(_command: &Command, _email: &MimeMessage) -> Vec<Action> {
    vec![]
}
