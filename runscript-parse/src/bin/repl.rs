use chumsky::Parser;
use reedline::{DefaultPrompt, DefaultPromptSegment, Reedline, Signal};
use runscript_parse::command_chain;

fn main() {
    let mut line_editor = Reedline::create();
    let prompt = DefaultPrompt::new(DefaultPromptSegment::Empty, DefaultPromptSegment::Empty);

    let commands = runscript_parse::pad_intercommand(command_chain())
        .repeated()
        .then(chumsky::prelude::end());

    loop {
        let sig = line_editor.read_line(&prompt);
        match sig {
            Ok(Signal::Success(buffer)) => {
                println!("{:#?}", commands.parse(buffer.as_bytes()))
            }
            Ok(Signal::CtrlD) | Ok(Signal::CtrlC) => {
                break;
            }
            _ => {}
        }
    }
}
