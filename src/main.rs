use bitbox::passes::DebugPass;
use cflat::Cli;

use cflat::front_end_compiler;
#[cfg(feature = "default")]
fn main() {
    let cli_options = Cli::parse();
    let source = std::fs::read_to_string(&cli_options.file_path).expect("Failed to read file");

    let mut module = match front_end_compiler(&source, &cli_options) {
        Ok(module) => module,
        Err(err) => {
            println!("{:?}", err);
            return;
        }
    };
    let compiler_debug_mode: Option<DebugPass> = cli_options.debug_mode.and_then(Into::into);
    if let Err(error) = bitbox::Compiler::new(
        &cli_options.file_path,
        cli_options.target,
        compiler_debug_mode,
    )
    .run(&mut module)
    {
        eprintln!("{}", error);
        std::process::exit(1);
    }
}
