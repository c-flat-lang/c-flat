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
            let errors = err.report(&cli_options.file_path, &source);
            eprintln!("{}", errors);
            std::process::exit(1);
        }
    };
    let compiler_debug_mode: Option<DebugPass> = cli_options.debug_mode.and_then(Into::into);
    let compiler = bitbox::Compiler::new(
        &cli_options.file_path,
        cli_options.target,
        compiler_debug_mode,
    );

    match compiler.run(&mut module) {
        Ok(Some(path)) if cli_options.run => {
            eprintln!("{: >30} {}", "Running", path);
            match cli_options.target {
                cflat::Target::Wasm32 | cflat::Target::Bitbeat => match runtime::run(path.as_str())
                {
                    Ok(()) => {}
                    Err(err) => {
                        eprintln!("{}", err);
                        std::process::exit(1);
                    }
                },
                cflat::Target::X86_64Linux => {
                    let cmd_result = std::process::Command::new(path.as_str()).output();
                    match cmd_result {
                        Ok(output) => {
                            if !output.status.success() {
                                println!("{}", String::from_utf8_lossy(&output.stderr));
                            }
                        }
                        Err(e) => {
                            println!("{}", e);
                            std::process::exit(1);
                        }
                    }
                }
            }
        }
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
        _ => {}
    }
}
