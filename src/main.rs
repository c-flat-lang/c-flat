use bitbox::passes::DebugPass;
use bitbox::passes::PassOutput;
use cflat::{Cli, StageContext, Target, drive};
use std::path::PathBuf;

fn main() {
    let cli_options = Cli::parse();

    eprintln!("{: >30} {}", "Compiling", cli_options.file_path);

    let ctx = StageContext {
        target: cli_options.target,
        debug_mode: cli_options.debug_mode,
        unix_newlines: cli_options.unix_newlines,
        entry: PathBuf::from(cli_options.file_path.as_str()),
        verbose: cli_options.verbose,
        ..Default::default()
    };

    let mut module = match drive(ctx) {
        Ok(module) => module,
        Err(err) => {
            let errors = err.report("");
            eprintln!("{}", errors);
            std::process::exit(1);
        }
    };

    let compiler_debug_mode: Option<DebugPass> = cli_options.debug_mode.and_then(Into::into);
    let mut compiler = bitbox::Compiler::new(
        &cli_options.file_path,
        cli_options.target,
        compiler_debug_mode,
    );

    if cli_options.verbose {
        compiler.verbose();
    }

    if let Some(linking_options) = cli_options.link {
        let options = linking_options
            .split(' ')
            .map(|s| s.trim().to_string())
            .collect::<Vec<String>>();
        compiler.set_linking_options(options);
    }

    let compiler_output = compiler.run(&mut module);

    eprintln!("{: >30}", "Finished");

    match compiler_output {
        Ok(PassOutput::Nothing) if cli_options.run => {
            let path = compiler.file_output_path();
            let Some(path) = path.as_path().to_str() else {
                panic!("Failed to create string from path");
            };
            eprintln!("{: >30} {}", "Running", path);
            match cli_options.target {
                #[cfg(feature = "wasm-runtime")]
                Target::Wasm32 | Target::Bitbeat => match runtime::run(path) {
                    Ok(()) => {}
                    Err(err) => {
                        eprintln!("{}", err);
                        std::process::exit(1);
                    }
                },
                #[cfg(not(feature = "wasm-runtime"))]
                Target::Wasm32 | Target::Bitbeat => {
                    eprintln!(
                        "Error: Running Wasm and Bitbeat targets is not supported without the 'wasm-runtime' feature."
                    );
                    std::process::exit(1);
                }

                Target::X86_64Linux => {
                    let cmd_result = std::process::Command::new(format!("./{}", path)).spawn();
                    match cmd_result {
                        Ok(_) => {}
                        Err(e) => {
                            println!("{}", e);
                            std::process::exit(1);
                        }
                    }
                }
            }
        }
        Ok(PassOutput::String(result)) => {
            println!("{}", result);
        }
        Ok(_) => {
            // Compilation succeeded, output written to file.
        }
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}
