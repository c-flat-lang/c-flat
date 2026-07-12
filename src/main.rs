use bitbox::passes::DebugPass;
use cflat::Cli;

use cflat::front_end_compiler;

fn main() {
    use bitbox::passes::PassOutput;

    let cli_options = Cli::parse();

    let mut module = match front_end_compiler(&cli_options) {
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

    if let Some(linking_options) = cli_options.link {
        let options = linking_options
            .split(' ')
            .map(|s| s.trim().to_string())
            .collect::<Vec<String>>();
        compiler.set_linking_options(options);
    }

    let compiler_output = compiler.run(&mut module);

    match compiler_output {
        Ok(PassOutput::Nothing) if cli_options.run => {
            let path = compiler.file_output_path();
            let Some(path) = path.as_path().to_str() else {
                panic!("Failed to create string from path");
            };
            eprintln!("{: >30} {}", "Running", path);
            match cli_options.target {
                #[cfg(feature = "wasm-runtime")]
                cflat::Target::Wasm32 | cflat::Target::Bitbeat => match runtime::run(path) {
                    Ok(()) => {}
                    Err(err) => {
                        eprintln!("{}", err);
                        std::process::exit(1);
                    }
                },
                #[cfg(not(feature = "wasm-runtime"))]
                cflat::Target::Wasm32 | cflat::Target::Bitbeat => {
                    eprintln!(
                        "Error: Running Wasm and Bitbeat targets is not supported without the 'wasm-runtime' feature."
                    );
                    std::process::exit(1);
                }

                cflat::Target::X86_64Linux => {
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
