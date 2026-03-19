fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    if args.len() != 1 {
        eprintln!(
            "Usage: {} <filename>",
            std::env::args().next().unwrap_or("No filename".to_string())
        );
        std::process::exit(1);
    }
    let file_path = &args[0];

    let Err(err) = runtime::run(file_path) else {
        return;
    };
    eprintln!("{}", err);
    std::process::exit(1);
}
