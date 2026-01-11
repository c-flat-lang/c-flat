use anyhow::Result;
use std::io::Write;
use wasmtime::*;
use wasmtime_wasi::p2::{WasiCtx, WasiCtxBuilder};

fn main() {
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    if args.len() != 1 {
        eprintln!("Usage: {} <filename>", std::env::args().next().unwrap());
        std::process::exit(1);
    }
    let file_path = &args[0];

    if file_path.ends_with(".wasm") {
        let Ok(data) = std::fs::read(file_path) else {
            eprintln!("Failed to read file {}", &args[0]);
            std::process::exit(1);
        };
        run_wasm(&data).unwrap();
    } else if file_path.ends_with(".bb") {
        run_bitbeat(std::fs::read_to_string(file_path).unwrap()).unwrap();
    } else {
        eprintln!("Unknown file type {}", &args[0]);
        std::process::exit(1);
    }
}

pub fn run_bitbeat(data: String) -> Result<()> {
    let module: bitbeat::Module = ron::from_str(&data)?;
    let mut machine = bitbeat::Machine::default();
    machine.register_module(module);
    machine.spawn("main", "main", &[]);

    let start = std::time::Instant::now();
    machine.run();

    let seconds = start.elapsed().as_secs_f32();
    println!("Done in {}", seconds);

    Ok(())
}

pub fn run_wasm(wasm_bytes: &[u8]) -> Result<()> {
    let engine = Engine::default();
    let wasi_ctx = WasiCtxBuilder::new().inherit_stdio().build();
    let mut store = Store::new(&engine, wasi_ctx);

    let module = Module::new(&engine, wasm_bytes)?;

    let mut linker: Linker<WasiCtx> = Linker::new(&engine);
    linker.func_wrap("core", "write_i32", |a: i32| {
        print!("{}", a);
    })?;
    linker.func_wrap(
        "core",
        "write",
        |mut ctx: Caller<'_, WasiCtx>, ptr: i32, len: i32| {
            let Some(exported_memory) = ctx.get_export("memory") else {
                return 1;
            };

            let Some(memory) = exported_memory.into_memory() else {
                println!("Memory export not found");
                return 1;
            };

            // let offset_ptr = iovs_ptr as usize;
            // let mut ptr_str_buf = [0u8; 4];
            // memory.read(&ctx, offset_ptr, &mut ptr_str_buf).unwrap();
            // let mut ptr_len_buf = [0u8; 4];
            // memory.read(&ctx, offset_ptr + 4, &mut ptr_len_buf).unwrap();
            // let len = u32::from_le_bytes(ptr_len_buf) as usize;
            // let ptr = u32::from_le_bytes(ptr_str_buf) as usize;
            let len = len as usize;
            let ptr = ptr as usize;
            let mut string = vec![0u8; len];
            memory.read(&ctx, ptr, &mut string).unwrap();

            print!("{}", String::from_utf8(string.clone()).unwrap());
            std::io::stdout().flush().unwrap();

            0
        },
    )?;

    let instance = linker.instantiate(&mut store, &module)?;
    let main_func = instance.get_typed_func::<(), i32>(&mut store, "main")?;

    let start = std::time::Instant::now();

    let ret = main_func.call(&mut store, ())?;

    let seconds = start.elapsed().as_secs_f32();

    println!("Done in {}", seconds);
    eprintln!("Return value: {}", ret);
    Ok(())
}
