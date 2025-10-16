use bitbeat::{Function, Machine, Module, Reg};

#[inline(always)]
fn _loop_fib() {
    let mut machine = Machine::default();
    let mut module = Module::new("main");

    // Iterative fib(n): returns fib(n)
    let mut fib_function = Function::new("fib").arity(1).returns(); // N in Reg(1)
    fib_function
        .instructions()
        .load_imm(Reg(2), 0) // a = 0
        .load_imm(Reg(3), 1) // b = 1
        .load_imm(Reg(4), 2) // i = 2
        .cmp_le(Reg(5), Reg(1), Reg(2)) // if N <= 0
        .jump_if(Reg(5), "exit_a")
        .cmp_le(Reg(5), Reg(1), Reg(3)) // if N <= 1
        .jump_if(Reg(5), "exit_b")
        .label("loop")
        .add(Reg(6), Reg(2), Reg(3)) // tmp = a + b
        .mov(Reg(2), Reg(3)) // a = b
        .mov(Reg(3), Reg(6)) // b = tmp
        .load_imm(Reg(6), 1)
        .add(Reg(4), Reg(4), Reg(6)) // i += 1
        .cmp_le(Reg(5), Reg(4), Reg(1)) // if i <= N
        .jump_if(Reg(5), "loop") // continue loop
        .label("exit_b")
        .send(Reg(0), Reg(3)) // send b
        .halt()
        .label("exit_a")
        .send(Reg(0), Reg(2)) // send a
        .halt();
    module.add_function(fib_function);

    // Main function
    let mut main_function = Function::new("main");
    main_function
        .instructions()
        .load_imm(Reg(0), 30) // N = 30
        .spawn("main", "fib", vec![Reg(0)], Reg(1)) // spawn fib(N)
        .recv(Reg(2)) // recv result
        .print(Reg(2)) // print result
        .halt();
    module.add_function(main_function);

    machine.register_module(module);
    machine.spawn("main", "main", &[]);

    let start = std::time::Instant::now();
    machine.run();
    let seconds = start.elapsed().as_secs_f32();
    println!("Done in {}", seconds);
}

#[inline(always)]
fn _fib() {
    let mut machine = Machine::default();

    let mut module = Module::new("main");

    // fib(N):
    //   if N <= 1 -> return N
    //   else:
    //     A = fib(N - 1)
    //     B = fib(N - 2)
    //     return A + B
    let mut fib_function = Function::new("fib").arity(1).returns(); // Arg is in Reg(0)
    fib_function
        .instructions()
        .load_imm(Reg(2), 1) // const_1 = 1
        .cmp_le(Reg(3), Reg(1), Reg(2)) // if N <= 1
        .jump_if(Reg(3), "exit") // jump to base case
        .load_imm(Reg(4), 1)
        .sub(Reg(5), Reg(1), Reg(4)) // N - 1 → Reg(5)
        .load_imm(Reg(6), 2)
        .sub(Reg(7), Reg(1), Reg(6)) // N - 2 → Reg(7)
        .spawn("main", "fib", vec![Reg(5)], Reg(8)) // pid1 = fib(N - 1)
        .recv(Reg(9)) // result1
        .spawn("main", "fib", vec![Reg(7)], Reg(10)) // pid2 = fib(N - 2)
        .recv(Reg(11)) // result2
        .add(Reg(12), Reg(9), Reg(11)) // sum = result1 + result2
        .send(Reg(0), Reg(12)) // send result to caller
        .halt()
        .label("exit")
        .send(Reg(0), Reg(1)) // base case: send N
        .halt();
    module.add_function(fib_function);

    // main():
    //   spawn fib(10)
    //   recv result
    //   print result
    let mut main_function = Function::new("main");
    main_function
        .instructions()
        .load_imm(Reg(0), 30) // N = 30
        .spawn("main", "fib", vec![Reg(0)], Reg(1)) // pid = spawn fib(N)
        .recv(Reg(2)) // recv result
        .print(Reg(2)) // print result
        .halt();
    module.add_function(main_function);

    machine.register_module(module);

    machine.spawn("main", "main", &[]); // no args for main

    let start = std::time::Instant::now();
    machine.run();
    let seconds = start.elapsed().as_secs_f32();
    println!("Done in {}", seconds);
}

#[inline(always)]
fn _matrix() {
    let mut machine = Machine::default();
    let mut module = Module::new("main");

    // Function to do matrix multiply (no spawn/recv needed)
    let mut matmul = Function::new("matmul");
    matmul
        .instructions()
        // Load matrix A
        .load_imm(Reg(1), 1) // A[0][0]
        .load_imm(Reg(2), 2) // A[0][1]
        .load_imm(Reg(3), 3) // A[1][0]
        .load_imm(Reg(4), 4) // A[1][1]
        // Load matrix B
        .load_imm(Reg(5), 5) // B[0][0]
        .load_imm(Reg(6), 6) // B[0][1]
        .load_imm(Reg(7), 7) // B[1][0]
        .load_imm(Reg(8), 8) // B[1][1]
        // Compute C[0][0] = A[0][0] * B[0][0] + A[0][1] * B[1][0]
        .mul(Reg(9), Reg(1), Reg(5)) // A[0][0] * B[0][0]
        .mul(Reg(10), Reg(2), Reg(7)) // A[0][1] * B[1][0]
        .add(Reg(13), Reg(9), Reg(10)) // C[0][0]
        // Compute C[0][1] = A[0][0] * B[0][1] + A[0][1] * B[1][1]
        .mul(Reg(11), Reg(1), Reg(6)) // A[0][0] * B[0][1]
        .mul(Reg(12), Reg(2), Reg(8)) // A[0][1] * B[1][1]
        .add(Reg(14), Reg(11), Reg(12)) // C[0][1]
        // Compute C[1][0] = A[1][0] * B[0][0] + A[1][1] * B[1][0]
        .mul(Reg(9), Reg(3), Reg(5))
        .mul(Reg(10), Reg(4), Reg(7))
        .add(Reg(15), Reg(9), Reg(10))
        // Compute C[1][1] = A[1][0] * B[0][1] + A[1][1] * B[1][1]
        .mul(Reg(11), Reg(3), Reg(6))
        .mul(Reg(12), Reg(4), Reg(8))
        .add(Reg(16), Reg(11), Reg(12))
        // Print results
        .print(Reg(13)) // C[0][0]
        .print(Reg(14)) // C[0][1]
        .print(Reg(15)) // C[1][0]
        .print(Reg(16)) // C[1][1]
        .halt();

    module.add_function(matmul);

    // Entry: call matmul directly
    let mut main = Function::new("main");
    main.instructions()
        .spawn("main", "matmul", vec![], Reg(0))
        .recv(Reg(1))
        .halt();

    module.add_function(main);
    machine.register_module(module);
    machine.spawn("main", "main", &[]);

    let start = std::time::Instant::now();
    machine.run();
    let seconds = start.elapsed().as_secs_f32();
    println!("Done in {}s", seconds);
}

fn main() {
    _fib();
    // _loop_fib();
    // _matrix();
}
