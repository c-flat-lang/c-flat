use bitbeat::{Function, Machine, Module, Reg};
use criterion::{criterion_group, criterion_main, Criterion};

pub fn fibbonacci_recursive(c: &mut Criterion) {
    let mut machine = Machine::default();

    let mut module = Module::new("main");

    // fib(N):
    //   if N <= 1 -> return N
    //   else:
    //     A = fib(N - 1)
    //     B = fib(N - 2)
    //     return A + B
    let mut fib_function = Function::new("fib").arity(1).returns(true); // Arg is in Reg(0)
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

    c.bench_function("fibbonacci", |b| {
        b.iter(|| {
            machine.run();
        })
    });
}

criterion_group!(benches, fibbonacci_recursive,);
criterion_main!(benches);
