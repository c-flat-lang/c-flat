use bitbeat::*;

fn main() {
    let mut vm = VM::default();
    let mut main_module = Module::new("main", 0);
    #[rustfmt::skip]
    let main_function = Function::new(
        "main",
        vec![
       /* 0*/instruction!(Opcode::Load, Operand::Immediate(Value::Integer(30))),
       /* 1*/instruction!(Opcode::Call, Operand::Immediate(Value::Identifier(Box::new("fib:fib".to_string())))),
        ],
        0,
    );
    main_module.add_function(main_function);

    let mut fib_module = Module::new("fib", 0);
    #[rustfmt::skip]
    let fib_function = Function::new(
        "fib",
       vec![
       /* 0*/instruction!(Opcode::LoadLocal, Operand::Address(0)),
       /* 1*/instruction!(Opcode::Load, Operand::Immediate(Value::Integer(0))),
       /* 2*/instruction!(Opcode::Eq),
       /* 3*/instruction!(Opcode::BranchIf, Operand::Address(18)), // jump to 1 before end

       /* 4*/instruction!(Opcode::LoadLocal, Operand::Address(0)),
       /* 5*/instruction!(Opcode::Load, Operand::Immediate(Value::Integer(1))),
       /* 6*/instruction!(Opcode::Eq),
       /* 7*/instruction!(Opcode::BranchIf, Operand::Address(18)), // jump to 1 before end

       /* 8*/instruction!(Opcode::LoadLocal, Operand::Address(0)),
       /* 9*/instruction!(Opcode::Load, Operand::Immediate(Value::Integer(1))),
       /*10*/instruction!(Opcode::Sub),
       /*11*/instruction!(Opcode::Call, Operand::Immediate(Value::Identifier(Box::new("fib:fib".to_string())))),

       /*12*/instruction!(Opcode::LoadLocal, Operand::Address(0)),
       /*13*/instruction!(Opcode::Load, Operand::Immediate(Value::Integer(2))),
       /*14*/instruction!(Opcode::Sub),
       /*15*/instruction!(Opcode::Call, Operand::Immediate(Value::Identifier(Box::new("fib:fib".to_string())))),

       /*16*/instruction!(Opcode::Add),
       /*17*/instruction!(Opcode::Branch, Operand::Address(19)),// jump to end

       /*18*/instruction!(Opcode::LoadLocal, Operand::Address(0)),
       /*19*/instruction!(Opcode::Return),
       ],
        1,
    );
    fib_module.add_function(fib_function);

    vm.load_module(main_module);
    vm.load_module(fib_module);

    vm.start_process("main", "main");
    match vm.run() {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{:?}", e);
            std::process::exit(1);
        }
    }
    // eprintln!("{:#?}", vm.processes.last());
}
