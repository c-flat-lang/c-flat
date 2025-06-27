pub struct Decoder<'a> {
    code: &'a [u8],
    index: usize,
    at: Option<usize>,
}

impl<'a> Decoder<'a> {
    pub fn new(code: &'a [u8]) -> Self {
        Self {
            code,
            index: 0,
            at: None,
        }
    }

    pub fn at(mut self, index: usize) -> Self {
        self.at = Some(index);
        self
    }

    fn u8(&mut self) -> u8 {
        let byte = self.code[self.index];
        self.index += 1;
        byte
    }

    fn u32(&mut self) -> u32 {
        let bytes = self.get_n_bytes(4);
        u32::from_le_bytes(bytes.try_into().unwrap())
    }

    fn i64(&mut self) -> i64 {
        let bytes = self.get_n_bytes(8);
        i64::from_le_bytes(bytes.try_into().unwrap())
    }

    fn string(&mut self) -> String {
        let len = self.u32() as usize;
        let bytes = self.code[self.index..self.index + len].to_vec();
        self.index += len;
        match String::from_utf8(bytes) {
            Ok(string) => string,
            Err(err) => panic!("{}", err),
        }
    }

    fn args(&mut self) -> Vec<u8> {
        let len = self.u32() as usize;
        let bytes = self.code[self.index..self.index + len].to_vec();
        self.index += len;
        bytes
    }

    fn get_n_bytes(&mut self, n: usize) -> &[u8] {
        if self.index + n > self.code.len() {
            panic!("UnexpectedEOF");
        }
        let slice = &self.code[self.index..self.index + n];
        self.index += n;
        slice
    }

    pub fn decode(&mut self) {
        while self.index < self.code.len() {
            let start = self.index;
            if matches!(self.at, Some(index) if index == self.index) {
                eprint!("\x1b[32m[{:>4}]\x1b[0m ", self.index);
            } else {
                eprint!("[{:>4}] ", self.index);
            }
            let opcode = self.u8();
            match opcode {
                0 => {
                    eprint!("Noop");
                }
                1 => {
                    eprint!("Halt");
                }
                2 => {
                    eprint!("LoadImm des: {}, value: {}", self.u8(), self.i64(),);
                }
                3 => {
                    eprint!(
                        "Add des: {}, lhs: {}, rhs: {}",
                        self.u8(),
                        self.u8(),
                        self.u8()
                    );
                }
                4 => {
                    eprint!(
                        "Sub des: {}, lhs: {}, rhs: {}",
                        self.u8(),
                        self.u8(),
                        self.u8()
                    );
                }
                5 => {
                    eprint!(
                        "Mul des: {}, lhs: {}, rhs: {}",
                        self.u8(),
                        self.u8(),
                        self.u8()
                    );
                }
                6 => {
                    eprint!(
                        "CmpLE des: {}, lhs: {}, rhs: {}",
                        self.u8(),
                        self.u8(),
                        self.u8()
                    );
                }
                7 => {
                    eprint!("Mov des: {}, src: {}", self.u8(), self.u8());
                }
                8 => {
                    eprint!("JumpIf cmp: {}, target: {}", self.u8(), self.u32());
                }
                9 => {
                    eprint!("Jump target: {}", self.u32());
                }
                10 => {
                    eprint!(
                        "Spawn module: {}, function: {}, args: {:?}, dst: {}",
                        self.string(),
                        self.string(),
                        self.args(),
                        self.u8()
                    );
                }
                11 => {
                    eprint!("Print reg: {}", self.u8());
                }
                12 => {
                    eprint!("Send dst_pid: {}, src_reg: {}", self.u8(), self.u8());
                }
                13 => {
                    eprint!("Recv reg: {}", self.u8());
                }
                _ => panic!(),
            }
            eprintln!(" {:?}", &self.code[start..self.index]);
        }
    }
}

// #[test]
// fn bytecode_test() {
//     let mut code = Vec::new();
//     {
//         InstructionBuilder::new(&mut code)
//             .load_imm(Reg(2), 1) // const_1 = 1
//             .cmp_le(Reg(3), Reg(1), Reg(2)) // if N <= 1
//             .jump_if(Reg(3), "exit") // jump to base case
//             .load_imm(Reg(4), 1)
//             .sub(Reg(5), Reg(1), Reg(4)) // N - 1 → Reg(5)
//             .load_imm(Reg(6), 2)
//             .sub(Reg(7), Reg(1), Reg(6)) // N - 2 → Reg(7)
//             .spawn("main", "fib", vec![Reg(5)], Reg(8)) // pid1 = fib(N - 1)
//             .recv(Reg(9)) // result1
//             .spawn("main", "fib", vec![Reg(7)], Reg(10)) // pid2 = fib(N - 2)
//             .recv(Reg(11)) // result2
//             .add(Reg(12), Reg(9), Reg(11)) // sum = result1 + result2
//             .send(Reg(0), Reg(12)) // send result to caller
//             .halt()
//             .label("exit")
//             .send(Reg(0), Reg(1)) // base case: send N
//             .halt();
//     }
//     Decoder::new(&code).decode();
//     assert!(false);
// }
