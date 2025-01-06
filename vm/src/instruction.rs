use super::{bail, Error, Module, Operand, Process, Result, Value};
use std::collections::HashMap;
pub fn opcode_load(
    operand: &Operand,
    process: &mut Process,
    _: &HashMap<String, Vec<Module>>,
) -> Result<Option<Value>> {
    match operand {
        Operand::Immediate(value) => {
            process.stack.push(value.clone());
        }
        Operand::Address(address) => {
            todo!(
                "grabbing from memory at address {} {:#?}",
                address,
                process.stack
            );
        }
        Operand::Range(_, _) => todo!(),
        Operand::None => todo!(),
    }
    Ok(None)
}

pub fn opcode_load_local(
    operand: &Operand,
    process: &mut Process,
    _: &HashMap<String, Vec<Module>>,
) -> Result<Option<Value>> {
    match operand {
        Operand::Address(address) => {
            let Some(value) = process.args.get(*address) else {
                bail!(Error::InvalidProgramAddress {
                    address: *address,
                    module: process.module.clone(),
                    function: process.function.clone(),
                });
            };
            process.stack.push(value.clone());
        }
        Operand::Range(..) | Operand::Immediate(..) | Operand::None => todo!(),
    }
    Ok(None)
}

pub fn opcode_store(
    operand: &Operand,
    process: &mut Process,
    _: &HashMap<String, Vec<Module>>,
) -> Result<Option<Value>> {
    match operand {
        Operand::Address(address) => {
            if let Some(value) = process.stack.pop() {
                process.memory[*address] = value;
            }
        }
        _ => {}
    }
    Ok(None)
}

pub fn opcode_add(
    operand: &Operand,
    process: &mut Process,
    _: &HashMap<String, Vec<Module>>,
) -> Result<Option<Value>> {
    let Some(rhs) = process.stack.pop() else {
        bail!(Error::StackUnderflow {
            module: process.module.clone(),
            function: process.function.clone(),
            pc: process.pc,
        });
    };
    let Some(lhs) = process.stack.pop() else {
        bail!(Error::StackUnderflow {
            module: process.module.clone(),
            function: process.function.clone(),
            pc: process.pc,
        });
    };
    match (&lhs, &rhs) {
        (Value::Integer(lhs), Value::Integer(rhs)) => {
            process.stack.push(Value::Integer(lhs + rhs));
        }
        (Value::Float(lhs), Value::Float(rhs)) => {
            process.stack.push(Value::Float(lhs + rhs));
        }
        (l, r) => {
            bail!(Error::InvalidOperationResult {
                module: process.module.clone(),
                function: process.function.clone(),
                pc: process.pc,
                args: vec![l.clone(), r.clone()],
                expected: "Integer or Float".to_string(),
            });
        }
    }
    Ok(None)
}

pub fn opcode_sub(
    operand: &Operand,
    process: &mut Process,
    _: &HashMap<String, Vec<Module>>,
) -> Result<Option<Value>> {
    let Some(rhs) = process.stack.pop() else {
        bail!(Error::StackUnderflow {
            module: process.module.clone(),
            function: process.function.clone(),
            pc: process.pc,
        });
    };
    let Some(lhs) = process.stack.pop() else {
        bail!(Error::StackUnderflow {
            module: process.module.clone(),
            function: process.function.clone(),
            pc: process.pc,
        });
    };
    match (&lhs, &rhs) {
        (Value::Integer(lhs), Value::Integer(rhs)) => {
            process.stack.push(Value::Integer(lhs - rhs));
        }
        (Value::Float(lhs), Value::Float(rhs)) => {
            process.stack.push(Value::Float(lhs - rhs));
        }
        (l, r) => {
            bail!(Error::InvalidOperationResult {
                module: process.module.clone(),
                function: process.function.clone(),
                pc: process.pc,
                args: vec![l.clone(), r.clone()],
                expected: "Integer or Float".to_string(),
            });
        }
    }
    Ok(None)
}

pub fn opcode_call(
    operand: &Operand,
    process: &mut Process,
    modules: &HashMap<String, Vec<Module>>,
) -> Result<Option<Value>> {
    let function_name = match operand {
        Operand::Immediate(Value::Identifier(function_name)) => function_name,
        operand => {
            bail!(Error::ExpectedOperand {
                expected: Operand::Immediate(Value::Identifier(Box::new("identifier".to_string()))),
                found: operand.clone(),
            });
        }
    };

    let Some((module, func_name)) = function_name.split_once(':') else {
        bail!(Error::MalformedFunctionName {
            name: function_name.to_string(),
        });
    };

    let Some(current_module) = modules.get(module) else {
        bail!(Error::NoSuchModule {
            module: module.to_string(),
        });
    };
    let Some(func) = current_module
        .iter()
        .find_map(|module| module.functions.get(func_name))
    else {
        bail!(Error::NoSuchFunction {
            module: module.to_string(),
            function: func_name.to_string(),
        });
    };

    let arg_count = func.locals;
    let end = process.stack.len();

    let mut p = Process {
        module: process.module.clone(),
        function: func.name.clone(),
        args: process.stack.drain(end - arg_count..).collect(),
        ..Default::default()
    };

    let Some(v) = func.execute(&mut p, modules)? else {
        return Ok(None);
    };
    process.stack.push(v);
    Ok(None)
}
pub fn opcode_return(
    operand: &Operand,
    process: &mut Process,
    _: &HashMap<String, Vec<Module>>,
) -> Result<Option<Value>> {
    let Some(value) = process.stack.pop() else {
        bail!(Error::StackUnderflow {
            module: process.module.clone(),
            function: process.function.clone(),
            pc: process.pc,
        });
    };
    return Ok(Some(value));
}
pub fn opcode_branch_if(
    operand: &Operand,
    process: &mut Process,
    _: &HashMap<String, Vec<Module>>,
) -> Result<Option<Value>> {
    let Some(value) = process.stack.pop() else {
        bail!(Error::StackUnderflow {
            module: process.module.clone(),
            function: process.function.clone(),
            pc: process.pc,
        });
    };
    let Value::Boolean(value) = value else {
        bail!(Error::TypeMismatch {
            expected: "Boolean".to_string(),
            found: value,
        });
    };
    if value {
        let Operand::Address(address) = &operand else {
            bail!(Error::ExpectedOperand {
                expected: Operand::Address(0),
                found: operand.clone(),
            });
        };
        process.pc = *address;
    }
    Ok(None)
}
pub fn opcode_branch(
    operand: &Operand,
    process: &mut Process,
    _: &HashMap<String, Vec<Module>>,
) -> Result<Option<Value>> {
    let Operand::Address(address) = &operand else {
        bail!(Error::ExpectedOperand {
            expected: Operand::Address(0),
            found: operand.clone(),
        });
    };
    process.pc = *address;
    Ok(None)
}

pub fn opcode_eq(
    operand: &Operand,
    process: &mut Process,
    _: &HashMap<String, Vec<Module>>,
) -> Result<Option<Value>> {
    let Some(right) = process.stack.pop() else {
        bail!(Error::StackUnderflow {
            module: process.module.clone(),
            function: process.function.clone(),
            pc: process.pc,
        });
    };
    let Some(left) = process.stack.pop() else {
        bail!(Error::StackUnderflow {
            module: process.module.clone(),
            function: process.function.clone(),
            pc: process.pc,
        });
    };
    process.stack.push(Value::Boolean(left == right));
    Ok(None)
}

pub fn opcode_or(
    operand: &Operand,
    process: &mut Process,
    _: &HashMap<String, Vec<Module>>,
) -> Result<Option<Value>> {
    let Some(right) = process.stack.pop() else {
        bail!(Error::StackUnderflow {
            module: process.module.clone(),
            function: process.function.clone(),
            pc: process.pc,
        });
    };
    let Value::Boolean(right) = right else {
        bail!(Error::TypeMismatch {
            expected: "Boolean".to_string(),
            found: right,
        });
    };
    let Some(left) = process.stack.pop() else {
        bail!(Error::StackUnderflow {
            module: process.module.clone(),
            function: process.function.clone(),
            pc: process.pc,
        });
    };
    let Value::Boolean(left) = left else {
        bail!(Error::TypeMismatch {
            expected: "Boolean".to_string(),
            found: left,
        });
    };
    process.stack.push(Value::Boolean(left || right));
    Ok(None)
}
