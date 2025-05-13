(module
  ;; Import the "write" function from module "core".
  (import "core" "write" (func $write (param i32 i32) (result i32)))
  
  ;; Global constant: input = 5
  (global $input i32 (i32.const 5))

  ;; Function: factorial
  (func $factorial (param $n i32) (result i32)
    ;; If (n == 1) return 1; else return n * factorial(n - 1)
    (if (result i32)
      (i32.eq (local.get $n) (i32.const 1))
      (then 
        (i32.const 1)
      )
      (else
        (local.get $n)
        (local.get $n)
        (i32.const 1)
        i32.sub
        (call $factorial)
        i32.mul
      )
    )
  )

  ;; Public function: main
  (func $main (result i32)
    (local $fact_result i32)
    (local $exit_code i32)
    ;; Call factorial with global input
    (global.get $input)
    (call $factorial)
    (local.set $fact_result)
    ;; Call write with fact_result and 0
    (local.get $fact_result)
    (i32.const 0)
    (call $write)
    (local.set $exit_code)
    ;; Return the exit_code from write
    (local.get $exit_code)
  )

  (export "main" (func $main))
)
