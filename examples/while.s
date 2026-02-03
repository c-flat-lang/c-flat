.intel_syntax noprefix
.globl main
  # Defining Function main
main:
  push rbp
  mov rbp, rsp
  sub rsp, 272
  # -- args --
entry_main:
  # @alloc s32 : tmp.0_v0, 64
  # lowering alloc
  # @assign u32 : tmp.1_v0, 0
  # lowering assign
  # lowering operand
  mov r10, 0
  mov r11, r10
  # @elemset s32 : tmp.0_v0, 0, tmp.1_v0
  # lowering elemset
  # lowering operand
  mov r10, 0
  # lowering operand
  lea r9, dword ptr [rbp-256]
  mov [r9 + r10 * 4], r11
  # @assign u32 : tmp.2_v0, 0
  # lowering assign
  # lowering operand
  mov r8, 0
  mov r11, r8
  # @elemset s32 : tmp.0_v0, 1, tmp.2_v0
  # lowering elemset
  # lowering operand
  mov r8, 1
  # lowering operand
  lea rdi, dword ptr [rbp-256]
  mov [rdi + r8 * 4], r11
  # @assign [2 x s32] : array_v0, tmp.0_v0
  # lowering assign
  # lowering operand
  mov r11d, dword ptr [rbp-256]
  mov dword ptr [rbp-264], r11d
  mov dword ptr [rbp-264], r11d
  # @assign u32 : tmp.3_v0, 0
  # lowering assign
  # lowering operand
  mov rsi, 0
  mov r11, rsi
  # @assign s32 : i_v0, tmp.3_v0
  # lowering assign
  # lowering operand
  mov dword ptr [rbp-268], r11d
  mov dword ptr [rbp-268], r11d
  # @jump start_lo
  # lowering jump if
  jmp start_loop_0
start_loop_0:
  # @assign u32 : tmp.4_v0, 2
  # lowering assign
  # lowering operand
  mov r11, 2
  mov r11, r11
  # @lt s32 : tmp.5_v0, i_v0, tmp.4_v0
  # lowering lt
  # lowering operand
  # lowering operand
  cmp dword ptr [rbp-268], r11d
  setl r11b
  movzx rsi, r11b
  # @jumpif tmp.5_v0, body_loo
  # lowering jump if
  # lowering operand
  test rsi, rsi
  jnz body_loop_1
  # @jump exit_loo
  # lowering jump if
  jmp exit_loop_2
body_loop_1:
  # @elemget s32 : tmp.6_v0, array_v0, i_v0
  # lowering elemget
  # lowering operand
  # lowering operand
  lea rsi, dword ptr [rbp-268]
  lea r11, dword ptr [rbp-264]
  mov r11, [r11 + rsi * 4]
  # @assign s32 : value_v0, tmp.6_v0
  # lowering assign
  # lowering operand
  mov dword ptr [rbp-272], r11d
  mov dword ptr [rbp-272], r11d
  # @call write_int(value_v0)
  # lowering call
  # lowering operand
  push rsi
  push rdi
  push r8
  push r9
  mov edi, dword ptr [rbp-272]
  call write_int
  pop r9
  pop r8
  pop rdi
  pop rsi
  # @call write_int(i_v0)
  # lowering call
  # lowering operand
  push rsi
  push rdi
  push r8
  push r9
  mov edi, dword ptr [rbp-268]
  call write_int
  pop r9
  pop r8
  pop rdi
  pop rsi
  # @assign u32 : tmp.7_v0, 10
  # lowering assign
  # lowering operand
  mov r11, 10
  mov r11, r11
  # @call write_char(tmp.7_v0)
  # lowering call
  # lowering operand
  push rsi
  push rdi
  push r8
  push r9
  mov rdi, r11
  call write_char
  pop r9
  pop r8
  pop rdi
  pop rsi
  # @assign u32 : tmp.8_v0, 1
  # lowering assign
  # lowering operand
  mov r11, 1
  mov r11, r11
  # @add s32 : tmp.9_v0, i_v0, tmp.8_v0
  # lowering add
  # lowering operand
  # lowering operand
  add dword ptr [rbp-268], r11d
  # @assign s32 : i_v0, tmp.9_v0
  # lowering assign
  # lowering operand
  mov r11d, dword ptr [rbp-268]
  mov dword ptr [rbp-268], r11d
  mov dword ptr [rbp-268], r11d
  # @jump start_lo
  # lowering jump if
  jmp start_loop_0
exit_loop_2:
  # @ret s32, i_v0
  # lowering return
  mov eax, dword ptr [rbp-268]
  jmp exit_main
exit_main:
  add rsp, 272
  mov rsp, rbp
  pop rbp
  ret
  # --- End of Function main ---
