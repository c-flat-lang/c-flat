.intel_syntax noprefix
.globl main
  # Defining Function min_s32
min_s32:
  push rbp
  mov rbp, rsp
  sub rsp, 8
  # -- args --
  # arg 0: x
  mov dword ptr [rbp-4], edi
  # arg 1: y
  mov dword ptr [rbp-8], esi
entry_min_s32:
  # @lt s32 : tmp.1_v0, x_v0, y_v0
  # lowering lt
  # lowering operand
  # lowering operand
  mov r11d, dword ptr [rbp-8]
  cmp dword ptr [rbp-4], r11d
  setl r11b
  movzx r10, r11b
  # @jumpif tmp.1_v0, then_1
  # lowering jump if
  # lowering operand
  test r10, r10
  jnz then_1
  # @jump else_2
  # lowering jump if
  jmp else_2
then_1:
  # @assign s32 : tmp.0_v0, x_v0
  # lowering assign
  # lowering operand
  mov r10d, dword ptr [rbp-4]
  # @jump merge_3
  # lowering jump if
  jmp merge_3
else_2:
  # @assign s32 : tmp.0_v0, y_v0
  # lowering assign
  # lowering operand
  mov r10d, dword ptr [rbp-8]
  # @jump merge_3
  # lowering jump if
  jmp merge_3
merge_3:
  # @ret s32, tmp.0_v0
  # lowering return
  mov eax, r10d
  jmp exit_min_s32
exit_min_s32:
  add rsp, 8
  mov rsp, rbp
  pop rbp
  ret
  # --- End of Function min_s32 ---
  # Defining Function main
main:
  push rbp
  mov rbp, rsp
  sub rsp, 8
  # -- args --
entry_main:
  # @assign u32 : tmp.0_v0, 123
  # lowering assign
  # lowering operand
  mov r10, 123
  mov r11, r10
  # @assign s32 : x_v0, tmp.0_v0
  # lowering assign
  # lowering operand
  mov dword ptr [rbp-4], r11d
  # @assign u32 : tmp.1_v0, 205
  # lowering assign
  # lowering operand
  mov r10, 205
  mov r11, r10
  # @assign s32 : y_v0, tmp.1_v0
  # lowering assign
  # lowering operand
  mov dword ptr [rbp-8], r11d
  # @call s32 : tmp.2_v0, min_s32(y_v0, x_v0)
  # lowering call
  # lowering operand
  # lowering operand
  mov esi, dword ptr [rbp-4]
  mov edi, dword ptr [rbp-8]
  call min_s32
  mov r11, rax
  # @ret s32, tmp.2_v0
  # lowering return
  mov eax, r11d
  jmp exit_main
exit_main:
  add rsp, 8
  mov rsp, rbp
  pop rbp
  ret
  # --- End of Function main ---
