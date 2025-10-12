(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0) (result i32)
    (local i32 i32 i32 i32 i32 i32)
    i32.const 123
    local.set 0
    local.get 0
    local.set 5
    i32.const 321
    local.set 4
    local.get 4
    local.set 1
    local.get 1
    local.get 4
    i32.gt_s
    if  ;; label = @1
      local.get 5
      local.set 2
    else
      local.get 0
      local.set 2
    end
    local.get 2
    return)
  (export "main" (func 0)))
