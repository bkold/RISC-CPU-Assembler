[Main]
Add_32 r0, r0, r0  --noop
Add_32 r0, r0, r0  --noop
BCPUJ_32 Core1_0
BCPU_32 1, 0
Sleep_32 30, 0
BCPUJ_32 Core1_1
[Main_Loop]
BCPU_32 1, 1

Add_32 r2, r30, r0 --save r30 into r2
Add_32 r0, r0, r0  --noop
Add_32 r0, r0, r0  --noop

Sleep_32 30, 0

BCPUJR_32 r2
BCPU_32 1, 0

Add_32 r2, r30, r0 --save r30 into r2
Add_32 r0, r0, r0  --noop
Add_32 r0, r0, r0  --noop
Sleep_32 30, 0

BCPUJR_32 r2
J_32 Main_Loop
Add_32 r0, r0, r0  --noop

--count local variable by 1s
[Core1_0]
Add_32 r0, r0, r0  --noop
Add_32 r0, r0, r0  --noop
Addi_32 r2, r0, 0
[Loop_Core1_0]
Add_32 r0, r0, r0  --noop
Addi_32 r2, r2, 1
J_32 Loop_Core1_0
Add_32 r0, r0, r0  --noop

--count local variable by 2s
[Core1_1]
Add_32 r0, r0, r0  --noop
Add_32 r0, r0, r0  --noop
Addi_32 r2, r0, 0
[Loop_Core1_1]
Add_32 r0, r0, r0  --noop
Addi_32 r2, r2, 2
J_32 Loop_Core1_1
Add_32 r0, r0, r0  --noop

