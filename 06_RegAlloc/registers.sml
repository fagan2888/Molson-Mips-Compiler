structure Registers =
struct
  type register = string

  fun makeRegs 0 = []
    | makeRegs n = Temp.newtemp()::makeRegs(n-1)

  val specialregs = makeRegs(5) (* $rv,$sp,$ra,$0,$fp *)
  val regs = makeRegs(27) (* $r0-$r27 *)
  val registers = ["rv","sp","ra","0","fp","r0","r1","r2","r3","r4","r5","r6","r7","r8","r9","r10","r11","r12","r13","r14","r15","r16","r17","r18","r19","r20","r21","r22","r23","r24","r25","r26"]

  fun make_temp_map ((temp,name), map) = Temp.Map.insert(map,temp,name)
  val tempMap = foldl make_temp_map Temp.Map.empty (ListPair.zip(specialregs@regs,registers))

  val RV = List.nth(specialregs,0)
  val SP = List.nth(specialregs,1)
  val RA = List.nth(specialregs,2)
  val ZERO = List.nth(specialregs,3)
  val FP = List.nth(specialregs,4)
end
