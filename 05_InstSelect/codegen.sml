
structure Codegen =
struct
  structure T = Translate
  structure S = Env.Symbol
  structure E = Env
  structure R = Registers

  type temp = Temp.temp
  type label = Temp.label

  datatype instr = OPER of {assem: string, dst: temp list, src: temp list, jump: label list option}
                 | LABEL of {assem: string, lab: Temp.label}
                 | MOVE of {assem: string, dst: temp, src: temp}

  fun format saytemp =
    let
      fun speak(assem,dst,src,jump) =
        let
          fun f(#"`":: #"s":: i::rest) = (explode(saytemp(List.nth(src,ord i - ord #"0"))) @ f rest)
            | f( #"`":: #"d":: i:: rest) = (explode(saytemp(List.nth(dst,ord i - ord #"0"))) @ f rest)
            | f( #"`":: #"j":: i:: rest) = (explode(List.nth(jump,ord i - ord #"0")) @ f rest)
            | f( #"`":: #"`":: rest) = #"`" :: f rest
            | f( #"`":: _ :: rest) = []
            | f(c :: rest) = (c :: f rest)
            | f nil = nil
          in
            implode(f(explode assem))
          end
    in
      fn OPER{assem,dst,src,jump=NONE} => speak(assem,dst,src,nil)
       | OPER{assem,dst,src,jump=SOME j} => speak(assem,dst,src,j)
       | LABEL{assem,...} => assem
       | MOVE{assem,dst,src} => speak(assem,[dst],[src],nil)
     end

  fun codegen venv stm =
    let
      val ilist = ref (nil: instr list)
      fun emit x = (ilist := x::(!ilist))
      fun result gen = let val t=Temp.newtemp() in gen t; t end

      fun munchStm (T.SEQ([])) = ()
        | munchStm (T.SEQ(a::l)) = (munchStm a; munchStm (T.SEQ(l)))
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i)),e2)) = emit(OPER{assem="sw `s1, " ^ (Int.toString i) ^ "(`s0)\n", src=[munchExp e1, munchExp e2], dst=[], jump=NONE})
        | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1)),e2)) = emit(OPER{assem="sw `s1, " ^ (Int.toString i) ^ "(`s0)\n", src=[munchExp e1, munchExp e2], dst=[], jump=NONE})
        | munchStm (T.MOVE(T.MEM(T.CONST i),e2)) = emit(OPER{assem="sw `s1, " ^ (Int.toString i) ^ "(`s0)\n", src=[munchExp e2], dst=[], jump=NONE})
        | munchStm (T.MOVE(T.MEM(e1), e2)) = emit(OPER{assem="sw `s1, 0(`s0)\n", src=[munchExp e1, munchExp e2], dst=[], jump=NONE})
        | munchStm (T.MOVE(T.TEMP i, e2)) = emit(OPER{assem="add `d0, `s0, `s1\n", src=[munchExp e2, R.ZERO], dst=[i], jump=NONE})
        | munchStm (T.MOVE(a,b)) = ()
        | munchStm (T.LABEL lab) = emit(LABEL{assem=lab ^ ":\n", lab=lab})
        | munchStm (T.JUMP(T.NAME(lab),lablist)) = emit(OPER{assem="j " ^ lab ^ "\n", src=[], dst=[], jump=SOME(lablist)})
        | munchStm (T.JUMP(e1,lablist)) = emit(OPER{assem="jr `j0\n", src=[munchExp e1],dst=[],jump=SOME(lablist)})
        | munchStm (T.CJUMP(relop,e1,e2,l1,l2)) = emit(OPER{assem=T.relstr(relop) ^ " `s0, `s1, " ^ l1 ^ "\n", src=[munchExp e1, munchExp e2], dst=[], jump=SOME[l1,l2]})
        | munchStm (T.EXP(e)) = (munchExp e; ())

      and munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) = result(fn r => emit(OPER{assem="lw `d0, " ^ (Int.toString (Int.abs i)) ^ "(`s0)\n", src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) = result(fn r => emit(OPER{assem="lw `d0, " ^ (Int.toString i) ^ "(`s0)\n", src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (T.MEM(e1)) = result(fn r => emit(OPER{assem="lw `d0, 0(`s0)\n", src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.PLUS, e1, T.CONST i)) = result(fn r => emit(OPER{assem="addi `d0, `s0, " ^ (Int.toString i) ^ "\n", src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.PLUS, T.CONST i, e1)) = result(fn r => emit(OPER{assem="addi `d0, `s0, " ^ (Int.toString i) ^ "\n", src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (T.CONST i) = result(fn r => emit(OPER{assem="addi `d0, `s0, " ^ (Int.toString i) ^ "\n", src=[R.ZERO], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.PLUS,e1,e2)) = result(fn r => emit(OPER{assem="add `d0, `s0, `s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.MINUS,e1,e2)) = result(fn r => emit(OPER{assem="sub `d0, `s0, `s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.MUL,e1,e2)) = result(fn r => emit(OPER{assem="mul `d0, `s0, `s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.DIV,e1,e2)) = result(fn r => emit(OPER{assem="div `d0, `s0, `s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.AND,e1,e2)) = result(fn r => emit(OPER{assem="and `d0, `s0, `s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.BINOP(T.OR,e1,e2)) = result(fn r => emit(OPER{assem="or `d0, `s0, `s1\n", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        | munchExp (T.ESEQ(s,e)) = (munchStm s; munchExp e)
        | munchExp (T.NAME(lab)) = result(fn r => emit(OPER{assem="la `d0, " ^ lab ^ "\n", src=[], dst=[r], jump=NONE}))
        | munchExp (T.CALL(T.NAME(lab),args)) = result(fn r => (emit(OPER{assem="jal " ^ lab ^"\n", src=munchArgs(lab, args), dst=R.specialregs, jump=NONE}); emit(MOVE{assem="move `d0, `s0\n", src=R.RV, dst=r})))
        | munchExp (T.CALL(e,elist)) = result(fn r => emit(OPER{assem="", src=[], dst=[], jump=NONE}))
        | munchExp (T.TEMP t) = t

      and munchArgs (lab, args) =
        let
          val temps = case S.find(venv,lab) of SOME (E.FunEntry{inputs,result=res,label,temps,level}) => temps | _ => []
          fun movearg (arg::args,temp::temps) = emit(MOVE{assem="move `d0, `s0\n", src=(munchExp arg), dst=temp})
            | movearg (_,_) = ()
          val _ = movearg (args,temps)
        in
          temps
        end

    in
      munchStm stm;
      rev(!ilist)
    end

end
