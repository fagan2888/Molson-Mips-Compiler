structure Translate =
struct
  structure A = Absyn

  datatype level = TOP | LEVEL of unit ref

  datatype stm = SEQ of stm list
               | LABEL of Temp.label
               | JUMP of exp * Temp.label list
               | CJUMP of relop * exp * exp * Temp.label * Temp.label
               | MOVE of exp * exp
               | EXP of exp

  and exp = BINOP of binop * exp * exp
          | MEM of exp
          | TEMP of Temp.temp
          | ESEQ of stm * exp
          | NAME of Temp.label
          | CONST of int
          | CALL of exp * exp list

  and binop = PLUS | MINUS | MUL | DIV | AND | OR

  and relop = EQ | NE | LT | GT | LE | GE | ULT | ULE | UGT | UGE

  and texp = Ex of exp
           | Nx of stm
           | Cx of Temp.label * Temp.label -> stm

  fun relstr x = case x of EQ => "beq" | NE => "bne" | LT => "blt" | GT => "bgt" | LE => "ble" | GE => "bge" | _ => "?!?"

  fun binstr x = case x of PLUS => "PLUS" | MINUS => "MINUS" | MUL => "MUL" | DIV => "DIV" | AND => "AND" | OR => "OR"

  fun unEx (Ex e) = e
    | unEx (Cx c) =
      let
        val r = Temp.newtemp()
        val t = Temp.newlabel() and f = Temp.newlabel()
      in
        ESEQ(SEQ[MOVE(TEMP r, CONST 1),c(t,f),LABEL f, MOVE(TEMP r, CONST 0), LABEL t], TEMP r)
      end
    | unEx (Nx n) = ESEQ(n, CONST 0)

  fun unCx(Cx c) = c
    | unCx(Ex(CONST 0)) = (fn (t,f) => JUMP(NAME f, [f]))
    | unCx(Ex(CONST _)) = (fn (t,f) => JUMP(NAME t, [t]))
    | unCx(Ex e) = (fn (t,f) => CJUMP(EQ, e, CONST 1, t, f))
    | unCx(Nx n) = (Error.msg 0 ("uncx an nx"); fn(t,f) => LABEL(Temp.newlabel()))

  fun unNx (Ex e) = EXP(e)
    | unNx (Cx c) = unNx (Ex (unEx (Cx c)))
    | unNx (Nx n) = n

  datatype frag = PROC of {body:stm, name:Temp.label, formals:Temp.temp list}
                | VARDEC of stm
                | STRING of Temp.label * string

  val frags = ref [] : frag list ref

  fun resetFrags () = (frags := [])

  fun getResult () = !frags

  fun varDec (temp,exp) = Nx (MOVE (TEMP temp, unEx exp))

  fun simpleVar (temp) = Ex (TEMP temp)

  fun fieldVar (exp,n) = Ex (MEM (BINOP (PLUS, unEx exp, CONST (n*4))))

  fun nilExp () = Ex (CONST 0)

  fun intExp (i) = Ex (CONST i)

  fun stringExp (str) =
    let
      val lab = Temp.newlabel()
      fun search_frags [] = ((frags := STRING(lab, str)::(!frags)); Ex (NAME lab))
        | search_frags (frag::frags) = (case frag of STRING(label,string) => if str=string then Ex (NAME label) else (search_frags frags) | _ => search_frags frags)
    in
      search_frags (!frags)
    end

  fun boolExp true = Ex (CONST 1)
    | boolExp false = Ex (CONST 0)

  fun callExp (label,exps) =
    let
      val r = Temp.newtemp()
    in
      Ex(ESEQ(MOVE(TEMP r,CALL(NAME label, map unEx exps)), TEMP r))
    end

  fun ifElseExp (exp1,exp2,exp3) =
    let
      val r = Temp.newtemp()
      val t = Temp.newlabel() and f = Temp.newlabel() and j = Temp.newlabel()
    in
      case (exp2,exp3) of
        (Cx c2, Cx c3) => Cx(fn(tt,ff) => (SEQ[(unCx exp1)(t, f),LABEL t,c2 (tt, ff),LABEL f,c3 (tt, ff)]))
        | (Nx n2, Nx n3) => Nx(SEQ[(unCx exp1)(t,f),LABEL t,n2,JUMP(NAME j, [j]),LABEL f,n3,LABEL j])
        | (Ex e2, Ex e3) => Ex(ESEQ(SEQ[(unCx exp1)(t,f),LABEL t,MOVE(TEMP r,e2),JUMP(NAME j,[j]),LABEL f,MOVE(TEMP r,e3),LABEL j], TEMP(r)))
        | (_,_) => Ex (CONST (0)) (* then and else differ *)
    end

  fun opExp (A.PlusOp, exp1, exp2) = Ex(BINOP(PLUS, unEx exp1, unEx exp2))
    | opExp (A.MinusOp, exp1, exp2) = Ex(BINOP(MINUS, unEx exp1, unEx exp2))
    | opExp (A.TimesOp, exp1, exp2) = Ex(BINOP(MUL, unEx exp1, unEx exp2))
    | opExp (A.DivideOp, exp1, exp2) = Ex(BINOP(DIV, unEx exp1, unEx exp2))
    | opExp (A.EqOp, exp1, exp2) = Cx(fn(t,f) => CJUMP(EQ, unEx(exp1), unEx(exp2), t, f))
    | opExp (A.NeqOp, exp1, exp2) = Cx(fn(t,f) => CJUMP(NE, unEx(exp1), unEx(exp2), t, f))
    | opExp (A.LtOp, exp1, exp2) = Cx(fn(t,f) => CJUMP(LT, unEx(exp1), unEx(exp2), t, f))
    | opExp (A.LeOp, exp1, exp2) = Cx(fn(t,f) => CJUMP(LE, unEx(exp1), unEx(exp2), t, f))
    | opExp (A.GtOp, exp1, exp2) = Cx(fn(t,f) => CJUMP(GT, unEx(exp1), unEx(exp2), t, f))
    | opExp (A.GeOp, exp1, exp2) = Cx(fn(t,f) => CJUMP(GE, unEx(exp1), unEx(exp2), t, f))
    | opExp (A.OrOp, exp1, exp2) = ifElseExp (exp1, Ex (CONST 1), exp2)
    | opExp (A.AndOp, exp1, exp2) = ifElseExp (exp1, exp2, Ex (CONST 0))

  fun tupleExp exps =
    let
      val r = Temp.newtemp() (* TODO: eventually make this the stack pointer *)
      fun fields (_,[]) = []
        | fields (n,exp::exps) = MOVE(MEM(BINOP(PLUS, TEMP r, CONST (n*4))), unEx exp)::fields(n+1,exps)
    in
      Ex (ESEQ(MOVE(TEMP r, BINOP(MINUS, TEMP r, CONST (List.length(exps)*4))),ESEQ(SEQ (fields(0,exps)), TEMP (r))))
    end

  fun seqExp [] = Ex (CONST 0)
    | seqExp [exp] = exp
    | seqExp (exps) = Ex(ESEQ(SEQ(map unNx (List.take(exps,List.length(exps)-1))), unEx (List.last(exps))))

  fun assignExp (v,e) = Nx (MOVE (unEx v, unEx e))

  fun whileExp (break,cond,body) =
    let
      val l1 = Temp.newlabel() and l2 = Temp.newlabel()
    in
      Nx (SEQ[JUMP(NAME l1,[l1]),LABEL l2,unNx body,LABEL l1, unCx cond (l2,break),LABEL break])
    end

  fun breakExp (break) = Nx (JUMP(NAME break,[break]))

end
