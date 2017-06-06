structure Semant =
struct
  structure S = Env.Symbol
  structure E = Env
  structure A = Absyn
  structure T = Translate

  val depth = ref 0
  val breakLabel = ref ""

  fun transProg decs =
    let
      val _ = T.resetFrags () (* Creates IRTrees. Nothing for types. Move exps to temps for vars. Jump to main. Process Functions.*)

      (* Creating type and variable environments and checking functions *)
      val tenv = typeDecs (decs, E.base_tenv) (* declare types *)
      val tenv' = typeInputs (decs, tenv) (* input types *)
      val venv = funcDecs (decs, tenv', E.base_venv) (* declare functions *)
      val venv' = varInputs (decs, tenv', venv) (* declare and input variables, convert to IR *)
      val venv'' = funcChecks (decs, tenv', venv') (* check functions, convert to IR *)
    in
      T.getResult ()
    end

  and transTy (tenv,typ,pos) = (case typ of
    A.NameTy(s,p) => (case S.find(tenv,s) of SOME t => actualTy (t,p) | NONE => (Error.msg p ("undefined type: " ^ s); E.BOTTOM))
    | A.TupleTy(list) => E.TUPLE(createTuple (list,tenv)))

  and actualTy (ty,p) = case ty of (E.NAME(n,rt)) => (case !rt of SOME t => actualTy (t,p) | NONE => E.BOTTOM) | _ => ty

  and createTuple ([],tenv) = []
    | createTuple ((ty::list),tenv) = case ty of
      A.NameTy(s,p) => (case S.find(tenv, s) of SOME t => (actualTy (t,p))::(createTuple (list,tenv)) | NONE => (Error.msg p ("undefined type: " ^ s); E.BOTTOM::(createTuple (list,tenv))))
      | A.TupleTy(l) => E.TUPLE(createTuple (l,tenv))::(createTuple (list,tenv))

  and transExp (tenv, venv, exp, level) =
    let
      fun trexp (A.VarExp(A.SimpleVar(s,p))) = (case S.find(venv,s) of
            SOME (E.VarEntry{ty,temp,level=varlevel}) => (if varlevel=level orelse varlevel=T.TOP then {ty=ty,exp=T.simpleVar(temp)} else (Error.msg p ("wrong scope for var: " ^ s); {ty=E.BOTTOM,exp=T.nilExp()}))
            | _ => (Error.msg p ("undefined var: " ^ s); {ty=E.BOTTOM,exp=T.nilExp()}))
        | trexp (A.VarExp(A.FieldVar(v,i,p))) = (case trexp (A.VarExp(v)) of {ty=E.TUPLE(tylist),exp} => {ty=List.nth(tylist,i),exp=T.fieldVar(exp,i)} | _ => (Error.msg p ("undefined tuple"); {ty=E.BOTTOM,exp=T.nilExp()}))
        | trexp (A.NilExp) = {ty=E.NIL,exp=T.nilExp()}
        | trexp (A.IntExp(int)) = {ty=E.INT,exp=T.intExp(int)}
        | trexp (A.StringExp(string)) = {ty=E.STRING,exp=T.stringExp(string)}
        | trexp (A.BoolExp(bool)) = {ty=E.BOOL,exp=T.boolExp(bool)}
        | trexp (A.CallExp{func,args,pos}) = funcCall (func, args, pos)
        | trexp (A.OpExp{left,oper,right,pos}) = if oper=A.PlusOp orelse oper=A.MinusOp orelse oper=A.TimesOp orelse oper=A.DivideOp then checkOp(oper,left,right,pos,E.INT) else checkOp(oper,left,right,pos,E.BOOL)
        | trexp (A.TupleExp{fields,typ,pos}) = newTuple (fields, typ, pos)
        | trexp (A.SeqExp(list)) = {ty = #ty (trexp (List.last(list))),exp=T.seqExp(map (fn e => #exp (trexp e)) list)}
        | trexp (A.AssignExp{var,exp,pos}) =
          let
            val v = trexp (A.VarExp(var))
            val e = trexp exp
          in
            if typesEqual (#ty v, #ty e) then {ty=E.BOTTOM,exp=T.assignExp(#exp v, #exp e)} else (Error.msg pos ("var and exp types do not match"); {ty=E.BOTTOM,exp=T.nilExp()})
          end
        | trexp (A.IfElseExp{cond,exp1,exp2,pos}) =
          let
           val c = trexp cond
           val e1 = trexp exp1
           val e2 = trexp exp2
          in
            if (#ty c)=E.BOOL andalso typesEqual(#ty e1, #ty e2) then {ty=(#ty e1), exp=T.ifElseExp(#exp c, #exp e1, #exp e2)} else (Error.msg pos ("if-else exps do not match or condition not boolean"); {ty=E.BOTTOM,exp=T.nilExp()})
          end
        | trexp (A.WhileExp{cond,body,pos}) = checkWhile (cond,body,pos)
        | trexp (A.BreakExp(pos)) = if !depth>0 then {ty=E.BOTTOM,exp=T.breakExp(!breakLabel)} else (Error.msg pos ("break not inside loop"); {ty=E.BOTTOM,exp=T.nilExp()})

      and funcCall (func, args, pos) = case S.find(venv,func) of
        SOME (E.FunEntry{inputs,result,label,temps,level}) =>
          let
            val types = map (fn arg => #ty (trexp arg)) args
            val exps = map (fn arg => #exp (trexp arg)) args
          in
            if types=inputs then {ty=result,exp=T.callExp(label,exps)} else (Error.msg pos ("arg type or num args do not match func call: " ^ label); {ty=E.BOTTOM,exp=T.nilExp()})
          end
        | _ => (Error.msg pos ("undefined func: " ^ func); {ty=E.BOTTOM,exp=T.nilExp()})

      and typesEqual (t1,t2) = if t1=E.BOTTOM orelse t2=E.BOTTOM then true else t1=t2

      and checkOp (oper,l,r,p,t) =
        let
          val left = trexp l
          val right = trexp r
        in
          if (#ty left)=E.INT andalso (#ty right)=E.INT then {ty=t,exp=T.opExp(oper,#exp left,#exp right)} else (Error.msg p ("op exp not int"); {ty=E.BOTTOM,exp=T.nilExp()})
        end

      and newTuple (fields,t,p) = case S.find(tenv,t) of
        SOME ty => (case actualTy (ty,p) of
          (E.TUPLE(tylist)) =>
            let
              val types = map (fn arg => #ty (trexp arg)) fields
              val exps = map (fn arg => #exp (trexp arg)) fields
            in
              if types=tylist then {ty=E.TUPLE(tylist),exp=T.tupleExp(exps)} else (Error.msg p ("arg type or num args do not match tuple: " ^ t); {ty=E.BOTTOM,exp=T.nilExp()})
            end
          | _ => (Error.msg p ("tuple does not exist: " ^ t); {ty=E.BOTTOM,exp=T.nilExp()}))
        | NONE => (Error.msg p ("tuple does not exist: " ^ t); {ty=E.BOTTOM,exp=T.nilExp()})

      and checkWhile (cond,body,pos) =
        let
          val c = trexp cond
          val _ = breakLabel := Temp.newlabel()
          val _ = depth := !depth + 1
          val t = trexp body
          val _ = depth := !depth - 1
          val _ = if (#ty t)=E.NIL then () else Error.msg pos ("while must have type nil")
        in
          {ty= E.NIL, exp=T.whileExp(!breakLabel, #exp c, #exp t)}
        end

    in
      trexp exp
    end

  and createFields (tenv,[],pos) = []
    | createFields (tenv, (typ::l),pos) = (case S.find(tenv, typ) of SOME t => (actualTy (t,pos))::(createFields (tenv,l,pos)) | _ => (Error.msg pos ("undefined type: " ^ typ); (createFields (tenv,l,pos))))

  and typeDecs (decs,tenv) =
    let
      fun declare (dec, tenv) = case dec of A.TypeDec{name, typ, pos} => S.insert(tenv, name, E.NAME(name, ref NONE)) | _ => tenv
    in
      foldl declare tenv decs
    end

  and typeInputs (decs, tenv) =
    let
      fun input (dec, tenv) = case dec of A.TypeDec{name, typ, pos} => (case S.find(tenv,name) of SOME(E.NAME(n, t)) => (t := SOME(transTy(tenv, typ, pos))) | _ => (); tenv) | _ => tenv
    in
      foldl input tenv decs
    end

  and funcDecs (decs, tenv, venv) =
    let
      fun resultType (result,pos) = (case S.find(tenv,result) of SOME t => actualTy (t,pos) | NONE => (Error.msg pos ("type does not exists: " ^ result); E.BOTTOM))
      fun transParam (name,typ,pos) = case S.find(tenv,typ) of SOME t => actualTy (t,pos) | _  => (Error.msg pos ("undefined type: " ^ typ); E.BOTTOM)
      fun makeLocal (name,typ,pos) = Temp.newtemp()
      fun declare (dec, venv) = case dec of A.FunctionDec{name, params, result, body, pos} => S.insert(venv, name, E.FunEntry{inputs=(map transParam params),result=(resultType (result,pos)), label=name, temps=(map makeLocal params), level=T.LEVEL(ref ())}) | _ => venv
    in
      foldl declare venv decs
    end

  and varInputs (decs, tenv, venv) =
    let
      fun input (dec, venv) = case dec of
        A.VarDec{name, init, pos} =>
          let
            val {ty,exp} = transExp (tenv,venv,init,T.TOP)
            val temp = Temp.newtemp()
            val _ = T.frags := T.getResult() @ [T.VARDEC(T.unNx (T.varDec(temp,exp)))]
          in
            S.insert(venv, name, E.VarEntry{ty=ty, temp=temp, level=T.TOP})
          end
        | _ => venv
    in
      foldl input venv decs
    end

  and typesEqual (E.BOTTOM, _) = true
    | typesEqual (_, E.BOTTOM) = true
    | typesEqual (t1, t2) = (t1=t2)

  and funcChecks (decs, tenv, venv) =
    let
      fun getType typ = case S.find(tenv,typ) of SOME t => actualTy (t,0) | _  => E.BOTTOM
      fun enter ((name,typ,pos)::params,temp::temps,level,venv) = enter(params, temps, level, S.insert(venv,name,E.VarEntry{ty = getType typ, temp=temp, level=level}))
        | enter ([],[],_,venv) = venv
        | enter (_,_,_,venv) = (Error.msg 0 ("temp/var input problem"); venv)
      fun checkBody (body,res,pos,level,label,temps,venv) =
        let
          val b = transExp (tenv,venv,body,level)
          val _ = if typesEqual (#ty b, getType res) then () else (Error.msg pos ("function body does not match definition"))
          val _ = T.frags := T.getResult() @ [T.PROC{body=(T.unNx (#exp b)),name=label,formals=temps}]
        in
          venv
        end
      fun check (dec, venv) = case dec of
        A.FunctionDec{name, params, result, body, pos} => (case S.find(venv,name) of
          SOME (E.FunEntry{inputs,result=res,label,temps,level}) => checkBody(body,result,pos,level,label,temps,enter (params,temps,level,venv))
          | _ => venv)
        | _ => venv
    in
      foldl check venv decs
    end
end
