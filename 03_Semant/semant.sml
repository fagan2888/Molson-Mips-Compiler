structure A = Absyn

signature SEMANT =
sig
  val transProg : A.dec list -> unit
end

structure Semant : SEMANT =
struct
  structure S = Env.Symbol
  structure E = Env

  val depth = ref 0

  fun transProg decs =
    let
      (* Creating type and variable environments and checking functions *)
      val tenv = typeDecs (decs, E.base_tenv) (* declare types *)
      val tenv' = typeInputs (decs, tenv) (* input types *)
      val venv = funcDecs (decs, tenv', E.base_venv) (* declare functions *)
      val venv' = varInputs (decs, tenv', venv) (* declare and input variables *)
      val venv'' = funcChecks (decs, tenv', venv') (* check functions *)
    in
      ()
    end

  and transTy (tenv,typ,pos) = (case typ of
    A.NameTy(s,p) => (case S.find(tenv,s) of SOME t => actualTy (t,p) | NONE => (Error.msg p ("undefined type: " ^ s); Env.BOTTOM))
    | A.TupleTy(list) => Env.TUPLE(createTuple (list,tenv)))

  and actualTy (ty,p) = case ty of (Env.NAME(n,rt)) => (case !rt of SOME t => actualTy (t,p) | NONE => Env.BOTTOM) | _ => ty

  and createTuple ([],tenv) = []
    | createTuple ((ty::list),tenv) = case ty of
      A.NameTy(s,p) => (case S.find(tenv, s) of SOME t => (actualTy (t,p))::(createTuple (list,tenv)) | NONE => (Error.msg p ("undefined type: " ^ s); Env.BOTTOM::(createTuple (list,tenv))))
      | A.TupleTy(l) => Env.TUPLE(createTuple (l,tenv))::(createTuple (list,tenv))

  and transExp (tenv, venv, exp) =
    let
      fun trexp (A.VarExp(A.SimpleVar(s,p))) = (case S.find(venv,s) of SOME (E.VarEntry{ty}) => ty | _ => (Error.msg p ("undefined var: " ^ s); Env.BOTTOM))
        | trexp (A.VarExp(A.FieldVar(v,i,p))) = (case trexp (A.VarExp(v)) of Env.TUPLE(tylist) => List.nth(tylist,i) | _ => (Error.msg p ("undefined tuple"); Env.BOTTOM))
        | trexp (A.NilExp) = Env.NIL
        | trexp (A.IntExp(int)) = Env.INT
        | trexp (A.StringExp(string)) = Env.STRING
        | trexp (A.BoolExp(bool)) = Env.BOOL
        | trexp (A.CallExp{func,args,pos}) = funcCall (func, args, pos)
        | trexp (A.OpExp{left,oper=A.PlusOp,right,pos}) = (checkInts(left,right,pos); Env.INT)
        | trexp (A.OpExp{left,oper=A.MinusOp,right,pos}) = (checkInts(left,right,pos); Env.INT)
        | trexp (A.OpExp{left,oper=A.TimesOp,right,pos}) = (checkInts(left,right,pos); Env.INT)
        | trexp (A.OpExp{left,oper=A.DivideOp,right,pos}) = (checkInts(left,right,pos); Env.INT)
        | trexp (A.OpExp{left,oper=A.EqOp,right,pos}) = (checkInts(left,right,pos); Env.BOOL)
        | trexp (A.OpExp{left,oper=A.NeqOp,right,pos}) = (checkInts(left,right,pos); Env.BOOL)
        | trexp (A.OpExp{left,oper=A.LtOp,right,pos}) = (checkInts(left,right,pos); Env.BOOL)
        | trexp (A.OpExp{left,oper=A.GtOp,right,pos}) = (checkInts(left,right,pos); Env.BOOL)
        | trexp (A.OpExp{left,oper=A.LeOp,right,pos}) = (checkInts(left,right,pos); Env.BOOL)
        | trexp (A.OpExp{left,oper=A.GeOp,right,pos}) = (checkInts(left,right,pos); Env.BOOL)
        | trexp (A.OpExp{left,oper=A.OrOp,right,pos}) = (checkInts(left,right,pos); Env.BOOL)
        | trexp (A.OpExp{left,oper=A.AndOp,right,pos}) = (checkInts(left,right,pos); Env.BOOL)
        | trexp (A.TupleExp{fields,typ,pos}) = checkTuple (fields, typ, pos)
        | trexp (A.SeqExp(list)) = trexp (List.last(list))
        | trexp (A.AssignExp{var,exp,pos}) = if typesEqual (trexp (A.VarExp(var)), trexp exp) then Env.BOTTOM else (Error.msg pos ("var and exp types do not match"); Env.BOTTOM)
        | trexp (A.IfElseExp{cond,exp1,exp2,pos}) = (checkBool(cond,pos); if typesEqual(trexp exp1, trexp exp2) then trexp exp1 else (Error.msg pos ("if-else exps do not match"); Env.BOTTOM))
        | trexp (A.WhileExp{cond,body,pos}) = checkWhile (cond,body,pos)
        | trexp (A.BreakExp(pos)) = if !depth>0 then Env.BOTTOM else (Error.msg pos ("break not inside loop"); Env.BOTTOM)

      and funcCall (func, args, pos) = case S.find(venv,func) of SOME (E.FunEntry{inputs=tylist,result=ty}) => (checkArgs (args,tylist,pos); ty) | _ => (Error.msg pos ("undefined func: " ^ func); Env.BOTTOM)

      and checkArgs (a::arglist,t::tylist, p) = if typesEqual (trexp a, t) then checkArgs(arglist,tylist,p) else (Error.msg p ("arg type does not match definition"))
        | checkArgs ([],[],_) = ()
        | checkArgs (_,_,p) = (Error.msg p ("num args does not match definition"))

      and typesEqual (t1,t2) = if t1=Env.BOTTOM orelse t2=Env.BOTTOM then true else t1=t2

      and checkInts (l,r,p) = if (trexp l)=Env.INT andalso (trexp r)=Env.INT then () else (Error.msg p ("op exp not int"))

      and checkBool (e,p) = if (trexp e)=Env.BOOL then () else (Error.msg p ("cond not bool"))

      and checkTuple (f,t,p) = case S.find(tenv,t) of SOME ty => (case actualTy (ty,p) of (Env.TUPLE(tylist)) => (checkArgs (f,tylist,p); Env.TUPLE(tylist)) | _ => (Error.msg p ("tuple does not exist: " ^ t); Env.BOTTOM)) | NONE => (Error.msg p ("tuple does not exist: " ^ t); Env.BOTTOM)

      and checkWhile (cond,body,pos) =
        let
          val _ = checkBool(cond,pos)
          val _ = depth := !depth + 1
          val t = trexp body
          val _ = depth := !depth - 1
        in
          t
        end

    in
      trexp exp
    end

  and createFields (tenv,[],pos) = []
    | createFields (tenv, (typ::l),pos) = (case S.find(tenv, typ) of SOME t => (actualTy (t,pos))::(createFields (tenv,l,pos)) | _ => (Error.msg pos ("undefined type: " ^ typ); (createFields (tenv,l,pos))))

  and typeDecs (decs,tenv) =
    let
      fun declare (dec, tenv) = case dec of A.TypeDec{name, typ, pos} => S.insert(tenv, name, Env.NAME(name, ref NONE)) | _ => tenv
    in
      foldl declare tenv decs
    end

  and typeInputs (decs, tenv) =
    let
      fun input (dec, tenv) = case dec of A.TypeDec{name, typ, pos} => (case S.find(tenv,name) of SOME(Env.NAME(n, t)) => (t := SOME(transTy(tenv, typ, pos))) | _ => (); tenv) | _ => tenv
    in
      foldl input tenv decs
    end

  and funcDecs (decs, tenv, venv) =
    let
      fun resultType (result,pos) = (case S.find(tenv,result) of SOME t => actualTy (t,pos) | NONE => (Error.msg pos ("type does not exists: " ^ result); Env.BOTTOM))
      fun transParam (name,typ,pos) = case S.find(tenv,typ) of SOME t => actualTy (t,pos) | _  => (Error.msg pos ("undefined type: " ^ typ); Env.BOTTOM)
      fun declare (dec, venv) = case dec of A.FunctionDec{name, params, result, body, pos} => S.insert(venv, name, E.FunEntry{inputs=(map transParam params),result=(resultType (result,pos))}) | _ => venv
    in
      foldl declare venv decs
    end

  and varInputs (decs, tenv, venv) =
    let
      fun input (dec, venv) = case dec of A.VarDec{name, init, pos} => S.insert(venv, name, E.VarEntry{ty=(transExp (tenv,venv,init))}) | _ => venv
    in
      foldl input venv decs
    end

  and typesEqual (Env.BOTTOM, _) = true
    | typesEqual (_, Env.BOTTOM) = true
    | typesEqual (t1, t2) = (t1=t2)

  and funcChecks (decs, tenv, venv) =
    let
      fun getType typ = case S.find(tenv,typ) of SOME t => actualTy (t,0) | _  => Env.BOTTOM
      fun enter ((name,typ,pos)::params,venv) = enter(params, S.insert(venv,name,E.VarEntry{ty = getType typ}))
        | enter ([],venv) = venv
      fun checkBody (body,res,pos,venv) = ((if typesEqual (transExp (tenv,venv,body), getType res) then () else (Error.msg pos ("function body does not match definition"))); venv)
      fun check (dec, venv) = case dec of A.FunctionDec{name, params, result, body, pos} => checkBody(body,result,pos,enter (params,venv)) | _ => venv
    in
      foldl check venv decs
    end
end
