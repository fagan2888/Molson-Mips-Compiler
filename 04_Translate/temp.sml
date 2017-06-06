structure Temp =
struct
  type temp = int
  type label = string

  val labelCount = ref 0
  val temps = ref 100

  fun newtemp() =
	  let
      val t  = !temps
	    val () = temps := t+1
	  in
	    t
	  end

  fun makestring t = "t" ^ Int.toString t

  fun newlabel() =
	  let
      val x  = !labelCount
	    val () = labelCount := x +1
	  in
	    "L" ^ Int.toString(!labelCount)
	  end

end
