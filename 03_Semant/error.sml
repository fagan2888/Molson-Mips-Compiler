structure Error =
struct

  val filename = ref ""
  fun set file = filename:=file
  fun msg pos str = print (!filename ^ "." ^ Int.toString (pos) ^ ":" ^ str ^ "\n")

end
