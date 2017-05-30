structure Main =
struct

  fun compile filename =
    let
      val file = TextIO.openIn filename
      fun get _ = TextIO.input file
      val lexer = Mlex.makeLexer get
      fun lex() =
        let
          val t = lexer()
        in
          print (t ^ "\n");
          if t="EOF" then () else lex()
        end
      val _ = lex()
    in
     TextIO.closeIn file
    end
    
end
