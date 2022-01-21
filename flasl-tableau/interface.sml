structure Intexp = struct

  structure IntexpLrVals = IntexpLrValsFun(structure Token = LrParser.Token)
  structure IntexpLex = IntexpLexFun(structure Tokens = IntexpLrVals.Tokens);
  structure IntexpParser =
      Join(structure LrParser = LrParser
          structure ParserData = IntexpLrVals.ParserData
          structure Lex = IntexpLex)

  val invoke = fn lexstream =>
    let val print_error = fn (str,pos,_) =>
              TextIO.output(TextIO.stdOut,
                  "***Intexp Parser Error at character position " ^ (Int.toString pos)
                  ^ "***\n" ^ str^ "\n")
  in IntexpParser.parse(0,lexstream,print_error,())
  end

  fun newLexer fcn =
    let val lexer = IntexpParser.makeLexer fcn
      val _ = IntexpLex.UserDeclarations.init()
    in lexer
    end

  fun stringToLexer str =
    let val done = ref false in
        newLexer (fn n => if (!done) then "" else (done := true; str))
    end

  fun fileToLexer filename =
    let val inStream = TextIO.openIn(filename)
      in newLexer (fn n => TextIO.inputAll(inStream))
    end

  fun lexerToParser (lexer) =
    let val dummyEOF = IntexpLrVals.Tokens.EOF(0,0)
        val (result,lexer) = invoke lexer
        val (nextToken,lexer) = IntexpParser.Stream.get lexer
    in if IntexpParser.sameToken(nextToken,dummyEOF) then
        result
      else (TextIO.output(TextIO.stdOut,
                          "*** INTEXP PARSER WARNING -- unconsumed input ***\n");
            result)
    end

  val parseString = lexerToParser o stringToLexer
  val parseFile = lexerToParser o fileToLexer

end
