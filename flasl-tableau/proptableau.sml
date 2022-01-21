open AST
exception BadProp;

fun contains p plist =
    case plist of
    [] => false
    |h::t => if (h = p)
             then true
             else false

fun containsAtom pstr plist =
    case plist of
    [] => false
    |ATOM(str)::t => if (str = pstr)
                     then true
                     else (containsAtom pstr t)
    |_::t => containsAtom pstr t

fun containsNegationAtom pstr plist =
    case plist of
    [] => false
    |NOT(ATOM(str))::t => if (str = pstr)
                          then true
                          else (containsNegationAtom pstr t)
    |_::t => containsNegationAtom pstr t

fun propTOstring p =
    case p of
    ATOM(s) => "\""^s^"\""
    |NOT(p1) => "NOT ( "^propTOstring(p1)^" )"
    |AND(p1,p2) => "( "^propTOstring(p1)^" ) AND ( "^propTOstring(p2)^" )"
    |OR(p1,p2) => "( "^propTOstring(p1)^" ) OR ( "^propTOstring(p2)^" )"
    |COND(p1,p2) => "( "^propTOstring(p2)^" ) IF ( "^propTOstring(p1)^" )"
    |BIC(p1,p2) => "( "^propTOstring(p1)^" ) IFF ( "^propTOstring(p2)^" )"
    |ITE(p1,p2,p3) => "IF ( "^propTOstring(p1)^" ) THEN ( "^propTOstring(p2)^" ) ELSE ( "^propTOstring(p3)^" )"


fun makeAssignment atomlist =
  case atomlist of
  [] => ""
  |ATOM(str)::t => "("^str^") : True\n"^(makeAssignment t)
  |NOT(ATOM(str))::t => "("^str^") : False\n"^(makeAssignment t)
  |_::t => makeAssignment t


fun printinFile filename bool atomlist =
  let val os = TextIO.openOut (filename^".out") in
  if(bool = true)
  then (TextIO.output(os,"THE PROPOSITIONAL LOGIC IS VALID!");
        TextIO.closeOut os)
  else (
        let val falsestr = makeAssignment atomlist in
        TextIO.output(os,"THE PROPOSITIONAL LOGIC IS INVALID!\n\nA possible false assignment of it is :\n\n"^falsestr);
        TextIO.closeOut os
        end
       )
  end


fun firstfun proplist atomlist filename =
  case proplist of
  [] => (printinFile filename false atomlist;
        false)
  |h::t => ( case h of
             ATOM(s) => if((containsNegationAtom s atomlist) = true)
                        then (printinFile filename true atomlist;
                              true)
                        else (firstfun (t) (atomlist@[ATOM(s)]) (filename))
            |NOT(ATOM(s)) => if((containsAtom s atomlist)  = true)
                             then (printinFile filename true atomlist;
                                   true)
                             else (firstfun (t) (atomlist@[NOT(ATOM(s))]) (filename))
            |NOT(NOT(ATOM(s))) => if((containsNegationAtom s atomlist) = true)
                                  then (printinFile filename true atomlist;
                                        true)
                                  else (firstfun (t) (atomlist@[ATOM(s)]) (filename))
            |NOT(NOT(p)) => firstfun (p::t) (atomlist) (filename)
            |NOT(AND(p1,p2)) => (firstfun (NOT(p1)::t) (atomlist) (filename)) andalso (firstfun (NOT(p2)::t) (atomlist) (filename))
            |NOT(OR(p1,p2)) => (firstfun (NOT(p1)::NOT(p2)::t) (atomlist) (filename))
            |NOT(COND(p1,p2)) => (firstfun (p1::NOT(p2)::t) (atomlist) (filename))
            |NOT(BIC(p1,p2)) => (firstfun ((AND(p1,NOT(p2)))::t) (atomlist) (filename)) andalso (firstfun ((AND(NOT(p1),p2))::t) (atomlist) (filename))
            |AND(p1,p2) => (firstfun (p1::p2::t) (atomlist) (filename))
            |OR(p1,p2) => (firstfun (p1::t) (atomlist) (filename)) andalso (firstfun (p2::t) (atomlist) (filename))
            |COND(p1,p2) => (firstfun (NOT(p1)::t) (atomlist) (filename)) andalso (firstfun (p2::t) (atomlist) (filename))
            |BIC(p1,p2) => (firstfun ((AND(p1,p2))::t) (atomlist) (filename)) andalso (firstfun ((AND(NOT(p1),NOT(p2)))::t) (atomlist) (filename))
            |ITE(p1, p2, p3) => firstfun (COND(p1,p2)::COND(NOT(p1),p3)::t) (atomlist) (filename)
      			|NOT(ITE(p1, p2, p3)) => firstfun (OR(NOT(COND(p1,p2)), NOT(COND(NOT(p1),p3)))::t) (atomlist) (filename)
          )

fun printlist l =
  case l of
  [] => print("\n")
  |h::t => (print((propTOstring h)^"\n\n");
            printlist t)


fun listProps prop =
  case prop of
  HENCE(pl,p) => pl@[NOT(p)]

fun listHence pp =
  case pp of
  HENCE(pl,p) => printlist pl


fun tableau (arg:Argument) filename =
    let val proplist =  listProps arg in
      let val atomlist = [] in
        firstfun proplist atomlist filename
      end
    end;
