open AST

fun propTOstring p =
    case p of
    ATOM(s) => "\""^s^"\""
    |NOT(p1) => "NOT ( "^propTOstring(p1)^" )"
    |AND(p1,p2) => "( "^propTOstring(p1)^" ) AND ( "^propTOstring(p2)^" )"
    |OR(p1,p2) => "( "^propTOstring(p1)^" ) OR ( "^propTOstring(p2)^" )"
    |COND(p1,p2) => "( "^propTOstring(p2)^" ) IF ( "^propTOstring(p1)^" )"
    |BIC(p1,p2) => "( "^propTOstring(p1)^" ) IFF ( "^propTOstring(p2)^" )"
    |ITE(p1,p2,p3) => "IF ( "^propTOstring(p1)^" ) THEN ( "^propTOstring(p2)^" ) ELSE ( "^propTOstring(p3)^" )"

fun propListTOstring pl =
    case pl of
    [] => ""
    |h::t => (propTOstring h)^"."^(propListTOstring t)

fun argTostring a =
    case a of
    HENCE(pplist,p) => (propListTOstring pplist)^" THEREFORE "^(propTOstring p)^" ."

fun ast2flasl (arg: Argument) =
    let val os = TextIO.openOut "arg-out.flasl"
        in
            TextIO.output(os,argTostring(arg));
            TextIO.closeOut os
        end;
