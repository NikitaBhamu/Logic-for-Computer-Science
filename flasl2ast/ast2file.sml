open AST

fun propTOstring p =
    case p of
    ATOM(s) => "ATOM(\""^s^"\")"
   |NOT(p1) => "NOT("^propTOstring(p1)^")"
   |AND(p1,p2) => "AND("^propTOstring(p1)^","^propTOstring(p2)^")"
   |OR(p1,p2) => "OR("^propTOstring(p1)^","^propTOstring(p2)^")"
   |COND(p1,p2) => "COND("^propTOstring(p1)^","^propTOstring(p2)^")"
   |BIC(p1,p2) => "BIC("^propTOstring(p1)^","^propTOstring(p2)^")"
   |ITE(p1,p2,p3) => "ITE("^propTOstring(p1)^","^propTOstring(p2)^","^propTOstring(p3)^")"

fun propListTOstring pl =
    case pl of
    [] => ""
    |h::[] => propTOstring h
    |h::t => (propListTOstring t)^","^(propTOstring h)

fun argTostring a =
    case a of
    HENCE(pplist,p) => "HENCE("^"["^propListTOstring(pplist)^"],"^propTOstring(p)^")"

fun ast2file (arg: Argument) =
    let val os = TextIO.openOut "arg.sml"
        in
            TextIO.output(os, "val arg = ");
            TextIO.output(os,argTostring(arg));
            TextIO.closeOut os
        end;
