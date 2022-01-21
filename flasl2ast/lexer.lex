open Tokens

type pos = int
type lexresult = (svalue,pos) token
fun eof() = Tokens.EOF(0,0)
val lineno = ref 1

fun modify (lst,lastchar) =
case lst of
[] => []
|h::t => if(h <> #" ")
         then h::modify(t,h)
         else
            if(lastchar = #" ")
            then modify(t,h)
            else h::modify(t,h)

fun removehead lst =
    case lst of
    [] => []
    |h::t => t
   
fun removetail lst =
    case lst of
    h::t => if (length t = 1)
             then [h]
             else h::removetail t
    |h => []

fun modify_string s =
    let val lst = explode s in
      let val lstnew = modify (lst,#" ") in
        let val lstnew1 = removehead lstnew in
            let val lstnew2 = removetail lstnew1 in
                let val s_new = implode lstnew2 in
                    s_new
                end
            end
        end
      end
    end

fun init() = ()

%%
%header (functor IntexpLexFun(structure Tokens: Intexp_TOKENS));
ws  = [\ \t];

%%

\n          =>  (lineno := !lineno + 1; lex());
"NOT"       =>  (NOT(yypos , !lineno));
"AND"       =>  (AND(yypos , !lineno));
"OR"        =>  (OR(yypos , !lineno));
"IF"        =>  (IF(yypos , !lineno));
"THEN"      =>  (THEN(yypos , !lineno));
"ELSE"      =>  (ELSE(yypos , !lineno));
"IFF"       =>  (IFF(yypos , !lineno));
"THEREFORE" =>  (THEREFORE(yypos , !lineno));
"("         =>  (LPAREN(yypos , !lineno));
")"         =>  (RPAREN(yypos , !lineno));
\.           => (FULLSTOP(yypos , !lineno));
[\"][^\"\.\(\)\127\000-\031]*[\"]  =>  (ATPROP(modify_string yytext,yypos , !lineno));
{ws}+        => (lex());
.           =>  (raise Fail("ScanError in line number : "^Int.toString(!lineno)^"at char position at"^Int.toString(yypos)); lex());
