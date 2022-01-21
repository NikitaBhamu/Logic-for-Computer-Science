functor IntexpLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Intexp_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\040\000\002\000\040\000\005\000\040\000\009\000\040\000\
\\010\000\040\000\000\000\
\\001\000\001\000\041\000\002\000\041\000\005\000\041\000\009\000\041\000\
\\010\000\041\000\000\000\
\\001\000\001\000\010\000\002\000\009\000\005\000\008\000\009\000\007\000\
\\010\000\006\000\013\000\039\000\000\000\
\\001\000\001\000\010\000\002\000\009\000\005\000\008\000\009\000\017\000\
\\010\000\006\000\000\000\
\\001\000\001\000\010\000\002\000\009\000\005\000\008\000\010\000\006\000\000\000\
\\001\000\003\000\042\000\004\000\042\000\005\000\042\000\006\000\042\000\
\\007\000\042\000\008\000\042\000\011\000\042\000\012\000\042\000\000\000\
\\001\000\003\000\043\000\004\000\043\000\005\000\043\000\006\000\043\000\
\\007\000\043\000\008\000\043\000\011\000\043\000\012\000\043\000\000\000\
\\001\000\003\000\044\000\004\000\044\000\005\000\044\000\006\000\044\000\
\\007\000\044\000\008\000\044\000\011\000\044\000\012\000\044\000\000\000\
\\001\000\003\000\045\000\004\000\045\000\005\000\045\000\006\000\045\000\
\\007\000\045\000\008\000\045\000\011\000\045\000\012\000\045\000\000\000\
\\001\000\003\000\046\000\004\000\046\000\005\000\046\000\006\000\046\000\
\\007\000\046\000\008\000\046\000\011\000\046\000\012\000\046\000\000\000\
\\001\000\003\000\047\000\004\000\047\000\005\000\047\000\006\000\047\000\
\\007\000\047\000\008\000\047\000\011\000\047\000\012\000\047\000\000\000\
\\001\000\003\000\048\000\004\000\048\000\005\000\048\000\006\000\048\000\
\\007\000\048\000\008\000\048\000\011\000\048\000\012\000\048\000\000\000\
\\001\000\003\000\050\000\004\000\050\000\005\000\050\000\006\000\050\000\
\\007\000\050\000\008\000\050\000\011\000\050\000\012\000\050\000\000\000\
\\001\000\003\000\051\000\004\000\051\000\005\000\051\000\006\000\051\000\
\\007\000\051\000\008\000\051\000\011\000\051\000\012\000\051\000\000\000\
\\001\000\003\000\052\000\004\000\052\000\005\000\052\000\006\000\052\000\
\\007\000\052\000\008\000\052\000\011\000\052\000\012\000\052\000\000\000\
\\001\000\003\000\052\000\004\000\052\000\005\000\052\000\008\000\052\000\
\\011\000\029\000\000\000\
\\001\000\003\000\052\000\004\000\052\000\005\000\052\000\008\000\052\000\
\\012\000\015\000\000\000\
\\001\000\003\000\052\000\004\000\052\000\005\000\052\000\008\000\052\000\
\\012\000\027\000\000\000\
\\001\000\003\000\052\000\004\000\052\000\005\000\052\000\008\000\052\000\
\\012\000\030\000\000\000\
\\001\000\003\000\052\000\004\000\052\000\005\000\052\000\008\000\052\000\
\\012\000\032\000\000\000\
\\001\000\003\000\014\000\004\000\013\000\005\000\012\000\006\000\031\000\
\\008\000\011\000\000\000\
\\001\000\003\000\014\000\004\000\013\000\005\000\012\000\008\000\011\000\
\\011\000\049\000\012\000\049\000\000\000\
\\001\000\013\000\000\000\000\000\
\\001\000\013\000\037\000\000\000\
\\001\000\013\000\038\000\000\000\
\"
val actionRowNumbers =
"\002\000\021\000\016\000\003\000\
\\004\000\004\000\004\000\004\000\
\\012\000\004\000\004\000\004\000\
\\004\000\000\000\017\000\004\000\
\\015\000\018\000\020\000\014\000\
\\005\000\011\000\010\000\007\000\
\\006\000\001\000\019\000\013\000\
\\024\000\004\000\023\000\009\000\
\\004\000\008\000\022\000"
val gotoT =
"\
\\001\000\034\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\003\000\014\000\004\000\001\000\000\000\
\\003\000\016\000\004\000\001\000\000\000\
\\003\000\017\000\004\000\001\000\000\000\
\\003\000\019\000\004\000\018\000\000\000\
\\003\000\019\000\004\000\020\000\000\000\
\\000\000\
\\003\000\019\000\004\000\021\000\000\000\
\\003\000\019\000\004\000\022\000\000\000\
\\003\000\019\000\004\000\023\000\000\000\
\\003\000\019\000\004\000\024\000\000\000\
\\000\000\
\\000\000\
\\003\000\026\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\019\000\004\000\031\000\000\000\
\\000\000\
\\000\000\
\\003\000\019\000\004\000\033\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 35
val numrules = 16
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | ATPROP of unit ->  (string) | stat of unit ->  (AST.Prop)
 | statement of unit ->  (AST.Prop)
 | statementlist of unit ->  (AST.Prop list)
 | start of unit ->  (AST.Argument)
end
type svalue = MlyValue.svalue
type result = AST.Argument
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 12) => true | _ => false
val showTerminal =
fn (T 0) => "ATPROP"
  | (T 1) => "NOT"
  | (T 2) => "AND"
  | (T 3) => "OR"
  | (T 4) => "IF"
  | (T 5) => "THEN"
  | (T 6) => "ELSE"
  | (T 7) => "IFF"
  | (T 8) => "THEREFORE"
  | (T 9) => "LPAREN"
  | (T 10) => "RPAREN"
  | (T 11) => "FULLSTOP"
  | (T 12) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, FULLSTOP1right)) :: ( _, ( MlyValue.statement 
statement1, _, _)) :: _ :: ( _, ( MlyValue.statementlist 
statementlist1, statementlist1left, _)) :: rest671)) => let val  
result = MlyValue.start (fn _ => let val  (statementlist as 
statementlist1) = statementlist1 ()
 val  (statement as statement1) = statement1 ()
 in (AST.HENCE(statementlist,statement))
end)
 in ( LrTable.NT 0, ( result, statementlist1left, FULLSTOP1right), 
rest671)
end
|  ( 1, ( ( _, ( _, _, FULLSTOP1right)) :: ( _, ( MlyValue.statement 
statement1, _, _)) :: ( _, ( _, THEREFORE1left, _)) :: rest671)) =>
 let val  result = MlyValue.start (fn _ => let val  (statement as 
statement1) = statement1 ()
 in (AST.HENCE([],statement))
end)
 in ( LrTable.NT 0, ( result, THEREFORE1left, FULLSTOP1right), rest671
)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.start (fn _ => (
raise Fail("ParseError")))
 in ( LrTable.NT 0, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( _, _, FULLSTOP1right)) :: ( _, ( MlyValue.statement 
statement1, statement1left, _)) :: rest671)) => let val  result = 
MlyValue.statementlist (fn _ => let val  (statement as statement1) = 
statement1 ()
 in (statement::[])
end)
 in ( LrTable.NT 1, ( result, statement1left, FULLSTOP1right), rest671
)
end
|  ( 4, ( ( _, ( _, _, FULLSTOP1right)) :: ( _, ( MlyValue.statement 
statement1, _, _)) :: ( _, ( MlyValue.statementlist statementlist1, 
statementlist1left, _)) :: rest671)) => let val  result = 
MlyValue.statementlist (fn _ => let val  (statementlist as 
statementlist1) = statementlist1 ()
 val  (statement as statement1) = statement1 ()
 in (statement::statementlist)
end)
 in ( LrTable.NT 1, ( result, statementlist1left, FULLSTOP1right), 
rest671)
end
|  ( 5, ( ( _, ( MlyValue.stat stat1, _, stat1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.statement (fn
 _ => let val  (stat as stat1) = stat1 ()
 in (AST.NOT(stat))
end)
 in ( LrTable.NT 2, ( result, NOT1left, stat1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.stat stat2, _, stat2right)) :: _ :: ( _, ( 
MlyValue.stat stat1, stat1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  stat1 = stat1 ()
 val  stat2 = stat2 ()
 in (AST.AND(stat1,stat2))
end)
 in ( LrTable.NT 2, ( result, stat1left, stat2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.stat stat2, _, stat2right)) :: _ :: ( _, ( 
MlyValue.stat stat1, stat1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  stat1 = stat1 ()
 val  stat2 = stat2 ()
 in (AST.OR(stat1,stat2))
end)
 in ( LrTable.NT 2, ( result, stat1left, stat2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.stat stat3, _, stat3right)) :: _ :: ( _, ( 
MlyValue.stat stat2, _, _)) :: _ :: ( _, ( MlyValue.stat stat1, _, _))
 :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  stat1 = stat1 ()
 val  stat2 = stat2 ()
 val  stat3 = stat3 ()
 in (AST.ITE(stat1,stat2,stat3))
end)
 in ( LrTable.NT 2, ( result, IF1left, stat3right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.stat stat2, _, stat2right)) :: _ :: ( _, ( 
MlyValue.stat stat1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.statement (fn _ => let val  stat1 = stat1
 ()
 val  stat2 = stat2 ()
 in (AST.COND(stat1,stat2))
end)
 in ( LrTable.NT 2, ( result, IF1left, stat2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.stat stat2, _, stat2right)) :: _ :: ( _, ( 
MlyValue.stat stat1, stat1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  stat1 = stat1 ()
 val  stat2 = stat2 ()
 in (AST.COND(stat1,stat2))
end)
 in ( LrTable.NT 2, ( result, stat1left, stat2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.stat stat2, _, stat2right)) :: _ :: ( _, ( 
MlyValue.stat stat1, stat1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  stat1 = stat1 ()
 val  stat2 = stat2 ()
 in (AST.BIC(stat1,stat2))
end)
 in ( LrTable.NT 2, ( result, stat1left, stat2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.stat stat1, stat1left, stat1right)) :: 
rest671)) => let val  result = MlyValue.statement (fn _ => let val  (
stat as stat1) = stat1 ()
 in (stat)
end)
 in ( LrTable.NT 2, ( result, stat1left, stat1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ATPROP ATPROP1, ATPROP1left, ATPROP1right))
 :: rest671)) => let val  result = MlyValue.stat (fn _ => let val  (
ATPROP as ATPROP1) = ATPROP1 ()
 in (AST.ATOM(ATPROP))
end)
 in ( LrTable.NT 3, ( result, ATPROP1left, ATPROP1right), rest671)
end
|  ( 14, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.statement 
statement1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.stat (fn _ => let val  (statement as 
statement1) = statement1 ()
 in (statement)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.statement statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = MlyValue.stat (fn
 _ => let val  (statement as statement1) = statement1 ()
 in (statement)
end)
 in ( LrTable.NT 3, ( result, statement1left, statement1right), 
rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Intexp_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ATPROP (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ATPROP (fn () => i),p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun IFF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun THEREFORE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun FULLSTOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
end
end
