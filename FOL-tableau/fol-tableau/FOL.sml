
signature FOL =
  sig
    datatype term = VAR of string
                  | FUN of string * term list
                  | CONST of string (*for generated constraints only*)

    datatype Pred = FF (*special constant for closing a tableau path*)
                  | ATOM of string * term list
                  | NOT of Pred
                  | AND of Pred * Pred
                  | OR of Pred * Pred
                  | COND of Pred * Pred
                  | BIC of Pred * Pred
                  | ITE of Pred * Pred * Pred
                  | ALL of term * Pred
                  | EX of term * Pred

    datatype Argument = HENCE of Pred list * Pred

    val mktableau: Pred list * Pred -> unit (* outputs file "tableau.dot" in dot format *)

    exception NotVAR (* Binding term in a quantified formula is not a variable *)
    exception NotWFT (* term is not well-formed *)
    exception NotWFP (* predicate is not well-formed *)
    exception NotWFA (* argument is not well-formed *)
    exception NotClosed (* a formula is not closed *)
  end
