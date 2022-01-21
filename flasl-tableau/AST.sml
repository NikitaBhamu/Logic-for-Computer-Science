
structure AST = struct
    exception Atom_exception
    datatype Prop =
          ATOM of string |
          NOT of Prop |
          AND of Prop * Prop |
          OR of Prop*Prop |
          COND of Prop * Prop |
          BIC of Prop * Prop |
          ITE of Prop * Prop * Prop
    and Argument = HENCE of Prop list * Prop
end
