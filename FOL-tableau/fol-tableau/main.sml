open fol
exception NotUnifiable

(*If the function or the variable is present then it will return its arity else minus 1 *)
fun contains1 a blist =
  case blist of
  [] => ~1
  |(hName,hValue)::t => if(hName = a) then hValue
           else contains1 a t

fun contains2 a blist =
  case blist of
  [] => false
  |h::t => if (h = a) then true
           else contains2 a t



fun termListappend tl bound =
  case tl of
  [] => []
  |h::t => [(h,bound)]@(termListappend t bound)


fun checkValid_Term termList funAritylist atomAritylist =
  case termList of
  [] => true
  |(h,hbound)::t => (case h of
                     CONST(s) => checkValid_Term t funAritylist atomAritylist
                     |VAR(s) => (if (contains2 s hbound)
                                 then checkValid_Term t funAritylist atomAritylist
                                 else raise NotClosed
                                )
                     |FUN(s,tl) => (
                                      let val arity = length tl in
                                        if( (contains1 s funAritylist) = ~1)
                                        then  checkValid_Term ((termListappend tl hbound)@t)  (funAritylist@[(s,arity)])  (atomAritylist)
                                        else(
                                              if (arity = (contains1 s funAritylist))
                                              then checkValid_Term ((termListappend tl hbound)@t)  (funAritylist)  (atomAritylist)
                                              else raise NotWFT
                                            )
                                      end
                                    )
                    )


fun checkValid_Predicate predList funAritylist atomAritylist =
  case predList of
  [] => true
  |(h,hbound)::t => (case h of
                     FF => raise NotWFA

                     |ATOM(s,tl) => (let val arity = length tl in
                                        if( (contains1 s atomAritylist) = ~1)
                                        then (
                                               if( (checkValid_Term (termListappend tl hbound) (funAritylist) (atomAritylist@[(s,arity)]) ) = true )
                                               then checkValid_Predicate (t) (funAritylist) (atomAritylist@[(s,arity)])
                                               else false
                                             )
                                        else(
                                             if (arity = (contains1 s atomAritylist))
                                             then (
                                                   if(checkValid_Term (termListappend tl hbound) funAritylist atomAritylist = true)
                                                   then checkValid_Predicate (t) (funAritylist) (atomAritylist)
                                                   else false
                                                  )
                                             else raise NotWFP
                                        )
                                     end
                                    )

                     |NOT(pd) => checkValid_Predicate ([(pd,hbound)]@t) (funAritylist) (atomAritylist)

                     |AND(pd1,pd2) => checkValid_Predicate ([(pd1,hbound), (pd2,hbound)]@t) (funAritylist) (atomAritylist)

                     |OR(pd1,pd2) => checkValid_Predicate ([(pd1,hbound), (pd2,hbound)]@t) (funAritylist) (atomAritylist)

                     |COND(pd1,pd2) => checkValid_Predicate ([(pd1,hbound), (pd2,hbound)]@t) (funAritylist) (atomAritylist)

                     |BIC(pd1,pd2) => checkValid_Predicate ([(pd1,hbound), (pd2,hbound)]@t) (funAritylist) (atomAritylist)

                     |ITE(pd1,pd2,pd3) => checkValid_Predicate ([(pd1,hbound), (pd2,hbound), (pd3,hbound)]@t) (funAritylist) (atomAritylist)

                     |ALL(te,pd) => (case te of
                                      VAR(s) => checkValid_Predicate ([(pd,hbound@[s])]@t) (funAritylist) (atomAritylist)
                                      |_ => raise NotVAR
                                    )

                     |EX(te,pd) => (case te of
                                      VAR(s) => checkValid_Predicate ([(pd,hbound@[s])]@t) (funAritylist) (atomAritylist)
                                      |_ => raise NotVAR
                                   )
                    )


fun checkValid_Argument (arg:Argument) =
  case arg of
  HENCE(pl,p) => checkValid_Predicate (termListappend (pl@[p]) []) ([]) ([])


(*Check whether terms are equal or not*)
fun equal_terms (t1:term) (t2:term) =
  case (t1,t2) of
  (CONST(s1),CONST(s2)) => if (s1 = s2) then true else false
  |(VAR(s1),VAR(s2)) => if (s1=s2) then true else false
  |(FUN(s1,tl1),FUN(s2,tl2)) => if(s1=s2)
                              then equal_termList tl1 tl2
                              else false
  |_ => false

and equal_termList (tl1 :term list) (tl2 :term list) =
  case (tl1,tl2) of
  ([],[]) => true
  |(h1::t1, h2::t2) => if(equal_terms h1 h2)
                     then equal_termList t1 t2
                     else false
  |_ => false


(*Checks whether predicates are equal or not*)
fun equal_predicates (p1 :Pred) (p2 :Pred) =
  case (p1,p2) of
  (FF,FF) => true
  |(ATOM(s1,tl1), ATOM(s2,tl2)) => if(s1=s2) then (equal_termList tl1 tl2) else false
  |(NOT(pd1), NOT(pd2)) => equal_predicates pd1 pd2
  |(AND(pd_a1,pd_b1), AND(pd_a2,pd_b2)) => (equal_predicates pd_a1 pd_a2) andalso (equal_predicates pd_b1 pd_b2)
  |(OR(pd_a1,pd_b1), OR(pd_a2,pd_b2)) => (equal_predicates pd_a1 pd_a2) andalso (equal_predicates pd_b1 pd_b2)
  |(COND(pd_a1,pd_b1), COND(pd_a2,pd_b2)) => (equal_predicates pd_a1 pd_a2) andalso (equal_predicates pd_b1 pd_b2)
  |(BIC(pd_a1,pd_b1), BIC(pd_a2,pd_b2)) => (equal_predicates pd_a1 pd_a2) andalso (equal_predicates pd_b1 pd_b2)
  |(ITE(pd_a1,pd_b1,pd_c1), ITE(pd_a2,pd_b2,pd_c2)) => ((equal_predicates pd_a1 pd_a2) andalso (equal_predicates pd_b1 pd_b2)) andalso equal_predicates pd_c1 pd_c2
  |(ALL(te1,pd1), ALL(te2,pd2)) => if(equal_terms te1 te2)
                                   then equal_predicates pd1 pd2
                                   else false
  |(EX(te1,pd1), EX(te2,pd2)) => if(equal_terms te1 te2)
                                 then equal_predicates pd1 pd2
                                 else false
  |_ => false

and equal_predicateList (pl1 :Pred list) (pl2 :Pred list) =
  case (pl1,pl2) of
  ([],[]) => true
  |(h1::t1, h2::t2) => if(equal_predicates h1 h2)
                     then equal_predicateList t1 t2
                     else false
  |_ => false

(*Disclaimer : In the below section we have assumed that the substitution we have is the valid substitution*)

(*Here s is a single valid substitution containing the variable,term tuple*)
fun singleSubstitute_term te (substr,subter) =
  case te of
  CONST(s) => te
  |VAR(s) => if(substr = s)
             then subter
             else te
  |FUN(s,tl) => FUN(s,(singleSubstitute_termlist tl (substr,subter)))

and singleSubstitute_termlist tl (substr,subter) =
  case tl of
  [] => []
  |h::t => [singleSubstitute_term h (substr,subter)]@(singleSubstitute_termlist t (substr,subter))


(*Subsitution (a,b) ki list hai hai matlab a ko b substitute kardo, Here a is a variable and b is a term*)
fun substitution_term te substitution =
  case substitution of
  [] => te
  |h::t => substitution_term (singleSubstitute_term te h) (t)

fun substitution_termlist tl substitution =
  case tl of
  [] => []
  |h::t => [substitution_term h substitution]@(substitution_termlist t substitution)


fun singleSubstitute_pred pred (substr,subter) =
  case pred of
  FF => pred
  |ATOM(s,tl) =>  ATOM(s,(singleSubstitute_termlist (tl) (substr,subter) ))
  |NOT(pd) =>  NOT(singleSubstitute_pred (pd) (substr,subter) )
  |AND(pd1,pd2) =>  AND(singleSubstitute_pred (pd1) (substr,subter) , singleSubstitute_pred (pd2) (substr,subter))
  |OR(pd1,pd2) =>  OR(singleSubstitute_pred (pd1) (substr,subter) , singleSubstitute_pred (pd2) (substr,subter))
  |COND(pd1,pd2) =>  COND(singleSubstitute_pred (pd1) (substr,subter) , singleSubstitute_pred (pd2) (substr,subter))
  |BIC(pd1,pd2) =>  BIC(singleSubstitute_pred (pd1) (substr,subter) , singleSubstitute_pred (pd2) (substr,subter))
  |ITE(pd1,pd2,pd3) =>  ITE(singleSubstitute_pred (pd1) (substr,subter) , singleSubstitute_pred (pd2) (substr,subter), singleSubstitute_pred (pd3) (substr,subter))
  |ALL(te,pd) => ALL( singleSubstitute_term (te) (substr,subter) , singleSubstitute_pred (pd) (substr,subter) )
  |EX(te,pd) => EX( singleSubstitute_term (te) (substr,subter) , singleSubstitute_pred (pd) (substr,subter) )

fun singleSubstitute_predlist pl (substr,subter) =
  case pl of
  [] => []
  |h::t => [singleSubstitute_pred (h) (substr,subter)]@(singleSubstitute_predlist (t) (substr,subter))



(*Subsitution (a,b) ki list hai hai matlab a ko b substitute kardo, Here a is a variable and b is a term*)
fun substitution_pred p substitution =
  case substitution of
  [] => p
  |h::t => substitution_pred (singleSubstitute_pred (p) (h)) (t)


fun substitution_predlist pl substitution =
  case pl of
  [] => []
  |h::t => [substitution_pred h substitution]@(substitution_predlist t substitution)


(*Given two subsitution (v1,t1) and (v2,t2) whether they are equal or not*)
fun subsEquality (s1,t1) (s2,t2) =
  if(s1=s2)
  then equal_terms t1 t2
  else false

fun substitutionList_union s1 s2 =
  case s1 of
  [] => s2
  |h1::t1 => (case s2 of
              [] => s1
              |h2::t2 => if(subsEquality h1 h2)
                         then [h1]@(substitutionList_union t1 t2)
                         else [h1,h2]@(substitutionList_union t1 t2)
             )

(*Finding a substitution in which terms don't repeat*)
fun find_norep_subst sub subans=
  case sub of
  [] => subans
  |h::t => if(contains2 h subans)
           then subans
           else find_norep_subst t [h]@subans


(*Composition of two substitution lists*)
fun composition s1 s2 =
  case s1 of
  [] => s2
  |(subvar,subter)::t => find_norep_subst ([(subvar,substitution_term subter s2)]@(composition t s2)) ([])


fun var_in_function vstr funterm =
  case funterm of
  CONST(s) => false
  |VAR(s) => if(s=vstr) then true else false
  |FUN(s1,tl1) => if(var_in_functionList vstr tl1) then true else false

and var_in_functionList vstr funtermlist =
  case funtermlist of
  [] => false
  |h::t => if(var_in_function vstr h) then true else (var_in_functionList vstr t)



(*This unification fucntion will give a tuple of (boolean,mgu)*)
fun unification_terms (t1:term) (t2:term) =
  case (t1,t2) of
  (CONST(s1),CONST(s2)) => if (s1=s2) then (true,[]) else (false,[])
  |(VAR(s1),VAR(s2)) => if(s1=s2) then (true,[]) else (true,[(s1,VAR(s2))])
  |(FUN(s1,tl1),FUN(s2,tl2)) => if(s1=s2)
                                then  unification_termlist tl1 tl2
                                else (false,[])
  |(VAR(s1),CONST(s2)) => (true,[(s1,CONST(s2))])
  |(CONST(s2),VAR(s1)) => (true,[(s1,CONST(s2))])
  |(VAR(s1),FUN(s2,tl2)) => if (var_in_functionList s1 tl2) then (false,[]) else (true,[(s1,FUN(s2,tl2))])
  |(FUN(s2,tl2),VAR(s1)) => if (var_in_functionList s1 tl2) then (false,[]) else (true,[(s1,FUN(s2,tl2))])
  |_ => (false,[])

and unification_termlist (tl1:term list) (tl2:term list) =
  case (tl1,tl2) of
  ([],[]) => (true,[])
  |(h1::t1,h2::t2) => (let val (a,b) = unification_terms h1 h2 in
                         if(a=false) then (false,[])
                         else ( let val (c,d) = unification_termlist t1 t2 in
                                    if(c=false) then (false,[])
                                    else (true, composition b d)
                                end
                              )
                         end
                      )
  |_ => (false,[])


fun unification_preds (p1:Pred) (p2:Pred) =
    case (p1,p2) of
    (FF,FF) => (true,[])
    |(ATOM(s1,tl1),ATOM(s2,tl2))  => if(s1=s2) then (unification_termlist tl1 tl2) else (false,[])
    |(NOT(pd1),NOT(pd2))  => unification_preds pd1 pd2
    |(AND(pd_a1,pd_a2),AND(pd_b1,pd_b2))  => (let val (unifbool1, unif1) = unification_preds pd_a1 pd_b1 in
                                                if(unifbool1 = false) then (false,[])
                                                else( let val (unifbool2, unif2) = unification_preds pd_a2 pd_b2 in
                                                          if(unifbool2 = false) then (false,[])
                                                          else (true, composition unif1 unif2)
                                                      end
                                                    )
                                              end
                                             )
    |(OR(pd_a1,pd_a2), OR(pd_b1,pd_b2))  => (let val (unifbool1, unif1) = unification_preds pd_a1 pd_b1 in
                                                if(unifbool1 = false) then (false,[])
                                                else( let val (unifbool2, unif2) = unification_preds pd_a2 pd_b2 in
                                                          if(unifbool2 = false) then (false,[])
                                                          else (true, composition unif1 unif2)
                                                      end
                                                    )
                                              end
                                             )
    |(COND(pd_a1,pd_a2), COND(pd_b1,pd_b2))  => (let val (unifbool1, unif1) = unification_preds pd_a1 pd_b1 in
                                                  if(unifbool1 = false) then (false,[])
                                                  else( let val (unifbool2, unif2) = unification_preds pd_a2 pd_b2 in
                                                            if(unifbool2 = false) then (false,[])
                                                            else (true, composition unif1 unif2)
                                                        end
                                                      )
                                                 end
                                                )
    |(BIC(pd_a1,pd_a2), BIC(pd_b1,pd_b2))  => (let val (unifbool1, unif1) = unification_preds pd_a1 pd_b1 in
                                                if(unifbool1 = false) then (false,[])
                                                else( let val (unifbool2, unif2) = unification_preds pd_a2 pd_b2 in
                                                          if(unifbool2 = false) then (false,[])
                                                          else (true, composition unif1 unif2)
                                                      end
                                                    )
                                               end
                                              )
    |(ITE(pd_a1,pd_a2,pd_a3), ITE(pd_b1,pd_b2,pd_b3))  => (let val (unifbool1, unif1) = unification_preds pd_a1 pd_b1 in
                                                              if(unifbool1 = false) then (false,[])
                                                              else( let val (unifbool2, unif2) = unification_preds pd_a2 pd_b2 in
                                                                        if(unifbool2 = false) then (false,[])
                                                                        else ( let val (unifbool3, unif3) = unification_preds pd_a3 pd_b3 in
                                                                                  if(unifbool3 = false) then (false,[])
                                                                                  else (true, composition (composition unif1 unif2) unif3)
                                                                               end
                                                                             )
                                                                    end
                                                                  )
                                                            end
                                                           )

    |(ALL(te1,pd_1), ALL(te2,pd_2))  => (let val (unifbool1, unif1) = unification_terms te1 te2 in
                                            if(unifbool1 = false) then (false,[])
                                            else ( let val new_pd1 = substitution_pred pd_1 unif1 in
                                                     let val new_pd2 = substitution_pred pd_2 unif1 in
                                                         unification_preds new_pd1 new_pd2
                                                     end
                                                   end
                                                 )
                                         end
                                        )

    |(EX(te1,pd_1), EX(te2,pd_2))  =>  (let val (unifbool1, unif1) = unification_terms te1 te2 in
                                            if(unifbool1 = false) then (false,[])
                                            else ( let val new_pd1 = substitution_pred pd_1 unif1 in
                                                     let val new_pd2 = substitution_pred pd_2 unif1 in
                                                         unification_preds new_pd1 new_pd2
                                                     end
                                                   end
                                                 )
                                         end
                                        )

    |_  => (false,[])

and unification_predlist (pl1:Pred list) (pl2:Pred list) =
    case (pl1,pl2) of
    ([],[]) => (true,[])
    |(h1::t1,h2::t2) => (let val (a,b) = unification_preds h1 h2 in
                           if(a=false) then (false,[])
                           else ( let val (c,d) = unification_predlist t1 t2 in
                                      if(c=false) then (false,[])
                                      else (true, composition b d)
                                  end
                                )
                           end
                        )
    |_ => (false,[])




(*


























case pred of
FF => pred
|ATOM(s,tl) =>
|NOT(pd) =>
|AND(pd1,pd2) =>
|OR(pd1,pd2) =>
|COND(pd1,pd2) =>
|BIC(pd1,pd2) =>
|ITE(pd1,pd2,pd3) =>
|ALL(te,pd) =>
|EX(te,pd) =>


*)






























fun printtt h =
  print(h)
