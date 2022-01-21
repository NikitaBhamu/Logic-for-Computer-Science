

%%

%term
  ATPROP of string
 |NOT |AND |OR |IF |THEN |ELSE |IFF |THEREFORE
 |LPAREN
 |RPAREN
 |FULLSTOP
 |EOF

%nonterm
    start of AST.Argument
   |statementlist of AST.Prop list
   |statement of AST.Prop
   |stat of AST.Prop
   
%start start

%pos int
%noshift EOF
%eop EOF
%nodefault
%name Intexp
%left ATPROP THEREFORE LPAREN RPAREN FULLSTOP IF THEN ELSE IFF AND OR NOT EOF

%%

start : statementlist THEREFORE statement FULLSTOP (AST.HENCE(statementlist,statement))
     |THEREFORE statement FULLSTOP (AST.HENCE([],statement))
     |(raise Fail("ParseError"))
     
statementlist : statement FULLSTOP (statement::[])
              |statementlist statement FULLSTOP (statement::statementlist)
    
statement : NOT stat                               (AST.NOT(stat))
        |   stat AND stat                           (AST.AND(stat1,stat2))
        |   stat OR stat                            (AST.OR(stat1,stat2))
        |   IF stat THEN stat ELSE stat             (AST.ITE(stat1,stat2,stat3))
        |   IF stat THEN stat                       (AST.COND(stat1,stat2))
        |   stat IF stat                            (AST.COND(stat1,stat2))
        |   stat IFF stat                           (AST.BIC(stat1,stat2))
        |   stat                                    (stat)
        
stat : ATPROP                                       (AST.ATOM(ATPROP))
     | LPAREN statement RPAREN                      (statement)
     | statement                                    (statement)

