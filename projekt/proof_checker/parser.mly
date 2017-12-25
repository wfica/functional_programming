%{
open Proof_type
%}

%token <string> NAME
%token <string> VARIABLE
%token AXIOMS
%token GOAL
%token BEGIN_PROOF
%token END
%token TRUE
%token FALSE
%token AND
%token OR
%token NOT
%token IMPL
%token IFF
%token COLON
%token SEMI_COLON
%token LEFT_BRACK
%token RIGHT_BRACK
%token LEFT_SQUARE_BRACK
%token RIGHT_SQUARE_BRACK
%token EOF

%token <string> ALL
%token <string> EXISTS
%token COMMA

%left IFF  /* lowest precedence  */
%right IMPL
%left OR
%left AND
%nonassoc NOT ALL EXISTS


%start <(Proof_type.formula list) * (Proof_type.task list)> prog

%%
prog:
  | tl = list(task) EOF { ([],tl) };
  | AXIOMS; COLON; ax =list(formula) ;  tl = list(task) EOF { (ax , tl) };


task:
  GOAL; task_name = NAME ; COLON;  goal = formula;
  BEGIN_PROOF;
  pil = proof;
  END                                               { Task( task_name, goal, pil) };
  
term: 
  | v = VARIABLE                                                           { Var(v) }
  | func = NAME; LEFT_BRACK; l = separated_list(COMMA, term); RIGHT_BRACK  { Fun(func, l) }

formula:
  | LEFT_BRACK; f = formula; RIGHT_BRACK  { f }
  | TRUE                                  { Const(true)  }
  | FALSE                                 { Const(false) }
  | pred = NAME; LEFT_BRACK; l = separated_list(COMMA, term); RIGHT_BRACK 
                                          { Pred(pred, l)}
  | NOT; f = formula                      { Not(f)       }
  | f1 = formula;  AND; f2 = formula      { And(f1,f2)   }
  | f1 = formula;  OR; f2 = formula       { Or(f1,f2)    }
  | f1 = formula;  IMPL; f2 = formula     { Impl(f1,f2)  }
  | f1 = formula;  IFF; f2 = formula      { Iff(f1,f2)   }
  | x = ALL; f = formula                  { All(x, f)    }
  | x = EXISTS; f = formula               { Exists(x, f) }

proof_item:
  | LEFT_SQUARE_BRACK; f = formula; COLON ;
    pil = proof;
    RIGHT_SQUARE_BRACK                      { Hypothesis(f, pil) }
  | f = formula                             { Formula(f) }

proof:
  | l = separated_list(SEMI_COLON, proof_item) { Proof(l) }