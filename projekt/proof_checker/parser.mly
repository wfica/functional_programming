%{
open Proof_type
%}

%token <string> TASK_NAME
%token <string> VARIABLE
%token GOAL
%token BEGIN_PROOF
%token END_PROOF
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

%left IFF  /* lowest precedence  */
%right IMPL
%left OR
%left AND
%left NOT


%start <Proof_type.task list> prog

%%
prog:
  tl = list(task) EOF { tl };

task:
  GOAL; task_name = TASK_NAME ; COLON;  goal = formula;
  BEGIN_PROOF;
  pil = proof;
  END_PROOF                                               { Task( task_name, goal, pil) };
  
formula:
  | LEFT_BRACK; f = formula; RIGHT_BRACK  { f }
  | TRUE                                  { Const(true)  }
  | FALSE                                 { Const(false) }
  | v = VARIABLE                          { Variable(v)  }
  | NOT; f = formula                      { Not(f)       }
  | f1 = formula;  AND; f2 = formula      { And(f1,f2)   }
  | f1 = formula;  OR; f2 = formula       { Or(f1,f2)    }
  | f1 = formula;  IMPL; f2 = formula     { Impl(f1,f2)  }
  | f1 = formula;  IFF; f2 = formula      { Iff(f1,f2)   }

proof_item:
  | LEFT_SQUARE_BRACK; f = formula; COLON ;
    pil = proof;
    RIGHT_SQUARE_BRACK                      { Hypothesis(f, pil) }
  | f = formula                             { Formula(f) }

proof:
  | l = separated_list(SEMI_COLON, proof_item) { Proof(l) }