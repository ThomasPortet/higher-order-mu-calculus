%{
	%}


	
%token TK_FORMULA
%token TK_MU
%token TK_LAMBDA
%token TK_VARIABLE
%token TK_NONE
%token TK_ANY
%token TK_INTER
%token TK_UNION

%nonassoc TK_LPAR TK_RPAR

%type <mu_calculus_syntax.formula> formula
%type <variance_syntax.variance> variance

%start formula
%%



%%