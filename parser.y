%{

#include "nodes.hpp"
#include "output.hpp"

// bison declarations
extern int yylineno;
extern int yylex();

void yyerror(const char*);

// root of the AST, set by the parser and used by other parts of the compiler
std::shared_ptr<ast::Node> program;

using namespace std;

// TODO: Place any additional declarations here
%}

%token VOID
%token INT
%token BYTE
%token BOOL
%token TRUE
%token FALSE
%token RETURN
%token IF
%right ELSE
%token WHILE
%token BREAK
%token CONTINUE
%token SC
%token COMMA
%right ASSIGN
%token ID
%token NUM
%token NUM_B
%token STRING
%token COMMENT

%left OR
%left AND
%left EQ NE
%left LT GT LE GE
%left ADD SUB
%left DIV MUL
%right NOT
%left LPAREN RPAREN LBRACE RBRACE
%%

// While reducing the start variable, set the root of the AST
Program:  Funcs { program = $1; }
;
Funcs : /* epsilon */ {$$ = std::make_shared<ast::Funcs>();}
    | FuncDecl Funcs
;
FuncDecl : RetType ID LPAREN Formals RPAREN LBRACE Statements RBRACE
;
RetType : Type
    | VOID
;
Formals : /* epsilon */
    | FormalsList
;
FormalsList : FormalDecl
    | FormalDecl COMMA FormalsList
;
FormalDecl : Type ID
;
Statements : Statement
    | Statements Statement
;
Statement : LBRACE Statements RBRACE
    | Type ID SC
    | Type ID ASSIGN Exp SC
    | ID ASSIGN Exp SC
    | Call SC
    | RETURN SC
    | RETURN Exp SC
    | IF LPAREN Exp RPAREN Statement
    | IF LPAREN Exp RPAREN Statement ELSE Statement
    | WHILE LPAREN Exp RPAREN Statement
    | BREAK SC
    | CONTINUE SC
;
Call : ID LPAREN ExpList RPAREN
    | ID LPAREN RPAREN
;
ExpList : Exp
    | Exp COMMA ExpList
;
Type : INT
    | BYTE
    | BOOL
;
Exp : LPAREN Exp RPAREN
    | Exp ADD Exp
    | Exp SUB Exp
    | Exp MUL Exp
    | Exp DIV Exp
    | ID
    | Call
    | NUM
    | NUM_B
    | STRING
    | TRUE
    | FALSE
    | NOT Exp
    | Exp AND Exp
    | Exp OR Exp
    | Exp EQ Exp
    | Exp NE Exp
    | Exp LT Exp
    | Exp GT Exp
    | Exp LE Exp
    | Exp GE Exp
    | LPAREN Type RPAREN Exp
;


%%

// TODO: Place any additional code here