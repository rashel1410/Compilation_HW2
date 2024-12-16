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
%token WHILE
%token BREAK
%token CONTINUE
%token SC
%token COMMA
%token ID
%token NUM
%token NUM_B
%token STRING
%token COMMENT

%right ASSIGN
%left OR
%left AND
%left EQ NE
%left LT GT LE GE
%left ADD SUB
%left DIV MUL
%right NOT
%left LPAREN RPAREN LBRACE RBRACE
%nonassoc ELSE
%%

// While reducing the start variable, set the root of the AST
Program:  Funcs { program = $1; }
;
Funcs : /* epsilon */ {$$ = std::make_shared<ast::Funcs>();}
    | FuncDecl Funcs
    {
        auto funcs_ptr = std::dynamic_pointer_cast<ast::Funcs>($2);
        auto funcdecl_ptr = std::dynamic_pointer_cast<ast::FuncDecl>($1);
        $$ = funcs_ptr;
        funcs_ptr->push_front(funcdecl_ptr);
    }
;
FuncDecl : RetType ID LPAREN Formals RPAREN LBRACE Statements RBRACE
    {
        auto type_ptr = dynamic_pointer_cast<ast::Type>($1);
        auto id_ptr = dynamic_pointer_cast<ast::ID>($2);
        auto formals_ptr = dynamic_pointer_cast<ast::Formals>($4);
        auto statements_ptr = dynamic_pointer_cast<ast::Statements>($7);
        $$ = make_shared<ast::FuncDecl>(id_ptr, type_ptr, formals_ptr, statements_ptr);
    }
;
RetType : Type
            {
                $$ = dynamic_pointer_cast<ast::Type>($1);
            }
    | VOID
    {
        $$ = make_shared<ast::Type>(ast::BuiltInType::VOID);
    }
;
Formals : /* epsilon */
          {
            $$ = make_shared<ast::Formals>();
          }
    | FormalsList
    {
        $$ = dynamic_pointer_cast<ast::Formals>($1);
    }
;
FormalsList : FormalDecl
              {
                auto formal_ptr = dynamic_pointer_cast<ast::Formal>($1);
                $$ = make_shared<ast::Formals>(formal_ptr);
              }
    | FormalDecl COMMA FormalsList
    {
        auto formal_ptr = dynamic_pointer_cast<ast::Formal>($1);
        auto formalList_ptr = dynamic_pointer_cast<ast::Formals>($3);
        $$ = formalList_ptr;
        formalList_ptr->push_front(formal_ptr);
    }
;
FormalDecl : Type ID
             {
                auto type_ptr = dynamic_pointer_cast<ast::Type>($1);
                auto id_ptr = dynamic_pointer_cast<ast::ID>($2);
                $$ = make_shared<ast::Formal>(id_ptr, type_ptr);
             }
;
Statements : Statement
             {
                $$ = make_shared<ast::Statements>(dynamic_pointer_cast<ast::Statement>($1));
             }
    | Statements Statement
    {
        auto list_ptr = dynamic_pointer_cast<ast::Statements>($1);
        auto statement_ptr = dynamic_pointer_cast<ast::Statement>($2);
        $$ = list_ptr;
        list_ptr->push_back(statement_ptr);
    }
;
Statement :
      LBRACE Statements RBRACE
      {
        $$ = dynamic_pointer_cast<ast::Statements>($2);
      }
    | Type ID SC
    {
        auto type_ptr = dynamic_pointer_cast<ast::Type>($1);
        auto id_ptr = dynamic_pointer_cast<ast::ID>($2);
        $$ = make_shared<ast::VarDecl>(id_ptr, type_ptr);
    }
    | Type ID ASSIGN Exp SC
    {
        auto type_ptr = dynamic_pointer_cast<ast::Type>($1);
        auto id_ptr = dynamic_pointer_cast<ast::ID>($2);
        auto exp_ptr = dynamic_pointer_cast<ast::Exp>($4);
        $$ = make_shared<ast::VarDecl>(id_ptr, type_ptr, exp_ptr);
    }
    | ID ASSIGN Exp SC
    {
        auto id_ptr = dynamic_pointer_cast<ast::ID>($1);
        auto exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        $$ = make_shared<ast::Assign>(id_ptr, exp_ptr);
    }
    | Call SC
    {
        $$ = dynamic_pointer_cast<ast::Call>($1);
    }
    | RETURN SC
    {
        $$ = make_shared<ast::Return>();
    }
    | RETURN Exp SC
    {
        auto exp_ptr = dynamic_pointer_cast<ast::Exp>($2);
        $$ = make_shared<ast::Return>(exp_ptr);
    }
    | IF LPAREN Exp RPAREN Statement
    {
        auto exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        auto stmnt_ptr = dynamic_pointer_cast<ast::Statement>($5);
        $$ = make_shared<ast::If>(exp_ptr, stmnt_ptr);
    }
    | IF LPAREN Exp RPAREN Statement ELSE Statement
    {
        auto exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        auto stmnt_then_ptr = dynamic_pointer_cast<ast::Statement>($5);
        auto stmnt_otherwise_ptr = dynamic_pointer_cast<ast::Statement>($7);
        $$ = make_shared<ast::If>(exp_ptr, stmnt_then_ptr, stmnt_otherwise_ptr);
    }
    | WHILE LPAREN Exp RPAREN Statement
    {
        auto exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        auto stmnt_ptr = dynamic_pointer_cast<ast::Statement>($5);
        $$ = make_shared<ast::While>(exp_ptr, stmnt_ptr);
    }
    | BREAK SC
    {
        $$ = make_shared<ast::Break>();
    }
    | CONTINUE SC
    {
        $$ = make_shared<ast::Continue>();
    }
;
Call :
      ID LPAREN ExpList RPAREN
      {
        auto id_ptr = dynamic_pointer_cast<ast::ID>($1);
        auto exp_list_ptr = dynamic_pointer_cast<ast::ExpList>($3);
        $$ = make_shared<ast::Call>(id_ptr, exp_list_ptr);
      }
    | ID LPAREN RPAREN
    {
        auto id_ptr = dynamic_pointer_cast<ast::ID>($1);
        $$ = make_shared<ast::Call>(id_ptr);
    }
;
ExpList :
    Exp
    {
        $$ = make_shared<ast::ExpList>(dynamic_pointer_cast<ast::Exp>($1));
    }
    | Exp COMMA ExpList
    {
        auto exp_ptr = dynamic_pointer_cast<ast::Exp>($1);
        auto exp_list_ptr = dynamic_pointer_cast<ast::ExpList>($3);
        $$ = exp_list_ptr;
        exp_list_ptr->push_front(exp_ptr);
    }
;
Type :
    INT
    {
        $$ = make_shared<ast::Type>(ast::BuiltInType::INT);
    }
    | BYTE
    {
        $$ = make_shared<ast::Type>(ast::BuiltInType::BYTE);
    }
    | BOOL
    {
        $$ = make_shared<ast::Type>(ast::BuiltInType::BOOL);
    }
;
Exp :
    LPAREN Exp RPAREN
    {
        $$ = dynamic_pointer_cast<ast::Exp>($2);
    }
    | Exp ADD Exp
    {
        auto left_exp_ptr = dynamic_pointer_cast<ast::Exp>($1);
        auto right_exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        $$ = make_shared<ast::BinOp>(left_exp_ptr, right_exp_ptr, ast::BinOpType::ADD);
    }
    | Exp SUB Exp
    {
        auto left_exp_ptr = dynamic_pointer_cast<ast::Exp>($1);
        auto right_exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        $$ = make_shared<ast::BinOp>(left_exp_ptr, right_exp_ptr, ast::BinOpType::SUB);
    }
    | Exp MUL Exp
    {
        auto left_exp_ptr = dynamic_pointer_cast<ast::Exp>($1);
        auto right_exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        $$ = make_shared<ast::BinOp>(left_exp_ptr, right_exp_ptr, ast::BinOpType::MUL);
    }
    | Exp DIV Exp
    {
        auto left_exp_ptr = dynamic_pointer_cast<ast::Exp>($1);
        auto right_exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        $$ = make_shared<ast::BinOp>(left_exp_ptr, right_exp_ptr, ast::BinOpType::DIV);
    }
    | ID
    {
        $$ = dynamic_pointer_cast<ast::ID>($1);
    }
    | Call
    {
        $$ = dynamic_pointer_cast<ast::Call>($1);
    }
    | NUM
    {
        $$ = dynamic_pointer_cast<ast::Num>($1);
    }
    | NUM_B
    {
        $$ = dynamic_pointer_cast<ast::NumB>($1);
    }
    | STRING
    {
        $$ = dynamic_pointer_cast<ast::String>($1);
    }
    | TRUE
    {
        $$ = make_shared<ast::Bool>(1);
    }
    | FALSE
    {
        $$ = make_shared<ast::Bool>(0);
    }
    | NOT Exp
    {
        auto exp_ptr = dynamic_pointer_cast<ast::Exp>($2);
        $$ = make_shared<ast::Not>(exp_ptr);
    }
    | Exp AND Exp
    {
        auto left_exp_ptr = dynamic_pointer_cast<ast::Exp>($1);
        auto right_exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        $$ = make_shared<ast::And>(left_exp_ptr, right_exp_ptr);
    }
    | Exp OR Exp
    {
        auto left_exp_ptr = dynamic_pointer_cast<ast::Exp>($1);
        auto right_exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        $$ = make_shared<ast::Or>(left_exp_ptr, right_exp_ptr);
    }
    | Exp EQ Exp
    {
        auto left_exp_ptr = dynamic_pointer_cast<ast::Exp>($1);
        auto right_exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        $$ = make_shared<ast::RelOp>(left_exp_ptr, right_exp_ptr, ast::RelOpType::EQ);
    }
    | Exp NE Exp
    {
        auto left_exp_ptr = dynamic_pointer_cast<ast::Exp>($1);
        auto right_exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        $$ = make_shared<ast::RelOp>(left_exp_ptr, right_exp_ptr, ast::RelOpType::NE);
    }
    | Exp LT Exp
    {
        auto left_exp_ptr = dynamic_pointer_cast<ast::Exp>($1);
        auto right_exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        $$ = make_shared<ast::RelOp>(left_exp_ptr, right_exp_ptr, ast::RelOpType::LT);
    }
    | Exp GT Exp
    {
        auto left_exp_ptr = dynamic_pointer_cast<ast::Exp>($1);
        auto right_exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        $$ = make_shared<ast::RelOp>(left_exp_ptr, right_exp_ptr, ast::RelOpType::GT);
    }
    | Exp LE Exp
    {
        auto left_exp_ptr = dynamic_pointer_cast<ast::Exp>($1);
        auto right_exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        $$ = make_shared<ast::RelOp>(left_exp_ptr, right_exp_ptr, ast::RelOpType::LE);
    }
    | Exp GE Exp
    {
        auto left_exp_ptr = dynamic_pointer_cast<ast::Exp>($1);
        auto right_exp_ptr = dynamic_pointer_cast<ast::Exp>($3);
        $$ = make_shared<ast::RelOp>(left_exp_ptr, right_exp_ptr, ast::RelOpType::GE);
    }
    | LPAREN Type RPAREN Exp
    {
        auto type_ptr = dynamic_pointer_cast<ast::Type>($2);
        auto exp_ptr = dynamic_pointer_cast<ast::Exp>($4);
        $$ = make_shared<ast::Cast>(exp_ptr, type_ptr);
    }
;


%%

// TODO: Place any additional code here
void yyerror(const char* ){
	output::errorSyn(yylineno);
}