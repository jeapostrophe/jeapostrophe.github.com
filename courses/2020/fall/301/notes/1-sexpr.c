#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

enum SE_Ty { SE_Mt, SE_Num, SE_Sym, SE_Cons };

struct SExpr {
  enum SE_Ty type;
  union {
    struct {
    } mt;
    struct {
      int n; } num;
    struct {
      const char* s; } sym;
    struct {
      struct SExpr *car;
      struct SExpr *cdr; } cons; } data; };

struct SExpr *new_Mt() {
  struct SExpr *se = malloc(sizeof(struct SExpr));
  se->type = SE_Mt;
  return se; }
struct SExpr *new_Num(int n) {
  struct SExpr *se = malloc(sizeof(struct SExpr));
  se->type = SE_Num;
  se->data.num.n = n;
  return se; }
struct SExpr *new_Sym(const char *s) {
  struct SExpr *se = malloc(sizeof(struct SExpr));
  se->type = SE_Sym;
  se->data.sym.s = s;
  return se; }
struct SExpr *new_Cons(struct SExpr *car, struct SExpr *cdr) {
  struct SExpr *se = malloc(sizeof(struct SExpr));
  se->type = SE_Cons;
  se->data.cons.car = car;
  se->data.cons.cdr = cdr;
  return se; }

void pplt( struct SExpr *se );

void pp1( struct SExpr *se ) {
  switch ( se->type ) {
  case SE_Mt:
    printf("()");
    break;
  case SE_Num:
    printf("%d ", se->data.num.n);
    break;
  case SE_Sym:
    printf("%s ", se->data.sym.s);
    break;
  case SE_Cons:
    printf("(");
    pp1(se->data.cons.car);
    pplt(se->data.cons.cdr);
    printf(")");
    break;
  }
}

void pplt( struct SExpr *se ) {
  switch ( se->type ) {
  case SE_Mt:
    break;
  case SE_Num:
    pp1( se ); printf(" ");
    break;
  case SE_Sym:
    pp1( se ); printf(" ");
    break;
  case SE_Cons:
    pp1(se->data.cons.car);
    printf(" ");
    pplt(se->data.cons.cdr);
    break;
  }
}

void ppt( struct SExpr *se ) {
  pp1(se); printf("\n"); }

char *IN = NULL;

struct SExpr *parset(struct SExpr *car);

struct SExpr *parse1() {
  if ( IN[0] == ' ' ) {
    IN++;
    return parse1(); }
  else if ( IN[0] == '(' ) {
    IN++;
    struct SExpr *car = parse1();    
    return parset(car);
  }
  else if ( isdigit(IN[0]) ) {
    int n = 0;
    while ( isdigit(IN[0]) ) {
      n = n*10 + (IN[0] - '0');
      IN++; }      
    return new_Num(n); }
  else if ( isalpha(IN[0]) ) {
    int len = 0;
    while ( isalpha(IN[len]) ) {
      len++; }
    char *s = calloc(len+1, sizeof(char));
    strncpy(s, IN, len);
    s[len] = 0;
    IN = IN+len;
    return new_Sym(s); }
  else {
    fprintf(stderr, "Failed at: %s\n", IN);
    exit(1); } }

struct SExpr *parset(struct SExpr *car) {
  if ( IN[0] == ' ' ) {
    IN++;
    return parset(car); }
  else if ( IN[0] == ')' ) {
    IN++;
    return new_Cons( car, new_Mt() ); }
  else {
    struct SExpr *cadr = parse1();    
    struct SExpr *cdr = parset(cadr);
    return new_Cons( car, cdr ); } }
  
struct SExpr *parse( char* in ) {
  IN = in;
  return parse1(); }

int main() {
  ppt( parse( "(if (lt x 1) (add 2 (mul 3 (add 4 5))) y)" ) );
  return 0; }
