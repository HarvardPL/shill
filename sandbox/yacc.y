%{
  #include <fcntl.h>
  #include <stdio.h>
  #include <stddef.h>
  #include <unistd.h>
  #include "sandbox.h"

  #define YYDEBUG 1

  extern int yydebug;
  extern FILE *yyin;
  int yylex(void);
  int yyerror(const char *s) {
    fprintf(stderr, "%s\n", s);
    exit(-1);
  }
  int yyparse(void);

  struct shill_cap *current = NULL;
%}

%union {
  struct shill_cap *cap;
  char *string;
}

%token LBRACK RBRACK COMMA
%token CHDIR CHROOT CREATEFILE CREATEDIR EXEC
%token LOOKUP CONTENT ADDLINK MAKELINK ADDSYMLINK
%token UNLINKFILE UNLINKDIR UNLINKSYM UNLINK READ WRITE APPEND READSYMLINK
%token CHMOD CHOWN CHFLAGS CHTIMES STAT
%token ID STDIN STDOUT STDERR PIPEFACTORY

%type <cap> cap rights right
%type <string> file ID

%%

grants : grant grants
| ;

grant : cap files { current = NULL; } ;

files : file files {
  printf("Opening %s\n", $1);
  int file = open($1, O_RDONLY);
  if (file == -1)
    yyerror("open failed");
  printf("Attempting to grant rights\n");
  if (0 != shill_grant(file, current)) {
    perror("fail");
    yyerror("shill_grant failed");
  }
}
| STDIN files {
  printf("Attempting to grant rights on stdin\n");
  if (0 != shill_grant(STDIN_FILENO, current)) {
    perror("fail");
    yyerror("shill_grant failed");
  }  
 }
| STDOUT files {
  printf("Attempting to grant rights on stdout\n");
  if (0 != shill_grant(STDOUT_FILENO, current)) {
    perror("fail");
    yyerror("shill_grant failed");
  }  
 }
| STDERR files {
  printf("Attempting to grant rights on stderr\n");
  if (0 != shill_grant(STDERR_FILENO, current)) {
    perror("fail");
    yyerror("shill_grant failed");
  }  
 }
| PIPEFACTORY files {
  printf("Attempting to grant rights for creating pipes\n");
  if (0 != shill_grant_pipefactory(current)) {
    perror("fail");
    yyerror("shill_pipefactory_grant failed");
  }
 }
| /* empty */ ;

file : ID {
  $$ = $1;
 };

cap : LBRACK rights RBRACK {
  $$ = $2;
  current = $$;
}
| LBRACK RBRACK {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = 0;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;
  if (current == NULL) current = $$;
 };

rights : right COMMA rights {
  if (($1->sc_flags & $3->sc_flags) != 0)
    yyerror("duplicate right");
  $1->sc_flags |= $3->sc_flags;
  if ($1->sc_lookup == NULL)
    $1->sc_lookup = $3->sc_lookup;
  if ($1->sc_createfile == NULL)
    $1->sc_createfile = $3->sc_createfile;
  if ($1->sc_createdir == NULL)
    $1->sc_createdir = $3->sc_createdir;
  free($3);
  $$ = $1;
  }
| right { $$ = $1; }
;

right : LOOKUP cap {
  struct shill_cap *lcap = malloc(sizeof(struct shill_cap));
  if (lcap == NULL)
    yyerror("malloc failed");
  lcap->sc_flags = C_LOOKUP;
  lcap->sc_lookup = $2;
  lcap->sc_createfile = NULL;
  lcap->sc_createdir = NULL;
  $$ = lcap;
  }
| LOOKUP {
  struct shill_cap *lcap = malloc(sizeof(struct shill_cap));
  if (lcap == NULL)
    yyerror("malloc failed");
  memset(lcap, 0, sizeof(struct shill_cap));
  lcap->sc_flags = C_LOOKUP;
  lcap->sc_lookup = NULL;
  lcap->sc_createfile = NULL;
  lcap->sc_createdir = NULL;
  $$ = lcap;  
  }
| CREATEFILE cap {
  struct shill_cap *ccap = malloc(sizeof(struct shill_cap));
  if (ccap == NULL)
    yyerror("malloc failed");
  ccap->sc_flags = C_CREATEFILE;
  ccap->sc_lookup = NULL;
  ccap->sc_createfile = $2;
  ccap->sc_createdir = NULL;
  $$ = ccap;
  }
| CREATEFILE {
  struct shill_cap *ccap = malloc(sizeof(struct shill_cap));
  if (ccap == NULL)
    yyerror("malloc failed");
  memset(ccap, 0, sizeof(struct shill_cap));
  ccap->sc_flags = C_CREATEFILE;
  ccap->sc_lookup = NULL;
  ccap->sc_createfile = NULL;
  ccap->sc_createdir = NULL;
  $$ = ccap;  
  }
| CREATEDIR cap {
  struct shill_cap *ccap = malloc(sizeof(struct shill_cap));
  if (ccap == NULL)
    yyerror("malloc failed");
  ccap->sc_flags = C_CREATEDIR;
  ccap->sc_lookup = NULL;
  ccap->sc_createfile = NULL;
  ccap->sc_createdir = $2;
  $$ = ccap;
  }
| CREATEDIR {
  struct shill_cap *ccap = malloc(sizeof(struct shill_cap));
  if (ccap == NULL)
    yyerror("malloc failed");
  memset(ccap, 0, sizeof(struct shill_cap));
  ccap->sc_flags = C_CREATEDIR;
  ccap->sc_lookup = NULL;
  ccap->sc_createfile = NULL;
  ccap->sc_createdir = NULL;
  $$ = ccap;  
  }
| CHDIR {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_CHDIR;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| CHROOT {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_CHROOT;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| EXEC {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_EXEC;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| CONTENT {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_CONTENT;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| ADDLINK {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_ADDLNK;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| MAKELINK {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_MAKELNK;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| UNLINKFILE {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_UNLINKFILE;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| UNLINKDIR {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_UNLINKDIR;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| UNLINKSYM {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_UNLINKSYM;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;
  }
| UNLINK {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_UNLINK;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| READ {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  memset(cap, 0, sizeof(struct shill_cap));
  cap->sc_flags = C_READ;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| ADDSYMLINK {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_ADDSYMLNK;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| READSYMLINK {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  memset(cap, 0, sizeof(struct shill_cap));
  cap->sc_flags = C_READLINK;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;
  }
| WRITE {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_WRITE;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| APPEND {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_APPEND;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| CHMOD {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_CHMODE;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| CHOWN {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_CHOWN;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap; 
  }
| CHFLAGS {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_CHFLAGS;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| CHTIMES {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_CHTIMES;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
| STAT {
  struct shill_cap *cap = malloc(sizeof(struct shill_cap));
  if (cap == NULL)
    yyerror("malloc failed");
  cap->sc_flags = C_STAT;
  cap->sc_lookup = NULL;
  cap->sc_createfile = NULL;
  cap->sc_createdir = NULL;
  $$ = cap;  
  }
;

%%

int main(int argc, char *argv[]) {
  int debug = 0;
  ++argv; --argc; /* skip over program name */

  if (argc == 0) {
    fprintf(stderr, "Usage: sandbox <policy> <command>\n");
    exit(-1);
  }

  if (strcmp("-d",argv[0]) == 0) {
    debug = 1;
    ++argv; --argc;
  }

  FILE *policy = fopen(argv[0], "r");
  if (policy == NULL) {
    fprintf(stderr, "Failed to open policy file: %s\n", argv[0]);
    exit(-1);
  }
  yyin = policy;

  ++argv; --argc;

  int ret;
  printf("Initializing sandbox...\n");
  if (0 != (ret = shill_init()))
    return ret;

  printf("Interpreting policy...\n");
  if (0 != (ret = yyparse()))
    return ret;

  if (debug) {
    printf("Entering debug sandbox...\n");
    if (0 != (ret = shill_debug()))
      return ret;
  } else {
    printf("Entering sandbox...\n");
    if (0 != (ret = shill_enter()))
      return ret;
  }

  return execvp(argv[0], argv);
}
