%{
#include <string.h>

#include "yacc.h"
%}

%option noyywrap
%option nounput

%%

"{"            { return LBRACK;     }
"}"            { return RBRACK;     }
","            { return COMMA;      }
"+chdir"       { return CHDIR;      }
"+chroot"      { return CHROOT;     }
"+create-file" { return CREATEFILE; }
"+create-dir"  { return CREATEDIR;  }
"+exec"        { return EXEC;       }
"+lookup"      { return LOOKUP;     }
"+content"     { return CONTENT;    }
"+addlink"     { return ADDLINK;    }
"+makelink"    { return MAKELINK;   }
"+unlink-file" { return UNLINKFILE; }
"+unlink-dir"  { return UNLINKDIR;  }
"+unlink-symlink" { return UNLINKSYM; }
"+unlink"      { return UNLINK;     }
"+read"        { return READ;       }
"+add-symlink"  { return ADDSYMLINK; }
"+read-symlink" { return READSYMLINK;}
"+write"       { return WRITE;      }
"+append"      { return APPEND;     }
"+chmod"       { return CHMOD;      }
"+chown"       { return CHOWN;      }
"+chflags"     { return CHFLAGS;    }
"+chtimes"     { return CHTIMES;    }
"+stat"        { return STAT;       }
"&stdout"      { return STDOUT;     }
"&stdin"       { return STDIN;      }
"&stderr"      { return STDERR;     }
"&pipefactory" { return PIPEFACTORY;}
^#.*           /* ignore comments */
^[^{\n].*      { yylval.string=strdup(yytext); return ID; }
[\ \t\n]       /* ignore whitespace */

%%
