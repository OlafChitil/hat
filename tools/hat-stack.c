#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include "art.h"
#include "artutils.h"

#define MAX_DEPTH 64
#define MAX_EXP_DEPTH 6

int
main (int argc, char** argv)
{
  char *expr; Ident* msg; SrcRef* sr;
  int i=0, infix;
  FileOffset parent;

  initialise(argc,argv);
  if (errorMsg) {
    if (errorMsg==Entered) {
      fprintf(stdout,"Program was interrupted with ^C\n");
    } else {
      msg = readAtomAt(errorMsg);
      fprintf(stdout,"Program terminated with error:\n  %s\n",msg->idname);
    }
    parent = errorRoot;
    fprintf(stdout,"%-56s  %13s/line/col\n","Virtual stack trace:","source file");
    while (parent) {
      parent = readTraceAt(parent,&expr,&sr,&infix,True,MAX_EXP_DEPTH);
      if (sr)
        fprintf(stdout,"  %-54s  %13s %3d %3d\n"
                      ,expr,sr->srcname,sr->line,sr->column);
      else
        fprintf(stdout,"  %-54s  %21s\n",expr,"no source reference");
      if (i++ > MAX_DEPTH) parent=0;
    }
  } else {
    fprintf(stdout,"No runtime error in program %s\n",argv[1]);
  }
  finalise();
}
