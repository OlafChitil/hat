#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include <signal.h>
#include <assert.h>
#include "art.h"
#include "artutils.h"
#include "pathutils.h"
#include "ntohl.h"

#define DEBUG 0

#if DEBUG
#define HIDE(x)	x
#else
#define HIDE(x)
#endif

#define MAX_STRING 0xff
#define MAX_BIG_STRING 0xffff

/* We open the .hat file *twice* with different handles, for efficiency.
 *
 * For random access, use the global HatFileRandom handle.  Most operations
 * assume this mode of access.
 *
 * For sequential access, use the global HatFileSeq handle: the
 * operation variants prefixed with q_ keep a global record (q_position)
 * of the current file position, which is much cheaper than doing ftell().
 */
FileOffset errorRoot, errorMsg, remoteStartNode=0;
FileOffset q_position;
FILE	*HatFileRandom, *HatFileSeq, *OutputFile, *BridgeFile;
unsigned filesize=0, outputsize=0;
char	*progname;		/* name of browser, not of the subject */
Bool	interrupt=False;	/* ^C interrupt at program runtime */
Bool	hat_interrupted=False;	/* ^C interrupt in browsing tool */

FileOffset
currentfilepos (void) { return q_position; }


void
finalise (void)
{
  fclose(HatFileRandom);
  fclose(HatFileSeq);
  fclose(OutputFile);
  fclose(BridgeFile);
}



/* Open a file for reading, given:
 *    the base name of the file
 *    the file extension
 */
FILE*
openFile (char* base, char* ext)
{
  char filename[MAX_STRING];
  FILE* file;
  strcpy(filename,base);
  strcat(filename,ext);
  if (file = fopen(filename,"rb")) {
    return file;
  } else {
    fprintf(stderr,"%s: cannot open %s\n",progname,filename);
    exit(1);
  }
}

/* Determine the size of a file, given:
 *    the base name of the file
 *    the file extension
 */
int
sizeFile (char* base, char* ext)
{
  char filename[MAX_STRING];
  struct stat buf;
  strcpy(filename,base);
  strcat(filename,ext);
  stat(filename,&buf);
  return buf.st_size;
}


/* freadAt() is just like fread(), except it seeks to a specific
 * file location first.  (Random Access)
 */
int
freadAt (FileOffset fo, void* ptr, int size, int nmemb, FILE* stream)
{
  int err;
  if (fo < 0x10) {
    fprintf(stderr,"%s: attempt to read inside .hat header\n",progname);
    fprintf(stderr,"%s: offset = 0x%x\n",progname,fo);
    exit(1);
  } else if (fo > filesize) {
    fprintf(stderr,"%s: attempt to read beyond end of file\n",progname);
    fprintf(stderr,"%s: offset = 0x%x, filesize = 0x%x\n",progname,fo,filesize);
    fprintf(stderr,"%s: errno = %d (%s)\n",progname,errno,strerror(errno));
    exit(1);
  }
  if (fseek(stream, fo, SEEK_SET)) {
    fprintf(stderr,"%s: seek error on file\n",progname);
    fprintf(stderr,"%s: errno = %d (%s)\n",progname,errno,strerror(errno));
    exit(1);
  }
  err = fread(ptr,size,nmemb,stream);
  return err;
}

/* q_fread() is just like fread() except it advances the global counter
 * recording the file position.  (Sequential Access)
 */
int
q_fread(void* buf, int siz, int num, FILE* file)
{
  int err;
  err = fread(buf,siz,num,file);
  q_position += err*siz;
//if (err<num) {
//  fprintf(stderr,"q_fread: warning, only read %d of %d values (size %d)\n"
//                ,err,num,siz);
//}
  return err;
}



/* readFO() reads a single FileOffset from the file and ensures it is
 * in host-endian order.  (Random Access)
 */
FileOffset
readFO (void)
{
  FileOffset fo;
  fread(&fo,sizeof(FileOffset),1,HatFileRandom);
  HIDE(fprintf(stderr,"readFO -> 0x%x\n",ntohl(fo));)
  return ntohl(fo);
}
/* (Sequential Access) */
FileOffset
q_readFO (void)
{
  FileOffset fo;
  q_fread(&fo,sizeof(FileOffset),1,HatFileSeq);
  HIDE(fprintf(stderr,"q_readFO -> 0x%x\n",ntohl(fo));)
  return ntohl(fo);
}



/* readString() reads a length-annotated string from the current position
 * in the file.  (Random Access)
 */
char*
readString (void)
{
  char *buf;
  int i, n;

  n = (int)fgetc(HatFileRandom);
  if (n==MAX_STRING) {
    n = (int)fgetc(HatFileRandom);
    n = (n<<8) + (int)fgetc(HatFileRandom);
  }
  buf = (char*)malloc((n+1)*sizeof(char));
  i = fread(buf,sizeof(char),n,HatFileRandom);
  buf[n] = '\0';
  if (i<n) {
    fprintf(stderr,"%s: warning, only read %d characters of %d in string"
                  ,progname,i,n);
  }
  HIDE(fprintf(stderr,"readString -> %s\n",buf);)
  return buf;
}
/* (Sequential Access) */
char*
q_readString (void)
{
  char *buf;
  int i, n;

  n = (int)fgetc(HatFileSeq);
  q_position += 1;
  if (n==MAX_STRING) {
    n = (int)fgetc(HatFileSeq);
    n = (n<<8) + (int)fgetc(HatFileSeq);
    q_position += 2;
  }
  buf = (char*)malloc((n+1)*sizeof(char));
  i = q_fread(buf,sizeof(char),n,HatFileSeq);
  buf[n] = '\0';
  if (i<n) {
    fprintf(stderr,"%s: warning, only read %d characters of %d in string"
                  ,progname,i,n);
  }
  HIDE(fprintf(stderr,"q_readString -> %s\n",buf);)
  return buf;
}


/* q_peek() takes a sneaky look at the next byte of the file, to
 * determine whether we want to go ahead and read it now.
 */
char
q_peek (void)
{
  char c;
  c = (char)fgetc(HatFileSeq);
  ungetc(c,HatFileSeq);
  return c;
}




/* readModuleAt() fills in the name of the module and its source file,
 * given the location of the module descriptor in the file.
 */
void
readModuleAt (FileOffset fo, char** modname, char** srcname, Bool* traced)
{
  char c;

  HIDE(fprintf(stderr,"readModuleAt 0x%x\n",fo);)
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  if (lower5(c)!=Module) {
    fprintf(stderr,"%s: expected a Module descriptor at position 0x%x\n"
                  ,progname,fo);
    exit(1);
  }
  *traced  = tracedModule(c);
  *modname = readString();
  *srcname = readString();
  HIDE(fprintf(stderr,"readModuleAt 0x%x -> %s %s\n",fo,*modname,*srcname);)
}


/* readAtomAt() fills in the name of the variable or constructor,
 * as well as its module, source file, fixity, and definition position,
 * given the location of the Atom descriptor in the file.
 */
Ident*
readAtomAt (FileOffset fo)
{
  char c;
  FileOffset modpos;
  Ident* id=0;
  int defnpos;
  int defnposend;

  if (fo==Lambda) {
    id = (Ident*)malloc(sizeof(Ident));
    id->idname   = "(\\..)";
    id->modname  = id->srcname = "";
    id->fixity   = noFixity;
    id->defnline = id->defncolumn = 0;
    id->defnlineend = id->defncolumnend = 0;
    id->isTraced = False;
    id->atomtype = Abstract;
  } else if (fo==DoLambda) {
    id = (Ident*)malloc(sizeof(Ident));
    id->idname   = "do";
    id->modname  = id->srcname = "";
    id->fixity   = noFixity;
    id->defnline = id->defncolumn = 0;
    id->defnlineend = id->defncolumnend = 0;
    id->isTraced = False;
    id->atomtype = Abstract;
  } else {
    HIDE(fprintf(stderr,"readAtomAt 0x%x\n",fo);)
    freadAt(fo,&c,sizeof(char),1,HatFileRandom);
    switch (lower5(c)) {
      case AtomVariable:
      case AtomConstructor:
          id = (Ident*)malloc(sizeof(Ident));
          modpos = readFO();
          fread(&defnpos,sizeof(int),1,HatFileRandom);
          fread(&defnposend,sizeof(int),1,HatFileRandom);
          fread(&(id->fixity),sizeof(char),1,HatFileRandom);
          fread(&(id->arity),sizeof(char),1,HatFileRandom);
          id->idname       = readString();
          id->defnline     = ntohl(defnpos)/10000;
          id->defncolumn   = ntohl(defnpos)%10000;
          id->defnlineend  = ntohl(defnposend)/10000;
          id->defncolumnend= ntohl(defnposend)%10000;
          if (lower5(c)==AtomVariable) {
              id->atomtype = Variable;
          } else if (hasFields(c)) {
              id->atomtype = ConstrFields;
          } else {
              id->atomtype = Constructor;
          }
          readModuleAt(modpos,&(id->modname),&(id->srcname),&(id->isTraced));
          break;
      case AtomAbstract: {
          char *tmp;
          id = (Ident*)malloc(sizeof(Ident));
          id->idname   = readString();
          id->modname  = id->srcname = "";
          id->fixity   = noFixity;
          id->defnline = id->defncolumn = id->defnlineend = 
            id->defncolumnend = 0;
          id->isTraced = False;
          id->atomtype = Abstract;
          tmp = (char*)malloc(strlen(id->idname)+3);
          strcpy(tmp,"{"); strcat(tmp,id->idname); strcat(tmp,"}");
          free(id->idname); id->idname=tmp;
          } break;
      default:
          fprintf(stderr,"%s: expected an Atom descriptor at position 0x%x\n"
                        ,progname,fo);
          exit(1);
          break;
    }
  }
  HIDE(fprintf(stderr,"readAtomAt 0x%x -> %s %s %s %d %d %d %d %d\n",fo,id->idname,id->modname,id->srcname,id->defnline,id->defncolumn,id->defnlineend,id->defncolumnend,id->fixity);)
  return id;
}



/* readSRAt() fills in a struct containing the filename and usage
 * position of a source reference, given the location of the
 * SrcPos descriptor in the file.
 */
SrcRef *
readSRAt (FileOffset fo)
{
  FileOffset modpos;
  char *modname, *srcname;
  int usepos;
  int useposend;
  char c;
  Bool dummy;
  SrcRef *sr;

  HIDE(fprintf(stderr,"readSRAt 0x%x\n",fo);)
  if (fo) {
    freadAt(fo,&c,sizeof(char),1,HatFileRandom);
    if (lower5(c)!=SrcPos) {
      fprintf(stderr,"%s: expected a SrcPos descriptor at position 0x%x\n"
                    ,progname,fo);
      fprintf(stderr,"%s: got a 0x%x\n",progname,c);
      exit(1);
    }
    sr = (SrcRef*)malloc(sizeof(SrcRef));
    modpos = readFO();
    fread(&usepos,sizeof(int),1,HatFileRandom);
    usepos = ntohl(usepos);
    fread(&useposend,sizeof(int),1,HatFileRandom);
    useposend = ntohl(useposend);
    readModuleAt(modpos, &modname, &(sr->srcname), &dummy);
    sr->line    = usepos/10000;
    sr->column  = usepos%10000;
    sr->lineend = useposend/10000;
    sr->columnend = useposend%10000;
    return sr;
  } else {
    return (SrcRef*)0;
  }
}

char*
srFile (SrcRef* sr)      { return sr->srcname; }
int
srLine (SrcRef* sr)      { return sr->line; }
int
srColumn (SrcRef* sr)    { return sr->column; }
int
srLineEnd (SrcRef* sr)   { return sr->lineend; }
int
srColumnEnd (SrcRef* sr) { return sr->columnend; }


/* readValueAt() returns a struct containing a readable notation of the
 * value-Exp stored at the given location in the file.
 */
Ident*
readValueAt (FileOffset fo)
{
  char c, buf[MAX_STRING];  
  Ident *id = (Ident*)malloc(sizeof(Ident));
  FileOffset parent, usepos = 0;

  /* defaults */
  id->idname   = (char*)0;
  id->modname  = (char*)0;
  id->srcname  = (char*)0;
  id->fixity   = noFixity;
  id->arity    = 0;
  id->defnline      = 0;
  id->defncolumn    = 0;
  id->defnlineend   = 0;
  id->defncolumnend = 0;
  id->isTraced   = False;
  id->atomtype   = Literal;

  HIDE(fprintf(stderr,"readValueAt 0x%x\n",fo);)
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  if ((lower5(c)<ExpChar) || (lower5(c)>ExpConstDef)) {
    fprintf(stderr,"%s: expected a value Exp descriptor at position 0x%x\n"
                  ,progname,fo);
    exit(1);
  }
  HIDE(fprintf(stderr,"readValueAt 0x%x -> tag 0x%x\n",fo,c);)
  if (hasSrcPos(c)) { usepos = readFO(); }
  parent = readFO();
  switch (lower5(c)) {
    case ExpChar:
		{ fread(&c,sizeof(char),1,HatFileRandom);
		  if ((c>31) && (c!='\''))
		    sprintf(buf,"'%c'",c);
		  else switch(c) {
		    case '\n': sprintf(buf,"'\\n'"); break;
		    case '\t': sprintf(buf,"'\\t'"); break;
		    case '\255' : sprintf(buf,"'\\e'"); break;
		    default  : sprintf(buf,"'\\0%X'",c); break;
		  }
		  id->idname = strdup(buf);
		} break;
    case ExpInt:
		{ int i;
		  fread(&i,sizeof(int),1,HatFileRandom);
		  sprintf(buf,"%d",ntohl(i));
		  id->idname = strdup(buf);
		} break;
    case ExpInteger:
		{ id->idname = readString();
		} break;
    case ExpRat:
		{ int i,j;
		  fread(&i,sizeof(int),1,HatFileRandom);
		  fread(&j,sizeof(int),1,HatFileRandom);
		  sprintf(buf,"%d%%%d",ntohl(i),ntohl(j));
		  id->idname = strdup(buf);
		} break;
    case ExpRational:
		{ sprintf(buf,"%s%%%s",readString(), readString());
		  id->idname = strdup(buf);
		} break;
    case ExpFloat:
		{ float f;
		  fread(&f,sizeof(float),1,HatFileRandom);
		  sprintf(buf,"%.6f",f);
		  id->idname = strdup(buf);
		} break;
    case ExpDouble:
		{ double d;
		  fread(&d,sizeof(double),1,HatFileRandom);
		  sprintf(buf,"%.15f",d);
		  id->idname = strdup(buf);
		} break;
    case ExpValueApp:
    case ExpValueUse:
		{ FileOffset atom;
                  free(id);
                  atom = readFO();
		  id = readAtomAt(atom);
		} break;
    case ExpConstUse:
		{ FileOffset def;
                  free(id);
                  def = readFO();
		  id = readValueAt(def);
		} break;
    case ExpConstDef:
		{ FileOffset atom;
                  free(id);
                  readFO();	/* skip result (? should we follow it ?) */
                  atom = readFO();
		  id = readAtomAt(atom);
		} break;
    default: break;
  }
  HIDE(fprintf(stderr,"readValueAt 0x%x -> %s %s %s %d %d %d\n",fo,id->idname,id->modname,id->srcname,id->defnline,id->defncolumn,id->fixity);)
  if (!id->idname) id->idname = strdup("Problem");
  return id;
}

/* Skip a complete node in the file, given that the tag byte has already
 * been read. (Sequential Access)  Returns the parent pointer of the node,
 * if it has one, although in many cases this is ignored.
 */
FileOffset
q_skipNode (char tag)
{
  FileOffset parent=0;
  switch (lower5(tag)) {
    case Module:
        q_readString(); q_readString();
        break;
    case SrcPos:
        q_readFO();
        { int x; q_fread(&x,sizeof(int),1,HatFileSeq); }
        { int x; q_fread(&x,sizeof(int),1,HatFileSeq); }
        break;
    case ExpApp:
        if (hasSrcPos(tag)) q_readFO();
        parent = q_readFO(); q_readFO(); q_readFO();
        { char arity; q_fread(&arity,sizeof(char),1,HatFileSeq);
          while (arity--) { q_readFO(); } }
        break;
    case ExpValueApp:
        if (hasSrcPos(tag)) q_readFO();
        parent = q_readFO(); q_readFO();
        { char arity; q_fread(&arity,sizeof(char),1,HatFileSeq);
          while (arity--) { q_readFO(); } }
        break;
    case ExpChar:
        if (hasSrcPos(tag)) q_readFO();
        parent = q_readFO();
        { char x; q_fread(&x,sizeof(char),1,HatFileSeq); }
        break;
    case ExpInt:
        if (hasSrcPos(tag)) q_readFO();
        parent = q_readFO();
        { int x; q_fread(&x,sizeof(int),1,HatFileSeq); }
        break;
    case ExpInteger:
        if (hasSrcPos(tag)) q_readFO();
        parent = q_readFO();
        q_readString();
        break;
    case ExpRat:
        if (hasSrcPos(tag)) q_readFO();
        parent = q_readFO();
        { int x[2]; q_fread(&x[0],sizeof(int),2,HatFileSeq); }
        break;
    case ExpRational:
        if (hasSrcPos(tag)) q_readFO();
        parent = q_readFO();
        q_readString(); q_readString();
        break;
    case ExpFloat:
        if (hasSrcPos(tag)) q_readFO();
        parent = q_readFO();
        { float x; q_fread(&x,sizeof(float),1,HatFileSeq); }
        break;
    case ExpDouble:
        if (hasSrcPos(tag)) q_readFO();
        parent = q_readFO();
        { double x; q_fread(&x,sizeof(double),1,HatFileSeq); }
        break;
    case ExpValueUse:
    case ExpConstUse:
    case ExpProjection:
        if (hasSrcPos(tag)) q_readFO();
        parent = q_readFO(); q_readFO();
        break;
    case ExpConstDef:
        parent = q_readFO(); q_readFO(); q_readFO();
        break;
    case ExpGuard:
    case ExpCase:
    case ExpIf:
        if (hasSrcPos(tag)) q_readFO();
        parent = q_readFO(); q_readFO(); q_readFO();
        break;
    case ExpFieldUpdate:
        if (hasSrcPos(tag)) q_readFO();
        parent = q_readFO(); q_readFO(); q_readFO();
        { char arity; q_fread(&arity,sizeof(char),1,HatFileSeq);
          arity = 2*arity;
          while (arity--) { q_readFO(); } }
        break;
    case ExpHidden:
        parent = q_readFO(); q_readFO(); q_readFO();
        break;
    case ExpForward:
        parent = q_readFO();	/* note different to value of parentNode() */
        break;
    case ExpDoStmt:
        if (hasSrcPos(tag)) q_readFO();
        parent = q_readFO();	/* note different to value of parentNode() */
        break;
    case AtomVariable:
        q_readFO();
        { int x; q_fread(&x,sizeof(int),1,HatFileSeq); }
        { int x; q_fread(&x,sizeof(int),1,HatFileSeq); }
        { char x; q_fread(&x,sizeof(char),1,HatFileSeq); }
        { char x; q_fread(&x,sizeof(char),1,HatFileSeq); }
        q_readString();
        break;
    case AtomConstructor:
        q_readFO();
        { int x; q_fread(&x,sizeof(int),1,HatFileSeq); }
        { int x; q_fread(&x,sizeof(int),1,HatFileSeq); }
        { char x; q_fread(&x,sizeof(char),1,HatFileSeq); }
        { char arity; q_fread(&arity,sizeof(char),1,HatFileSeq);
          q_readString();
          if (hasFields(tag)) while (arity--) q_readFO(); }
        break;
    case AtomAbstract:
        q_readString();
        break;
    case ListCons:
        q_readFO();
        q_readFO();
    default: break;
  }
  return parent;
}


/* readTraceAt() fills in a string containing a readable notation of the
 * Trace stored at the given location in the file.  It returns the
 * parent trace.  This routine is only currently used by the "virtual
 * stack trace" program.
 */
FileOffset
readTraceAt (FileOffset fo, char** expr, SrcRef** sr, int* infix
            ,int followHidden, int depth)
{
  char c, buf[10000];  /* fixed size no final solution */
  FileOffset parent;

  *infix = (int)noFixity;	/* default */

  if (depth <= 0) {
    *expr = strdup("[7m [0m");
    return fo;
  }

  if (fo<=DoLambda) {
    switch (fo) {
      case Root:        sprintf(buf,"<Root>"); break;
      case Unevaluated: sprintf(buf,"_"); break;
      case Entered:     sprintf(buf,"_|_"); break;
      case Interrupted: sprintf(buf,"{^C}"); break;
      case Lambda:      sprintf(buf,"(\\..)"); break;
      case DoLambda:    sprintf(buf,"do"); break;
    }
    *expr = strdup(buf);
    *infix = noFixity;
    *sr = 0;
    return fo;
  } else {
    HIDE(fprintf(stderr,"readTraceAt 0x%x\n",fo);)
    freadAt(fo,&c,sizeof(char),1,HatFileRandom);
    if ((lower5(c)<ExpApp) || (lower5(c)>ExpDoStmt)) {
      fprintf(stderr,"%s: expected Exp descriptor at position 0x%x (got 0x%x)\n"
                    ,progname,fo,c);
      exit(1);
    }
    HIDE(fprintf(stderr,"readTraceAt 0x%x -> tag 0x%x\n",fo,c);)
    switch (lower5(c)) {
      case ExpApp:
	{ unsigned char i, arity;
	  FileOffset foExprs[20], foSR=0;
	  char* exprs[20];
	  int  fixexp[20];
          if (hasSrcPos(c)) { foSR = readFO(); }
          parent = readFO();			/* get parent */
          HIDE(fprintf(stderr,"enter parent of 0x%x -> 0x%x\n",fo,parent);)
          readFO();				/* skip result */
	  foExprs[0] = readFO();		/* get fun */
	  fread(&arity,sizeof(unsigned char),1,HatFileRandom);
	  for (i=1; i<=arity; i++) {
	    foExprs[i] = readFO();
          }
	  for (i=0; i<=arity; i++) {
	    (void)readTraceAt(getResult(foExprs[i],True)
                             ,&(exprs[i]),sr,&(fixexp[i]),False,depth-1);
          }
	  *infix = fixexp[0];
	  if (isInfix(fixexp[0]) && c >= 2) {
	    sprintf(buf,"%s",infixPrint(exprs[1],fixexp[1],exprs[0],fixexp[0]
			    ,exprs[2],fixexp[2]));
	    for (i=3; i<=arity; i++) {
	      strcat(buf," ");
	      strcat(buf,exprs[i]);
            }
	  } else {	/* no fixity */
	    sprintf(buf,"(%s",exprs[0]);
	    for (i=1; i<=arity; i++) {
	      strcat(buf," ");
	      if (isInfix(fixexp[i])) {
	        strcat(buf,"(");
	        strcat(buf,exprs[i]);
	        strcat(buf,")");
	      } else
	        strcat(buf,exprs[i]);
            }
	    strcat(buf,")");
	  }
	  *expr = strdup(buf);
          *sr   = readSRAt(foSR);
          HIDE(fprintf(stderr,"return parent of 0x%x -> 0x%x\n",fo,parent);)
	  return parent;
	} break;
      case ExpValueApp:
	{ unsigned char i, arity; Ident* id;
	  FileOffset foExprs[20], foSR=0;
	  char* exprs[20];
	  int  fixexp[20];
          if (hasSrcPos(c)) { foSR = readFO(); }
          parent = readFO();			/* get parent */
          HIDE(fprintf(stderr,"enter parent of 0x%x -> 0x%x\n",fo,parent);)
	  foExprs[0] = readFO();		/* get fun */
	  fread(&arity,sizeof(unsigned char),1,HatFileRandom);
	  for (i=1; i<=arity; i++) {
	    foExprs[i] = readFO();
          }
          id = readAtomAt(foExprs[0]);
	  for (i=1; i<=arity; i++) {
	    (void)readTraceAt(getResult(foExprs[i],True)
                             ,&(exprs[i]),sr,&(fixexp[i]),False,depth-1);
          }
	  *infix = id->fixity;
	  if (isInfix(id->fixity) && c >= 2) {
	    sprintf(buf,"%s",infixPrint(exprs[1],fixexp[1],id->idname,id->fixity
			    ,exprs[2],fixexp[2]));
	    for (i=3; i<=arity; i++) {
	      strcat(buf," ");
	      strcat(buf,exprs[i]);
            }
	  } else {	/* no fixity */
	    sprintf(buf,"(%s",id->idname);
	    for (i=1; i<=arity; i++) {
	      strcat(buf," ");
	      if (isInfix(fixexp[i])) {
	        strcat(buf,"(");
	        strcat(buf,exprs[i]);
	        strcat(buf,")");
	      } else
	        strcat(buf,exprs[i]);
            }
	    strcat(buf,")");
	  }
	  *expr = strdup(buf);
          *sr   = readSRAt(foSR);
          HIDE(fprintf(stderr,"return parent of 0x%x -> 0x%x\n",fo,parent);)
	  return parent;
	} break;
      case ExpChar:
      case ExpInt:
      case ExpInteger:
      case ExpRat:
      case ExpRational:
      case ExpFloat:
      case ExpDouble:
      case ExpValueUse:
      case ExpConstUse:
      case ExpConstDef:
        { FileOffset foSR=0;
          Ident *id;
          if (hasSrcPos(c)) { foSR = readFO(); }
	  parent = readFO();
	  id	 = readValueAt(fo);
	  *infix = id->fixity;
	  sprintf(buf,"%s",id->idname);
	  *expr = strdup(buf);
          *sr   = readSRAt(foSR);
	  return parent;
	} break;
      case ExpGuard:
      case ExpCase:
      case ExpIf:
        { FileOffset foCond, foSR;
	  char* cond;
	  int fixcond;
          if (hasSrcPos(c)) { foSR = readFO(); }
          parent = readFO();			/* get parent */
          HIDE(fprintf(stderr,"enter parent of 0x%x -> 0x%x\n",fo,parent);)
          readFO();				/* skip result */
	  foCond = readFO();			/* get condition */
	  readTraceAt(foCond,&cond,sr,&fixcond,False,depth-1);
          switch (lower5(c)) {
	    case ExpGuard: sprintf(buf,"| "); break;
	    case ExpCase:  sprintf(buf,"case "); break;
	    case ExpIf:    sprintf(buf,"if "); break;
            default: break;
          }
	  if (isInfix(fixcond)) {
	    strcat(buf,"(");
	    strcat(buf,cond);
	    strcat(buf,")");
	  } else
	    strcat(buf,cond);
	  *expr = strdup(buf);
          *sr   = readSRAt(foSR);
          HIDE(fprintf(stderr,"return parent of 0x%x -> 0x%x\n",fo,parent);)
        } break;
      case ExpFieldUpdate:
        {
	} break;
      case ExpProjection:
        { if (hasSrcPos(c)) { readFO(); }
          parent = readFO();	/* throw projective parent away */
          parent = readFO();	/* choose original expression */
	  return readTraceAt(parent, expr, sr, infix, followHidden, depth);
	} break;
      case ExpHidden:
        { FileOffset result;
          parent = readFO();
          result = readFO();
	  if (!followHidden) {
	    sprintf(buf,"{?}");
	    *expr = strdup(buf);
	    return parent; 
          } else {
            return readTraceAt(result, expr, sr, infix, followHidden, depth);
          }
	} break;
      case ExpForward:
	{ FileOffset result;
	  result = readFO();
	  return readTraceAt(result, expr, sr, infix, followHidden, depth);
	} break;
      case ExpDoStmt:
        { FileOffset foStmt, foSR;
	  char* stmt;
	  int fixstmt;
          if (hasSrcPos(c)) { foSR = readFO(); }
          HIDE(fprintf(stderr,"enter parent of 0x%x -> 0x%x\n",fo,parent);)
          foStmt = readFO();
	  parent = readTraceAt(foStmt,&stmt,sr,&fixstmt,False,depth-1);
          sprintf(buf,"do "); break;
	  if (isInfix(fixstmt)) {
	    strcat(buf,"(");
	    strcat(buf,stmt);
	    strcat(buf,")");
	  } else
	    strcat(buf,stmt);
	  *expr = strdup(buf);
          *sr   = readSRAt(foSR);
          HIDE(fprintf(stderr,"return parent of 0x%x -> 0x%x\n",fo,parent);)
	} break;
      default: break;
    }
    return parent;
  }
}


/* print an infix expression correctly according to the given priorities. */
char*
infixPrint (char* str1, int arg1, char* strfn, int fn, char* str2, int arg2)
{
  char buf[10000]; /* fixed size no final solution */

  if (!isInfix(arg1))
      sprintf(buf,"%s",str1);
  else if (priority(arg1) > priority(fn))
      sprintf(buf,"%s",str1);
  else if (priority(arg1) < priority(fn))
      sprintf(buf,"(%s)",str1);
  else if (isInfixN(fn))
      sprintf(buf,"(%s)",str1);
  else
      sprintf(buf,"%s",str1);

  strcat(buf,strfn);

  if (!isInfix(arg2)) {
      strcat(buf,str2);
  } else if (priority(arg2) > priority(fn)) {
      strcat(buf,str2);
  } else if (priority(arg2) < priority(fn)) {
      strcat(buf,"(");
      strcat(buf,str2);
      strcat(buf,")");
  } else if (isInfixN(fn)) {
      strcat(buf,"(");
      strcat(buf,str2);
      strcat(buf,")");
  } else {
      strcat(buf,str2);
  }

  return strdup(buf);
}



/* The next bunch of utility functions are for hat-trail.
 * ------------------------------------------------------
 *
 *
 */


/* Open the .hat file */
void
openHatFile (char* prog, char* arg)
{
  int err;
  char header[8];
  progname = basename(prog,0);

  HatFileRandom  = openFile(arg, "");
  HatFileSeq  = openFile(arg, "");
  BridgeFile = openFile(arg,".bridge");
  filesize = sizeFile(arg, "");
  q_position = 0;

  err = fread(header,sizeof(char),8,HatFileRandom);
  if (err!=8) {
    fprintf(stderr,"%s (error): file %s is too short\n",progname,arg);
    exit(1);
  }
  if (strncmp(header,"Hat",3)) {
    fprintf(stderr,"%s (error): file %s\n",progname,arg);
    fprintf(stderr,"   does not appear to be a Hat archive.  Quitting.\n");
    exit(1);
  }
  if (strncmp(header+3,FILEVERSION,4)) {
    fprintf(stderr,"%s (warning): file %s\n",progname,arg);
    fprintf(stderr,"   appears to be a Hat archive in format %s\n",header+3);
    fprintf(stderr,"   but this tool deals with format version %s\n",FILEVERSION);
    fprintf(stderr,"   I'm continuing, but there may be unexpected errors.\n");
  }
  errorRoot = readFO();
  errorMsg  = readFO();
  if (errorMsg==Entered) interrupt=True;
//  signal(SIGINT,SIG_IGN);
}

void closeHatFile (void)
{
  fclose(HatFileRandom);
  fclose(HatFileSeq);
  fclose(BridgeFile);
}

/* Return the contents of the bridge file, one item at a time.  */
FileOffset
getBridgeValue (void)
{
  int err;
  FileOffset fo;
  err = fread(&fo,sizeof(FileOffset),1,BridgeFile);
  if (err==0) {
     fclose(BridgeFile);
     return 0;
  } else return ntohl(fo);
}

/* Get the trace reference for an error, and the string associated with it. */
FileOffset
getErrorLoc (void)
{
  FileOffset fo;
  fseek(HatFileRandom,8,SEEK_SET);
  fo = readFO();
  return fo;
}
char *
errorMessage (void)
{
  FileOffset fo;
  char c='\0';
  fseek(HatFileRandom,12,SEEK_SET);
  fo = readFO();
  if (fo==Entered) return "Interrupted (^C)";
  if (fo) {
    HIDE(fprintf(stderr,"errorMessage 0x%x\n",fo);)
    freadAt(fo,&c,sizeof(char),1,HatFileRandom);
    if (lower5(c)==AtomAbstract) return readString();
  }
  return "";
}
char*
versionNumber (void)
{
  return strdup(VERSION);
}


/* Read the tag bits and report the NodeType. */
int
getNodeType (FileOffset fo)
{
  char c;
  if (fo<=DoLambda) return -1;
  HIDE(fprintf(stderr,"getNodeType 0x%x\n",fo);)
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  return (int)(lower5(c));	/* lower 5 bits are the identifying tag */
}

/* For any node type, get its parent.  If it doesn't have one, we give 0.  */
FileOffset
parentNode (FileOffset fo)
{
  char c;
  HIDE(fprintf(stderr,"parentNode 0x%x\n",fo);)
  if (fo<=DoLambda) return 0;
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  switch (lower5(c)) {
    case ListCons:
    case Module:
    case SrcPos:
    case AtomVariable:
    case AtomConstructor:
    case AtomAbstract:
        return 0;
        break;
    case ExpApp:
    case ExpValueApp:
    case ExpChar:
    case ExpInt:
    case ExpInteger:
    case ExpRat:
    case ExpRational:
    case ExpFloat:
    case ExpDouble:
    case ExpValueUse:
    case ExpConstUse:
    case ExpConstDef:
    case ExpGuard:
    case ExpCase:
    case ExpIf:
    case ExpFieldUpdate:
    case ExpProjection:
    case ExpHidden:
        if (hasSrcPos(c)) { readFO(); }
        return readFO();
        break;
    case ExpForward:
    case ExpDoStmt:
        if (hasSrcPos(c)) { readFO(); }
        return parentNode(readFO());
        break;
  }
}

/* Only for Exp nodes of value kind, we give back a string representation
 * of the name (identifier, Integer, Double, etc), and its fixity etc.
 * The predicate isLiteral reports True for values of basic
 * types like Int, Char, Double etc, and isConstructor identifies Constrs.
 */
char *
getNm (FileOffset fo)
{
  char *id;
  Ident *name = readValueAt(fo);
  if (name) {
    id = name->idname;
    free(name);
    HIDE(fprintf(stderr,"getNm: %s\n",id);)
    return id;
  } else
    return "";
}
char *
getNmMod (FileOffset fo)
{
  char *id;
  Ident *name = readValueAt(fo);
  if (name) {
    id = name->modname;
    free(name);
    return id;
  } else
    return "";
}
int
getFixity (FileOffset fo)
{
  int f;
  Ident *name = readValueAt(fo);
  if (name) {
    f = (int)name->fixity;
    HIDE(fprintf(stderr,"getFixity: %d (%s)\n",f,name->idname);)
    free(name);
    return f;
  } else
    return (int)noFixity;
}
Bool
isLiteral (FileOffset fo)
{
  Bool b;
  Ident *name = readValueAt(fo);
  if (name) {
    b = (name->atomtype == Literal);
    free(name);
    return b;
  } else
    return False;
}
Bool
isConstructor (FileOffset fo)
{
  Bool b;
  Ident *name = readValueAt(fo);
  if (name) {
    b = (name->atomtype == Constructor) || (name->atomtype == ConstrFields);
    free(name);
    return b;
  } else
    return False;
}
Bool
isConstrFields (FileOffset fo)
{
  Bool b;
  char c;
  Ident *name;
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  switch (lower5(c)) {
      case AtomVariable:
      case AtomConstructor:
      case AtomAbstract:
          name = readAtomAt(fo);
          break;
      default:
          name = readValueAt(fo);
          break;
  }
  if (name) {
    b = (name->atomtype == ConstrFields);
    free(name);
    HIDE(fprintf(stderr,"isConstrFields 0x%x: %s %s\n",fo,name->idname,
                                                  (b ? "True" : "False"));)
    return b;
  } else
    return False;
}
Bool
isLambda (FileOffset fo)
{
  Bool b; unsigned char c;
  HIDE(fprintf(stderr,"isLambda 0x%x\n",fo);)
  freadAt(fo,&c,sizeof(unsigned char),1,HatFileRandom);
  switch (lower5(c)) {
    case ExpValueUse:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        if (readFO()==Lambda) return True;
        else return False;
        break;
    default:
        return False;
        break;
  }
}
Bool
isDoLambda (FileOffset fo)
{
  Bool b; unsigned char c;
  HIDE(fprintf(stderr,"isDoLambda 0x%x\n",fo);)
  freadAt(fo,&c,sizeof(unsigned char),1,HatFileRandom);
  switch (lower5(c)) {
    case ExpValueUse:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        if (readFO()==DoLambda) return True;
        else return False;
        break;
    default:
        return False;
        break;
  }
}


/* Get the an name of an identifier out of its struct Ident. */
char*
identName     (Ident* id) { return id->idname; }
char*
identModName  (Ident* id) { return id->modname; }
char*
identSrcFile  (Ident* id) { return id->srcname; }
int
identFixity   (Ident* id) { return (int)id->fixity; }
int
identArity    (Ident* id) { return (int)id->arity; }
int
identDefnLine (Ident* id) { return id->defnline; }
int
identDefnCol  (Ident* id) { return id->defncolumn; }
int
identDefnLineEnd (Ident* id) { return id->defnlineend; }
int
identDefnColEnd  (Ident* id) { return id->defncolumnend; }
Bool
identIsTraced (Ident* id) { return id->isTraced; }


/* For all nodes, get number of arguments. */
int
getExpArity (FileOffset fo)
{
  char c;
  HIDE(fprintf(stderr,"getExpArity 0x%x\n",fo);)
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  switch (lower5(c)) {
    case ExpApp:
    case ExpFieldUpdate:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        readFO();				/* skip result */
        readFO();				/* skip fun/constructor */
        fread(&c,sizeof(char),1,HatFileRandom);	/* get arity */
        return (int)(c);
        break;
    case ExpValueApp:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        readFO();				/* skip constructor */
        fread(&c,sizeof(char),1,HatFileRandom);	/* get arity */
        return (int)c;
        break;
    case AtomConstructor:			/* only interested in fields */
        if (!hasFields(c)) { return 0; }
        readFO();				/* skip module */
        { int x; fread(&x,sizeof(int),1,HatFileRandom); }
        { int x; fread(&x,sizeof(int),1,HatFileRandom); }
        { char x; fread(&x,sizeof(char),1,HatFileRandom); }
        { char arity; fread(&arity,sizeof(char),1,HatFileRandom);
          return (int)arity; }
        break;
    case Module:
    case SrcPos:
    case ExpChar:
    case ExpInt:
    case ExpInteger:
    case ExpRat:
    case ExpRational:
    case ExpFloat:
    case ExpDouble:
    case ExpValueUse:
    case ExpConstUse:
    case ExpConstDef:
    case ExpGuard:
    case ExpCase:
    case ExpIf:
    case ExpProjection:
    case ExpHidden:
    case ExpForward:
    case ExpDoStmt:
    case AtomVariable:
    case AtomAbstract:
        return 0;
        break;
  }
}

/* For all nodes, get the value of the n'th subexpression.  For basic values,
 * the 0'th subexpr is the node itself.  For an application, the 0'th subexpr
 * is the final result of the fun ptr, the k'th subexpr is the final result of
 * the k'th argument.  In the case where a fun/arg resolves to an atom,
 * we return the atom pointer rather than an Exp pointer - the Haskell code
 * can interpret either form.
 */
FileOffset
getExpArg (FileOffset fo, int n)
{
  char c;
  int i=0;
  FileOffset ptr;
  HIDE(fprintf(stderr,"getExpArg 0x%x\n",fo);)
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  switch (lower5(c)) {
    case ExpApp:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        readFO();				/* skip result */
        ptr = readFO();				/* fun/constructor */
        if (n==0) return getResult(ptr,True);
        fread(&c,sizeof(char),1,HatFileRandom);	/* get arity */
        if (n<=c) {
          for (i=1; i<n; i++) readFO();		/* skip other args */
          ptr = readFO();			/* get n'th arg */
          return getResult(ptr,True);
        } else
          return fo;
        break;
    case ExpValueApp:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        ptr = readFO();				/* fun/constructor */
        if (n==0) return ptr;	/* no result-chain - fun is already an atom */
        fread(&c,sizeof(char),1,HatFileRandom);	/* get arity */
        if (n<=c) {
          for (i=1; i<n; i++) readFO();		/* skip other args */
          ptr = readFO();			/* get n'th arg */
          return getResult(ptr,True);
        } else
          return fo;
        break;
    case ExpValueUse:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        ptr = readFO();				/* CAF */
        return ptr;	/* no result-chain - fun is already an atom */
        break;
    case ExpConstDef:
    case ExpConstUse:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        ptr = readFO();				/* CAF */
        return getResult(ptr,True);
        break;
    case ExpGuard:
    case ExpCase:
    case ExpIf:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        readFO();				/* skip result */
        ptr = readFO();				/* get condition */
        return getResult(ptr,True);
        break;
    case ExpFieldUpdate:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        readFO();				/* skip result */
        ptr = readFO();				/* exp/constructor */
        if (n==0) return getResult(ptr,True);
        fread(&c,sizeof(char),1,HatFileRandom);	/* get arity */
        if (n<=c) {
          for (i=0; i<c; i++) readFO();		/* skip binder labels */
          for (i=1; i<n; i++) readFO();		/* skip other bindees */
          ptr = readFO();			/* get n'th bindee */
          return getResult(ptr,True);
        } else
          return fo;
        break;
    case ExpProjection:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        ptr = readFO();				/* get expr */
        return ptr;
        break;
    case ExpForward:
        ptr = readFO();				/* get expr */
        return ptr;
        break;
    case ExpChar:
    case ExpInt:
    case ExpInteger:
    case ExpRat:
    case ExpRational:
    case ExpFloat:
    case ExpDouble:
    case ExpHidden:
    case ExpDoStmt:
    case Module:
    case SrcPos:
    case AtomVariable:
    case AtomConstructor:
    case AtomAbstract:
    default:
        return fo;
        break;
  }
}

/* Get the n'th subexpression (*not* its value), 
 * for any node that has subexpressions. 
 */
FileOffset
peekExpArg (FileOffset fo, int n)
{
  char c;
  int i=0;
  FileOffset ptr;
  HIDE(fprintf(stderr,"peekExpArg 0x%x\n",fo);)
  assert (n>=0);
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  switch (lower5(c)) {
    case ExpApp:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        readFO();				/* skip result */
        ptr = readFO();				/* fun/constructor */
        if (n==0) return ptr;
        fread(&c,sizeof(char),1,HatFileRandom);	/* get arity */
        assert (n<=c);
        for (i=1; i<n; i++) readFO();		/* skip other args */
        ptr = readFO();		                /* get n'th arg */
        return ptr;
        break;
    case ExpValueApp:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        ptr = readFO();				/* fun/constructor */
        if (n==0) return ptr;
        fread(&c,sizeof(char),1,HatFileRandom);	/* get arity */
        assert (n<=c);
        for (i=1; i<n; i++) readFO();		/* skip other args */
        ptr = readFO();			        /* get n'th arg */
        return ptr;
        break;
    case ExpValueUse:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        ptr = readFO();				/* only arg */
        return ptr;	/* this is an atom */
        break;
    case ExpConstDef:
    case ExpConstUse:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        ptr = readFO();				/* only arg */
        return ptr;
        break;
    case ExpGuard:
    case ExpCase:
    case ExpIf:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        readFO();				/* skip result */
        ptr = readFO();				/* get condition */
        return ptr;
        break;
    case ExpFieldUpdate:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        readFO();				/* skip result */
        ptr = readFO();				/* exp/constructor */
        if (n==0) return ptr;
        fread(&c,sizeof(char),1,HatFileRandom);	/* get arity */
        assert (n<=c);
        for (i=0; i<c; i++) readFO();		/* skip binder labels */
        for (i=1; i<n; i++) readFO();		/* skip other bindees */
        ptr = readFO();			        /* get n'th bindee */
        return ptr;
        break;
    case ExpProjection:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        ptr = readFO();				/* get expr */
        return ptr;
        break;
    case ExpForward:
        ptr = readFO();				/* get expr */
        return ptr;
        break;
    case ExpHidden:
        readFO();				/* skip parent */
        readFO();                               /* skip result */
        ptr = readFO();			        /* get child list */
        return ptr;
        break;
    case ListCons:
        ptr = readFO();                         /* get element */
        if (n==1) return ptr;
        ptr = readFO();                         /* get tail */
        return ptr;
        break;
    case ExpChar:
    case ExpInt:
    case ExpInteger:
    case ExpRat:
    case ExpRational:
    case ExpFloat:
    case ExpDouble:
    case ExpDoStmt:
    case Module:
    case SrcPos:
    case AtomVariable:
    case AtomConstructor:
    case AtomAbstract:
    default:
        assert (False);
        break;
  }
}

/* Only for an ExpFieldUpdate node, or an ExpConstructor that has fields,
 * get the atom pointer of the n'th label.
 */
FileOffset
getFieldLabel (FileOffset fo, int n)
{
  char c;
  int i=0;
  FileOffset ptr;
  HIDE(fprintf(stderr,"getFieldLabel 0x%x: n=%d\n",fo,n);)
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  switch (lower5(c)) {
    case ExpFieldUpdate:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        readFO();				/* skip result */
        readFO();				/* skip exp/constructor */
        fread(&c,sizeof(char),1,HatFileRandom);	/* get arity */
        if (n<=c) {
          for (i=1; i<n; i++) readFO();		/* skip other labels */
          ptr = readFO();			/* get n'th label */
          return ptr;
        } else
          return 0;
        break;
    case ExpValueUse:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        ptr = readFO();				/* atom pointer */
        return getFieldLabel(ptr,n);
        break;
    case AtomConstructor:
        if (hasFields(c)) {
          readFO();				/* skip module ptr */
          { int x; fread(&x,sizeof(int),1,HatFileRandom); }
          { int x; fread(&x,sizeof(int),1,HatFileRandom); }
          { char x; fread(&x,sizeof(char),1,HatFileRandom); }
          { char arity; fread(&arity,sizeof(char),1,HatFileRandom);
            readString();
            while (n--) readFO();
            ptr = readFO();
            HIDE(fprintf(stderr,"getFieldLabel: return 0x%x\n",ptr);)
            return ptr; }
        } else {
          fprintf(stderr,"constructor at 0x%x has no field labels\n",fo);
          return 0;
        } break;
    default:
        return 0;
        break;
  }
}

/* Get the SrcRef belonging to any node type.  If the node doesn't have
 * one, we give back a 0 reference.
 */
FileOffset
getSrcRef (FileOffset fo)
{
  char c;
  int i;
  HIDE(fprintf(stderr,"getSrcRef 0x%x\n",fo);)
  if (fo<=DoLambda) return 0;
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  if (hasSrcPos(c)) {
    return readFO();			/* get SrcRef */
  } else {
    return 0;
  }
}

/* Get the Atom node for the fun position of an expression. 
 * If the node doesn't look right, we give back a 0 reference.
 */
FileOffset
getDefnRef (FileOffset fo)
{
  char c;
  int i;
  FileOffset ptr;
  if (fo==0) return 0;
  HIDE(fprintf(stderr,"getDefnRef 0x%x\n",fo);)
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  switch (lower5(c)) {
    case ExpApp:
        if (hasSrcPos(c)) { readFO(); }		/* skip use position */
        readFO();				/* skip parent */
        readFO();				/* skip result */
        ptr = readFO();				/* get fun */
        return getDefnRef(ptr);			/* and follow it */
        break;
    case ExpValueApp:
    case ExpValueUse:
        if (hasSrcPos(c)) { readFO(); }		/* skip use position */
        readFO();				/* skip parent */
        ptr = readFO();				/* get Atom */
        return ptr;
        break;
    case ExpConstUse:
        if (hasSrcPos(c)) { readFO(); }		/* skip use position */
        readFO();				/* skip parent */
        ptr = readFO();				/* get ExpConstDef pointer */
        return getDefnRef(ptr);			/* and follow it */
        break;
    case ExpConstDef:				/* never a use position */
        readFO();				/* skip parent */
        readFO();				/* skip result */
        ptr = readFO();				/* get Atom */
        return ptr;
        break;
    case AtomVariable:
    case AtomConstructor:
        return fo;
        break;
    default:
        return 0;
        break;
  }
}


/* peekTrace() takes a peek backwards at a trace (or indirection), skipping
 * over any Hidden or Projection nodes to find the nearest "real" trace.
 * It is used by the code that clumps together characters in the output
 * by their shared parentage.
 */
FileOffset
peekTrace(FileOffset fo)
{
  char c;

  while (1) {		/* iterate until we find a `real' node */
    if (fo<=DoLambda) return 0;	/* trace is Root/Unevaluated/Entered/etc */
    HIDE(fprintf(stderr,"peekTrace 0x%x\n",fo);)
    freadAt(fo,&c,sizeof(char),1,HatFileRandom);
    switch (c) {
      case ExpProjection:
          if (hasSrcPos(c)) { readFO(); }	/* skip use position */
          fo = readFO();			/* get parent */
          break;				/* and look again */
      case ExpHidden:
      case ExpForward:
          fo = readFO();			/* get parent */
          break;				/* and look again */

      case Module:
      case SrcPos:
      case AtomVariable:
      case AtomConstructor:
      case AtomAbstract:
          fprintf(stderr,"peekTrace failed\n");
          exit(1);

      default:
          return fo;			/* found a `real' node! */
          break;
    }
  }
}


/* getResult() returns the result of an application, or 0 if there is
 * apparently no result.  (Results can also be Unevaluated, Entered,
 * or Interrupted).  To find the result, it is necessary to follow
 * the result-pointer chain right to the end (but only as far as the
 * terminating Exp, not right down to an Atom).  If the final result
 * is Unevaluated, Entered, Interrupted, or Lambda, then we return
 * the previous step of the chain.  (The pretty printer then decides
 * how to represent these to the user.)
 *
 * It turns out that a result chain can contain a cycle, which we must
 * detect.  A cycle always involves some Forwards and one Hidden.  It
 * is sufficient to record the most recent Hidden that was seen, and
 * if we come across it again, just stop and return the Hidden.
 */
static FileOffset mostRecentHidden=0;

FileOffset
getResult (FileOffset fo, Bool stopAtHidden)
{
  if (fo==mostRecentHidden) return fo;
  mostRecentHidden=0;
  return getResultNoCycle(fo,stopAtHidden);
}

#define fixInterrupt(fo)	(interrupt && fo==Entered ? Interrupted : fo)
FileOffset
getResultNoCycle (FileOffset fo, Bool stopAtHidden)
{
  char c;
  FileOffset result;

  HIDE(fprintf(stderr,"getResult 0x%x\n",fo);)
  if (fo<=DoLambda) return fixInterrupt(fo);      /* trace is Unevaluated etc */
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  switch (lower5(c)) {
    case ExpApp:
    case ExpGuard:
    case ExpCase:
    case ExpIf:
    case ExpFieldUpdate:
        if (hasSrcPos(c)) { readFO(); }		/* skip use position */
        readFO();				/* skip parent */
        result = readFO();			/* get result */
        HIDE(fprintf(stderr,"getResult: result is 0x%x\n",result);)
        if (result==fo) return fo;
        else if (result<=DoLambda) return fixInterrupt(fo);
        else return getResultNoCycle(result,False);
        break;
//  case ExpValueApp:
//  case ExpValueUse:
//      if (hasSrcPos(c)) { readFO(); }		/* skip use position */
//      readFO();				/* skip parent */
//      return readFO();			/* return Atom pointer */
//      break;
    case ExpConstUse:
        if (hasSrcPos(c)) { readFO(); }		/* skip use position */
        readFO();				/* skip parent */
        result = readFO();			/* get result */
        if (result<=DoLambda) return fixInterrupt(fo);
        else return getResultNoCycle(result,False);
						/* follow ExpConstDef pointer */
        break;
    case ExpConstDef:
        readFO();				/* skip parent */
        result = readFO();			/* get result */
        if (result<=DoLambda) return fixInterrupt(fo);
        return getResultNoCycle(result,False);
        break;
    case ExpForward:
        return getResultNoCycle(readFO(),stopAtHidden);
        break;					/* continue to detect Hidden */
    case ExpDoStmt:
        return getResultNoCycle(readFO(),False);	/* get result */
        break;
    case ExpProjection:
        return fo;
    case ExpValueApp:
    case ExpValueUse:
    case ExpChar:
    case ExpInt:
    case ExpInteger:
    case ExpRat:
    case ExpRational:
    case ExpFloat:
    case ExpDouble:
//  case AtomVariable:
//  case AtomConstructor:
//  case AtomAbstract:
        HIDE(fprintf(stderr,"getResult: result is itself\n");)
        return fo;
        break;
    case ExpHidden:
        if (stopAtHidden) return fo;
        else if (fo==mostRecentHidden) return fo;
        else {
          mostRecentHidden = fo;		/* keep, to detect a loop */
          readFO();				/* skip parent */
          result = readFO();			/* get result */
          if (result==fo) return fo;
          else if (result<=DoLambda) return fixInterrupt(fo);
          else return getResultNoCycle(result,False);
        }
        break;
    case AtomVariable:
    case AtomConstructor:
    case AtomAbstract:
    default:
        return 0;
        break;
  }
}

/* peekResult() is a one-step look at the result pointer.  It does not
 * follow the whole chain.  It is mainly used to decide whether the
 * pointer returned by getResult() leads directly to an Unevaluated,
 * Entered, Interrupted, or Lambda.
 */
FileOffset
peekResult (FileOffset fo)
{
  char c;
  FileOffset result;

  HIDE(fprintf(stderr,"peekResult 0x%x\n",fo);)
  if (fo<=DoLambda) return fixInterrupt(fo);      /* trace is Unevaluated etc */
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  switch (lower5(c)) {
    case ExpApp:
    case ExpGuard:
    case ExpCase:
    case ExpIf:
    case ExpFieldUpdate:
        if (hasSrcPos(c)) { readFO(); }		/* skip use position */
        readFO();				/* skip parent */
        result = readFO();			/* get result */
        return fixInterrupt(result);
        break;
    case ExpConstUse:
    case ExpProjection:
        if (hasSrcPos(c)) { readFO(); }		/* skip use position */
        readFO();				/* skip parent */
        result = readFO();			/* get result */
        return fixInterrupt(result);
        break;
    case ExpConstDef:
        readFO();				/* skip parent */
        result = readFO();			/* get result */
        return fixInterrupt(result);
        break;
    case ExpForward:
    case ExpDoStmt:
        result = readFO();			/* get result */
        return fixInterrupt(result);
        break;
    case ExpValueApp:
    case ExpValueUse:
    case ExpChar:
    case ExpInt:
    case ExpInteger:
    case ExpRat:
    case ExpRational:
    case ExpFloat:
    case ExpDouble:
        HIDE(fprintf(stderr,"getResult: result is itself\n");)
        return fo;
        break;
    case ExpHidden:
        readFO();				/* skip parent */
        result = readFO();			/* get result */
        return fixInterrupt(result);
        break;
    case AtomVariable:
    case AtomConstructor:
    case AtomAbstract:
    default:
        return 0;
        break;
  }
}


/* Comparison of FileOffsets is the basic key-ordering for finite maps,
 * used for gathering and looking up information during a linear sweep
 * of the .hat file (in hat-observe and hat-detect).
 */
int
fileoffset_compare (FileOffset i, FileOffset j) /* for ordering the tree */
{
  if (i<j) return -1;
  else if (i==j) return 0;
  else return 1;
}

/* Signal handler for user-interruption */
void
ctrlC (int sig)
{ hat_interrupted = True; }


/* sequential access from Haskell */
void
q_init (void)
{
  q_position  = 0x10;
  fseek(HatFileSeq,q_position,SEEK_SET);
}
char
q_tag (void)
{
  char c;
  c = (char)fgetc(HatFileSeq);
  q_position+=1;
  return lower5(c);
}

