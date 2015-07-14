/* hat-check: reads hat trace files, checking at least the basic format
 * other checks and information can be requested by options
 * Colin Runciman, University of York
 * Original version February 2001
 *   text output as default, with -v option for added verification check
 * 21 March 2001
 *   added -s option for statistics, made text an option (-a)
 * 28 March 2001
 *   added -r option to check proportion of nodes reachable
 * 6 April 2001
 *   -v extended to check for inappropriate zero pointers
 *   -n option added to request textual dump of single node
 *   when both -a & -r set show reachability of each node in text lines
 * 8 May 2001
 *   allow (but ignore) bits to mark SATs with no APP
 *   accept progname with or without .hat
 * 2-4 April 2002 - Malcolm Wallace
 *   Update to new file format.
 * 12 June 2002 - Colin
 *   Added -g option to generate dot-code for graph diagram.
 * 4 August 2004 - Olaf
 *   Update to extended position format (begin and end location).
 */

/* #include <unistd.h> */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <inttypes.h>

#include "art.h"
#include "ntohl.h"

/* out-of-range values must be <= 31 && not overlap Exps and Atoms */
#define ANYEXP	22
#define ANYATOM	23
#define HEADER  24
#define INVALID	30 
#define BEYOND	31

/* prototypes */
void badusage (void);
void initstats (void);
void reportstats (void);
void cleartag (char *b);
void header (void);
void nodes (void);
void nextnode (void);
void markfromheader (uint32_t *buf);
void markfromoutput (char *bridgefile, uint32_t *buf);
void markfrom (uint32_t root, uint32_t *buf);
int strends (char *e, char*s);

/* strings for symbolic constants */
char*
tag2str (int k)
{
  switch (k) {
  case Module:          return "Module"; break;
  case SrcPos:          return "SrcPos"; break;
  case ExpApp:          return "ExpApp"; break;
  case ExpValueApp:     return "ExpValueApp"; break;
  case ExpChar:         return "ExpChar"; break;
  case ExpInt:          return "ExpInt"; break;
  case ExpInteger:      return "ExpInteger"; break;
  case ExpRat:          return "ExpRat"; break;
  case ExpRational:     return "ExpRational"; break;
  case ExpFloat:        return "ExpFloat"; break;
  case ExpDouble:       return "ExpDouble"; break;
  case ExpValueUse:     return "ExpValueUse"; break;
  case ExpConstUse:     return "ExpConstUse"; break;
  case ExpConstDef:     return "ExpConstDef"; break;
  case ExpGuard:        return "ExpGuard"; break;
  case ExpCase:         return "ExpCase"; break;
  case ExpIf:           return "ExpIf"; break;
  case ExpFieldUpdate:  return "ExpFieldUpdate"; break;
  case ExpProjection:   return "ExpProjection"; break;
  case ExpHidden:       return "ExpHidden"; break;
  case ExpForward:      return "ExpForward"; break;
  case ExpDoStmt:       return "ExpDoStmt"; break;
  case AtomVariable:    return "AtomVariable"; break;
  case AtomConstructor: return "AtomConstructor"; break;
  case AtomAbstract:    return "AtomAbstract"; break;
  case ListCons:        return "ListCons"; break;

  case ANYEXP:  return "Exp"; break;
  case ANYATOM: return "Atom"; break;
  case HEADER:  return "HEADER/INVALID"; break; /* same for INVALID */
  case BEYOND:  return "beyond end of file";

  default:              return "unknown/unused"; break;
  }
}

char*
ref2str (int k)
{
  switch (k) {
  case Root:         return "Root"; break;
  case Unevaluated:  return "Unevaluated"; break;
  case Entered:      return "Entered"; break;
  case Interrupted:  return "Interrupted"; break;
  case Lambda:       return "Lambda"; break;
  case DoLambda:     return "DoLambda"; break;
  default:           return "Ref"; break;
  }
}


/* Main driver and routines to provide basic interface to archive file.
 */

FILE* f;             /* file descriptor for archive */
uint32_t nextoffset=0;	/* current position in file */

int vmode = 0;       /* verify -- check tag-types of pointer destinations */
int smode = 0;       /* statistics -- counts and space usage for node types */
int amode = 0;       /* ascii -- show archive in a `readable' text format */
int gmode = 0;       /* graph -- generate source for dot graph-drawing */
int nmode = 0;       /* node -- show node at given offset in text format */
int rmode = 0;       /* reachable -- show how many nodes are reachable */
int xmode = 0;       /* exit mode -- cleanup after signal to halt */

unsigned filesize = 0; /* used in precondition for seeks ... */
struct stat statbuf;   /* ... to catch seek beyond EOF */

#define FILENAMESIZE 200
char filename[FILENAMESIZE];

/* byte buffer for use in reachability mark-phase */
#define BUFFERSIZE 100000
uint32_t buffer[BUFFERSIZE];

/* signal handler -- only installed for -r */
void
restoretags (int signum)
{
  fprintf(stderr, "hat-check cleaning up -- please wait\n");
  amode = 0; rmode = 0; smode = 0; vmode = 0;
  xmode = 1;
  fseek(f,0,SEEK_SET);
  nextoffset = 0;
  header();
  nodes();
  exit(1);
}

int
hat_check (int argc, char *argv[])
/* main (int argc, char *argv[]) */
{
  int i = 0;
  while (++i < argc-1) {
    if (strcmp(argv[i], "-v") == 0) {
      vmode = 1;
    } else if (strcmp(argv[i], "-s") == 0) {
      smode = 1;
    } else if (strcmp(argv[i], "-a") == 0) {
      amode = 1;
    } else if (strcmp(argv[i], "-g") == 0) {
      gmode = 1;
    } else if (strcmp(argv[i], "-n") == 0) {
      nmode = 1;
      sscanf(argv[++i], "0x%x", &nextoffset);      
    } else if (strcmp(argv[i], "-r") == 0) {
      smode = 1;
      rmode = 1;
    } else {
      badusage();
    }
  }
  if (i > argc-1) badusage();
  strcpy(filename, argv[i]);
  if (!strends(".hat", filename)) strcat(filename, ".hat");
  stat(filename, &statbuf);
  filesize = statbuf.st_size;
  f = fopen(filename, (rmode ? "r+b" : "rb"));
  if (f==(FILE*)0) {
    fprintf(stderr, "cannot open trace file %s\n",filename);
    exit(1);
  }

  /* version checking added by MW */
  { int err;
    char *header = (char*)malloc(10*sizeof(char));
    err = fread(header,sizeof(char),8,f);
    if (err!=8) {
      fprintf(stderr,"hat-check (error): file %s is too short\n",filename);
      exit(1);
    }
    if (strncmp(header,"Hat",3)) {
      fprintf(stderr,"hat-check (error): file %s\n",filename);
      fprintf(stderr,"   does not appear to be a Hat archive.  Quitting.\n");
      exit(1);
    }
    if (strncmp(header+3,FILEVERSION,4)) {
      fprintf(stderr,"hat-check (warning): file %s\n",filename);
      fprintf(stderr,"   appears to be a Hat archive in format %s\n",header+3);
      fprintf(stderr,"   but this tool deals with format version %s\n",FILEVERSION);
      fprintf(stderr,"   I'm continuing, but there may be unexpected errors.\n");
    }
    fseek(f,0,SEEK_SET);	/* reset to beginning of file */
    free(header);
  }

  printf("Hat archive has expected version %s.\n", FILEVERSION);

  if (nmode) {
    if (nextoffset >= filesize) {
      fprintf(stderr, "-n 0x%x is beyond end of trace file\n", nextoffset);
      exit(1);
    }
    amode = 1; rmode = 0; smode = 0;
  } else {
    nextoffset = 0L;
  }
  if (rmode) {
    signal(SIGINT, restoretags);
#ifdef SIGQUIT
    signal(SIGQUIT, restoretags);
#endif
    markfromheader(buffer);
    strcat(filename, ".bridge");
    markfromoutput(filename,buffer);
    fseek(f,0,SEEK_SET);
  }
  fseek(f,nextoffset,SEEK_SET);
  if (nmode) {
    nextnode();
  } else {
    if (smode) initstats();
    if (gmode) printf("digraph hat{\n");
    header();
    nodes();
    if (gmode) printf("}\n");
    if (amode && smode) putchar('\n');
    if (smode) reportstats();
  }
  return 0;
}

int
strends (char *e, char *s)
{
  int d = strlen(s) - strlen(e);
  return d>=0 && strcmp(s+d, e)==0;
}

void
badusage (void)
{
  fprintf(stderr,"usage: hat-check [-a] [-n <hexnode>][-r] [-s] [-v] prog-name\n");
  fprintf(stderr,"\t-a\tprint ascii text version of hat file\n");
  fprintf(stderr,"\t-g\tprint dot graph-drawing code\n");
  fprintf(stderr,"\t-n\tprint text for specified node only (disables -r, -s)\n");
  fprintf(stderr,"\t-r\tprint statistics about reachable nodes (implies -s)\n");
  fprintf(stderr,"\t-s\tprint statistics about frequency and size of nodes\n");
  fprintf(stderr,"\t-v\tverify tag types of pointer destinations\n");
  exit(1);
}

#define MAX_TAG 32
uint32_t count[MAX_TAG]; /* indexed by tag byte: Module, SrcPos, etc */
uint32_t space[MAX_TAG];/* ditto */
uint32_t headspace;     /* space oocupied by header */
uint32_t reachcount[MAX_TAG];

void
initstats (void)
{
  int k;
  for (k=0; k<MAX_TAG; k++) {
    count[k] = 0;
    reachcount[k] = 0;
    space[k] = 0L;
  }
}

float
pc (uint32_t i, uint32_t j)
{
  return (float)((i*100.0)/j);
}

void
reportstats (void)
{
  unsigned int grandcount = 1;
  unsigned int grandreachcount = 1;
  uint32_t grandspace = headspace;
  int k;
  for (k=0; k<MAX_TAG; k++) {
    grandcount += count[k];
    grandreachcount += reachcount[k];
    grandspace += space[k];
  }
  if (rmode) printf("%7s", "% Reach");
  printf("%10s   %-20s%12s%10s\n\n",
    "Number", "Description", "Bytes", "% Space");
  if (rmode) printf("%7.1f", pc(1,1));
  printf("%10u   %-20s%12u%10.1f\n",
    1, "header", headspace, pc(headspace,grandspace));
  for (k=0; k<=ListCons; (k==ExpDoStmt? k=AtomVariable: k++)) {
    if (rmode) printf("%7.1f", pc(reachcount[k], count[k]));
    printf("%10u   %-20s%12u%10.1f\n",
      count[k], tag2str(k), space[k],
      pc(space[k],grandspace));
  }
  { int w;
    putchar('\n');
    if (rmode) for (w=0; w<7; w++) putchar(' ');
    for (w=0; w<55; w++) putchar(w<13 ? ' ' : '-');
    putchar('\n');
  }
  if (rmode) printf("%7.1f", pc(grandreachcount,grandcount));
  printf("%10u   %-20s%12u%10.1f\n",
    grandcount, "whole trace file", grandspace, pc(grandspace,grandspace));
}

char
nextbyte (void)
{
  char c; int err;
  err = fread(&c,sizeof(char),1,f);
  nextoffset+=1;
  if (err!=1) {
    fflush(stdout);
    fprintf(stderr, "unexpected end of trace file at 0x%x\n",nextoffset);
    exit(1);
  }
  return c;
}

/* Routines to extract values encoded as one or more bytes.
 */

#define LARGEST_STRING 65536
char stringbuf[LARGEST_STRING];
/* readstring() reads a length-annotated string from the current position
 * in the file and stores it in a global buffer.
 */
char*
readstring (void)
{
  char *buf=stringbuf;
  int i, n;

  n = (int)fgetc(f);
  nextoffset+=1;
  if (n==0xff) {
    n = (int)fgetc(f);
    n = (n<<8) + (int)fgetc(f);
    nextoffset+=2;
  }
  i = fread(buf,sizeof(char),n,f);
  nextoffset+=i;
  buf[n] = '\0';
  if (i<n) {
    fprintf(stderr
           ,"hat-check: warning, only read %d characters of %d in string\n"
           ,i,n);
  }
  return buf;
}

typedef uint32_t fourbytes;

fourbytes
readfourbytes (void)
{
  int err;
  fourbytes slot;
  err = fread(&slot,sizeof(fourbytes),1,f);
  nextoffset+=4;
  if (err!=1) {
    fflush(stdout);
    fprintf(stderr, "unexpected end of trace file before 0x%x\n",nextoffset);
    fprintf(stderr, "actual file position is 0x%lx\n",ftell(f));
    exit(1);
  }
  return ntohl(slot);
}

#define POSNMAX 40
char posnbuf[POSNMAX+1];

char*
readposn (void)
{
  uint32_t begin = readfourbytes();
  uint32_t end = readfourbytes();
  sprintf(posnbuf, "position %u:%u-%u:%u,", 
          begin/10000, begin%10000, end/10000, end%10000);
  return posnbuf;
}

uint32_t
readpointer (void)
{ return readfourbytes(); }

char
readchar (void)
{ return nextbyte(); }

int
readint (void)
{ return readfourbytes(); }

char*
readinteger (void)
{ return readstring(); }

char*
readrational (void)
{
  char *num, *denom;
  num = strdup(readinteger());
  denom = strdup(readinteger());
  strcpy(stringbuf,num);
  strcat(stringbuf,":%");
  strcat(stringbuf,denom);
  free(num); free (denom);
  return stringbuf;
}

float
readfloat (void)
{
  int err;
  float buf;
  err = fread(&buf,sizeof(float),1,f);
  nextoffset+=sizeof(float);
  if (err!=1) {
    fflush(stdout);
    fprintf(stderr, "unexpected end of trace file before 0x%x\n",nextoffset);
    fprintf(stderr, "actual file position is 0x%lx\n",ftell(f));
    fprintf(stderr, "while reading a float\n");
    exit(1);
  }
  return buf;
}

double
readdouble (void)
{
  int err;
  double buf;
  err = fread(&buf,sizeof(double),1,f);
  nextoffset+=sizeof(double);
  if (err!=1) {
    fflush(stdout);
    fprintf(stderr, "unexpected end of trace file before 0x%x\n",nextoffset);
    fprintf(stderr, "actual file position is 0x%lx\n",ftell(f));
    fprintf(stderr, "while reading a double\n");
    exit(1);
  }
  return buf;
}

int
lo5 (char b)
{ return (int)(b&0x1f); }

unsigned int
readarity (void)
{ return (unsigned int)(unsigned char)nextbyte(); }

#define FIXPRIMAX 10
char fixpribuf[FIXPRIMAX+1];

char*
readfixpri (void)
{
  int b = (int)(nextbyte());
  switch (b % 4) {
  case 0:
    sprintf(fixpribuf, " infix %d,", b/4);
    break;
  case 1:
    sprintf(fixpribuf, " infixr %d,", b/4);
    break;
  case 2:
    sprintf(fixpribuf, " infixl %d,", b/4);
    break;
  case 3:
    sprintf(fixpribuf, "");
    break;
  }
  return fixpribuf;
}

int
tagat (uint32_t offset)
{
  char byte[1];
  int i;
  if (offset <= filesize) {
    fseek(f, offset, SEEK_SET);
    i = fread(byte,sizeof(char),1,f);
    fseek(f, nextoffset, SEEK_SET);
    if (rmode) cleartag(byte);
    return (i==1 ? lo5(*byte) : INVALID);
  } else return BEYOND;
}

void
newtagat (char *t, uint32_t offset)
{
  fseek(f, offset, SEEK_SET);
  fwrite(t,sizeof(char),1,f);
  fseek(f, nextoffset, SEEK_SET);
}



/* dopointer():
 * When checking for correctness (-v), there are several parameters to
 * take into account.
 *    okzero:   Is the pointer permitted to be root,unevaluated,error,lambda?
 *    requiretag:    What kind of node should the pointer point to?
 *    requireoffset: The pointer itself.
 *    contexttag:    What kind of node did the pointer come from?
 *    contextoffset: Where did the pointer come from?
 */
#define NONZERO 0
#define MAYBEZERO 1
#define MAYBELAMBDA 4
   
void
dopointer (int okzero,
           int requiretag, uint32_t requireoffset,
           int contexttag, uint32_t contextoffset,
	   char *edgelabel)
{
  if (vmode && ((okzero==NONZERO && requireoffset <= Interrupted)
               || (okzero==MAYBELAMBDA && requireoffset < Lambda))) {
      fprintf(stderr, "bad %s pointer in %s 0x%x\n"
             ,ref2str(requireoffset), tag2str(contexttag), contextoffset);     
  }
  if (vmode && requireoffset>DoLambda) {
    int t = tagat(requireoffset);
    if (t != requiretag) {
      if ((requiretag==ANYEXP) && (ExpApp<=t) && (t<=ExpDoStmt))
        ;
      else if ((requiretag==ANYATOM) && (AtomVariable<=t) && (t<=AtomAbstract))
        ;
      else fprintf(stderr, "tag at 0x%x is %s, not %s as %s at 0x%x implies\n"
                         , requireoffset, tag2str(t), tag2str(requiretag)
	                 , tag2str(contexttag), contextoffset);
    }
  }
  if (amode) {
    switch(requireoffset) {
      case Root:
      case Unevaluated:
      case Entered:
      case Interrupted:
      case Lambda:
      case DoLambda:
                printf("(%s)",ref2str(requireoffset));
                break;
      default:  printf("(%s 0x%x)", tag2str(requiretag), requireoffset);
                break;
    }
  }
  if (gmode && requireoffset!=0 && *edgelabel != '\0') {
    if (strcmp(edgelabel,"p")==0) {
      printf("%d -> %d [style=dashed]\n",
        contextoffset, requireoffset);
    } else {
      printf("%d -> %d [label=\"%s\"]\n",
        contextoffset, requireoffset, edgelabel);
    }
  }
}
  
#define ismarked(b) ((b)&0x80)

void
marktag (char *b)
{ *b |= 0x80; }

void
cleartag (char *b)
{ *b &= 0x7F; }

/* reading, checking and/or writing header and node information
 */
 
void
header (void)
{
  char s[9];
  fread(s,sizeof(char),8,f); s[8]='\0';
  nextoffset = 8;
  if (amode) printf("%s", s);
  if (amode) printf("\nEntry point: ");
  dopointer(MAYBEZERO, ANYEXP, readpointer(), HEADER, 0L, "");
  if (amode) printf("\nError message: ");
  dopointer(MAYBEZERO, AtomAbstract, readpointer(), HEADER, 0L, "");
  if (amode) printf("\n");
  headspace = nextoffset;
}

void
nodes (void)
{ 
  while (!feof(f)) { nextnode(); }
}

void
nextnode (void)
{
  uint32_t offset = nextoffset;
  char b;
  int marked, err;
  err = fread(&b,sizeof(char),1,f); nextoffset+=1;
  if (err!=1) return;
  if (rmode || xmode) {
    marked = ismarked(b);
    if (marked) {
      cleartag(&b);
      newtagat(&b, offset);
    }
    if (amode) printf("%s", (marked ? "=> " : "   "));
  }
  {
    int k = lo5(b);
    if ((ExpDoStmt<k && k<AtomVariable) || k>ListCons) {
      fprintf(stderr, "strange tag %d at byte offset 0x%x\n",
                      k, offset);
      exit(1);
    } else if (smode) {
      count[k]++;
      if (rmode && marked) reachcount[k]++;
    }
    switch (k) {
    case ListCons:
	if (gmode) printf("%d [label=\"0x%x ListCons\"]\n", offset, offset);
        if (amode) printf("0x%x:  %-20s\t", offset, tag2str(k));
	if (amode) printf("elem=");
        dopointer(NONZERO, ANYEXP, readpointer(), k, offset, "e");
	if (amode) printf(" tail=");
        dopointer(MAYBEZERO, ListCons, readpointer(), k, offset, "t");
      break;
    case Module:
      if (amode) {
        if (tracedModule(b))
             printf("0x%x:  Module (suspect) \t", offset);
        else printf("0x%x:  Module (trusted) \t", offset);
      }
      { char *s = readstring(); if (amode) printf("%s\t", s); }
      { char *s = readstring(); if (amode) printf("\"%s\"", s); }
      break;
    case SrcPos:
      if (amode) printf("0x%x:  SrcPos\t\t\t", offset);
      dopointer(NONZERO, Module, readpointer(), k, offset, "");
      { char *p = readposn(); if (amode) printf(" %s", p); }
      break;
    case AtomVariable:
      if (amode) {
        if (localDef(b))
             printf("0x%x:  AtomVariable (local)\t", offset);
        else printf("0x%x:  AtomVariable (toplevel)\t", offset);
      }
      if (gmode) printf("%d [label=\"0x%x AtomVariable", offset, offset);
      dopointer(NONZERO, Module, readpointer(), k, offset, "");
      { char *p = readposn(); if (amode) printf(" %s", p); }
      { char *fp = readfixpri(); if (*fp!='\0' && amode) printf("%s ", fp); }
      { unsigned int a = readarity();
        if (amode || gmode) printf(amode ? " arity=%u," : " %u", a); }
      { char *n = readstring(); if (amode || gmode) printf(" %s", n); }
      if (gmode) printf("\"]\n");
      break;
    case AtomConstructor:
      if (amode) printf("0x%x:  %-20s\t", offset, tag2str(k));
      if (gmode) printf("%d [label=\"0x%x AtomConstructor", offset, offset);
      dopointer(NONZERO, Module, readpointer(), k, offset, "");
      { char *p = readposn(); if (amode) printf(" %s", p); }
      { char *fp = readfixpri(); if (*fp!='\0' && amode) printf("%s", fp); }
      { unsigned int a = readarity();
        if (amode || gmode) printf(amode ? " arity=%u," : " %u", a);
        { char *n = readstring(); if (amode || gmode) printf(" %s", n); }
        if (gmode) printf("\"]\n");
        if hasFields(b) {
	  int i;
          if (amode) printf(" fields:");
          for (i=1; i<=a; i++) {
            dopointer(NONZERO, AtomVariable, readpointer(), k, offset,
	      (gmode ? (sprintf(stringbuf,"%d",i), stringbuf) : "")
	    );
          }
        }
      }
      break;
    case AtomAbstract:
      if (amode) printf("0x%x:  %-20s\t", offset, tag2str(k));
      if (gmode) printf("%d [label=\"0x%x AtomAbstract ", offset, offset);
      { char *s = readstring(); if (amode || gmode) printf("%s", s); }
      if (gmode) printf("\"]\n");
      break;
    default: {
      if (amode) printf("0x%x:  %-20s\t", offset, tag2str(k));
      if (hasSrcPos(b)) {
	  if (amode) printf("use=");
          dopointer(NONZERO, SrcPos, readpointer(), k, offset, "");
	  if (amode) printf(" ");
      }
   // if (amode && (ExpChar <= k) && (k <= ExpConstUse)) {
   //     printf("(");
   //     if (!isEntered(b)) printf("not ");
   //     printf("entered) ");
   // }
      switch (k) {
      case ExpApp:
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
	if (amode) printf(" result=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "r");
	if (amode) printf(" fun=");
        dopointer(NONZERO, ANYEXP, readpointer(), k, offset, "f");
        if (gmode) printf("%d [label=\"0x%x ExpApp", offset, offset);
        { unsigned int a = readarity();
	  int i;
	  if (amode || gmode)
	    printf(amode ? " arity=%u, args " : " %u\"]\n",a);
	  for (i=1; i<=a; i++)
	    dopointer(NONZERO, ANYEXP, readpointer(), k, offset,
	      (gmode ? (sprintf(stringbuf,"%d",i), stringbuf) : "")
	    );
	}
	break;
      case ExpValueApp:
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
	if (amode) printf(" fun=");
        dopointer(NONZERO, ANYATOM, readpointer(), k, offset, "f");
        if (gmode) printf("%d [label=\"0x%x ExpValueApp", offset, offset);
        { unsigned int a = readarity();
	  int i;
	  if (amode || gmode)
	    printf(amode ? " arity=%u, args " : " %u\"]\n",a);
	  for (i=1; i<=a; i++)
	    dopointer(NONZERO, ANYEXP, readpointer(), k, offset,
	      (gmode ? (sprintf(stringbuf,"%d",i), stringbuf) : "")
	    );
	}
	break;
      case ExpChar:
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
        { char c = nextbyte();
	  if (gmode) printf("%d [label=\"ExpChar", offset);
	  if (amode || gmode) printf(" '%c'", c);
	  if (gmode) printf("\"]\n");
	}
	break;
      case ExpInt:
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
        { int i; i = readfourbytes();
	  if (gmode) printf("%d [label=\"ExpInt", offset);
	  if (amode || gmode) printf(" %d", i);
	  if (gmode) printf("\"]\n");
	}
	break;
      case ExpInteger:
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
        { char* i; i = readinteger();
	  if (gmode) printf("%d [label=\"ExpInteger", offset);
	  if (amode || gmode) printf(" %s", i);
	  if (gmode) printf("\"]\n");
	}
	break;
      case ExpRat:
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
        { int n,d; n=readfourbytes(); d=readfourbytes();
	  if (gmode) printf("%d [label=\"ExpRat", offset);
          if (amode || gmode) printf(" %d%%%d", n,d);
	  if (gmode) printf("\"]\n");
        }
	break;
      case ExpRational:
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
        { char* r = readrational();
	  if (gmode) printf("%d [label=\"ExpRational", offset);
	  if (amode || gmode) printf(" %s", r);
	  if (gmode) printf("\"]\n");
        }
	break;
      case ExpFloat:
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
        { float f = readfloat();
	  if (gmode) printf("%d [label=\"ExpFloat", offset);
	  if (amode || gmode) printf(" %f", f);
	  if (gmode) printf("\"]\n");
        }
	break;
      case ExpDouble:
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
        { double d = readdouble();
	  if (gmode) printf("%d [label=\"ExpDouble", offset);
	  if (amode || gmode) printf(" %f", d);
	  if (gmode) printf("\"]\n");
        }
	break;
      case ExpValueUse:
	if (gmode) printf("%d [label=\"0x%x ExpValueUse\"]\n", offset, offset);
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
	if (amode) printf(" value=");
        dopointer(MAYBELAMBDA, ANYATOM, readpointer(), ExpValueUse, offset,"v");
	break;
      case ExpConstUse:
	if (gmode) printf("%d [label=\"0x%x ExpConstUse\"]\n", offset, offset);
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
	if (amode) printf(" const=");
        dopointer(NONZERO, ExpConstDef, readpointer(), k, offset, "c");
	break;
      case ExpConstDef:
	if (gmode) printf("%d [label=\"0x%x ExpConstDef\"]\n", offset, offset);
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
	if (amode) printf(" result=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "r");
	if (amode) printf(" var=");
        dopointer(NONZERO, AtomVariable, readpointer(), k, offset, "v");
	break;
      case ExpGuard:
      case ExpCase:
      case ExpIf:
	if (gmode) printf("%d [label=\"0x%x %s\"]\n", offset, offset,
                  k==ExpGuard ? "ExpGuard" : k==ExpCase ? "ExpCase" : "ExpIf");
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
	if (amode) printf(" result=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "r");
	if (amode) printf(" cond=");
        dopointer(NONZERO, ANYEXP, readpointer(), k, offset, "c");
	break;
      case ExpFieldUpdate:
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
	if (amode) printf(" result=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "r");
	if (amode) printf(" arg=");
        dopointer(NONZERO, ANYEXP, readpointer(), k, offset, "a");
        { unsigned int i, arity = readarity();
	  if (gmode) printf("%d [label=\"0x%x ExpFieldUpdate %u\"]\n", offset, offset, arity);
	  if (amode) printf(" arity=%u, binders ",arity);
          for (i=0; i<arity; i++) {
            dopointer(NONZERO, AtomVariable, readpointer(), k, offset, "");
          }
	  if (amode) printf(" arity=%u, bindees ",arity);
          for (i=0; i<arity; i++) {
            dopointer(NONZERO, ANYEXP, readpointer(), k, offset, "");
          }
        }
	break;
      case ExpProjection:
	if (gmode) printf("%d [label=\"0x%x ExpProjection\"]\n", offset, offset);
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
	if (amode) printf(" exp=");
        dopointer(NONZERO, ANYEXP, readpointer(), k, offset, "e");
	break;
      case ExpHidden:
	if (gmode) printf("%d [label=\"0x%x ExpHidden\"]\n", offset, offset);
	if (amode) printf("parent=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "p");
	if (amode) printf(" result=");
        dopointer(MAYBEZERO, ANYEXP, readpointer(), k, offset, "r");
	if (amode) printf(" children=");
	dopointer(MAYBEZERO, ListCons, readpointer(), k, offset, "c");
	break;
      case ExpForward:
	if (gmode) printf("%d [label=\"0x%x ExpForward\"]\n", offset, offset);
	if (amode) printf("result=");
        dopointer(NONZERO, ANYEXP, readpointer(), k, offset, "r");
	break;
      case ExpDoStmt:
	if (gmode) printf("%d [label=\"0x%x ExpDoStmt\"]\n", offset, offset);
	if (amode) printf("stmt=");
        dopointer(NONZERO, ANYEXP, readpointer(), k, offset, "s");
	break;
      }
    }}
    if (amode) printf("\n");
    if (smode) space[k] += nextoffset - offset;
  }
}


/* Traverse the trail structure starting from output and error
 * roots, marking all reachable nodes.
 */
uint32_t
getpointer (uint32_t *buf)
{  return ntohl(*buf); }

/* reading, checking and/or writing header and node information
 */
void
markfromheader (uint32_t *buf)
{
  fseek(f,8L,SEEK_SET);
  fread(buf,sizeof(uint32_t),2,f);
  markfrom(getpointer(buf+1),buf+1);
  markfrom(getpointer(buf),  buf);
}

void
markfromoutput (char *bridgefile, uint32_t *buf)
{
  FILE* bridge = fopen(bridgefile, "rb");
  if (bridge==(FILE*)0) return;
  for (;;) {
    int n = fread(buf,sizeof(uint32_t),1,bridge);
    if (n<1) return;
    markfrom(getpointer(buf),buf);
  }
  fclose(bridge);
}

/* mark all the nodes in the hat file that are reachable
 * from the given root -- setting the highest bit in the
 * tag byte
 */
void
markfrom (uint32_t root, uint32_t *buf)
{
  char tag;
  if (root > 8 && root < filesize) {
    /* First read the tag byte.  If it is marked, return.
     * If it is not marked, then mark it now.
     */
    fseek(f,root,SEEK_SET);
    fread(&tag,sizeof(char),1,f);
    if (ismarked(tag)) return;
    marktag(&tag);
    fseek(f,root,SEEK_SET);
    fwrite(&tag,sizeof(char),1,f);
    cleartag(&tag);
    /* Examine the tag to determine the kind of node.
     * Read pointers from the node into buf, then
     * markfrom() these pointers recursively.  The buffer is
     * overwritten where possible to minimise the risk of overflow:
     * for this reason, pointers are recursively traced in
     * reverse order.
     */
    {
      int k = lo5(tag);
      if ((ExpDoStmt < k && k < AtomVariable) || k > ListCons) {
	fprintf(stderr, "strange tag %d at 0x%x\n",
                	k, root);
	exit(1);
      }
      switch (k) {
        case ListCons:
          fread(buf,sizeof(uint32_t),2,f);	/* two pointers */
          markfrom(getpointer(buf+1),buf+1);
          markfrom(getpointer(buf),buf);
	  break;
        case Module: break;
        case AtomAbstract: break;

        case SrcPos:
        case AtomVariable:
        case AtomConstructor: 	/* ignore fieldnames for now */
          fread(buf,sizeof(uint32_t),1,f);	/* points to module mode */
          markfrom(getpointer(buf),buf);
          break;

        default: {
          int pos = 0;
          if (hasSrcPos(tag)) {
            fread(buf+pos,sizeof(uint32_t),1,f);
            pos++;
          }
          fread(buf+pos,sizeof(uint32_t),1,f);	/* parent pointer */
          pos++;
          switch (k) {
            case ExpApp:
              fread(buf+pos,sizeof(uint32_t),2,f); /* result+fun */
              pos += 2;
              { unsigned char arity;
                fread(&arity,sizeof(unsigned char),1,f);
                fread(buf+pos,sizeof(uint32_t),(unsigned int)arity,f);
                pos += (int)arity;
              }
	      break;
            case ExpValueApp:
              fread(buf+pos,sizeof(uint32_t),1,f); /* fun */
              pos += 1;
              { unsigned char arity;
                fread(&arity,sizeof(unsigned char),1,f);
                fread(buf+pos,sizeof(uint32_t),(unsigned int)arity,f);
                pos += (int)arity;
              }
	      break;
            case ExpValueUse:
            case ExpConstUse:
            case ExpProjection:
              fread(buf+pos,sizeof(uint32_t),1,f);	/* one extra pointer */
              pos++;
	      break;
            case ExpHidden:
            case ExpConstDef:
            case ExpFieldUpdate:	/* ignore fieldnames for now */
            case ExpGuard:
            case ExpCase:
            case ExpIf:
              fread(buf+pos,sizeof(uint32_t),2,f);	/* two pointers */
              pos+=2;
	      break;
            default: break;				/* no pointers */
          }
	  for (;pos-->0;) markfrom(getpointer(buf+pos), buf+pos);
        } break;
      }
    }
  }
}


