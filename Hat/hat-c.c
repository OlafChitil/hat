/* Primitive functions for writing the ART trace */

/* Control C interruption not yet implemented */

/* highest bit of fileoffset is used to mark those pointing to hidden nodes */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include "ntohl.h"
#include "hat-c.h"


#if defined(DEBUG)
#define HIDE(x) x
#define CHECKCOUNTER(name) if (HatCounter != ftell(HatFile)) { fprintf(stderr,"wrong %s 0x%x 0x%x\n",name,HatCounter,ftell(HatFile)); exit(-1);}
#else
#define HIDE(x)
#define CHECKCOUNTER(name)
#endif

/* forward references */
static void initBuffers();
static FileOffset writeTag (int tag, int size);
static void writeByte (int byte);
static void writeFileOffset (FileOffset offset);
static void writeInt (int number);
static void writeFloat (float number);
static void writeDouble (double number);
static void updateFileOffset (FileOffset at, FileOffset entry); 
static int stringSize(char *s);
static void writeString(char *s);
static void dumpBuffers();
static void initHiddenChildrenSet();
static void dumpHiddenChildrenSet();


/* fileoffset indication end of list */
#define LISTEND ((FileOffset) 0)

/* global variables */

#define HeaderSize (8 + 2*sizeof(FileOffset))
typedef char byte;

static FILE *HatFile, *HatOutput, *HatBridge;
static FileOffset HatCounter = HeaderSize;
static FileOffset LastExp = 0;
static Bool traceOpen = False;
static Bool atTraceEnd = True;
static Bool controlC = False;
static FileOffset hiddenMask;
static FileOffset unevaluated;
static FileOffset entered;

/* helper functions */

/*
void
writeString(char *s)
{
  int length = strlen(s);
  if (length < 255)
    fputc(length,HatFile);
  else {
    fputc(255,HatFile);
    fputc((unsigned char) (length>>8),HatFile); 
    fputc((unsigned char) length,HatFile);
  }
  fprintf(HatFile,"%s",s);
}
*/

/* catching errors of the runtime system */

#if defined(__GLASGOW_HASKELL__)
void
OutOfHeapHook (unsigned long x, unsigned long y)
{ hat_ErrorExit("Run out of heap memory.", hat_topStack(), -1); }

void 
StackOverflowHook (long int x)
{ hat_ErrorExit("Run out of stack memory.", hat_topStack(), -1); }

void 
MallocFailHook (long int x)
{ hat_ErrorExit("Cannot allocate enough memory.", hat_topStack(), -1); }
#endif

#if defined(__NHC__) 
extern void (*haskellErrorHandler)(char *errorMsg);
#endif 

/* general functions */

void 
hat_Open(char *progname)
{
    FileOffset p = 0;
    char filename[256];

    unevaluated = htonl(Unevaluated);
    entered = htonl(Entered);    
    
    hiddenMask = htonl(~(1<<31)); /* only 0 at highest bit */
    /* logical & with possibly hidden fileoffset yields fileoffset */

    strcpy(filename,progname);
    strcat(filename,".hat");		/* the .hat file holds the archive */
    HatFile = fopen(filename,"wb");	/* of redex trails */
    p = ftell(HatFile);                 /* should be 0 */
    fprintf(HatFile,"Hat%s",FILEVERSION);	/* initialise file */
    fputc(0,HatFile);
    fwrite(&p,sizeof(FileOffset),1,HatFile);
    fwrite(&p,sizeof(FileOffset),1,HatFile);

    initBuffers();
    initHiddenChildrenSet();

    strcpy(filename,progname);		/* the .output file is a copy of */
    strcat(filename,".hat.output");	/* stdout */
    HatOutput = fopen(filename,"wb");
    strcpy(filename,progname);		/* the .bridge file links the output */
    strcat(filename,".hat.bridge");	/* to the archived trails */
    HatBridge = fopen(filename,"wb");

    controlC = False;
    /* SIGQUIT here before, why? */
#if defined(SIGQUIT)
    signal(SIGQUIT, hat_Interrupt);    /* install handler for abortion? */
#endif
    signal(SIGABRT, hat_Interrupt);    /* install handler for abortion? */
    signal(SIGFPE, hat_ArithmeticError);
    signal(SIGINT, hat_Interrupt); /* install handler for Control-C */

#if defined(__NHC__) 
    haskellErrorHandler = hat_Error;
#endif 
    traceOpen = True;
}

void hat_Close(void)
{
  if (!traceOpen) 
    return;
  dumpHiddenChildrenSet();
  dumpBuffers();
  hat_dumpBuffer();
  fclose(HatFile);
  fclose(HatOutput);
  fclose(HatBridge);
  traceOpen = False;
}

void hat_ErrorClose(char* errmsg, FileOffset trace)
{
  FileOffset nt;

  if (!traceOpen) 
    return;

  nt = mkAbstract(errmsg);
  nt &= hiddenMask;
  trace &= hiddenMask;
  fseek(HatFile,8,SEEK_SET);
  fwrite(&trace, sizeof(FileOffset), 1, HatFile);
  fwrite(&nt, sizeof(FileOffset), 1, HatFile);

  hat_dumpStack();
  hat_Close();
}

void hat_Error(char* errmsg)
{
  hat_ErrorClose(errmsg,hat_topStack());
}

void
hat_ErrorExit(char* errmsg, FileOffset trace, int ecode)
{
  fflush(stdout);
  fprintf(stderr, "\nError: %s\n", errmsg);
  hat_ErrorClose(errmsg,trace);
  exit(ecode);	
}

void 
hat_Abort(char *msg)
{ hat_ErrorExit(msg, hat_topStack(), -1); }

void
hat_InterruptExit()
{
  FileOffset node = hat_topStack();

  fflush(stdout);
  fprintf(stderr, "Interrupted\n");

  fseek(HatFile,8,SEEK_SET);
  fwrite(&node, sizeof(FileOffset), 1, HatFile);
  fwrite(&entered, sizeof(FileOffset), 1, HatFile);

  hat_dumpStack();
  hat_Close();

  exit(-1);	
}

void 
hat_Interrupt(int sig)
{ controlC = True; }

void
hat_ArithmeticError(int sig)
{ hat_Abort("Arithmetic error."); }


void hat_OutputTrace(FileOffset trace, char *output)
{
  fprintf(HatOutput,"%s",output);  /* copy of output */
  trace &= hiddenMask;
  while (*output++) {
    fwrite(&trace, sizeof(FileOffset), 1, HatBridge);
				/* link trace to output */
  }
}

Bool
hat_Hidden(FileOffset node)
{
  return (!((node & hiddenMask) == node));
}

FileOffset
mkRoot()
{
  return (htonl(Root));
}


/* Write trace nodes */

FileOffset
mkModule(char *modname, char *srcfile, Bool traced)
{
  FileOffset fo;

  fo = writeTag(Module | (traced?TracedModule:0)
               ,1+stringSize(modname)+stringSize(srcfile));
  writeString(modname);
  writeString(srcfile);
  HIDE(fprintf(stderr,"\tmkModule %s (%s) -> 0x%x\n",modname,srcfile,fo);)
  return fo;
}

FileOffset
mkSrcPos(FileOffset moduleTraceInfo,int begin,int end)
{
  FileOffset fo;

  fo = writeTag(SrcPos,1+sizeof(FileOffset)+2*sizeof(int));
  writeFileOffset(moduleTraceInfo);
  writeInt(begin);
  writeInt(end);
  HIDE(fprintf(stderr,"\tmkSrcPos %d %d -> 0x%x\n",begin,end,fo);)
  return fo;
}


/* Exp nodes; if use is 0, then the variant without a use field is written */

FileOffset
mkResApp1(FileOffset parent,FileOffset use,FileOffset fun
         ,FileOffset arg1)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (4*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if (use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fo);
  writeFileOffset(fun);
  writeByte(1);
  writeFileOffset(arg1);
  HIDE(fprintf(stderr,"\tmkResApp1 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fo,fun,arg1,fo);)
  return fo;
}

FileOffset
mkApp1(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (4*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(1);
  writeFileOffset(arg1);
  HIDE(fprintf(stderr,"\tmkApp1 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,fo);)
  return fo;
}

FileOffset
mkApp2(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (5*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(2);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  HIDE(fprintf(stderr,"\tmkApp2 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,arg2,fo);)
  return fo;
}

FileOffset
mkApp3(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (6*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(3);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  HIDE(fprintf(stderr,"\tmkApp3 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,arg2,arg3,fo);)
  return fo;
}

FileOffset
mkApp4(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (7*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(4);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  HIDE(fprintf(stderr,"\tmkApp4 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,arg2,arg3,arg4,fo);)
  return fo;
}

FileOffset
mkApp5(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (8*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(5);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  HIDE(fprintf(stderr,"\tmkApp5 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,arg2,arg3,arg4,arg5,fo);)
  return fo;
}

FileOffset
mkApp6(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (9*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(6);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  HIDE(fprintf(stderr,"\tmkApp6 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,arg2,arg3,arg4,arg5,arg6,fo);)
  return fo;
}

FileOffset
mkApp7(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (10*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(7);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  HIDE(fprintf(stderr,"\tmkApp7 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,fo);)
  return fo;
}

FileOffset
mkApp8(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (11*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(8);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  HIDE(fprintf(stderr,"\tmkApp8 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,fo);)
  return fo;
}

FileOffset
mkApp9(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (12*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(9);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  writeFileOffset(arg9);
  HIDE(fprintf(stderr,"\tmkApp9 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,fo);)
  return fo;
}

FileOffset
mkApp10(FileOffset parent,FileOffset use,FileOffset fun
       ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
       ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
       ,FileOffset arg9,FileOffset arg10)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (13*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(10);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  writeFileOffset(arg9);
  writeFileOffset(arg10);
  HIDE(fprintf(stderr,"\tmkApp10 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,fo);)
  return fo;
}

FileOffset
mkApp11(FileOffset parent,FileOffset use,FileOffset fun
       ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
       ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
       ,FileOffset arg9,FileOffset arg10,FileOffset arg11)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (14*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(11);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  writeFileOffset(arg9);
  writeFileOffset(arg10);
  writeFileOffset(arg11);
  HIDE(fprintf(stderr,"\tmkApp11 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,fo);)
  return fo;
}

FileOffset
mkApp12(FileOffset parent,FileOffset use,FileOffset fun
       ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
       ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
       ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (15*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(12);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  writeFileOffset(arg9);
  writeFileOffset(arg10);
  writeFileOffset(arg11);
  writeFileOffset(arg12);
  HIDE(fprintf(stderr,"\tmkApp12 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,fo);)
  return fo;
}

FileOffset
mkApp13(FileOffset parent,FileOffset use,FileOffset fun
       ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
       ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
       ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12
       ,FileOffset arg13)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (16*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(13);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  writeFileOffset(arg9);
  writeFileOffset(arg10);
  writeFileOffset(arg11);
  writeFileOffset(arg12);
  writeFileOffset(arg13);
  HIDE(fprintf(stderr,"\tmkApp13 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,fo);)
  return fo;
}

FileOffset
mkApp14(FileOffset parent,FileOffset use,FileOffset fun
       ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
       ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
       ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12
       ,FileOffset arg13,FileOffset arg14)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (17*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(14);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  writeFileOffset(arg9);
  writeFileOffset(arg10);
  writeFileOffset(arg11);
  writeFileOffset(arg12);
  writeFileOffset(arg13);
  writeFileOffset(arg14);
  HIDE(fprintf(stderr,"\tmkApp14 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,fo);)
  return fo;
}

FileOffset
mkApp15(FileOffset parent,FileOffset use,FileOffset fun
       ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
       ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
       ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12
       ,FileOffset arg13,FileOffset arg14,FileOffset arg15)
{
  FileOffset fo;

  fo = writeTag(ExpApp | (use?HasSrcPos:0)
	       ,2 + (18*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(fun);
  writeByte(15);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  writeFileOffset(arg9);
  writeFileOffset(arg10);
  writeFileOffset(arg11);
  writeFileOffset(arg12);
  writeFileOffset(arg13);
  writeFileOffset(arg14);
  writeFileOffset(arg15);
  HIDE(fprintf(stderr,"\tmkApp15 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,fo);)
  return fo;
}

FileOffset
mkValueApp1(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1)
{
  FileOffset fo;

  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
	       ,2 + (3*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(1);
  writeFileOffset(arg1);
  HIDE(fprintf(stderr,"\tmkValueApp1 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,fo);)
  return fo;
}

FileOffset
mkValueApp2(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2)
{
  FileOffset fo;

  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
		,2 + (4*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(2);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  HIDE(fprintf(stderr,"\tmkValueApp2 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,arg2,fo);)
  return fo;
}

FileOffset
mkValueApp3(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3)
{
  FileOffset fo;

  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
	       ,2 + (5*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(3);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  HIDE(fprintf(stderr,"\tmkValueApp3 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,arg2,arg3,fo);)
  return fo;
}

FileOffset
mkValueApp4(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4)
{
  FileOffset fo;

  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
	       ,2 + (6*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(4);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  HIDE(fprintf(stderr,"\tmkValueApp4 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,arg2,arg3,arg4,fo);)
  return fo;
}

FileOffset
mkValueApp5(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5)
{
  FileOffset fo;
  
  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
	       ,2 + (7*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(5);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  HIDE(fprintf(stderr,"\tmkValueApp5 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,arg2,arg3,arg4,arg5,fo);)
  return fo;
}

FileOffset
mkValueApp6(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6)
{
  FileOffset fo;

  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
	       ,2 + (8*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(6);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  HIDE(fprintf(stderr,"\tmkValueApp6 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,arg2,arg3,arg4,arg5,arg6,fo);)
  return fo;
}

FileOffset
mkValueApp7(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7)
{
  FileOffset fo;

  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
	       ,2 + (9*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(7);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  HIDE(fprintf(stderr,"\tmkValueApp7 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,fo);)
  return fo;
}

FileOffset
mkValueApp8(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8)
{
  FileOffset fo;

  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
	       ,2 + (10*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(8);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  HIDE(fprintf(stderr,"\tmkValueApp8 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,LastExp);)
  return fo;
}

FileOffset
mkValueApp9(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9)
{
  FileOffset fo;

  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
	       ,2 + (11*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(9);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  writeFileOffset(arg9);
  HIDE(fprintf(stderr,"\tmkValueApp9 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,LastExp);)
  return fo;
}

FileOffset
mkValueApp10(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9,FileOffset arg10)
{
  FileOffset fo;

  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
	       ,2 + (12*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(10);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  writeFileOffset(arg9);
  writeFileOffset(arg10);
  HIDE(fprintf(stderr,"\tmkValueApp10 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,LastExp);)
  return fo;
}

FileOffset
mkValueApp11(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9,FileOffset arg10,FileOffset arg11)
{
  FileOffset fo;

  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
	       ,2 + (13*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(11);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  writeFileOffset(arg9);
  writeFileOffset(arg10);
  writeFileOffset(arg11);
  HIDE(fprintf(stderr,"\tmkValueApp11 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,LastExp);)
  return fo;
}

FileOffset
mkValueApp12(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12)
{
  FileOffset fo;

  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
	       ,2 + (14*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(12);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  writeFileOffset(arg9);
  writeFileOffset(arg10);
  writeFileOffset(arg11);
  writeFileOffset(arg12);
  HIDE(fprintf(stderr,"\tmkValueApp12 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,LastExp);)
  return fo;
}

FileOffset
mkValueApp13(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12
      ,FileOffset arg13)
{
  FileOffset fo;

  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
	       ,2 + (15*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(13);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  writeFileOffset(arg9);
  writeFileOffset(arg10);
  writeFileOffset(arg11);
  writeFileOffset(arg12);
  writeFileOffset(arg13);
  HIDE(fprintf(stderr,"\tmkValueApp13 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,LastExp);)
  return fo;
}

FileOffset
mkValueApp14(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12
      ,FileOffset arg13,FileOffset arg14)
{
  FileOffset fo;

  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
	       ,2 + (16*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(14);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  writeFileOffset(arg9);
  writeFileOffset(arg10);
  writeFileOffset(arg11);
  writeFileOffset(arg12);
  writeFileOffset(arg13);
  writeFileOffset(arg14);
  HIDE(fprintf(stderr,"\tmkValueApp14 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,LastExp);)
  return fo;
}

FileOffset
mkValueApp15(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12
      ,FileOffset arg13,FileOffset arg14,FileOffset arg15)
{
  FileOffset fo;

  fo = writeTag(ExpValueApp | (use?HasSrcPos:0)
	       ,2 + (17*sizeof(FileOffset)) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(fun);
  writeByte(15);
  writeFileOffset(arg1);
  writeFileOffset(arg2);
  writeFileOffset(arg3);
  writeFileOffset(arg4);
  writeFileOffset(arg5);
  writeFileOffset(arg6);
  writeFileOffset(arg7);
  writeFileOffset(arg8);
  writeFileOffset(arg9);
  writeFileOffset(arg10);
  writeFileOffset(arg11);
  writeFileOffset(arg12);
  writeFileOffset(arg13);
  writeFileOffset(arg14);
  writeFileOffset(arg15);
  HIDE(fprintf(stderr,"\tmkValueApp15 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,fun,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,LastExp);)
  return fo;
}

FileOffset
mkChar(FileOffset parent,FileOffset use,char c)
{
  FileOffset fo;

  fo = writeTag(ExpChar | (use?HasSrcPos:0)
	       ,1 + sizeof(FileOffset) + sizeof(char)
                + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeByte(c);
  HIDE(fprintf(stderr,"\tmkChar 0x%x 0x%x %c -> 0x%x\n",parent,use,c,fo);)
  return fo;
}

FileOffset
mkInt(FileOffset parent,FileOffset use,int i)
{
  FileOffset fo;

  fo = writeTag(ExpInt | (use?HasSrcPos:0)
	       ,1 + sizeof(FileOffset) + sizeof(int) 
                + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeInt(i);  
  HIDE(fprintf(stderr,"\tmkInt 0x%x 0x%x %d -> 0x%x\n",parent,use,i,fo);)
  return fo;
}

FileOffset
mkInteger(FileOffset parent,FileOffset use,char *i)
{
  FileOffset fo;

  fo = writeTag(ExpInteger | (use?HasSrcPos:0)
	       ,1 + sizeof(FileOffset) + stringSize(i) 
		+ (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeString(i);
  HIDE(fprintf(stderr,"\tmkInteger 0x%x 0x%x %s -> 0x%x\n",parent,use,i,fo);)
  return fo;
}

FileOffset
mkRat(FileOffset parent,FileOffset use,int num,int denom)
{
  FileOffset fo;

  fo = writeTag(ExpRat | (use?HasSrcPos:0)
	       ,1 + sizeof(FileOffset) + 2*sizeof(int) 
                + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeInt(num);
  writeInt(denom);
  HIDE(fprintf(stderr,"\tmkRat 0x%x 0x%x %d/%d -> 0x%x\n",parent,use,num,denom,fo);)
  return fo;
}

FileOffset
mkRational(FileOffset parent,FileOffset use
          ,char *num,char *denom)
{
  FileOffset fo;

  fo = writeTag(ExpRational | (use?HasSrcPos:0)
	       ,1+ sizeof(FileOffset) + stringSize(num) + stringSize(denom)
                + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeString(num);
  writeString(denom);
  HIDE(fprintf(stderr,"\tmkRational 0x%x 0x%x %s/%s -> 0x%x\n",parent,use,num,denom,fo);)
  return fo;
}

FileOffset
mkFloat(FileOffset parent,FileOffset use,float f)
{
  FileOffset fo;

  fo = writeTag(ExpFloat | (use?HasSrcPos:0)
	       ,1 + sizeof(FileOffset) + sizeof(float) 
		+ (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFloat(f);
  HIDE(fprintf(stderr,"\tmkFloat 0x%x 0x%x %f -> 0x%x\n",parent,use,f,fp);)
  return fo;
}

FileOffset
mkDouble(FileOffset parent,FileOffset use,double d)
{
  FileOffset fo;

  fo = writeTag(ExpDouble | (use?HasSrcPos:0)
	       ,1 + sizeof(FileOffset) + sizeof(double) 
		+ (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeDouble(d);
  HIDE(fprintf(stderr,"\tmkDouble 0x%x 0x%x %f -> 0x%x\n",parent,use,d,fo);)
  return fo;
}

FileOffset
mkValueUse(FileOffset parent,FileOffset use,FileOffset value)
{
  FileOffset fo;

  fo = writeTag(ExpValueUse | (use?HasSrcPos:0)
	       ,1 + 2*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(value);
  HIDE(fprintf(stderr,"\tmkValueUse 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,value,fo);)
  return fo;
}

FileOffset
mkConstUse(FileOffset parent,FileOffset use,FileOffset con)
{
  FileOffset fo;

  fo = writeTag(ExpConstUse | (use?HasSrcPos:0)
	       , 1 + 2*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(con);
  HIDE(fprintf(stderr,"\tmkConstUse 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,con,fo);)
  return fo;
}

FileOffset
mkConstDef(FileOffset context,FileOffset var)
{
  FileOffset fo;

  fo = writeTag(ExpConstDef, 1 + 3*sizeof(FileOffset));
  writeFileOffset(context);
  writeFileOffset(unevaluated);
  writeFileOffset(var);
  HIDE(fprintf(stderr,"\tmkConstDef 0x%x 0x%x -> 0x%x\n",context,var,fo);)
  return fo;
}


FileOffset
mkGuard(FileOffset parent,FileOffset use,FileOffset cond)
{
  FileOffset fo;

  fo = writeTag(ExpGuard | (use?HasSrcPos:0)
	       ,1 + 3*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(cond);  
  HIDE(fprintf(stderr,"\tmkGuard 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,unevaluated,cond,fo);)
  return fo;
}

FileOffset
mkCase(FileOffset parent,FileOffset use,FileOffset cond)
{
  FileOffset fo;

  fo = writeTag(ExpCase | (use?HasSrcPos:0)
	       ,1 + 3*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(cond);  
  HIDE(fprintf(stderr,"\tmkCase 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,cond,fo);)
  return fo;
}

FileOffset
mkIf(FileOffset parent,FileOffset use,FileOffset cond)
{
  FileOffset fo;

  fo = writeTag(ExpIf | (use?HasSrcPos:0)
	       ,1 + 3*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(cond);  
  HIDE(fprintf(stderr,"\tmkIf 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,cond,fo);)
  return fo;
}

FileOffset
mkFieldUpdate1(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1)
{
  FileOffset fo;

  fo = writeTag(ExpFieldUpdate | (use?HasSrcPos:0)
	       ,2 + 5*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(arg);
  writeByte(1);
  writeFileOffset(binder1);
  writeFileOffset(bindee1);
  HIDE(fprintf(stderr,"\tmkFieldUpdate1 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,arg,binder1,bindee1,fo);)
  return fo;
}


FileOffset
mkFieldUpdate2(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2)
{
  FileOffset fo;

  fo = writeTag(ExpFieldUpdate | (use?HasSrcPos:0)
	       ,2 + 7*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(arg);
  writeByte(2);
  writeFileOffset(binder1);
  writeFileOffset(bindee1);
  writeFileOffset(binder2);
  writeFileOffset(bindee2);
  HIDE(fprintf(stderr,"\tmkFieldUpdate2 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,arg,binder1,bindee1,binder2,bindee2,fo);)
  return fo;
}

FileOffset
mkFieldUpdate3(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3)
{
  FileOffset fo;

  fo = writeTag(ExpFieldUpdate | (use?HasSrcPos:0)
	       ,2 + 9*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(arg);
  writeByte(3);
  writeFileOffset(binder1);
  writeFileOffset(bindee1);
  writeFileOffset(binder2);
  writeFileOffset(bindee2);
  writeFileOffset(binder3);
  writeFileOffset(bindee3);
  HIDE(fprintf(stderr,"\tmkFieldUpdate3 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,arg,binder1,bindee1,binder2,bindee2,binder3,bindee3,fo);)
  return fo;
}

FileOffset
mkFieldUpdate4(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3
              ,FileOffset binder4,FileOffset bindee4)
{
  FileOffset fo;

  fo = writeTag(ExpFieldUpdate | (use?HasSrcPos:0)
	       ,2 + 11*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(arg);
  writeByte(4);
  writeFileOffset(binder1);
  writeFileOffset(bindee1);
  writeFileOffset(binder2);
  writeFileOffset(bindee2);
  writeFileOffset(binder3);
  writeFileOffset(bindee3);
  writeFileOffset(binder4);
  writeFileOffset(bindee4);
  HIDE(fprintf(stderr,"\tmkFieldUpdate4 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,arg,binder1,bindee1,binder2,bindee2,binder3,bindee3,binder4,bindee4,fo);)
  return fo;
}

FileOffset
mkFieldUpdate5(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3
              ,FileOffset binder4,FileOffset bindee4
              ,FileOffset binder5,FileOffset bindee5)
{
  FileOffset fo;

  fo = writeTag(ExpFieldUpdate | (use?HasSrcPos:0)
	       ,2 + 13*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(arg);
  writeByte(5);
  writeFileOffset(binder1);
  writeFileOffset(bindee1);
  writeFileOffset(binder2);
  writeFileOffset(bindee2);
  writeFileOffset(binder3);
  writeFileOffset(bindee3);
  writeFileOffset(binder4);
  writeFileOffset(bindee4);
  writeFileOffset(binder5);
  writeFileOffset(bindee5);
  HIDE(fprintf(stderr,"\tmkFieldUpdate5 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,arg,binder1,bindee1,binder2,bindee2,binder3,bindee3,binder4,bindee4,binder5,bindee5,fo);)
  return fo;
}

FileOffset
mkFieldUpdate6(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3
              ,FileOffset binder4,FileOffset bindee4
              ,FileOffset binder5,FileOffset bindee5
              ,FileOffset binder6,FileOffset bindee6)
{
  FileOffset fo;

  fo = writeTag(ExpFieldUpdate | (use?HasSrcPos:0)
	       ,2 + 15*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(arg);
  writeByte(6);
  writeFileOffset(binder1);
  writeFileOffset(bindee1);
  writeFileOffset(binder2);
  writeFileOffset(bindee2);
  writeFileOffset(binder3);
  writeFileOffset(bindee3);
  writeFileOffset(binder4);
  writeFileOffset(bindee4);
  writeFileOffset(binder5);
  writeFileOffset(bindee5);
  writeFileOffset(binder6);
  writeFileOffset(bindee6);
  HIDE(fprintf(stderr,"\tmkFieldUpdate6 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,arg,binder1,bindee1,binder2,bindee2,binder3,bindee3,binder4,bindee4,binder5,bindee5,binder6,bindee6,fo);)
  return fo;
}

FileOffset
mkFieldUpdate7(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3
              ,FileOffset binder4,FileOffset bindee4
              ,FileOffset binder5,FileOffset bindee5
              ,FileOffset binder6,FileOffset bindee6
              ,FileOffset binder7,FileOffset bindee7)
{
  FileOffset fo;

  fo = writeTag(ExpFieldUpdate | (use?HasSrcPos:0)
	       ,2 + 17*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(arg);
  writeByte(7);
  writeFileOffset(binder1);
  writeFileOffset(bindee1);
  writeFileOffset(binder2);
  writeFileOffset(bindee2);
  writeFileOffset(binder3);
  writeFileOffset(bindee3);
  writeFileOffset(binder4);
  writeFileOffset(bindee4);
  writeFileOffset(binder5);
  writeFileOffset(bindee5);
  writeFileOffset(binder6);
  writeFileOffset(bindee6);
  writeFileOffset(binder7);
  writeFileOffset(bindee7);
  HIDE(fprintf(stderr,"\tmkFieldUpdate7 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,arg,binder1,bindee1,binder2,bindee2,binder3,bindee3,binder4,bindee4,binder5,bindee5,binder6,bindee6,binder7,bindee7,fo);)
  return fo;
}

FileOffset
mkFieldUpdate8(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3
              ,FileOffset binder4,FileOffset bindee4
              ,FileOffset binder5,FileOffset bindee5
              ,FileOffset binder6,FileOffset bindee6
              ,FileOffset binder7,FileOffset bindee7
              ,FileOffset binder8,FileOffset bindee8)
{
  FileOffset fo;

  fo = writeTag(ExpFieldUpdate | (use?HasSrcPos:0)
	       ,2 + 19*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(arg);
  writeByte(8);
  writeFileOffset(binder1);
  writeFileOffset(bindee1);
  writeFileOffset(binder2);
  writeFileOffset(bindee2);
  writeFileOffset(binder3);
  writeFileOffset(bindee3);
  writeFileOffset(binder4);
  writeFileOffset(bindee4);
  writeFileOffset(binder5);
  writeFileOffset(bindee5);
  writeFileOffset(binder6);
  writeFileOffset(bindee6);
  writeFileOffset(binder7);
  writeFileOffset(bindee7);
  writeFileOffset(binder8);
  writeFileOffset(bindee8);
  HIDE(fprintf(stderr,"\tmkFieldUpdate8 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,arg,binder1,bindee1,binder2,bindee2,binder3,bindee3,binder4,bindee4,binder5,bindee5,binder6,bindee6,binder7,bindee7,binder8,bindee8,fo);)
  return fo;
}

FileOffset
mkFieldUpdate9(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3
              ,FileOffset binder4,FileOffset bindee4
              ,FileOffset binder5,FileOffset bindee5
              ,FileOffset binder6,FileOffset bindee6
              ,FileOffset binder7,FileOffset bindee7
              ,FileOffset binder8,FileOffset bindee8
              ,FileOffset binder9,FileOffset bindee9)
{
  FileOffset fo;

  fo = writeTag(ExpFieldUpdate | (use?HasSrcPos:0)
	       ,2 + 21*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(arg);
  writeByte(9);
  writeFileOffset(binder1);
  writeFileOffset(bindee1);
  writeFileOffset(binder2);
  writeFileOffset(bindee2);
  writeFileOffset(binder3);
  writeFileOffset(bindee3);
  writeFileOffset(binder4);
  writeFileOffset(bindee4);
  writeFileOffset(binder5);
  writeFileOffset(bindee5);
  writeFileOffset(binder6);
  writeFileOffset(bindee6);
  writeFileOffset(binder7);
  writeFileOffset(bindee7);
  writeFileOffset(binder8);
  writeFileOffset(bindee8);
  writeFileOffset(binder9);
  writeFileOffset(bindee9);
  HIDE(fprintf(stderr,"\tmkFieldUpdate9 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,arg,binder1,bindee1,binder2,bindee2,binder3,bindee3,binder4,bindee4,binder5,bindee5,binder6,bindee6,binder7,bindee7,binder8,bindee8,binder9,bindee9,fo);)
  return fo;
}

FileOffset
mkFieldUpdate10(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3
              ,FileOffset binder4,FileOffset bindee4
              ,FileOffset binder5,FileOffset bindee5
              ,FileOffset binder6,FileOffset bindee6
              ,FileOffset binder7,FileOffset bindee7
              ,FileOffset binder8,FileOffset bindee8
              ,FileOffset binder9,FileOffset bindee9
              ,FileOffset binder10,FileOffset bindee10)
{
  FileOffset fo;

  fo = writeTag(ExpFieldUpdate | (use?HasSrcPos:0)
	       ,2 + 23*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(arg);
  writeByte(10);
  writeFileOffset(binder1);
  writeFileOffset(bindee1);
  writeFileOffset(binder2);
  writeFileOffset(bindee2);
  writeFileOffset(binder3);
  writeFileOffset(bindee3);
  writeFileOffset(binder4);
  writeFileOffset(bindee4);
  writeFileOffset(binder5);
  writeFileOffset(bindee5);
  writeFileOffset(binder6);
  writeFileOffset(bindee6);
  writeFileOffset(binder7);
  writeFileOffset(bindee7);
  writeFileOffset(binder8);
  writeFileOffset(bindee8);
  writeFileOffset(binder9);
  writeFileOffset(bindee9);
  writeFileOffset(binder10);
  writeFileOffset(bindee10);
  HIDE(fprintf(stderr,"\tmkFieldUpdate10 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,Unevaluated,arg,binder1,bindee1,binder2,bindee2,binder3,bindee3,binder4,bindee4,binder5,bindee5,binder6,bindee6,binder7,bindee7,binder8,bindee8,binder9,bindee9,binder10,bindee10,fo);)
  return fo;
}

FileOffset
mkProjection(FileOffset parent,FileOffset use,FileOffset exp)
{
  FileOffset fo;

  fo = writeTag(ExpProjection | (use?HasSrcPos:0)
	       ,1 + 2*sizeof(FileOffset) + (use?sizeof(FileOffset):0));
  if(use)
    writeFileOffset(use);
  writeFileOffset(parent);
  writeFileOffset(exp);
  HIDE(fprintf(stderr,"\tmkProjection 0x%x 0x%x 0x%x -> 0x%x\n",parent,use,exp,fo);)
  return fo;
}

FileOffset
mkHidden(FileOffset parent)
{
  FileOffset fo = writeTag(ExpHidden,1 + 3*sizeof(FileOffset));
  writeFileOffset(parent);
  writeFileOffset(unevaluated);
  writeFileOffset(LISTEND);  /* initially empty child list */
  HIDE(fprintf(stderr,"\tmkHidden 0x%x 0x%x -> 0x%x\n",parent,Unevaluated,fo);)
  return (fo|~hiddenMask);
}

FileOffset
mkForward(FileOffset result)
{
  FileOffset fo;

  fo = writeTag(ExpForward,1 + sizeof(FileOffset));
  writeFileOffset(result);
  HIDE(fprintf(stderr,"\tmkForward 0x%x -> 0x%x\n",Unevaluated,fo);)
  return fo;
}

FileOffset
mkDoStmt(FileOffset stmt)
{
  FileOffset fo;

  fo = writeTag(ExpDoStmt, 1 + sizeof(FileOffset));
  writeFileOffset(stmt);
  HIDE(fprintf(stderr,"\tmkDoStmt 0x%x -> 0x%x\n",stmt,fo);)
  return fo;
}

/* Atom */

FileOffset
mkLambda()
{   return (htonl(Lambda)); }

FileOffset
mkDoLambda()
{   return (htonl(DoLambda)); }

FileOffset
mkVariable(FileOffset module,int begin,int end,int fixity,int arity,char *name
	  ,Bool local)
{
  FileOffset fo;

  fo = writeTag(AtomVariable | (local?LocalDef:0)
               ,1+sizeof(FileOffset)+2*sizeof(int)+2*sizeof(byte)
                +stringSize(name));
  writeFileOffset(module);
  writeInt(begin);
  writeInt(end);
  writeByte(fixity);
  writeByte(arity);
  writeString(name);
  HIDE(fprintf(stderr,"\tmkVariable 0x%x %d %d %d %d %s %d -> 0x%x\n",module,begin,end,fixity,arity,name,local,fo);)
  return fo;
}

FileOffset
mkConstructor(FileOffset module,int begin,int end,int fixity,int arity
             ,char *name)
{
  FileOffset fo;

  fo = writeTag(AtomConstructor
               ,1+sizeof(FileOffset)+2*sizeof(int)+2*sizeof(byte)
                +stringSize(name));
  writeFileOffset(module);
  writeInt(begin);
  writeInt(end);
  writeByte(fixity);
  writeByte(arity);
  writeString(name);
  HIDE(fprintf(stderr,"\tmkConstructor 0x%x %d %d %d %d %s %d -> 0x%x\n",module,begin,end,fixity,arity,name,fo);)
  return fo;
}

FileOffset
mkConstructorWFields(FileOffset module,int begin,int end,int fixity,int arity
                    ,char *name,FileOffset labels[])
{
  int i;
  FileOffset fo;

  fo = writeTag(AtomConstructor|HasFields
	       ,1+(1+arity)*sizeof(FileOffset)+2*sizeof(int)+2*sizeof(byte)
                +stringSize(name));
  writeFileOffset(module);
  writeInt(begin);
  writeInt(end);
  writeByte(fixity);
  writeByte(arity);
  writeString(name);
  for (i=0;i<arity;i++) 
    writeFileOffset(labels[i]);
  HIDE(fprintf(stderr,"\tmkConstructorWFields 0x%x %d %d %d %d %s -> 0x%x\n",module,begin,end,fixity,arity,name,fo);)
  return fo;
}


FileOffset
mkAbstract(char *description)
{
  FileOffset fo;

  fo = writeTag(AtomAbstract, 1+stringSize(description));
  writeString(description);
  HIDE(fprintf(stderr,"\tmkAbstract %s -> 0x%x\n",description,fo);)
  return fo;
}

FileOffset
mkListCons(FileOffset elem,FileOffset tail)
{
  FileOffset fo;

  fo = writeTag(ListCons,1 + 2*sizeof(FileOffset));
  writeFileOffset(elem);
  writeFileOffset(tail);
  HIDE(fprintf(stderr,"\tmkListCons 0x%x 0x%x -> 0x%x\n",elem,tail,fo);)
  return fo;
}


/* Update node that it was entered */

void entResult(FileOffset node,FileOffset use)
{
  LastExp = node;
  node &= hiddenMask;
  HIDE(fprintf(stderr,"\tentResult 0x%x 0x%x 0x%x\n",node,use,ntohl(node)+1+sizeof(FileOffset)+(use?sizeof(FileOffset):0));)
  hat_enter(ntohl(node),1+sizeof(FileOffset)+(use?sizeof(FileOffset):0),
	    entered);
  if (controlC) 
    hat_InterruptExit();
}

void entForward(FileOffset node,FileOffset hidden)
{
  hat_enter(ntohl(node),1,hidden);
  if (controlC) 
    hat_InterruptExit();
}

/* Update node with result */

void resResult(FileOffset node,FileOffset result,FileOffset use)
{
  LastExp = node;
  node &= hiddenMask;
  result &= hiddenMask;
  hat_reduce(ntohl(node),1+sizeof(FileOffset)+(use?sizeof(FileOffset):0),
	     result);
}

void resForward(FileOffset node,FileOffset result)
{
  result &= hiddenMask;
  hat_reduce(ntohl(node),1,result);
}

/* Data structures and functions for various buffers.
 * All sequential writes are buffered in buffers, which consists of
 * WriteBufferNo buffers of size WriteBufferSize.
 * If updates effect data still in buffers, they are
 * made there (the majority, this it the purpose buffers);
 * the remaining ones are buffered in buffer of size BufferSize.
 * Information about entered redexes is put on the stack `stack',
 * which can grow and shrink in units of StackpartSize. Only when
 * stack elements are popped, updates are made.
 * The stack both avoids updates and enables locating an error position.
 *
 * Most FileOffsets are stored in network byte order, but some in host
 * byte order.
 */

/* Put enter updates on a stack. A reduce update pops an enter update from
 * the stack. To reduce number of seeks the reduce updates are buffered.
 * The stack is unbounded, it grows in parallel with the runtime stack.
 * An exception releases large parts of the stack. Notice this situation
 * by always comparing the at-positions of the reduce update with the
 * at-position of enter updates on the stack.
 * The stack is only written to the trace if the computation is aborted.
 */

typedef struct update {
  FileOffset at;
  FileOffset new;
} Update;

typedef struct entry {
  FileOffset at;
  FileOffset offset;
  FileOffset parent; /* or entered for anything but Forward */
} Entry;

#define StackpartSize 8000

typedef struct stackpart {
  struct stackpart *previous;
  Entry entry[StackpartSize];
} Stackpart;

static Stackpart *stack = NULL ;
static int stacktop = StackpartSize; 
  /* next free entry, initialisation important */


void hat_pushStack(FileOffset at, FileOffset offset, FileOffset new)
{
  if (stacktop >= StackpartSize) {
    Stackpart *newStackpart;
    newStackpart = malloc(sizeof(Stackpart));
    if (newStackpart == NULL) {
      fprintf(stderr,"No space for Hat stack.");
      exit (-1);
    }
    (*newStackpart).previous = stack;
    stack = newStackpart;
    stacktop = 0;
  }
  (*stack).entry[stacktop].at = at;
  (*stack).entry[stacktop].offset = offset;
  (*stack).entry[stacktop].parent = new;
  ++stacktop;
}

/* Returns at-position from top of stack; 
 * if top of stack is a Forward, then returns its parent;
 * result in network byte order; assumes stack != NULL; stack unchanged 
 */
FileOffset hat_topStack()
{
  int curStacktop = stacktop;
  Stackpart *curStack = stack;

  --curStacktop;
  if (curStacktop < 0) {
    curStack = (*curStack).previous; 
    curStacktop = StackpartSize-1;
    if (curStack == NULL) 
      return 0;
  }
  if ((*curStack).entry[curStacktop].offset == 1) /* is Forward */
    return (((*curStack).entry[curStacktop].parent)&hiddenMask);
  else
    return (htonl ((*curStack).entry[curStacktop].at));
}

/* returns at-position in host byte order; assumes stack != NULL */
FileOffset hat_popStack()
{
  if (stacktop == 0) {
    Stackpart *oldstack;
    oldstack = stack;
    stack = (*oldstack).previous;
    free(oldstack);
    stacktop = StackpartSize;
    if (stack == NULL) {
      fprintf(stderr,"Pop empty Hat stack.");
      exit (-1);
    }
  }
   --stacktop;
  return ((*stack).entry[stacktop].at);
}


/* writes top of stack to trace until at position equal given one */
void hat_writeStackUntil(FileOffset at)
{
  while (True) {
    FileOffset thisAt = hat_popStack();
    if (thisAt == at) 
      break;
    updateFileOffset(thisAt+(*stack).entry[stacktop].offset
                    ,((*stack).entry[stacktop].parent)&hiddenMask);
  }
}

/* Afterwards stack is unusable and even memory is not freed. */
void hat_dumpStack() {
  int i;

  while (stack != NULL) {
    for (i=0;i<stacktop;i++) {
      updateFileOffset((*stack).entry[i].at+(*stack).entry[i].offset
                      ,((*stack).entry[i].parent)&hiddenMask);
    }
    stacktop = StackpartSize;
    stack = (*stack).previous;
  }
}

/* Buffer for deferred updates */

#define BufferSize 4000
static Update buffer[BufferSize];
static int buffertop = 0;  /* next free one */

void hat_putInBuffer(FileOffset at, FileOffset new) {
  if (buffertop >= BufferSize) 
    hat_dumpBuffer();
  buffer[buffertop].at = at;
  buffer[buffertop].new = new;
  ++buffertop;
}

void hat_dumpBuffer() {

  int i;

  for (i=0;i<buffertop;i++) {
    fseek(HatFile,buffer[i].at,SEEK_SET);
    fwrite(&(buffer[i].new),sizeof(FileOffset),1,HatFile);
  }
  buffertop = 0;

  fseek(HatFile,0,SEEK_END);
}

void hat_enter(FileOffset at, FileOffset offset, FileOffset entry)
{
  hat_pushStack(at,offset,entry);
}

void hat_reduce(FileOffset at, FileOffset offset, FileOffset entry)
{
  hat_writeStackUntil(at);
  updateFileOffset(at+offset,entry);
}


/* Buffering scheme for sequential writing with updates */

#define WriteBufferNo 4  /* minimum: 1 */
#define WriteBufferSize 40000  /* minimum: size of largest node */

typedef struct writeBuffer {
  FileOffset begin;  /* in host byte order */
  int pos;
  byte content[WriteBufferSize];
} WriteBuffer;

WriteBuffer buffers[WriteBufferNo];
WriteBuffer *currentWriteBuffer = buffers;
int currentWriteBufferNo = 0;

static void 
initBuffers() 
{
  int i;

  for (i=0;i<WriteBufferNo;i++) {
    buffers[i].begin = 0;
    buffers[i].pos = 0;
  }
  currentWriteBuffer->begin = HeaderSize;
}

/* returns offset of newly started node in network byte order */
static FileOffset 
writeTag (int tag, int size) 
{
  FileOffset currentOffset;

  if (currentWriteBuffer->pos + size > WriteBufferSize) {
    currentOffset = currentWriteBuffer->begin + currentWriteBuffer->pos;
    currentWriteBufferNo = (currentWriteBufferNo+1) % WriteBufferNo;
    currentWriteBuffer = &(buffers[currentWriteBufferNo]);
    fwrite(currentWriteBuffer->content,sizeof(byte),currentWriteBuffer->pos
          ,HatFile);
    currentWriteBuffer->begin = currentOffset;
    currentWriteBuffer->pos = 0;
  }
  currentWriteBuffer->content[currentWriteBuffer->pos++] = tag;
  return (htonl(currentWriteBuffer->begin + currentWriteBuffer->pos - 1));
}

static void 
writeByte (int byte)
{
  currentWriteBuffer->content[currentWriteBuffer->pos++] = byte;
}

static void 
writeFileOffset (FileOffset offset) 
{
  offset &= hiddenMask;
  memcpy(&(currentWriteBuffer->content[currentWriteBuffer->pos])
        ,&offset,sizeof(FileOffset));
  currentWriteBuffer->pos += sizeof(FileOffset);
}

static void 
writeInt (int number) 
{
  number = htonl(number);
  memcpy(&(currentWriteBuffer->content[currentWriteBuffer->pos])
        ,&number,sizeof(int));
  currentWriteBuffer->pos += sizeof(int);
}

static void 
writeFloat (float number) 
{
  memcpy(&(currentWriteBuffer->content[currentWriteBuffer->pos])
        ,&number,sizeof(float));
  currentWriteBuffer->pos += sizeof(float);
}

static void 
writeDouble (double number) 
{
  memcpy(&(currentWriteBuffer->content[currentWriteBuffer->pos])
        ,&number,sizeof(double));
  currentWriteBuffer->pos += sizeof(double);
}

static void 
updateFileOffset (FileOffset at, FileOffset entry) 
{
  int i;

  if (buffers[(currentWriteBufferNo+1) % WriteBufferNo].begin > at) {
    hat_putInBuffer(at,entry);
  } else {
    for (i=0;i<WriteBufferNo;i++) {
      if (at >= buffers[i].begin && at < buffers[i].begin + buffers[i].pos) {
        memcpy(&(buffers[i].content[at - buffers[i].begin])
              ,&entry,sizeof(FileOffset));
        break;
      }
    } 
  }
}

/* size of string when stored in hat file (including length information) */
static int
stringSize(char *s)
{
  int length = strlen(s);
  if (length < 255)
    return (length+1);
  else
    return (length+3);
}

static void
writeString(char *s)
{
  int length = strlen(s);
  if (length < 255)
    writeByte(length);
  else {
    writeByte(255);
    writeByte(length>>8); /* high byte first */
    writeByte(length);
  }
  memcpy(&(currentWriteBuffer->content[currentWriteBuffer->pos]),s,length);
  currentWriteBuffer->pos += length;
}

static void
dumpBuffers()
{
  int i;
  fseek(HatFile,0,SEEK_END);
  for (i=0;i<WriteBufferNo;i++) {
    currentWriteBufferNo = (currentWriteBufferNo + 1) % WriteBufferNo;
    fwrite(buffers[currentWriteBufferNo].content,sizeof(byte)
          ,buffers[currentWriteBufferNo].pos,HatFile);
  }
}


/* Store children of Hidden nodes in linked list of ListCons nodes 
 * The child pointer of every Hidden node unequal NULL is kept in 
 * a HiddenChildren buffer. That buffer is written at the end.
 * The buffer serves both for *reading* the current and writing a 
 * new child pointer.
 * This avoids having to read the trace; the assumption is that
 * there are few Hidden nodes with children, hence the buffer is small.
 */

typedef struct hcEntry {
  FileOffset at;
  FileOffset child;
} HCEntry;

#define HCPartSize 8000

typedef struct hcPart {
  HCEntry entry[HCPartSize];
  struct hcPart *previous;
  int top;
} HCPart;


static HCPart *hiddenChildrenSet = NULL ;

static void initHiddenChildrenSet()
{
  hiddenChildrenSet = malloc(sizeof(HCPart));
  if (hiddenChildrenSet == NULL) {
    fprintf(stderr,"No space for Hat Children set.");
    exit (-1);
  }
  hiddenChildrenSet->top = 0;
  hiddenChildrenSet->previous = NULL;
}

/* Can be invoked any time. The hiddenChildrenSet data structure keeps alive. */
static void dumpHiddenChildrenSet() 
{
  int i;
  HCPart *curHC = hiddenChildrenSet;
  
  while (curHC != NULL) {
    for (i=0;i<curHC->top;i++) {
      updateFileOffset(curHC->entry[i].at,curHC->entry[i].child);
    }
    curHC = curHC->previous;
  }
}

void recordChild(FileOffset hidden,FileOffset child)
{
  /* basically: read child pointer of Hidden node, write ListCons with
   * child and read old child pointer, overwrite child pointer of Hidden
   * node.
   * The difficulty is that the child pointer is (hopefully) in one of
   * several buffers, avoiding file read and write access.
   */
  FileOffset listTail,newCons;
  int i;
  HCPart *curHC;
  FileOffset at = ntohl(hidden&hiddenMask) + 1 + 2*sizeof(FileOffset);

  for (curHC = hiddenChildrenSet; curHC != NULL; curHC = curHC->previous)
    for (i = 0; i<curHC->top;i++)
      if (curHC->entry[i].at == at)
        goto found;

  /* not found in set, need to add an entry */

  if (hiddenChildrenSet->top<HCPartSize) {
    /* add in first part */
    curHC = hiddenChildrenSet;
    i = curHC->top++;
  } else {
    /* add another part at front */
    curHC = malloc(sizeof(HCPart));
    if (curHC == NULL) {
      fprintf(stderr,"No space for Hat Children set.");
      exit (-1);
    }
    curHC->previous = hiddenChildrenSet; 
    hiddenChildrenSet = curHC;
    curHC->top = 1;
    i = 0;
  }
  curHC->entry[i].at = at;
  curHC->entry[i].child = LISTEND;

 found:

  /* read current child tail of Hidden node */
  listTail = curHC->entry[i].child;
 
  /* create extended children list */
  newCons = mkListCons(child,listTail);
    
  /* overwrite child tail pointer of Hidden node */
  curHC->entry[i].child = newCons;
}

