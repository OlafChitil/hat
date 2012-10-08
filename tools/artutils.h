#ifndef _ARTUTILS_H
#define _ARTUTILS_H

#include "art.h"
#if defined(sparc)
#include <sys/byteorder.h>
#endif

extern FileOffset errorRoot, errorMsg, remoteStartNode;
extern FileOffset q_position;
extern FILE	*HatFileRandom, *HatFileSeq, *OutputFile, *BridgeFile;
extern char*	progname;			/* browser program name */
extern unsigned	filesize, outputsize;
extern Bool	hat_interrupted;

typedef enum {
  Literal, Variable, Constructor, ConstrFields, Abstract
} AtomType;

typedef struct {
  char* srcname;
  int   line;
  int   column;
  int   lineend;
  int   columnend;
} SrcRef;

typedef struct {
  char* idname;
  char* modname;
  char* srcname;
  char  fixity;
  char  arity;
  int   defnline;
  int   defncolumn;
  int   defnlineend;
  int   defncolumnend;
  Bool  isTraced;
  AtomType  atomtype;
} Ident;

void		finalise	(void);
FILE*		openFile	(char* base, char* ext);
int		sizeFile	(char* base, char* ext);
int		freadAt		(FileOffset fo, void* ptr
				,int size, int nmemb, FILE* stream);
FileOffset	readFO		(void);
char*		readString	(void);
int		q_fread		(void* ptr, int size, int nmemb, FILE* stream);
FileOffset	q_readFO	(void);
char*		q_readString	(void);
char		q_peek		(void);
char		q_tag		(void);
void		q_init		(void);
void		readModuleAt	(FileOffset fo, char** modname, char** srcname
				,Bool* traced);
Ident*		readAtomAt	(FileOffset fo);
Ident*		readValueAt	(FileOffset fo);
SrcRef*		readSRAt	(FileOffset fo);
FileOffset	q_skipNode	(char tag);
FileOffset	currentfilepos	(void);

FileOffset	readTraceAt	(FileOffset fo, char** expr, SrcRef** sr
				,int* infix,int followHidden,int depth);
char*		infixPrint	( char* str1, int arg1
				, char* strfn, int fn
				, char* str2, int arg2);

#define noFixity      ((char)3)
#define isInfix(fix)  ((fix%4)!=3)
#define isInfixL(fix) ((fix%4)==2)
#define isInfixR(fix) ((fix%4)==1)
#define isInfixN(fix) ((fix%4)==0)
#define priority(fix) (fix/4)

/* prototypes for common ui */
void		openHatFile	(char* prog, char* arg);
void        closeHatFile (void);
FileOffset	getBridgeValue	(void);
FileOffset	getErrorLoc	(void);
char*		errorMessage	(void);
char*		versionNumber	(void);
int		getNodeType	(FileOffset fo);
FileOffset	parentNode	(FileOffset fo);
char*		getNm		(FileOffset fo);
char*		getNmMod	(FileOffset fo);
int		getFixity	(FileOffset fo);
Bool		isLiteral	(FileOffset fo);
Bool		isConstructor	(FileOffset fo);
Bool		isConstrFields	(FileOffset fo);
Bool		isLambda	(FileOffset fo);
Bool		isDoLambda	(FileOffset fo);
char*		identName	(Ident* id);
char*		identModName	(Ident* id);
char*		identSrcFile	(Ident* id);
int		identFixity	(Ident* id);
int		identArity	(Ident* id);
int		identDefnLine	(Ident* id);
int		identDefnCol	(Ident* id);
int		identDefnLineEnd(Ident* id);
int		identDefnColEnd	(Ident* id);
Bool		identIsTraced	(Ident* id);
int		getExpArity	(FileOffset fo);
FileOffset	getExpArg	(FileOffset fo, int n);
FileOffset      peekExpArg      (FileOffset fo, int n);
FileOffset	getFieldLabel	(FileOffset fo, int n);
FileOffset	getSrcRef	(FileOffset fo);
FileOffset	getDefnRef	(FileOffset fo);
FileOffset	peekTrace	(FileOffset fo);
FileOffset	getResult	(FileOffset fo, Bool stopAtHidden);
FileOffset	getResultNoCycle(FileOffset fo, Bool stopAtHidden);
FileOffset	peekResult	(FileOffset fo);

char*		srFile		(SrcRef* sr);
int		srLine		(SrcRef* sr);
int		srColumn	(SrcRef* sr);
int		srLineEnd	(SrcRef* sr);
int		srColumnEnd	(SrcRef* sr);


/* prototypes for other tools */
void		ctrlC			(int sig);
int		fileoffset_compare	(FileOffset i, FileOffset j);

#endif
