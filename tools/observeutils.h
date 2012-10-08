#ifndef _OBSERVEUTILS_H
#define _OBSERVEUTILS_H

#include "art.h"
#include "finitemap.h"

extern Bool hat_interrupted;
void ctrlC (int);

/* Some finite maps (i.e. lookup tables). */
extern FiniteMap mapAtom2Info, mapExp2Atom, mapContext2Atom;

/* mapAtom2Info :: FileNode -> Info */
typedef struct {
  FileOffset    node;
  unsigned char arity;
  char*         var;
} Info;
/* mapExp2Atom, mapContext2Atom :: FileNode -> Atom */
typedef struct {
  FileOffset    atom;
  unsigned char arity;
} Atom;


/* Insert into finite maps */
void		insert_mapAtom2Info	(FileOffset atom, char* var
					,unsigned char arity);
void		insert_map2		(FiniteMap map2, FileOffset exp
					,FileOffset atom, unsigned char arity);
/* Routines visible from Haskell world */
void		setObserveContext	(Bool hascontext, Bool rec, int arity
					,char* caller);
FileOffset	lookForFirstApp		(char* callee);
FileOffset	lookForFirstSrc		(int line, int col, char* module);
FileOffset	nextObservation		(FileOffset seen);

/* Internal routines */
FileOffset	varSearch		(void);
FileOffset	srcSearch		(void);
void		searchCAFResult		(FileOffset caf, FileOffset result
					,unsigned char arity
					,FileOffset mostRecentHidden);

#endif
