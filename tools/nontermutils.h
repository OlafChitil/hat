#ifndef __NONTERMUTILS_H
#define __NONTERMUTILS_H
#include "art.h"
#include "artutils.h"


// filenode manipulators
FileOffset getRootNode( void );
FileOffset nextFileNode( FileOffset fo );

// function node manipulators
FileOffset getFuncPtr( FileOffset fo );
char * getFuncNm( FileOffset fo );
char * getFuncMod(FileOffset fo);

// visited-node count manipulators
void initialiseCount(void);
void incCount(long x);
long getCount(void);

// modifications of Hat functions 
#define fixInterrupt(fo)	(interrupt && fo==Entered ? Interrupted : fo)
FileOffset getImmediateExpArg (FileOffset fo, int n);
FileOffset peekResultMod (FileOffset fo);
FileOffset getResultHT (FileOffset fo, Bool stopAtHidden);
FileOffset getResultNoCycleHT(FileOffset fo, Bool stopAtHidden);

// Hash table implementation
typedef struct FOHashItem FOHashItem;

struct FOHashItem {
  FileOffset key, entry;
  FOHashItem *next;
};

typedef struct FOHashTable {
  unsigned long size;
  FOHashItem **ht;
} FOHashTable;

FOHashTable *foInitTable ( unsigned long size );
void foDestroyTable (FOHashTable *table);
void foHashInsert ( FOHashTable *table, FileOffset key, FileOffset entry );
unsigned long foHashFunc ( FOHashTable *table, FileOffset key );
FileOffset foHashRetrieve (FOHashTable *table, FileOffset key);

extern Bool interrupt;

#endif
