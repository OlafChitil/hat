#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "nontermutils.h"

/* nodecount holds the number of node accesses in this run of the program */
long nodecount; 

/* This function searches for the root of the ART graph. The function
 * scans through the file linearly, looking for an ExpConstDef node
 * which points to an Atom called 'main'. This is (sort of) the root of
 * the graph. It's good enough, anyway
 */
FileOffset 
getRootNode( void )
{
  FileOffset curr, root, atom;
  int err; char tag;
  Ident *atomvariable;

  q_position = 0x10;
  fseek(HatFileSeq,q_position,SEEK_SET);
  root = 0x00;
  while (root == 0x00) {
    nodecount++;
    curr = q_position;
    err = q_fread(&tag,sizeof(unsigned char),1,HatFileSeq);
    if (tag == ExpConstDef) {
      q_readFO(); // throw away the parent
      q_readFO(); // throw away the result
      atom = q_readFO();
      atomvariable = readAtomAt(atom); 
      if ( strcmp( atomvariable->idname, "main") == 0 ) {
	root = curr;
      }
      free(atomvariable);
    }
    q_skipNode(tag);
  }
  return root;
}

/* take a FileOffset pointing to a node and get the node that
 * immediately follows it in the ART file. This uses q_skipNode, which
 * is a Hat function that skips a filenode, but which requires a tag
 * giving the node type. The nextFileNode function mainly just gets that
 * tag.
 */
FileOffset 
nextFileNode( FileOffset fo )
{
  int err; char tag;

  if ( q_position != fo ) {
    q_position = fo; 
    fseek(HatFileSeq,q_position,SEEK_SET);
  }

  err = q_fread(&tag,sizeof(unsigned char),1,HatFileSeq);
  q_skipNode(tag);

  if (q_position <= filesize ) {
    return q_position;
  } else {
    return 0; 
  }
}

/* get the pointer to an Expression Application's function. This just
 * involves using readFO to pull out the correct pointer.
 */
FileOffset 
getFuncPtr( FileOffset fo )
{
  char *id, c;
  FileOffset ptr = 0;
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  //fprintf(stderr, "considering 1st expression at %x\n", fo);
  switch (lower5(c)) {
    case ExpApp:
	if (hasSrcPos(c)) { readFO(); }
	readFO();				/* skip parent */
        readFO();				/* skip result */
        fo = readFO();				/* value app */
	freadAt(fo,&c,sizeof(char),1,HatFileRandom);
        //fprintf(stderr, "considering 2nd expression at %x\n", fo);
	readFO();                             	/* skip srcref */
	readFO();                         	/* skip parent */
        ptr = readFO();			  	/* function ptr */
	break;
    default: 
        fprintf(stderr, "%s: expected Expression Application at 0x%x\n",
		progname,fo);
        exit(1);
  }
  //fprintf(stderr, "pointer: %x\n", ptr);
  return ptr;
}

/* take a FileOffset pointing to a function atom and get its name */
char *
getFuncNm( FileOffset fo )
{
  char *name;
  Ident *func;
  func = readAtomAt(fo);
  name = func->idname;
  free(func);
  return(name);
}

/* take a FileOffset pointing to a function atom and get the module name */
char *
getFuncMod(FileOffset fo)
{
  char *mod;
  Ident *func;
  func = readAtomAt(fo);
  mod = func->modname;
  free(func);
  return(mod);
}



/* initialise the counter variable for the number of accesses to the ART
 * nodes
 */
void initialiseCount(void)
{
  nodecount = 0;
}

/* increment the node-access count by the value of the argument */
void incCount(long x)
{
  nodecount = nodecount + x;
}

/* get the node-access count */
long getCount(void)
{
  return nodecount;
}

/* This function is used by getImmediateExpArg, to follow argument
 * pointers. It follows ExpConstUse pointers, but nothing else.
 * Otherwise, it simply returns the pointer value.
 * I could probably fold this into the getImmediateExpArg function body.
 */

FileOffset
getResultRestricted(FileOffset fo)
{
  char c;
  FileOffset ptr;
  if (fo<=DoLambda) return fixInterrupt(fo);
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  switch (lower5(c)) {
    case ExpConstUse:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        ptr = readFO();				/* CAF */
	return getResultRestricted(ptr);
        break;
    default:
	return fo;
	break;
  }
}

/* This is a modified version of the Hat function getExpArg. The
 * original function takes a filenode, and gets the value of a
 * particular agument. However, it also followed some of the argument
 * pointers, specifically the pointers for Expression Applications. This
 * had some odd results for black-hat, so I've stopped it following most
 * pointers. The getResultRestricted function takes the place of
 * getResult, and only follows a small number of pointer-types.
 */
FileOffset
getImmediateExpArg (FileOffset fo, int n)
{
  char c;
  int i=0;
  FileOffset ptr;

  nodecount++;

  //fprintf(stderr,"getExpArg 0x%x\n",fo);
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  switch (lower5(c)) {
    case ExpApp:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        readFO();				/* skip result */
        ptr = readFO();				/* fun/constructor */
        if (n==0) return getResultRestricted(ptr);
        myfread(&c,sizeof(char),1,HatFileRandom);	/* get arity */
        if (n<=c) {
          for (i=1; i<n; i++) readFO();		/* skip other args */
          ptr = readFO();			/* get n'th arg */
          return getResultRestricted(ptr);
        } else
          return fo;
        break;
    case ExpValueApp:
        if (hasSrcPos(c)) { readFO(); }		/* skip usage position */
        readFO();				/* skip parent */
        ptr = readFO();				/* fun/constructor */
        if (n==0) return ptr;	/* no result-chain - fun is already an atom */
        myfread(&c,sizeof(char),1,HatFileRandom);	/* get arity */
        if (n<=c) {
          for (i=1; i<n; i++) readFO();		/* skip other args */
          ptr = readFO();			/* get n'th arg */
          return getResultRestricted(ptr);
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
        return getResultRestricted(ptr);
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
        myfread(&c,sizeof(char),1,HatFileRandom);	/* get arity */
        if (n<=c) {
          for (i=0; i<c; i++) readFO();		/* skip binder labels */
          for (i=1; i<n; i++) readFO();		/* skip other bindees */
          ptr = readFO();			/* get n'th bindee */
          return getResultRestricted(ptr);
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


  
/* peekResultMod is a modified version of peekResult from Hat, which takes a
 * single step down the result chain. It has been rewritten, to avoid
 * some expressions appearing twice in the search path
 *
 */

FileOffset
peekResultMod (FileOffset fo)
{
  char c;
  FileOffset result;
  
  nodecount++;

  //HIDE(fprintf(stderr,"peekResult 0x%x\n",fo);)
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
        break;
    case ExpConstUse:
    case ExpProjection:
        if (hasSrcPos(c)) { readFO(); }		/* skip use position */
        readFO();				/* skip parent */
        result = readFO();			/* get result */
        break;
    case ExpConstDef:
        readFO();				/* skip parent */
        result = readFO();			/* get result */
        break;
    case ExpForward:
    case ExpDoStmt:
        result = readFO();			/* get result */
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
        //HIDE(fprintf(stderr,"getResult: result is itself\n");)
        result = fo;
        break;
    case ExpHidden:
        readFO();				/* skip parent */
        result = readFO();			/* get result */
        break;
    case AtomVariable:
    case AtomConstructor:
    case AtomAbstract:
    default:
        return 0;
        break;
  }

  if (result<=DoLambda) return fixInterrupt(result); 
  freadAt(result,&c,sizeof(char),1,HatFileRandom);
  switch (lower5(c)) {
    case ExpConstDef:
	return peekResultMod(result);
        break;
    case ExpProjection:
        return peekResultMod(result);
	break;
    default:
	return fixInterrupt(result); 
	break;
  }
}


/* The getResultHT function is a modified version of getResult that
 * uses a hashtable to reduce the stupid amount of time it takes if you try
 * and get the result of a function in a non-terminating file.  
 */
FOHashTable *hashTable, *hashTableNew = NULL;

static FileOffset mostRecentHidden=0;

/* getResultHt starts to get a result value for a function. Basically
 * all it does is initialise the mostRecentHidden variable for catching
 * rogue Hidden loops
 */
FileOffset
getResultHT (FileOffset fo, Bool stopAtHidden)
{
  FileOffset result;
  //fprintf(stderr, "starting on 0x%x\n", fo);
  if (fo==mostRecentHidden) return fo;
  mostRecentHidden=0;
  result = getResultNoCycleHT(fo,stopAtHidden);
  //fprintf(stderr, "result: %x\n", result);
  return result;
}

/* getResultNoCycleHT actually does the leg-work of getting the function
 * results. The old getResult used lots of calls like this to express
 * recursion:
 *    return getResult(foo, bar);
 * In order that the results are actually entered into the hash table,
 * these have been replaced with:
 *    returnval = getResult(foo, bar);
 * the value is then entered into the hash table once it has been
 * aquired. The only problem is that, while the original version was
 * probably compiled into constant-space iteration, this version can't be. So,
 * with very large ART files, you will eventually end up with a huge number
 * of calls to getResultNoCycleHT piling up on the stack, which may 
 * break black-hat / hat-nonterm in some horrible non-reproducible manner. 
 * However, this hasn't happened to me, yet.
 */
FileOffset
getResultNoCycleHT(FileOffset fo, Bool stopAtHidden)
{
  char c;
  FileOffset result, returnval;

  nodecount++;
   
  if (hashTable == NULL) hashTable = foInitTable(10000);
  if ((returnval = foHashRetrieve(hashTable, fo)) != 0) return returnval;
  
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
        if (result==fo) return fo;
        else if (result<=DoLambda) return fixInterrupt(fo);
        else returnval = getResultNoCycleHT(result,False);
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
        else returnval = getResultNoCycleHT(result,False);
						/* follow ExpConstDef pointer */
        break;
    case ExpConstDef:
        readFO();				/* skip parent */
        result = readFO();			/* get result */
        if (result<=DoLambda) return fixInterrupt(fo);
        returnval = getResultNoCycleHT(result,False);
        break;
    case ExpForward:
        returnval = getResultNoCycleHT(readFO(),stopAtHidden);
        break;					/* continue to detect Hidden */
    case ExpDoStmt:
        returnval = getResultNoCycleHT(readFO(),False);	/* get result */
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
        return fo;
        break;
    case ExpHidden:
        if (stopAtHidden) return fo;
	// instead of returning the file offset of the hidden in the 
	// case of a loop, return the Entered filePointer
        else if (fo==mostRecentHidden) return Entered; 
        else {
          mostRecentHidden = fo;		/* keep, to detect a loop */
          readFO();				/* skip parent */
          result = readFO();			/* get result */
          if (result==fo) return fo;
          else if (result<=DoLambda) return fixInterrupt(fo);
          else returnval = getResultNoCycleHT(result,False);
        }
        break;
    case AtomVariable:
    case AtomConstructor:
    case AtomAbstract:
    default:
        returnval = 0;
        break;
  }

  foHashInsert(hashTable, fo, returnval);
  return returnval;
}


/* Functions for manipulating a hash table 
 *
 * The hash table is a simple bucket-table, with each table item
 * corresponding to a linked-list of entries. 
 *
 * The hash-table datatype is in the header file, nontermutils.h
 */

/* Initialise the hash table */
FOHashTable *
foInitTable ( unsigned long size ) 
{
  unsigned long i;
  FOHashTable *table;
  table = (FOHashTable*)malloc(sizeof(FOHashTable));
  table->size = size;
  table->ht = (FOHashItem**)malloc( sizeof(FOHashItem *) * table->size);
  for (i=0; i < table->size; i++) table->ht[i] = NULL;
  return(table);
}

/* Deallocate the hash table */
void
foDestroyTable (FOHashTable *table)
{
  FOHashItem *item;
  unsigned long i;
  for (i=0; i < table->size; i++) {
    item = table->ht[i];
    while (item != NULL) {
      free(item);
      item = item->next;
    }
  }
  free(table);
}

/* insert an item into the hash table */
void
foHashInsert ( FOHashTable *table, FileOffset key, FileOffset entry )
{
  FOHashItem *item;
  unsigned long offset;
  offset = foHashFunc(table, key);

  if (table->ht[offset] == NULL) {
    item = (FOHashItem*)malloc(sizeof(FOHashItem));
    table->ht[offset] = item;
  } else {
    item = table->ht[offset];
    while (item->next != NULL) {
      item = item->next;
    }
    item->next = (FOHashItem*)malloc(sizeof(FOHashItem));
    item = item->next;
  }

  item->key = key;
  item->entry = entry;
  item->next = NULL;
}
  
/* make a hash-table key */
unsigned long
foHashFunc ( FOHashTable *table, FileOffset key )
{
  return( key % table->size );
}

/* retrieve an item from the hash table, or fail and return 0 */
FileOffset
foHashRetrieve (FOHashTable *table, FileOffset key)
{
  unsigned long offset;
  FOHashItem *item;
  FileOffset result;
  offset = foHashFunc( table, key );
  item = table->ht[offset];
  while (item != NULL) {
    if (item->key == key) return(item->entry);
    item = item->next;
  }
  return(0);
}

