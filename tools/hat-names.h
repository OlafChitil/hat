#ifndef _HAT_NAMES_H
#define _HAT_NAMES_H

#include <unistd.h>
#include <stdlib.h>
#include "art.h"

#define MAX_FILENAME 1024

/* We are interested in distinguishing three different kinds of identifier:
 *    variables defined at the top level
 *    variables defined in a local scope
 *    data constructors
 */
typedef enum {
    TopId, LocalId, Construct
} idkind;

/* These are the things we are interested in for each identifier:
 *   - its name, (+ info: defining module, srcloc, fixity, etc.)
 *   - its kind (local, global, constructor)
 *   - its arity
 *   - how many times it was applied at its full arity and gave a result
 *   - how many times it was applied at its full arity but did not give a result
 *   - how many times it was applied at its full arity but never demanded
 */
typedef struct {
    char* name;
    FileOffset thispos;
    idkind kind;
    unsigned char arity;
    int uses;		/* number of evaluated applications */
    int pending;	/* number of entered but unevaluated applications */
    int thunks;		/* number of applications never demanded */
} item;

struct _defn {
    FileOffset atom;
    unsigned char arity;
    struct _defn *next;
};
typedef struct _defn defn;

/* Return the data to Haskell-land */
FileOffset itemIdent   (item* i);
int        itemArity   (item* i);
int        itemUses    (item* i);
int        itemPending (item* i);
int        itemThunks  (item* i);
item*      getItemPtr  (item **arr, int n);
item**     getGlobals  (void);
item**     getLocals   (void);
item**     getConstrs  (void);
int        getGlobalsN (void);
int        getLocalsN  (void);
int        getConstrsN (void);


/* do the work */
void	collateIdents	(void);
void	q_oneNode	(void);
void	countCAFResult	(FileOffset caf
			,FileOffset value
			,defn *def
			,unsigned char arity
			,FileOffset mostRecentHidden);

#endif
