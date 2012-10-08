#ifndef _PARENTSET_H
#define _PARENTSET_H

#include "art.h"

#define CHUNK_OTHER_PARENTS 10

typedef struct {
    FileOffset  trueParent;
    int         numOthers;
    int         sizeOthers;
    FileOffset* others;
} ParentSet;

/* prototypes */
ParentSet*	newParentSet	(FileOffset p);
void		freeParentSet	(ParentSet* ps);
void		extendParentSet	(ParentSet* ps, FileOffset p);
FileOffset	trueParent	(ParentSet* ps); 
Bool		elemParentSet	(FileOffset p, ParentSet* ps);

#endif
