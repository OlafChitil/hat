/*
 *  hatNodeHash.h
 *  
 *
 *  Created by Thomas Davie on Fri Nov 21 2003.
 *  Copyright (c) 2003 Thomas Davie. All rights reserved.
 *
 */

#include "animnode.h"
#include "nodelist.h"

#ifndef _NODE_HASH
#define _NODE_HASH

#define HASH_SIZE 100

typedef struct nodehash_s
{
    nodelist **hashLists;
    unsigned long filesize;
    FILE *hatFile;
} nodehash;

nodehash* nodehash_newByOpeningFile(const char *filename);
void nodehash_delete(nodehash *theHash);

node* nodehash_getNode(nodehash *theHash, unsigned long fileOffset);

#endif
