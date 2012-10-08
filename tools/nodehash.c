/*
 *  hatNodeHash.c
 *  
 *
 *  Created by Thomas Davie on Fri Nov 21 2003.
 *  Copyright (c) 2003 Thomas Davie. All rights reserved.
 *
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>

#include "nodehash.h"

struct stat statbuf;

void freeItems(nodelist *theList);
long hash(unsigned long offset);

nodehash* nodehash_newByOpeningFile(const char *filename)
{
    char *header = (char*)malloc(10*sizeof(char));
    long listNumber;
    int  err;
    nodehash *theHash;
    
    theHash = (nodehash *)malloc(sizeof(nodehash));
    theHash->filesize = 0;
    
    stat(filename, &statbuf);
    theHash->filesize = statbuf.st_size;
    theHash->hatFile = fopen(filename, ("r"));
    if (!theHash->hatFile)
    {
        fprintf(stderr, "cannot open trace file %s\n", filename);
        free(theHash);
        return 0L;
    }
    
    // Version check, by MW
    err = fread(header, sizeof(char), 8, theHash->hatFile);
    if (err!=8)
    {
        fprintf(stderr, "hat-anim (error): file %s is too short\n", filename);
        free(theHash);
        return 0L;
    }
    if (strncmp(header, "Hat",3))
    {
        fprintf(stderr, "hat-anim (error): file %s\n", filename);
        fprintf(stderr, "   does not appear to be a Hat archive.  Quitting.\n");
        free(theHash);
        return 0L;
    }
    if (strncmp(header+3,FILEVERSION,4))
    {
        fprintf(stderr,"hat-anim (warning): file %s\n", filename);
        fprintf(stderr,"   appears to be a Hat archive in format %s\n", header + 3);
        fprintf(stderr,"   but this tool deals with format version %s\n", FILEVERSION);
        fprintf(stderr,"   I'm continuing, but there may be unexpected errors.\n");
    }
    
    fseek(theHash->hatFile,0,SEEK_SET);	/* reset to beginning of file */
    free(header);
    
    
    theHash->hashLists = (nodelist **)malloc(sizeof(nodelist *) * HASH_SIZE);
    for (listNumber = 0; listNumber < HASH_SIZE; listNumber++)
    {
        theHash->hashLists[listNumber] = nodelist_newEmptyList();
    }
    
    return theHash;
}

void nodehash_delete(nodehash *theHash)
{
    long listNumber;
    
    fclose(theHash->hatFile);
    
    for (listNumber = 0; listNumber < HASH_SIZE; listNumber++)
    {
        freeItems(theHash->hashLists[listNumber]);
        nodelist_delete(theHash->hashLists[listNumber]);
    }
    free(theHash->hashLists);
    free(theHash);
}

node* nodehash_getNode(nodehash *theHash, unsigned long fileOffset)
{
    node* theNode = nodelist_retrieve(theHash->hashLists[hash(fileOffset)], fileOffset);
    
    if (theNode == NULL)
    {
        theNode = readNode(theHash->hatFile, fileOffset);
        nodelist_add(theHash->hashLists[hash(fileOffset)], theNode);
    }
    
    return theNode;
}

void freeItems(nodelist *theList)
{
    if (theList->item != 0L)
    {
        free(theList->item);
        freeItems(theList->next);
    }
}

long hash(unsigned long offset)
{
    return ((int)(offset / 2)) % HASH_SIZE;
}

