/*
 *  nodelist.h
 *  
 *
 *  Created by Thomas Davie on Thu Dec 04 2003.
 *
 */

#include "animnode.h"

#ifndef _NODELIST
#define _NODELIST

#ifndef false
#define false 0
#endif
#ifndef true
#define true !false
#endif

typedef struct nodelist_s
{
    node *item;
    struct nodelist_s *next;
} nodelist;

nodelist *nodelist_newEmptyList(void);
void nodelist_delete(nodelist *list);
void nodelist_add(nodelist *list, node *newItem);
void nodelist_push(nodelist *list, node *newItem);
node* nodelist_pop(nodelist *list);
char nodelist_contains (nodelist *list, unsigned long nodeOffset);
node* nodelist_retrieve(nodelist *list, unsigned long nodeOffset);

#endif
