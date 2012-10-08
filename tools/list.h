/*
 *  list.h
 *  
 *
 *  Created by Thomas Davie on Fri Jan 22 2004.
 *
 */

#ifndef _LIST
#define _LIST

#ifndef false
#define false 0
#endif
#ifndef true
#define true !false
#endif

typedef struct list_s
{
    void *item;
    struct list_s *next;
} list;

list *list_newEmptyList(void);
void list_delete(list *theList);
void list_add(list *theList, void *newItem);
void list_remove(list **theList, void *theItem);
void list_remove_string(list **theList, char *theItem);
void list_push(list *theList, void *newItem);
void* list_pop(list *theList);
char list_contains(list *theList, void *searchItem);
char list_contains_string(list *theList, char *searchItem);

#endif
