/*
 *  list.c
 *  
 *
 *  Created by Thomas Davie on Fri Jan 22 2004.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include "list.h"

/**
 * Creates a new list with nothing in it.
 *
 * @return A new list containing no elements.
 */
list *list_newEmptyList(void)
{
    list *newList = (list *)malloc(sizeof(list));
    newList->next = NULL;
    newList->item = NULL;
    
    return newList;
}

void list_delete(list *theList)
{
    void *temp = list_pop(theList);
    if (temp)
    {
        if (theList->item)
        {
            list_delete(theList);
        }
    }
}

void list_add(list *theList, void *newItem)
{
    list_push(theList, newItem);
}

void list_remove(list **theList, void *theItem)
{
    if ((*theList)->item != 0L)
    {
        if ((*theList)->item == theItem)
        {
            *theList = (*theList)->next;
            if ((*theList)->next != 0L)
            {
                list_remove(&((*theList)->next), theItem);
            }
        }
        else
        {
            if ((*theList)->next != 0L)
            {
                list_remove(&((*theList)->next), theItem);
            }
        }
    }
}

void list_remove_string(list **theList, char *theItem)
{
    if ((*theList)->item != 0L)
    {
        if (!strcmp((char *)(*theList)->item, theItem))
        {            
            *theList = (*theList)->next;
            if ((*theList)->next != 0L)
            {
                list_remove_string(&((*theList)->next), theItem);
            }
        }
        else
        {
            if ((*theList)->next != 0L)
            {
                list_remove_string(&((*theList)->next), theItem);
            }
        }
    }
}

/**
 * "pushes" and item onto the list - i.e. adds it to the head.
 * 
 * @param list    The list to nodelist_push the item onto.
 * @param newItem A node to add to the head of the list.
 */
void list_push(list *theList, void *newItem)
{
    list *newNode = (list *)malloc(sizeof(list));
    newNode->item = theList->item;
    newNode->next = theList->next;
    theList->item = newItem;
    theList->next = newNode;        
}

/**
 * "pops" an item from the list - i.e. removes the first item and returns it.
 *
 * @param list The list to return.
 */
void* list_pop(list *theList)
{
    void *returnVal = 0L;
    list *newNext;
    
    if (theList->item)
    {
        returnVal = theList->item;
        theList->item = theList->next->item;
        newNext = theList->next->next;
        free(theList->next);
        theList->next = newNext;
    }
    
    return returnVal;
}

/**
 * Checks if a node with the offset supplied is in the list.
 *
 * @param list       The list to search.
 * @param nodeOffset The offset to search for.
 * @return 0 iff the item is not in the list.
 */
char list_contains (list *theList, void *searchItem)
{
    if (theList->item != 0L)
    {
        if (theList->item == searchItem)
        {
            return true;
        }
        else
        {
            return list_contains(theList->next, searchItem);
        }
    }
    
    return false;
}

char list_contains_string (list *theList, char *searchItem)
{
    if (theList->item != 0L)
    {
        if (!strcmp((char *)theList->item, searchItem))
        {
            return true;
        }
        else
        {
            return list_contains_string(theList->next, searchItem);
        }
    }
    
    return false;
}

