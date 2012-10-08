/*
 *  nodelist.c
 *  
 *
 *  Created by Thomas Davie on Thu Dec 04 2003.
 *
 */

#include "nodelist.h"

/**
 * Creates a new list with nothing in it.
 *
 * @return A new list containing no elements.
 */
nodelist *nodelist_newEmptyList(void)
{
    nodelist *newList = (nodelist *)malloc(sizeof(nodelist));
    newList->next = NULL;
    newList->item = NULL;
    
    return newList;
}

void nodelist_delete (nodelist *list)
{
    node *temp = nodelist_pop(list);
    if (temp)
    {
        if (list->item)
        {
            nodelist_delete(list);
        }
    }
}

void nodelist_add(nodelist *list, node *newItem)
{
    nodelist_push(list, newItem);
}

/**
 * "nodelist_pushes" and item onto the list - i.e. adds it to the head.
 * 
 * @param list    The list to nodelist_push the item onto.
 * @param newItem A node to add to the head of the list.
 */
void nodelist_push(nodelist *list, node *newItem)
{
    nodelist *newNode = (nodelist *)malloc(sizeof(nodelist));
    newNode->item = list->item;
    newNode->next = list->next;
    list->item = newItem;
    list->next = newNode;        
}

/**
 * "pops" an item from the list - i.e. removes the first item and returns it.
 *
 * @param list The list to return.
 */
node* nodelist_pop (nodelist *list)
{
    node *returnVal = 0L;
    nodelist *newNext;
    
    if (list->item)
    {
        returnVal = list->item;
        list->item = list->next->item;
        newNext = list->next->next;
        free(list->next);
        list->next = newNext;
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
char nodelist_contains (nodelist *list, unsigned long nodeOffset)
{
    if (list->item != 0L)
    {
        if (list->item->offset == nodeOffset)
        {
            return true;
        }
        else
        {
            return nodelist_contains(list->next, nodeOffset);
        }
    }
    
    return false;
}

node* nodelist_retrieve(nodelist *list, unsigned long nodeOffset)
{
    if (list->item != 0L)
    {
        if (list->item->offset == nodeOffset)
        {
            return list->item;
        }
        else
        {
            return nodelist_retrieve(list->next, nodeOffset);
        }
    }
    
    return 0L;
}
