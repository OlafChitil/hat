#include <stdlib.h>
#include <stdio.h>

#include "stack.h"

stack* stack_newEmptyStack()
{
    stack *newStack = (stack *)malloc(sizeof(stack));
    
    newStack->element = NULL;
    newStack->next = NULL;
    
    return newStack;
}

stack* stack_newByCopyingStack(stack* theStack)
{
    stack *newStack = (stack *)malloc(sizeof(stack));
    
    newStack->element = theStack->element;
    if (theStack->next == NULL)
    {
        newStack->next = NULL;
    }
    else
    {
        newStack->next = stack_newByCopyingStack(theStack->next);
    }
    
    return newStack;
}

void stack_delete(stack* theStack)
{
    void *element = stack_pop(theStack);
    if (element)
    {
        if (theStack->element)
        {
            stack_delete(theStack);
        }
    }
}

void* stack_pop(stack* theStack)
{
    void *returnVal = NULL;
    stack *newNext;
    
    if (theStack->element)
    {
        returnVal = theStack->element;
        theStack->element = theStack->next->element;
        newNext = theStack->next->next;
        free(theStack->next);
        theStack->next = newNext;
    }
    
    return returnVal;
}

void stack_push(stack* theStack, void *element)
{
    stack *newNode = (stack *)malloc(sizeof(stack));
    
    newNode->element = theStack->element;
    newNode->next = theStack->next;
    theStack->element = element;
    theStack->next = newNode;
}

int stack_size(stack* theStack)
{
    if (theStack->element == NULL)
    {
        return 0;
    }
    else
    {
        return stack_size(theStack->next) + 1;
    }
}
