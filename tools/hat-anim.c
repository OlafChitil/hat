/* hat-anim: animates the progress of a haskell evaluation
* Thomas Davie, Student, University of York
* Original version November 2003
*/

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <curses.h>
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>

#include "art.h"	
#include "animnode.h"
#include "list.h"
#include "nodelist.h"
#include "nodehash.h"
#include "stack.h"

#define ANYEXP	22
#define ANYATOM	23
#define HEADER	29
#define INVALID	30
#define BEYOND	31

#define NONZERO 0
#define MAYBEZERO 1
#define MAYBELAMBDA 4

#define OPEN_HASH_SIZE 211

#define MAX_U_LONG 2147483647

#define COMMAND_BUFFER_SIZE 256
#define MAX_REDUCTION_LENGTH 36636

#define MAX_BUF	1024

/* FUNCTION PROTOTYPES */
// The main recursion structure.
char getPrintedNode (nodehash *fileNodes, unsigned long offset, unsigned long maxOffset, nodelist *visitedNodes, char *reductionBuffer, char showWhenUnevaluated, char showResult, char trusted, char shouldBracket);
char tryGetPrintedNode (nodehash *fileNodes, unsigned long offset, unsigned long maxOffset, nodelist *visitedNodes, char *reductionBuffer, char showWhenUnevaluated, char showResult, char trusted, char shouldBracket);
unsigned long findNextDisplayingNode(nodehash *fileNodes, unsigned long offset, unsigned long maxOffset, nodelist *visitedNodes);

// Specific sections of the file structure.
node *findAtomNode(nodehash *fileNodes, unsigned long offset, nodelist *visitedNodes);
char getExpValueApp(nodehash *fileNodes, unsigned long offset, unsigned long maxOffset, nodelist *visitedNodes, char *reductionBuffer, char showWhenUnevaluated, char showResult, char trusted, char shouldBracket);
char getExpApp(nodehash *fileNodes, unsigned long offset, unsigned long maxOffset, nodelist *visitedNodes, char *reductionBuffer, char showWhenUnevaluated, char showResult, char trusted, char shouldBracket);
char getListPrettyPrinted(nodehash *fileNodes, unsigned long offset, unsigned long maxOffset, nodelist *visitedNodes, char *reductionBuffer, char showWhenUnevaluated, char showResult, char trusted);
char isMoreList(nodehash *fileNodes, unsigned long offset, unsigned long maxOffset);
char isString(nodehash *fileNodes, unsigned long offset);
char canBePrettyPrinted(nodehash *fileNodes, unsigned long offset, unsigned long maxOffset);

char* tag2str(int k);

int strends(char *e, char *s);
void badUsage(void);

void repaint(stack *redexTrail, char *command, char *debugStr);
void printReduction(int lineNo, char *reduction);
void doCommand(char *command);
unsigned long reloadCache(nodehash *fileHash, unsigned long maxOffset);

void displayHelp(void);

unsigned long startOffset,
              currentOffset,
              lastMaxOffset,
              scrollPos;
bool exiting = false;
char stringMode = false;

list *trustees;
stack *redexTrail = NULL;

int main (int argc, char *argv[])
{
    char *filename;
    node *theNode;
    nodehash *fileHash;
    unsigned long nextOffset;
    char input;
    nodelist *visitedNodes;
    char commandBuffer[COMMAND_BUFFER_SIZE];
    int commandBufferPos;
    bool commandMode = false;
    char *reductionBuffer;
    char debugReductionBuffer[MAX_REDUCTION_LENGTH];
    char somewhere;
    char debugStr[128];
    
    initscr();
    raw();
    noecho();
    nodelay(stdscr, false);
    
    trustees = list_newEmptyList();
    
    /* File open and version check taken from hat-check */
    if (argc < 2)
    {
        badUsage();
    }

    if (strends(".hat", argv[1]))
    {
        filename = (char *)malloc(strlen(argv[1]) * sizeof(char));
    }
    else
    {
        filename = (char *)malloc((strlen(argv[1]) + 4) * sizeof(char));
    }
    strcpy(filename, argv[1]);
    if (!strends(".hat", filename))
    {
        strcat(filename, ".hat");
    }
    if (fileHash = nodehash_newByOpeningFile(filename))
    {
        if (argc >= 3)
        {
            sscanf(argv[2], "%d", &startOffset);
        }
        else
        {
            startOffset = 0;
            printf("hat-anim: no offset provided, assuming 0x0\n");
        }
        lastMaxOffset = currentOffset = startOffset;
        
        if (startOffset >= fileHash->filesize)
        {
            fprintf(stderr, "hat-anim (error: 0x%x is beyond end of trace file\n", startOffset);
            endwin();
            exit(1);
        }
        
        scrollPos = 0;
        nextOffset = reloadCache(fileHash, startOffset);
        while(!exiting)
        {
            repaint(redexTrail, (commandMode ? commandBuffer : ""), NULL);
            input = getch();
            if (!commandMode)
            {
                if (input == '\n' && nextOffset != MAX_U_LONG) // return advances the trail.
                {
                    nextOffset = reloadCache(fileHash, nextOffset);
                }
                else if (input == '\b')
                {
                    nextOffset = reloadCache(fileHash, lastMaxOffset);
                }
                else if (input == '.')
                {
                    scrollPos += (COLS - 10);
                }
                else if (input == ',')
                {
                    if (scrollPos > COLS - 10)
                    {
                        scrollPos -= (COLS - 10);
                    }
                    else
                    {
                        scrollPos = 0;
                    }
                }
                else if (input == ':') // Colon characters put us into command mode.
                {
                    commandMode = true;
                    strcpy(commandBuffer, ":");
                    commandBufferPos = 1;
                }
            }
            else
            {
                if (input == '\n') // When in command mode, return executes the command
                {
                    doCommand(commandBuffer);
                    commandMode = false;
                    nextOffset = reloadCache(fileHash, currentOffset);
                    repaint(redexTrail, (commandMode ? commandBuffer : ""), NULL);
                }
                else if (input == '\b') // backspace should delete.
                {
                    commandBufferPos--;
                    commandBuffer[commandBufferPos] = '\0';
                }
                else // Otherwise we add to the command buffer.
                {
                    if (commandBufferPos < COMMAND_BUFFER_SIZE - 1)
                    {
                        commandBuffer[commandBufferPos] = input;
                        commandBufferPos++;
                        commandBuffer[commandBufferPos] = '\0';
                    }
                    mvprintw(20, 0, commandBuffer);
                }
            }
        }
        
        stack_delete(redexTrail);
        nodehash_delete(fileHash);
    }
    
    list_delete(trustees);
    
    endwin();
    
    return 0;
}

char getPrintedNode (nodehash *fileNodes, unsigned long offset, unsigned long maxOffset, nodelist *visitedNodes, char *reductionBuffer, char showWhenUnevaluated, char showResult, char trusted, char shouldBracket)
{
    node *theNode = NULL;
    node *atomNode;
    int  argIndex;
    char showedSomething = false;
    char showedArg = false;
    char returnVal = false;
        
    theNode = nodehash_getNode (fileNodes, offset);
    
    nodelist_push(visitedNodes, theNode);
    
    /*char *temp;
    asprintf(&temp, "(0x%x, %s)", theNode->offset, tag2str(theNode->nodeType));
    strncat(reductionBuffer, temp, MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
    free(temp);*/
    switch (theNode->nodeType)
    {
#pragma mark AtomAbstact
        case AtomAbstract:
            strncat(reductionBuffer, theNode->params.atomAbstract.value, MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            returnVal = true;
            break;
#pragma mark AtomConstructor
        case AtomConstructor:
            strncat(reductionBuffer, theNode->params.atomConstructor.name, MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            returnVal = true;
            break;
#pragma mark AtomVariable
        case AtomVariable:
            strncat(reductionBuffer, theNode->params.atomVariable.name, MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            returnVal = true;
            break;
#pragma mark ExpApp
        case ExpApp:
        {
            returnVal = getExpApp(fileNodes, offset, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, shouldBracket);
            break;
        }
#pragma mark ExpCase
        case ExpCase:
            if (findNextDisplayingNode(fileNodes, theNode->params.expCase.condition, lastMaxOffset, visitedNodes) != MAX_U_LONG)
            {
                if (shouldBracket)
                {
                    strncat(reductionBuffer, "(", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                }
                strncat(reductionBuffer, "case ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expCase.condition, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, true);
                if (findNextDisplayingNode(fileNodes, theNode->params.expCase.condition, maxOffset, visitedNodes) != MAX_U_LONG)
                {
                    strncat(reductionBuffer, " then _", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                }
                else
                {
                    strncat(reductionBuffer, " then ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                    showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expCase.result, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, shouldBracket);
                    showedSomething = true;
                }
                if (shouldBracket)
                {
                    strncat(reductionBuffer, ")", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                }
            }
            else
            {
                showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expCase.result, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, shouldBracket);
                showedSomething = true;
            }

            returnVal = showedSomething;
            break;
#pragma mark ExpChar
        case ExpChar:
        {
            char temp[2];
            sprintf(temp, "%c", theNode->params.expChar.value);
            if (!stringMode)
            {
                strncat(reductionBuffer, "\'", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            }
            strncat(reductionBuffer, temp, MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            if (!stringMode)
            {
                strncat(reductionBuffer, "\'", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            }
            returnVal = true;
            break;
        }
#pragma mark ExpConstDef
        case ExpConstDef:
            returnVal = tryGetPrintedNode (fileNodes, theNode->params.expConstDef.result, maxOffset, visitedNodes, reductionBuffer, false, showResult, trusted, shouldBracket);
            if (!returnVal)
            {
                returnVal = tryGetPrintedNode (fileNodes, theNode->params.expConstDef.var, maxOffset, visitedNodes, reductionBuffer, false, showResult, trusted, shouldBracket);
            }
            break;
#pragma mark ExpConstUse
        case ExpConstUse:
            returnVal = tryGetPrintedNode(fileNodes, theNode->params.expConstUse.value, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, shouldBracket);
            break;
#pragma mark ExpDouble
        case ExpDouble:
        {
            char *temp=(char*)malloc(MAX_BUF*sizeof(char));	/*asprintf*/
            sprintf(temp, "%f", theNode->params.expDouble.value);
            strncat(reductionBuffer, temp, MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            free(temp);
            returnVal = true;
            break;
        }
#pragma mark ExpFieldUpdate
        case ExpFieldUpdate:
            showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expFieldUpdate.result, maxOffset, visitedNodes, reductionBuffer, false, showResult, trusted, shouldBracket);
            
            if (!showedSomething)
            {
                showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expFieldUpdate.arg, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, true);
                strncat(reductionBuffer, "{", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                if (showedSomething)
                {
                    for (argIndex = 0; argIndex < theNode->params.expFieldUpdate.arity - 1; argIndex++)
                    {
                        showedArg = tryGetPrintedNode(fileNodes, theNode->params.expFieldUpdate.binders[argIndex], maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, true);
                        strncat(reductionBuffer, "=", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                        showedArg = tryGetPrintedNode(fileNodes, theNode->params.expFieldUpdate.bindees[argIndex], maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, true);
                        strncat(reductionBuffer, ", ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                    }
                    showedArg = tryGetPrintedNode(fileNodes, theNode->params.expFieldUpdate.binders[theNode->params.expFieldUpdate.arity], maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, true);
                    strncat(reductionBuffer, "=", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                    showedArg = tryGetPrintedNode(fileNodes, theNode->params.expFieldUpdate.bindees[theNode->params.expFieldUpdate.arity], maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, true);
                    strncat(reductionBuffer, "}", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                }
            }
            returnVal = showedSomething;
            break;
#pragma mark ExpFloat
        case ExpFloat:
        {
            char *temp=(char*)malloc(MAX_BUF*sizeof(char));	/*asprintf*/
            sprintf(temp, "%f", theNode->params.expFloat.value);
            strncat(reductionBuffer, temp, MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            free(temp);
            returnVal = true;
            break;
        }
#pragma mark ExpForward
        case ExpForward:
            returnVal = tryGetPrintedNode(fileNodes, theNode->params.expForward.result, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, shouldBracket);
            break;
#pragma mark ExpGuard
        case ExpGuard:
            if (findNextDisplayingNode(fileNodes, theNode->params.expGuard.condition, lastMaxOffset, visitedNodes) != MAX_U_LONG)
            {
                if (shouldBracket)
                {
                    strncat(reductionBuffer, "(", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                }
                strncat(reductionBuffer, " guard ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expGuard.condition, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, true);
                if (findNextDisplayingNode(fileNodes, theNode->params.expGuard.condition, maxOffset, visitedNodes) != MAX_U_LONG)
                {
                    strncat(reductionBuffer, " then _ otherwise _", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                }
                else
                {
                    if (!strncmp(reductionBuffer + strlen(reductionBuffer) - 5, "False", 5))
                    {
                        strncat(reductionBuffer, " then _ otherwise ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                        showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expGuard.result, maxOffset, visitedNodes, reductionBuffer, true, showResult, trusted, true);
                        showedSomething = true;
                    }
                    else
                    {
                        strncat(reductionBuffer, " then ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                        showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expGuard.result, maxOffset, visitedNodes, reductionBuffer, true, showResult, trusted, true);
                        showedSomething = true;
                        strncat(reductionBuffer, " otherwise _", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                    }
                }
                if (shouldBracket)
                {
                    strncat(reductionBuffer, ")", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                }
            }
            else
            {
                showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expGuard.result, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, shouldBracket);
                showedSomething = true;
            }

            returnVal = showedSomething;
            break;
#pragma mark ExpHidden
        case ExpHidden:
            returnVal = tryGetPrintedNode (fileNodes, theNode->params.expHidden.result, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, shouldBracket);
            break;
#pragma mark ExpIf
        case ExpIf:
            if (findNextDisplayingNode(fileNodes, theNode->params.expIf.condition, lastMaxOffset, visitedNodes) != MAX_U_LONG)
            {
                if (shouldBracket)
                {
                    strncat(reductionBuffer, "(", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                }
                strncat(reductionBuffer, "if ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expIf.condition, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, true);
                if (findNextDisplayingNode(fileNodes, theNode->params.expIf.condition, maxOffset, visitedNodes) != MAX_U_LONG)
                {
                    strncat(reductionBuffer, " then _ else _", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                }
                else
                {
                    if (!strncmp(reductionBuffer + strlen(reductionBuffer) - 5, "False", 5))
                    {
                        strncat(reductionBuffer, " then _ else ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                        showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expIf.result, maxOffset, visitedNodes, reductionBuffer, true, showResult, trusted, true);
                        showedSomething = true;
                    }
                    else
                    {
                        strncat(reductionBuffer, " then ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                        showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expIf.result, maxOffset, visitedNodes, reductionBuffer, true, showResult, trusted, true);
                        showedSomething = true;
                        strncat(reductionBuffer, " else _", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                    }
                }
                if (shouldBracket)
                {
                    strncat(reductionBuffer, ")", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                }
            }
            else
            {
                showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expIf.result, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, shouldBracket);
                showedSomething = true;
            }
            
            returnVal = showedSomething;
            break;
#pragma mark ExpInt
        case ExpInt:
        {
            char *temp=(char*)malloc(MAX_BUF*sizeof(char));	/*asprintf*/
            sprintf(temp, "%ld", theNode->params.expInt.value);
            strncat(reductionBuffer, temp, MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            free(temp);
            returnVal = true;
            break;
        }
#pragma mark ExpInteger
        case ExpInteger:
        {
            char *temp=(char*)malloc(MAX_BUF*sizeof(char));	/*asprintf*/
            sprintf(temp, "%s", theNode->params.expInteger.value);
            strncat(reductionBuffer, temp, MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            free(temp);
            returnVal = true;
            break;
        }
#pragma mark ExpProjection
        case ExpProjection:
            returnVal = tryGetPrintedNode(fileNodes, theNode->params.expProjection.exp, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, shouldBracket);
            break;
#pragma mark ExpRat
        case ExpRat:
        {
            char *temp=(char*)malloc(MAX_BUF*sizeof(char));	/*asprintf*/
            sprintf(temp, "%d/%d", theNode->params.expRat.numerator, theNode->params.expRat.denominator);
            strncat(reductionBuffer, temp, MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            free(temp);
            returnVal = true;
            break;
        }
#pragma mark ExpRational
        case ExpRational:
        {
            char *temp=(char*)malloc(MAX_BUF*sizeof(char));	/*asprintf*/
            sprintf(temp, "%d/%d", theNode->params.expRational.numerator, theNode->params.expRational.denominator);
            strncat(reductionBuffer, temp, MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            free(temp);
            returnVal = true;
            break;
        }
#pragma mark ExpValueApp
        case ExpValueApp:
            returnVal = getExpValueApp(fileNodes, offset, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, shouldBracket);
            break;
#pragma mark ExpValueUse
        case ExpValueUse:
            if (!theNode->params.expValueUse.isLambda)
            {
                returnVal = tryGetPrintedNode(fileNodes, theNode->params.expValueUse.value, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, shouldBracket);
            }
            else 
            {
                strncat(reductionBuffer, "\\..", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            }
            break;
    }
    
    nodelist_pop(visitedNodes);
    
    shouldBracket = true;
    //fprintf(stderr, "Finished evaluating %s.\n", tag2str(theNode->nodeType));
    
    return returnVal;
}

char tryGetPrintedNode(nodehash *fileNodes, unsigned long offset, unsigned long maxOffset, nodelist *visitedNodes, char *reductionBuffer, char showWhenUnevaluated, char showResult, char trusted, char shouldBracket)
{
    node *theNode;
    
    theNode = nodehash_getNode (fileNodes, offset);
    // Check that we're still ahead of the file limit - ExpHidden, ExpForward and ExpProjection don't count.
    if (offset <= maxOffset || theNode->nodeType == ExpForward || theNode->nodeType == ExpHidden || theNode->nodeType == ExpProjection || trusted)
    {
        //Check we haven't visited the node already.
        if (!nodelist_contains(visitedNodes, offset))
        {
            // Display the node
            return getPrintedNode(fileNodes, offset, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, shouldBracket);
        }
        else // If we've visited the node already then we can't show it, so show an underscore.
        {
            if (showWhenUnevaluated)
            {
                strncat(reductionBuffer, "_", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            }
            return false;
        }
    }
    else // If we're beyond the limit then display only the function calls - no results.
    {
        if (showWhenUnevaluated)
        {
            return getPrintedNode(fileNodes, offset, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, false, false, shouldBracket);
        }
        
        return false;
    }
}

char getExpValueApp(nodehash *fileNodes, unsigned long offset, unsigned long maxOffset, nodelist *visitedNodes, char *reductionBuffer, char showWhenUnevaluated, char showResult, char trusted, char shouldBracket)
{
    node *atomNode;
    node *theNode = nodehash_getNode (fileNodes, offset);
    fixPri fix;
    char *name;
    int  argIndex;
    char showedSomething = false;
    char showedArg = false;
    
    /* Find out what kind of function this is - we need to know if it is a special case.
       There are two special cases - the "," function and the ":" function
       these represent the tuple construction and the list construction in turn. */
    atomNode = findAtomNode(fileNodes, theNode->params.expValueApp.function, visitedNodes);
    if (atomNode)
    {
        if (atomNode->nodeType == AtomConstructor)
        {
            fix = atomNode->params.atomConstructor.fix;
            name = atomNode->params.atomConstructor.name;
        }
        else if (atomNode->nodeType == AtomVariable)
        {
            fix = atomNode->params.atomVariable.fix;
            name = atomNode->params.atomVariable.name;
        }
    }
    else
    {
        fix.fix = defaultFix;
        fix.something = theNode->params.expValueApp.arity;
        name = " ";
    }
    
    if (!strcmp(name, ",")) // Special case: The tuple function - start with an open bracket.
    {
        fix.fix = infixl;
    }
    if (!strcmp(name, ":"))
    {
        if (canBePrettyPrinted(fileNodes, offset, maxOffset))
        {
            if (!isString(fileNodes, offset))
            {
                strncat(reductionBuffer, "[", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                showedSomething = getListPrettyPrinted(fileNodes, offset, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted);
                strncat(reductionBuffer, "]", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            }
            else
            {
                strncat(reductionBuffer, "\"", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                stringMode = true;
                showedSomething = getListPrettyPrinted(fileNodes, offset, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted);
                stringMode = false;
                strncat(reductionBuffer, "\"", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            }
        }
    }

    if (!showedSomething)
    {
        if (shouldBracket || !strcmp(name, ","))
        {
            strncat(reductionBuffer, "(", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
        }
        if (fix.fix == defaultFix) // If the function is not infix, start with it's function name.
        {
            showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expValueApp.function, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, true);
            strncat(reductionBuffer, " ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
        }
        // Either the function is not infix and we have shown it's name, or we have already
        // found it's name - in this case we know we can display it, so it's safe to continue.
        if (showedSomething || fix.fix != defaultFix)
        {
            showedSomething = true;
            for (argIndex = 0; argIndex < theNode->params.expValueApp.arity - 1; argIndex++)
            {
                showedArg = tryGetPrintedNode(fileNodes, theNode->params.expValueApp.args[argIndex], maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, true);
                if (fix.fix != defaultFix)
                {
                    strncat(reductionBuffer, name, MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                }
                else
                {
                    strncat(reductionBuffer, " ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                }
            }
            showedArg = tryGetPrintedNode(fileNodes, theNode->params.expValueApp.args[theNode->params.expValueApp.arity - 1], maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, true);
        }
        if (shouldBracket || !strcmp(name, ","))
        {
            strncat(reductionBuffer, ")", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
        }
    }

    return showedSomething;
}

char getExpApp(nodehash *fileNodes, unsigned long offset, unsigned long maxOffset, nodelist *visitedNodes, char *reductionBuffer, char showWhenUnevaluated, char showResult, char trusted, char shouldBracket)
{
    node *theNode = nodehash_getNode(fileNodes, offset);
    node *atomNode;
    fixPri fix;
    char *name;
    int  argIndex;
    char showedSomething = false;
    char showedArg = false;
    
    // If we have evaluated something about the function, display it
    /*char *temp;
    asprintf(&temp, "0x%x", theNode->offset);
    strncat(reductionBuffer, temp, MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
    free(temp);*/
    if (showResult)
    {
        showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expApp.result, maxOffset, visitedNodes, reductionBuffer, false, showResult, trusted, shouldBracket);
    }
    
    
    // If we haven't done any evaluation, find out what we know about the function and display it.
    if (!showedSomething)
    {
        /* Find out what kind of function this is - we need to know if it is a special case.
        There are two special cases - the "," function and the ":" function
        these represent the tuple construction and the list construction in turn. */
        atomNode = findAtomNode(fileNodes, theNode->params.expApp.function, visitedNodes);
        if (atomNode)
        {
            if (atomNode->nodeType == AtomConstructor)
            {
                fix = atomNode->params.atomConstructor.fix;
                name = atomNode->params.atomConstructor.name;
            }
            else if (atomNode->nodeType == AtomVariable)
            {
                fix = atomNode->params.atomVariable.fix;
                name = atomNode->params.atomVariable.name;
            }
        }
        else
        {
            fix.fix = defaultFix;
            fix.something = theNode->params.expApp.arity;
            name = " ";
        }
        
        if (list_contains_string(trustees, name))
        {
            showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expApp.result, maxOffset, visitedNodes, reductionBuffer, false, showResult, true, shouldBracket);
        }
        else
        {
            if (shouldBracket)
            {
                strncat(reductionBuffer, "(", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            }
            if (fix.fix == defaultFix) // If the function is not infix, start with it's function name.
            {
                showedSomething = tryGetPrintedNode(fileNodes, theNode->params.expApp.function, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, true);
                strncat(reductionBuffer, " ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            }        
            
            if (showedSomething || fix.fix != defaultFix)
            {
                showedSomething = true;
                for (argIndex = 0; argIndex < theNode->params.expApp.arity - 1; argIndex++)
                {
                    showedArg = tryGetPrintedNode(fileNodes, theNode->params.expApp.args[argIndex], maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, true);
                    if (fix.fix != defaultFix)
                    {
                        strncat(reductionBuffer, " ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                        strncat(reductionBuffer, name, MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                        strncat(reductionBuffer, " ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                    }
                    else
                    {
                        strncat(reductionBuffer, " ", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                    }
                }
                showedArg = tryGetPrintedNode(fileNodes, theNode->params.expApp.args[theNode->params.expApp.arity - 1], maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, true);
            }
            if (shouldBracket)
            {
                strncat(reductionBuffer, ")", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
            }
        }
    }
    
    return showedSomething;    
}

char getListPrettyPrinted(nodehash *fileNodes, unsigned long offset, unsigned long maxOffset, nodelist *visitedNodes, char *reductionBuffer, char showWhenUnevaluated, char showResult, char trusted)
{
    node *theNode = nodehash_getNode(fileNodes, offset);
    node *testNode;
    char showedArg = false;
    
    if (theNode->offset <= maxOffset || theNode->nodeType == ExpHidden || theNode->nodeType == ExpForward || theNode->nodeType == ExpProjection)
    {
        switch (theNode->nodeType)
        {
            case ExpApp:
                return getListPrettyPrinted(fileNodes, theNode->params.expApp.result, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted);
            case ExpCase:
                return getListPrettyPrinted(fileNodes, theNode->params.expCase.result, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted);
            case ExpConstDef:
                return getListPrettyPrinted(fileNodes, theNode->params.expConstDef.result, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted);
            case ExpConstUse:
                return getListPrettyPrinted(fileNodes, theNode->params.expConstUse.value, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted);
            case ExpFieldUpdate:
                return getListPrettyPrinted(fileNodes, theNode->params.expFieldUpdate.result, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted);
            case ExpForward:
                return getListPrettyPrinted(fileNodes, theNode->params.expForward.result, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted);
            case ExpGuard:
                return getListPrettyPrinted(fileNodes, theNode->params.expGuard.result, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted);
            case ExpHidden:
                return getListPrettyPrinted(fileNodes, theNode->params.expHidden.result, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted);
            case ExpIf:
                return getListPrettyPrinted(fileNodes, theNode->params.expIf.result, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted);
            case ExpProjection:
                return getListPrettyPrinted(fileNodes, theNode->params.expProjection.exp, maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted);
            case ExpValueApp:
                showedArg = tryGetPrintedNode(fileNodes, theNode->params.expValueApp.args[0], maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted, false);
                if (isMoreList(fileNodes, theNode->params.expValueApp.args[1], maxOffset))
                {
                    if (!isString(fileNodes, offset))
                    {
                        strncat(reductionBuffer, ",", MAX_REDUCTION_LENGTH - 1 - strlen(reductionBuffer));
                    }
                    showedArg = getListPrettyPrinted(fileNodes, theNode->params.expValueApp.args[1], maxOffset, visitedNodes, reductionBuffer, showWhenUnevaluated, showResult, trusted);
                }
            case ExpValueUse:
                showedArg = true;
            default:
                showedArg = true;
        }
    }
    return showedArg;
}

char isMoreList(nodehash *fileNodes, unsigned long offset, unsigned long maxOffset)
{
    node *theNode = nodehash_getNode(fileNodes, offset);

    if (theNode->offset <= maxOffset || theNode->nodeType == ExpHidden || theNode->nodeType == ExpForward || theNode->nodeType == ExpProjection)
     {
        switch (theNode->nodeType)
        {
            case ExpApp:
                return isMoreList(fileNodes, theNode->params.expApp.result, maxOffset);
            case ExpCase:
                return isMoreList(fileNodes, theNode->params.expCase.result, maxOffset);
            case ExpConstDef:
                return isMoreList(fileNodes, theNode->params.expConstDef.result, maxOffset);
            case ExpConstUse:
                return isMoreList(fileNodes, theNode->params.expConstUse.value, maxOffset);
            case ExpFieldUpdate:
                return isMoreList(fileNodes, theNode->params.expFieldUpdate.result, maxOffset);
            case ExpForward:
                return isMoreList(fileNodes, theNode->params.expForward.result, maxOffset);
            case ExpGuard:
                return isMoreList(fileNodes, theNode->params.expGuard.result, maxOffset);
            case ExpHidden:
                return isMoreList(fileNodes, theNode->params.expHidden.result, maxOffset);
            case ExpIf:
                return isMoreList(fileNodes, theNode->params.expIf.result, maxOffset);
            case ExpProjection:
                return isMoreList(fileNodes, theNode->params.expProjection.exp, maxOffset);
            case ExpValueApp:
                return true;
            default:
                return false;
        }
    }
}

char isString(nodehash *fileNodes, unsigned long offset)
{
    node *theNode = nodehash_getNode(fileNodes, offset);
    node *itemNode = nodehash_getNode(fileNodes, theNode->params.expValueApp.args[0]);
    
    if (itemNode->nodeType == ExpChar)
    {
        return true;
    }
    else
    {
        return false;
    }
}

char canBePrettyPrinted(nodehash *fileNodes, unsigned long offset, unsigned long maxOffset)
{
    node *theNode = nodehash_getNode(fileNodes, offset);
    node *testNode;

    if (theNode->offset > maxOffset && !(theNode->nodeType == ExpHidden || theNode->nodeType == ExpForward || theNode->nodeType == ExpProjection))
    {
        return false;
    }

    switch (theNode->nodeType)
    {
        case ExpApp:
            return canBePrettyPrinted(fileNodes, theNode->params.expApp.result, maxOffset);
        case ExpCase:
            return canBePrettyPrinted(fileNodes, theNode->params.expCase.result, maxOffset);
        case ExpConstDef:
            return canBePrettyPrinted(fileNodes, theNode->params.expConstDef.result, maxOffset);
        case ExpConstUse:
            return canBePrettyPrinted(fileNodes, theNode->params.expConstUse.value, maxOffset);
        case ExpFieldUpdate:
            return canBePrettyPrinted(fileNodes, theNode->params.expFieldUpdate.result, maxOffset);
        case ExpForward:
            return canBePrettyPrinted(fileNodes, theNode->params.expForward.result, maxOffset);
        case ExpGuard:
            return canBePrettyPrinted(fileNodes, theNode->params.expGuard.result, maxOffset);
        case ExpHidden:
            return canBePrettyPrinted(fileNodes, theNode->params.expHidden.result, maxOffset);
        case ExpIf:
            return canBePrettyPrinted(fileNodes, theNode->params.expIf.result, maxOffset);
        case ExpProjection:
            return canBePrettyPrinted(fileNodes, theNode->params.expProjection.exp, maxOffset);
        case ExpValueApp:
            if (!strcmp(nodehash_getNode(fileNodes, theNode->params.expValueApp.function)->params.atomVariable.name, ":"))
            {
                return canBePrettyPrinted(fileNodes, theNode->params.expValueApp.args[1], maxOffset);
            }
            return false;
        case ExpValueUse:
            testNode = nodehash_getNode(fileNodes, theNode->params.expValueUse.value);
            if (testNode->nodeType == AtomConstructor)
            {
                return !strcmp(testNode->params.atomConstructor.name, "[]");
            }
            return false;
        default:
            return false;
    }

    return false;
}


unsigned long findNextDisplayingNode(nodehash *fileNodes, unsigned long offset, unsigned long maxOffset, nodelist *visitedNodes)
{
    node *theNode = NULL;
    int  argIndex;
    unsigned long testOffset;
    unsigned long workingOffset = MAX_U_LONG;
    
    theNode = nodehash_getNode(fileNodes, offset);
    
    nodelist_push(visitedNodes, theNode);
    
    //fprintf(stderr, "<%s offset=\"0x%x\">\n", tag2str(theNode->nodeType), offset);
    
    switch (theNode->nodeType)
    {
        case AtomAbstract:
        case AtomConstructor:
        case AtomVariable:
            workingOffset = offset;
            break;
        case ExpApp:
            if (offset > maxOffset)
            {
                workingOffset = offset;
            }
            else
            {
                node *atomNode = findAtomNode(fileNodes, theNode->params.expApp.function, visitedNodes);
                char *name;
                
                
                if (atomNode->nodeType == AtomConstructor)
                {
                    name = atomNode->params.atomConstructor.name;
                }
                else if (atomNode->nodeType == AtomVariable)
                {
                    name = atomNode->params.atomVariable.name;
                }
                // Check if this is a trusted function.
                if (!list_contains_string(trustees, name))
                {
                    // Check the function arguments
                    for (argIndex = 0; argIndex < theNode->params.expApp.arity; argIndex++)
                    {
                        if (!nodelist_contains(visitedNodes, theNode->params.expApp.args[argIndex]))
                        {
                            testOffset = findNextDisplayingNode(fileNodes, theNode->params.expApp.args[argIndex], maxOffset, visitedNodes);
                            if (testOffset > maxOffset && testOffset < workingOffset)
                            {
                                workingOffset = testOffset;
                            }
                        }
                    }
                    
                    // Check for the whole function being evaluated.
                    if (!nodelist_contains(visitedNodes, theNode->params.expApp.result))
                    {
                        testOffset = findNextDisplayingNode(fileNodes, theNode->params.expApp.result, maxOffset, visitedNodes);
                        if (testOffset > maxOffset && testOffset < workingOffset)
                        {
                            workingOffset = testOffset;
                        }
                    }
                }
            }
            break;
        case ExpCase:
            // Check when the condition is evaluated.
            testOffset = findNextDisplayingNode(fileNodes, theNode->params.expCase.condition, maxOffset, visitedNodes);
            if (testOffset > maxOffset)
            {
                workingOffset = testOffset;
            }
                
            // Check when the result is evaluated.
            testOffset = findNextDisplayingNode(fileNodes, theNode->params.expCase.result, maxOffset, visitedNodes);
            if (testOffset > maxOffset && testOffset < workingOffset)
            {
                workingOffset = testOffset;
            }
            break;
        case ExpChar:
            workingOffset = offset;
            break;
        case ExpConstDef:
            testOffset = findNextDisplayingNode(fileNodes, theNode->params.expConstDef.var, maxOffset, visitedNodes);
            if (testOffset > maxOffset)
            {
                workingOffset = testOffset;
            }
                
            testOffset = findNextDisplayingNode(fileNodes, theNode->params.expConstDef.result, maxOffset, visitedNodes);
            if (testOffset > maxOffset && testOffset < workingOffset)
            {
                workingOffset = testOffset;
            }
            break;
        case ExpConstUse:
            testOffset = findNextDisplayingNode(fileNodes, theNode->params.expConstUse.value, maxOffset, visitedNodes);
            break;
        case ExpDouble:
            workingOffset = offset;
            break;
        case ExpFieldUpdate:
            testOffset = findNextDisplayingNode(fileNodes, theNode->params.expFieldUpdate.arg, maxOffset, visitedNodes);
            if (testOffset <= maxOffset)
            {
                for (argIndex = 0; argIndex < theNode->params.expFieldUpdate.arity; argIndex++)
                {
                    testOffset = findNextDisplayingNode(fileNodes, theNode->params.expFieldUpdate.binders[argIndex], maxOffset, visitedNodes);
                    if (testOffset > maxOffset && testOffset < workingOffset)
                    {
                        workingOffset = testOffset;
                    }
                    testOffset = findNextDisplayingNode(fileNodes, theNode->params.expFieldUpdate.bindees[argIndex], maxOffset, visitedNodes);
                    if (testOffset > maxOffset && testOffset < workingOffset)
                    {
                        workingOffset = testOffset;
                    }
                }
            }
            else
            {
                workingOffset = testOffset;
            }
            
            testOffset = findNextDisplayingNode(fileNodes, theNode->params.expFieldUpdate.result, maxOffset, visitedNodes);
            if (testOffset < workingOffset && testOffset > maxOffset)
            {
                workingOffset = testOffset;
            }
                
            break;
        case ExpFloat:
            workingOffset = offset;
            break;
        case ExpForward:
            workingOffset = findNextDisplayingNode(fileNodes, theNode->params.expForward.result, maxOffset, visitedNodes);
            break;
        case ExpGuard:
            testOffset = findNextDisplayingNode(fileNodes, theNode->params.expGuard.condition, maxOffset, visitedNodes);
            if (testOffset > maxOffset)
            {
                workingOffset = testOffset;
            }
                
            testOffset = findNextDisplayingNode(fileNodes, theNode->params.expGuard.result, maxOffset, visitedNodes);
            if (testOffset > maxOffset && testOffset < workingOffset)
            {
                workingOffset = testOffset;
            }
            break;
        case ExpHidden:
            workingOffset = findNextDisplayingNode(fileNodes, theNode->params.expHidden.result, maxOffset, visitedNodes);
            break;
        case ExpIf:
            testOffset = findNextDisplayingNode(fileNodes, theNode->params.expIf.condition, maxOffset, visitedNodes);
            if (testOffset > maxOffset)
            {
                workingOffset = testOffset;
            }
                
            testOffset = findNextDisplayingNode(fileNodes, theNode->params.expIf.result, maxOffset, visitedNodes);
            if (testOffset > maxOffset && testOffset < workingOffset)
            {
                workingOffset = testOffset;
            }
            break;
        case ExpInt:
        case ExpInteger:
            workingOffset = offset;
            break;
        case ExpProjection:
            workingOffset = findNextDisplayingNode(fileNodes, theNode->params.expProjection.exp, maxOffset, visitedNodes);
            break;            
        case ExpRat:
        case ExpRational:
            workingOffset = offset;
            break;
        case ExpValueApp:
            // Check for each argument being evaluated.
            if (offset > maxOffset)
            {
                workingOffset = offset;
            }
            else
            {
                for (argIndex = 0; argIndex < theNode->params.expValueApp.arity; argIndex++)
                {
                    if (!nodelist_contains(visitedNodes, theNode->params.expValueApp.args[argIndex]))
                    {
                        testOffset = findNextDisplayingNode(fileNodes, theNode->params.expValueApp.args[argIndex], maxOffset, visitedNodes);
                        if (testOffset > maxOffset && testOffset < workingOffset)
                        {
                            workingOffset = testOffset;
                        }
                    }
                }
            }
            
            break;
        case ExpValueUse:
            if (!theNode->params.expValueUse.isLambda)
            {
                workingOffset = findNextDisplayingNode(fileNodes, theNode->params.expValueUse.value, maxOffset, visitedNodes);
            }
            else
            workingOffset = offset;
            break;
    }
    
    nodelist_pop (visitedNodes);
    
    //fprintf(stderr, "</%s>", tag2str(theNode->nodeType));

    return (workingOffset < offset ? offset : workingOffset);
}

node *findAtomNode(nodehash *fileNodes, unsigned long offset, nodelist *visitedNodes)
{
    node *theNode = NULL;
    node *foundNode = NULL;
    
    theNode = nodehash_getNode(fileNodes, offset);
    
    nodelist_push(visitedNodes, theNode);
    
    switch (theNode->nodeType)
    {
        case AtomAbstract:
        case AtomConstructor:
        case AtomVariable:
            foundNode = theNode;
            break;
        case ExpApp:
            foundNode = findAtomNode(fileNodes, theNode->params.expApp.result, visitedNodes);
            break;
        case ExpCase:
            foundNode = findAtomNode(fileNodes, theNode->params.expCase.result, visitedNodes);
            break;
        case ExpConstDef:
            foundNode = findAtomNode(fileNodes, theNode->params.expConstDef.var, visitedNodes);
            break;
        case ExpConstUse:
            foundNode = findAtomNode(fileNodes, theNode->params.expConstUse.value, visitedNodes);
            break;
        case ExpFieldUpdate:
            foundNode = findAtomNode(fileNodes, theNode->params.expFieldUpdate.result, visitedNodes);
            break;
        case ExpForward:
            foundNode = findAtomNode(fileNodes, theNode->params.expForward.result, visitedNodes);
            break;
        case ExpGuard:
            foundNode = findAtomNode(fileNodes, theNode->params.expGuard.result, visitedNodes);
            break;
        case ExpHidden:
            foundNode = findAtomNode(fileNodes, theNode->params.expHidden.result, visitedNodes);
            break;
        case ExpIf:
            foundNode = findAtomNode(fileNodes, theNode->params.expIf.result, visitedNodes);
            break;
        case ExpProjection:
            foundNode = findAtomNode(fileNodes, theNode->params.expProjection.exp, visitedNodes);
            break;
        case ExpValueApp:
            foundNode = findAtomNode(fileNodes, theNode->params.expValueApp.function, visitedNodes);
            break;
        case ExpValueUse:
            foundNode = findAtomNode(fileNodes, theNode->params.expValueUse.value, visitedNodes);
            break;
    }
    
    nodelist_pop (visitedNodes);
    
    return foundNode;
}

/* strings for symbolic constants - taken from hat-check */
char* tag2str (int k)
{
    switch (k)
    {
        case Module:
            return "Module";
        case SrcPos:
            return "SrcPos";
        case ExpApp:
            return "ExpApp";
        case ExpValueApp:
            return "ExpValueApp";
        case ExpChar:
            return "ExpChar";
        case ExpInt:
            return "ExpInt";
        case ExpInteger:
            return "ExpInteger";
        case ExpRat:
            return "ExpRat";
        case ExpRational:
            return "ExpRational";
        case ExpFloat:
            return "ExpFloat";
        case ExpDouble:
            return "ExpDouble";
        case ExpValueUse:
            return "ExpValueUse";
        case ExpConstUse:
            return "ExpConstUse";
        case ExpConstDef:
            return "ExpConstDef";
        case ExpGuard:
            return "ExpGuard";
        case ExpCase:
            return "ExpCase";
        case ExpIf:
            return "ExpIf";
        case ExpFieldUpdate:
            return "ExpFieldUpdate";
        case ExpProjection:
            return "ExpProjection";
        case ExpHidden:
            return "ExpHidden";
        case ExpForward:
            return "ExpForward";
        case ExpDoStmt:
            return "ExpDoStmt";
        case AtomVariable:
            return "AtomVariable";
        case AtomConstructor:
            return "AtomConstructor";
        case AtomAbstract:
            return "AtomAbstract";
            
        case ANYEXP:
            return "Exp";
        case ANYATOM:
            return "Atom";
        case HEADER:
            return "HEADER";
        case INVALID:
            return "INVALID";
        case BEYOND:
            return "beyond end of file";
            
        default:
            return "unknown/unused";
    }
}

/**
 * Taken from hat-check.
 * Checks if the end of a string ends in a certain way.
 * 
 * @return |true| iff e ends with s. \
 *         |false| otherwise.
 */
int strends (char *e, char *s)
{
    int d = strlen(s) - strlen(e);
    return d>=0 && strcmp(s+d, e)==0;
}

/**
 * Displays the usage string.
 */
void badUsage (void)
{
    fprintf(stderr,"usage: hat-anim prog-name [offset]\n");
    endwin();
    exit(1);
}

void repaint(stack *redexTrail, char *command, char *debugStr)
{
    int redexTrailSize = 0,
        redexTrailWindowLine,
        trailSpace = LINES - 3,
        colNum;
    stack *redexTrailBackup = stack_newByCopyingStack(redexTrail);
    
    erase();
    
    // Print a heading.
    attron(A_BOLD);
    mvprintw(0, 0, "Animation:");
    attroff(A_BOLD);
    mvprintw(0, 10, " ------- file.hs line: ? col: ? -");
    for (colNum = 43; colNum < COLS; colNum++)
    {
        mvprintw(0, colNum, "-");
    }

    redexTrailSize = stack_size(redexTrailBackup);
    
    redexTrailWindowLine = (redexTrailSize > trailSpace ? trailSpace : redexTrailSize);
    while (redexTrailWindowLine > 2)
    {
        mvprintw(redexTrailWindowLine, 0, "->");
        printReduction(redexTrailWindowLine, stack_pop(redexTrailBackup));
        redexTrailWindowLine--;
    }
    
    if (stack_size(redexTrailBackup) == 2)
    {
        mvprintw(2, 0, "-> ");
        printReduction(2, stack_pop(redexTrailBackup));
    }
    else if (stack_size(redexTrailBackup) > 2)
    {
        mvprintw(2, 0, "-> . . .");
        while (stack_size(redexTrailBackup) > 1)
        {
            stack_pop(redexTrailBackup);
        }
    }
    mvprintw(1, 0, "   ");
    printReduction(1, stack_pop(redexTrailBackup));
    for (colNum = 0; colNum < COLS; colNum++)
    {
        mvprintw(LINES - 2, colNum, "-");
    }
    mvprintw(LINES - 1, 0, command);
    
    if (debugStr)
    {
        mvprintw(LINES - 2, 2, debugStr);
    }
    
    stack_delete(redexTrailBackup);
}

void printReduction(int lineNo, char *reduction)
{
    char *line = (char *)malloc(sizeof(char) * COLS - 2);
    
    if (scrollPos < strlen(reduction))
    {
        if (scrollPos == 0)
        {
            if (strlen((char *)((int)reduction + scrollPos)) > COLS - 3)
            {
                strncpy(line, (char *)((int)reduction + scrollPos), COLS - 6);
                line[COLS - 6] = '.';
                line[COLS - 5] = '.';
                line[COLS - 4] = '.';
                line[COLS - 3] = '\0';
                mvprintw(lineNo, 3, "%s", line);
            }
            else
            {
                mvprintw(lineNo, 3, "%s", reduction);
            }
        }
        else
        {
            line[0] = '.';
            line[1] = '.';
            line[2] = '.';
            if (strlen((char *)((int)reduction + scrollPos)) > COLS - 6)
            {
                strncpy((char *)((int)line + 3), (char *)((int)reduction + scrollPos), COLS - 6);
                line[COLS - 6] = '.';
                line[COLS - 5] = '.';
                line[COLS - 4] = '.';
                line[COLS - 3] = '\0';
                mvprintw(lineNo, 3, "%s", line);
            }
            else
            {
                strcpy((char *)((int)line + 3), (char *)((int)reduction + scrollPos));
                mvprintw(lineNo, 3, "%s", line);
            }
        }
    }
    else
    {
        mvprintw(lineNo, 3, "...");
    }
}

void doCommand(char *command)
{
    if (!strcmp(command, ":q") || !strcmp(command, ":quit"))
    {
        exiting = true;
    }
    else if (!strcmp(command, ":help"))
    {
        displayHelp();
    }
    else if (!strncmp(command, ":trust ", 7))
    {
        char *newString = (char *)malloc((strlen(command) - 6) * sizeof(char));
        
        strcpy(newString, command+7);
        list_add(trustees, newString);
    }
    else if (!strncmp(command, ":untrust ", 9))
    {
        char *newString = (char *)malloc((strlen(command) - 8) * sizeof(char));
        
        strcpy(newString, command+9);
        list_remove_string(&trustees, newString);
        free(newString);
    }
    else if (!strcmp(command, ":untrust_all"))
    {
        list_delete(trustees);
        trustees = list_newEmptyList();
    }
}

unsigned long reloadCache(nodehash *fileHash, unsigned long maxOffset)
{
    unsigned long nextOffset;
    nodelist *visitedNodes;
    char *reductionBuffer;
    char debugReductionBuffer[MAX_REDUCTION_LENGTH];
    
    //fprintf(stderr, "offset: %x", startOffset);
    //scanf("%c", debugReductionBuffer);
    currentOffset = startOffset;
    if (redexTrail)
    {
        stack_delete(redexTrail);
    }
    redexTrail = stack_newEmptyStack();
    nextOffset = MAX_U_LONG;
    reductionBuffer = (char *)malloc(sizeof(char) * MAX_REDUCTION_LENGTH);
    reductionBuffer[0] = '\0';
    visitedNodes = nodelist_newEmptyList();
    getPrintedNode(fileHash, startOffset, currentOffset, visitedNodes, reductionBuffer, true, true, false, false);
    nodelist_delete(visitedNodes);
    visitedNodes = nodelist_newEmptyList();
    nextOffset = findNextDisplayingNode(fileHash, startOffset, currentOffset, visitedNodes);
    if (nextOffset == startOffset)
    {
        nextOffset = MAX_U_LONG;
    }
    nodelist_delete(visitedNodes);
    stack_push(redexTrail, reductionBuffer);
    while(currentOffset < maxOffset)
    {
        lastMaxOffset = currentOffset;
        currentOffset = nextOffset;
        nextOffset = MAX_U_LONG;
        reductionBuffer = (char *)malloc(sizeof(char) * MAX_REDUCTION_LENGTH);
        reductionBuffer[0] = '\0';
        debugReductionBuffer[0] = '\0';
        visitedNodes = nodelist_newEmptyList();
        //getPrintedNode(fileHash, startOffset, currentOffset, visitedNodes, debugReductionBuffer, true, true, false, false);
        //fprintf(stderr, "Getting next node.\n");
        getPrintedNode(fileHash, startOffset, currentOffset, visitedNodes, reductionBuffer, true, true, false, false);
        nodelist_delete(visitedNodes);
        visitedNodes = nodelist_newEmptyList();
        //fprintf(stderr, "Finding next node.\n");
        nextOffset = findNextDisplayingNode(fileHash, startOffset, currentOffset, visitedNodes);
        //fprintf(stderr, "found next node.\n");
        if (nextOffset == startOffset)
        {
            nextOffset = MAX_U_LONG;
        }
        nodelist_delete(visitedNodes);
        //sprintf(reductionBuffer, "%x - %s", nextOffset, debugReductionBuffer);
        stack_push(redexTrail, reductionBuffer);
    }
    
    return nextOffset;
}

void displayHelp(void)
{
    erase();

    // Print a heading.
    attron(A_BOLD);
    mvprintw( 0, 0, "hat-anim - Help");
    attroff(A_BOLD);
    mvprintw( 1, 0, "hat-anim is a forward tracer that animates Haskell programs.");
    mvprintw( 2, 0, "Commands may be entered by pressing ':'.");
    mvprintw( 3, 0, "Commands:");
    mvprintw( 4, 0, "help         Brings up this help screen");
    mvprintw( 5, 0, "trust <f>    Trusts a function with name <f>.");
    mvprintw( 6, 0, "             Applications of this function will not be shown.");
    mvprintw( 7, 0, "untrust <f>  Un-trusts a function with name <f>.");
    mvprintw( 8, 0, "             Reverses the effect of trust, does nothing if <f>");
    mvprintw( 9, 0, "             is not trusted.");
    mvprintw(10, 0, "untrust_all  Untrusts all functions.");
    mvprintw(11, 0, "q or quit    Exits hat-anim.");
    while (getch() != '\n')
    {

    }
}
