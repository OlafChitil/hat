/*
 *  animnode.c
 *  
 *  Created by Thomas Davie on Fri Nov 21 2003.
 *  Copyright (c) 2003 Thomas Davie. All rights reserved.
 *
 */

#include <stdio.h>

#include "art.h"
#include "animnode.h"

// Private functions
node* readAtomAbstract(FILE *hatFile, unsigned long offset);
node* readAtomConstructor(FILE *hatFile, unsigned long offset);
node* readAtomVariable(FILE *hatFile, unsigned long offset);
node* readExpApp(FILE *hatFile, unsigned long offset);
node* readExpCase(FILE *hatFile, unsigned long offset);
node* readExpChar(FILE *hatFile, unsigned long offset);
node* readExpConstDef(FILE *hatFile, unsigned long offset);
node* readExpConstUse(FILE *hatFile, unsigned long offset);
node* readExpDouble(FILE *hatFile, unsigned long offset);
node* readExpDoStmt(FILE *hatFile, unsigned long offset);
node* readExpFieldUpdate(FILE *hatFile, unsigned long offset);
node* readExpFloat(FILE *hatFile, unsigned long offset);
node* readExpForward(FILE *hatFile, unsigned long offset);
node* readExpGuard(FILE *hatFile, unsigned long offset);
node* readExpHidden(FILE *hatFile, unsigned long offset);
node* readExpIf(FILE *hatFile, unsigned long offset);
node* readExpInt(FILE *hatFile, unsigned long offset);
node* readExpInteger(FILE *hatFile, unsigned long offset);
node* readExpProjection(FILE *hatFile, unsigned long offset);
node* readExpRat(FILE *hatFile, unsigned long offset);
node* readExpRational(FILE *hatFile, unsigned long offset);
node* readExpValueApp(FILE *hatFile, unsigned long offset);
node* readExpValueUse(FILE *hatFile, unsigned long offset);
node* readModule(FILE *hatFile, unsigned long offset);
node* readSrcPos(FILE *hatFile, unsigned long offset);

void setFilePos (FILE *hatFile, unsigned long offset);

unsigned int readArity (FILE *hatFile);
unsigned long readPointer (FILE *hatFile);
unsigned long readInt (FILE *hatFile);
float readFloat (FILE *hatFile);
double readDouble (FILE *hatFile);
position readPosition (FILE *hatFile);
fixPri readFixPri (FILE *hatFile);
char *readString (FILE *hatFile);
fourbytes readFourBytes (FILE *hatFile);
char readByte (FILE *hatFile);

// Code

/**
 * Read a node from a hat file.
 *
 * @param  hatFile The file to read from - assumed to be open.
 * @param  offset  The offset to start reading in the hat file.
 * @return A node pointer to the node found in the hatFile at offset.
 */
node* readNode (FILE *hatFile, unsigned long offset)
{
    int  err;
    char b;
    node *newNode;
    
    // Find out which type of node we have.
    fseek (hatFile, offset, SEEK_SET);
    err = fread (&b, sizeof(char), 1, hatFile);
    if (err != 1)
    {
        return NULL;
    }
    else
    {
        int tag = lower5(b);
        if ((ExpDoStmt < tag && tag < AtomVariable) || tag > AtomAbstract)
        {
            fprintf (stderr, "strange tag %d at byte offset 0x%x\n", tag, offset);
            exit (1);
        }
        
        switch (tag)
        {
            case AtomAbstract:
                newNode = readAtomAbstract(hatFile, offset);
                break;
            case AtomConstructor:
                newNode = readAtomConstructor(hatFile, offset);
                break;
            case AtomVariable:
                newNode = readAtomVariable(hatFile, offset);
                break;
            case ExpApp:
                newNode = readExpApp(hatFile, offset);
                break;
            case ExpCase:
                newNode = readExpCase(hatFile, offset);
                break;
            case ExpChar:
                newNode = readExpChar(hatFile, offset);
                break;
            case ExpConstDef:
                newNode = readExpConstDef(hatFile, offset);
                break;
            case ExpConstUse:
                newNode = readExpConstUse(hatFile, offset);
                break;
            case ExpDouble:
                newNode = readExpDouble(hatFile, offset);
                break;
            case ExpDoStmt:
                newNode = readExpDoStmt(hatFile, offset);
                break;
            case ExpFieldUpdate:
                newNode = readExpFieldUpdate(hatFile, offset);
                break;
            case ExpFloat:
                newNode = readExpFloat(hatFile, offset);
                break;
            case ExpForward:
                newNode = readExpForward(hatFile, offset);
                break;
            case ExpGuard:
                newNode = readExpGuard(hatFile, offset);
                break;
            case ExpHidden:
                newNode = readExpHidden(hatFile, offset);
                break;
            case ExpIf:
                newNode = readExpIf(hatFile, offset);
                break;
            case ExpInt:
                newNode = readExpInt(hatFile, offset);
                break;
            case ExpInteger:
                newNode = readExpInteger(hatFile, offset);
                break;
            case ExpProjection:
                newNode = readExpProjection(hatFile, offset);
                break;
            case ExpRat:
                newNode = readExpRat(hatFile, offset);
                break;
            case ExpRational:
                newNode = readExpRational(hatFile, offset);
                break;
            case ExpValueApp:
                newNode = readExpValueApp(hatFile, offset);
                break;
            case ExpValueUse:
                newNode = readExpValueUse(hatFile, offset);
                break;
            case Module:
                newNode = readModule(hatFile, offset);
                break;
            case SrcPos:
                newNode = readSrcPos(hatFile, offset);
                break;
        }
        
        return newNode;
    }
}

// Read individual nodes

/**
 * Attempt to read an atom abstract type node at a certain offset.
 *
 * @param  hatFile The file to read the node from.
 * @param  offset  The offset where the start of the node should be.
 * @return A node construct containing the read values.
 */
node* readAtomAbstract(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    
    newNode->nodeType = AtomAbstract;
    newNode->offset = offset;

    setFilePos(hatFile, offset + 1);
    newNode->params.atomAbstract.value = readString(hatFile);
    
    return newNode;
}

/**
 * Attempt to read an atom constructor type node at a certain offset.
 *
 * @param  hatFile The file to read the node from.
 * @param  offset  The offset where the start of the node should be.
 * @return A node construct containing the read values.
 */
node* readAtomConstructor(FILE *hatFile, unsigned long offset)
{
    int  argIndex;
    char tag;
    node *newNode = (node *)malloc(sizeof(node));
    
    newNode->nodeType = AtomConstructor;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    newNode->params.atomConstructor.module = readPointer(hatFile);
    newNode->params.atomConstructor.filePos = readPosition(hatFile);
    readPosition(hatFile);  /* skip position end */
    newNode->params.atomConstructor.fix = readFixPri(hatFile);
    newNode->params.atomConstructor.arity = readArity(hatFile);
    newNode->params.atomConstructor.name = readString(hatFile);
    if (newNode->params.atomConstructor.hasFields = hasFields(tag))
    {
        newNode->params.atomConstructor.args = (unsigned long *)malloc(newNode->params.atomConstructor.arity * sizeof(unsigned long));
        for (argIndex = 0; argIndex < newNode->params.atomConstructor.arity; argIndex++)
        {
            newNode->params.atomConstructor.args[argIndex] = readPointer(hatFile);
        }
    }
    
    return newNode;
}

node* readAtomVariable(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = AtomVariable;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    newNode->params.atomVariable.local = localDef(tag);
    newNode->params.atomVariable.module = readPointer(hatFile);
    newNode->params.atomVariable.filePos = readPosition(hatFile);
    readPosition(hatFile);  /* skip position end */
    newNode->params.atomVariable.fix = readFixPri(hatFile);
    newNode->params.atomVariable.arity = readArity(hatFile);
    newNode->params.atomVariable.name = readString(hatFile);
    
    return newNode;
}

/**
 * Attempt to read an expression application type node at a certain offset.
 *
 * @param  hatFile The file to read the node from.
 * @param  offset  The offset where the start of the node should be.
 * @return A node construct containing the read values.
 */
node* readExpApp(FILE *hatFile, unsigned long offset)
{
    int  argIndex;
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpApp;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expApp.hasUse = hasSrcPos(tag))
    {
        newNode->params.expApp.use = readPointer(hatFile);
    }
    newNode->params.expApp.parent = readPointer(hatFile);
    newNode->params.expApp.result = readPointer(hatFile);
    newNode->params.expApp.function = readPointer(hatFile);
    newNode->params.expApp.arity = readArity(hatFile);
    newNode->params.expApp.args = (unsigned long *)malloc(newNode->params.expApp.arity * sizeof(unsigned long));
    for (argIndex = 0; argIndex < newNode->params.expApp.arity; argIndex++)
    {
        newNode->params.expApp.args[argIndex] = readPointer(hatFile);
    }
    
    return newNode;
}

node* readExpCase(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpCase;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expCase.hasUse = hasSrcPos(tag))
    {
        newNode->params.expCase.use = readPointer(hatFile);
    }
    newNode->params.expCase.parent = readPointer(hatFile);
    newNode->params.expCase.result = readPointer(hatFile);
    newNode->params.expCase.condition = readPointer(hatFile);
    
    return newNode;
}

node* readExpChar(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpChar;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expChar.hasUse = hasSrcPos(tag))
    {
        newNode->params.expChar.use = readPointer(hatFile);
    }
    newNode->params.expChar.parent = readPointer(hatFile);
    newNode->params.expChar.value = readByte(hatFile);
    
    return newNode;
}

/**
 * Attempt to read a constant deffinition expression type node at a certain offset.
 *
 * @param  hatFile The file to read the node from.
 * @param  offset  The offset where the start of the node should be.
 * @return A node construct containing the read values.
 */
node* readExpConstDef(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    
    newNode->nodeType = ExpConstDef;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset + 1);
    newNode->params.expConstDef.parent = readPointer(hatFile);
    newNode->params.expConstDef.result = readPointer(hatFile);
    newNode->params.expConstDef.var = readPointer(hatFile);
    
    return newNode;
}

node* readExpConstUse(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpConstUse;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expConstUse.hasUse = hasSrcPos(tag))
    {
        newNode->params.expConstUse.use = readPointer(hatFile);
    }
    newNode->params.expConstUse.parent = readPointer(hatFile);
    newNode->params.expConstUse.value = readPointer(hatFile);
    
    return newNode;
}

node* readExpDoStmt(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpDoStmt;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expDoStmt.hasUse = hasSrcPos(tag))
    {
        newNode->params.expDoStmt.use = readPointer(hatFile);
    }
    newNode->params.expDoStmt.statement = readPointer(hatFile);
    
    return newNode;
}

node* readExpDouble(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpDouble;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expDouble.hasUse = hasSrcPos(tag))
    {
        newNode->params.expDouble.use = readPointer(hatFile);
    }
    newNode->params.expDouble.parent = readPointer(hatFile);
    newNode->params.expDouble.value = readDouble(hatFile);
    
    return newNode;
}

node* readExpFloat(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpFloat;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expFloat.hasUse = hasSrcPos(tag))
    {
        newNode->params.expFloat.use = readPointer(hatFile);
    }
    newNode->params.expFloat.parent = readPointer(hatFile);
    newNode->params.expFloat.value = readFloat(hatFile);
    
    return newNode;
}

node* readExpFieldUpdate(FILE *hatFile, unsigned long offset)
{
    int argIndex;
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpFieldUpdate;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expFieldUpdate.hasUse = hasSrcPos(tag))
    {
        newNode->params.expFieldUpdate.use = readPointer(hatFile);
    }
    newNode->params.expFieldUpdate.parent = readPointer(hatFile);
    newNode->params.expFieldUpdate.result = readPointer(hatFile);
    newNode->params.expFieldUpdate.arg = readPointer(hatFile);
    newNode->params.expFieldUpdate.arity = readArity(hatFile);
    newNode->params.expFieldUpdate.bindees = (unsigned long *)malloc(newNode->params.expFieldUpdate.arity * sizeof(unsigned long));
    newNode->params.expFieldUpdate.binders = (unsigned long *)malloc(newNode->params.expFieldUpdate.arity * sizeof(unsigned long));
    for (argIndex = 0; argIndex < newNode->params.expFieldUpdate.arity; argIndex++)
    {
        newNode->params.expFieldUpdate.binders[argIndex] = readPointer(hatFile);
    }
    for (argIndex = 0; argIndex < newNode->params.expFieldUpdate.arity; argIndex++)
    {
        newNode->params.expFieldUpdate.bindees[argIndex] = readPointer(hatFile);
    }
    
    return newNode;
}

/**
 * Attempt to read a forward type node at a certain offset.
 *
 * @param  hatFile The file to read the node from.
 * @param  offset  The offset where the start of the node should be.
 * @return A node construct containing the read values.
 */
node* readExpForward(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    
    newNode->nodeType = ExpForward;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset + 1);
    newNode->params.expForward.result = readPointer(hatFile);
    
    return newNode;
}

node* readExpGuard(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpGuard;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expGuard.hasUse = hasSrcPos(tag))
    {
        newNode->params.expGuard.use = readPointer(hatFile);
    }
    newNode->params.expGuard.parent = readPointer(hatFile);
    newNode->params.expGuard.result = readPointer(hatFile);
    newNode->params.expGuard.condition = readPointer(hatFile);
    
    return newNode;
}

/**
 * Attempt to read a hidden expression type node at a certain offset.
 *
 * @param  hatFile The file to read the node from.
 * @param  offset  The offset where the start of the node should be.
 * @return A node construct containing the read values.
 */
node* readExpHidden(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    
    newNode->nodeType = ExpHidden;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset + 1);
    newNode->params.expHidden.parent = readPointer(hatFile);
    newNode->params.expHidden.result = readPointer(hatFile);
    
    return newNode;
}

node* readExpIf(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpIf;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expIf.hasUse = hasSrcPos(tag))
    {
        newNode->params.expIf.use = readPointer(hatFile);
    }
    newNode->params.expIf.parent = readPointer(hatFile);
    newNode->params.expIf.result = readPointer(hatFile);
    newNode->params.expIf.condition = readPointer(hatFile);
    
    return newNode;
}

node* readExpInt(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpInt;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expInt.hasUse = hasSrcPos(tag))
    {
        newNode->params.expInt.use = readPointer(hatFile);
    }
    newNode->params.expInt.parent = readPointer(hatFile);
    newNode->params.expInt.value = readInt(hatFile);
    
    return newNode;
}

node* readExpInteger(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpInteger;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expInteger.hasUse = hasSrcPos(tag))
    {
        newNode->params.expInteger.use = readPointer(hatFile);
    }
    newNode->params.expInteger.parent = readPointer(hatFile);
    newNode->params.expInteger.value = readString(hatFile);
    
    return newNode;
}

node* readExpProjection(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpProjection;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expProjection.hasUse = hasSrcPos(tag))
    {
        newNode->params.expProjection.use = readPointer(hatFile);
    }
    newNode->params.expProjection.parent = readPointer(hatFile);
    newNode->params.expProjection.exp = readPointer(hatFile);
    
    return newNode;
}

node* readExpRat(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpRat;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expRat.hasUse = hasSrcPos(tag))
    {
        newNode->params.expRat.use = readPointer(hatFile);
    }
    newNode->params.expRat.parent = readPointer(hatFile);
    newNode->params.expRat.numerator = readInt(hatFile);
    newNode->params.expRat.denominator = readInt(hatFile);
    
    return newNode;
}

node* readExpRational(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpRational;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expRational.hasUse = hasSrcPos(tag))
    {
        newNode->params.expRational.use = readPointer(hatFile);
    }
    newNode->params.expRational.parent = readPointer(hatFile);
    newNode->params.expRational.numerator = readString(hatFile);
    newNode->params.expRational.denominator = readString(hatFile);
    
    return newNode;
}

/**
 * Attempt to read a value applicaiton expression type node at a certain offset.
 *
 * @param  hatFile The file to read the node from.
 * @param  offset  The offset where the start of the node should be.
 * @return A node construct containing the read values.
 */
node* readExpValueApp(FILE *hatFile, unsigned long offset)
{
    int  argIndex;
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpValueApp;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expValueApp.hasUse = hasSrcPos(tag))
    {
        newNode->params.expValueApp.use = readPointer(hatFile);
    }
    newNode->params.expValueApp.parent = readPointer(hatFile);
    newNode->params.expValueApp.function = readPointer(hatFile);
    newNode->params.expValueApp.arity = readArity(hatFile);
    newNode->params.expValueApp.args = (unsigned long *)malloc(newNode->params.expValueApp.arity * sizeof(unsigned long));
    for (argIndex = 0; argIndex < newNode->params.expValueApp.arity; argIndex++)
    {
        newNode->params.expValueApp.args[argIndex] = readPointer(hatFile);
    }
    
    return newNode;
}

/**
* Attempt to read a value use expression type node at a certain offset.
 *
 * @param  hatFile The file to read the node from.
 * @param  offset  The offset where the start of the node should be.
 * @return A node construct containing the read values.
 */
node* readExpValueUse(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = ExpValueUse;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    if (newNode->params.expValueUse.hasUse = hasSrcPos(tag))
    {
        newNode->params.expValueUse.use = readPointer(hatFile);
    }
    newNode->params.expValueUse.parent = readPointer(hatFile);
    newNode->params.expValueUse.value = readPointer(hatFile);
    newNode->params.expValueUse.isLambda = (newNode->params.expValueUse.value == 4);
    
    return newNode;
}

node* readModule(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    char tag;
    
    newNode->nodeType = Module;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    tag = readByte(hatFile);
    newNode->params.module.name = readString(hatFile);
    newNode->params.module.filename = readString(hatFile);
    newNode->params.module.wasTraced = tracedModule(tag);
    
    return newNode;
}

node* readSrcPos(FILE *hatFile, unsigned long offset)
{
    node *newNode = (node *)malloc(sizeof(node));
    
    newNode->nodeType = SrcPos;
    newNode->offset = offset;
    
    setFilePos(hatFile, offset);
    newNode->params.srcPos.module = readPointer(hatFile);
    newNode->params.srcPos.filePos = readPosition(hatFile);
    readPosition(hatFile);  /* skip position end */

    return newNode;
}

// Lower level file reading opps

void setFilePos (FILE *hatFile, unsigned long offset)
{
    fseek (hatFile, offset, SEEK_SET);
}

unsigned int readArity (FILE *hatFile)
{
    return (unsigned int)(unsigned char)readByte(hatFile);
}

unsigned long readPointer (FILE *hatFile)
{
    return readFourBytes(hatFile);
}

unsigned long readInt (FILE *hatFile)
{
    return readFourBytes(hatFile);
}

float readFloat (FILE *hatFile)
{
    return readFourBytes(hatFile);
}

double readDouble (FILE *hatFile)
{
    unsigned long bytes[2];
    *(bytes+1) = readFourBytes(hatFile);
    *(bytes) = readFourBytes(hatFile);
    
    return (double)(*bytes);
}

position readPosition (FILE *hatFile)  /* reads only position beginning, not end */
{
    position returnVal;
    unsigned long posn = readFourBytes(hatFile);
    
    returnVal.line = posn / 10000;
    returnVal.column = posn % 10000;
    
    return returnVal;
}

fixPri readFixPri (FILE *hatFile)
{
    int b = (int)readByte(hatFile);
    fixPri returnVal;
    
    switch (b % 4)
    {
        case 0:
            returnVal.fix = infix;
            returnVal.something = b / 4;
            break;
        case 1:
            returnVal.fix = infixr;
            returnVal.something = b / 4;
            break;
        case 2:
            returnVal.fix = infixl;
            returnVal.something = b / 4;
            break;
        case 3:
            returnVal.fix = defaultFix;
            returnVal.something = 0;
            break;
    }
    return returnVal;    
}
     
/* readstring() reads a length-annotated string from the current position
* in the file and stores it in a global buffer.
*/
char* readString (FILE *hatFile)
{
    char   *buf;
    int    charsRead,
        stringLength;

    stringLength = (int)fgetc(hatFile);
    if (stringLength == 0xff)
    {
        stringLength = (int)fgetc(hatFile);
        stringLength = (stringLength << 8) + (int)fgetc(hatFile);
    }
    
    buf = (char *)malloc(stringLength * sizeof(char));
    
    charsRead = fread(buf, sizeof(char), stringLength, hatFile);
    buf[stringLength] = '\0';
    if (charsRead < stringLength)
    {
        fprintf(stderr,"hat-anim: warning, only read %d characters of %d in string\n", charsRead, stringLength);
    }
    
    return buf;
}

fourbytes readFourBytes (FILE *hatFile)
{
    int err;
    fourbytes slot;

    err = fread(&slot, sizeof(fourbytes), 1, hatFile);
    if (err!=1)
    {
        fflush(stdout);
        fprintf(stderr, "unexpected end of trace file\n");
        fprintf(stderr, "actual file position is 0x%x\n",ftell(hatFile));
        exit(1);
    }
    return ntohl(slot);
}

char readByte (FILE *hatFile)
{
    char c;
    int err;

    err = fread(&c, sizeof(char), 1, hatFile);
    if (err!=1)
    {
        fflush(stdout);
        fprintf(stderr, "unexpected end of trace file\n");
        exit(1);
    }
    return c;
}
