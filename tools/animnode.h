/*
 *  anim-node.h
 *  
 *
 *  Created by Thomas Davie on Fri Nov 21 2003.
 *  Copyright (c) 2003 Thomas Davie. All rights reserved.
 *
 */

#include <stdio.h>

#ifndef _ANIM_NODE
#define _ANIM_NODE

typedef struct position_s
{
    unsigned int line;
    unsigned int column;
} position;

typedef enum 
{
    infix = 0,
    infixl,
    infixr,
    defaultFix
} fixity;

typedef struct fixPri_s
{
    fixity fix;
    int something;
} fixPri;

typedef struct node_s
{
    int nodeType;
    unsigned long offset;
    
    union
    {
        struct {char *value;} atomAbstract;
        struct {unsigned long module; position filePos; fixPri fix; int arity; char *name; char hasFields; unsigned long *args;} atomConstructor;
        struct {unsigned long module; position filePos; fixPri fix; int arity; char *name; char local;} atomVariable;
        struct {char hasUse; unsigned long use; unsigned long parent; unsigned long result; unsigned long function; int arity; unsigned long *args;} expApp;
        struct {char hasUse; unsigned long use; unsigned long parent; unsigned long result; unsigned long condition;} expCase;
        struct {char hasUse; unsigned long use; unsigned long parent; char value;} expChar;
        struct {unsigned long parent; unsigned long result; unsigned long var;} expConstDef;
        struct {char hasUse; unsigned long use; unsigned long parent; unsigned long value;} expConstUse;
        struct {char hasUse; unsigned long use; unsigned long statement;} expDoStmt;
        struct {char hasUse; unsigned long use; unsigned long parent; double value;} expDouble;
        struct {char hasUse; unsigned long use; unsigned long parent; unsigned long result; unsigned long arg; int arity; unsigned long *binders; unsigned long *bindees;} expFieldUpdate;
        struct {char hasUse; unsigned long use; unsigned long parent; float value;} expFloat;
        struct {unsigned long result;} expForward;
        struct {char hasUse; unsigned long use; unsigned long parent; unsigned long result; unsigned long condition;} expGuard;
        struct {unsigned long parent; unsigned long result;} expHidden;
        struct {char hasUse; unsigned long use; unsigned long parent; unsigned long result; unsigned long condition;} expIf;
        struct {char hasUse; unsigned long use; unsigned long parent; unsigned long value;} expInt;
        struct {char hasUse; unsigned long use; unsigned long parent; char *value;} expInteger;
        struct {char hasUse; unsigned long use; unsigned long parent; unsigned long exp;} expProjection;
        struct {char hasUse; unsigned long use; unsigned long parent; unsigned long numerator; unsigned long denominator;} expRat;
        struct {char hasUse; unsigned long use; unsigned long parent; char *numerator; char *denominator;} expRational;
        struct {char hasUse; unsigned long use; unsigned long parent; unsigned long function; int arity; unsigned long *args;} expValueApp;
        struct {char hasUse; char isLambda; unsigned long use; unsigned long parent; unsigned long value;} expValueUse;
        struct {char wasTraced; char *name; char *filename;} module;
        struct {unsigned long module; position filePos;} srcPos;
    } params;
} node;

typedef unsigned long fourbytes;

node* readNode (FILE *hatFile, unsigned long offset);

#endif
