#ifndef _ART_H
#define _ART_H

#include <stdint.h>

/* FileOffset is just a byte-pointer into the file.
 * There are four distinguished values that are otherwise not valid pointers.
 */
typedef uint32_t FileOffset;
#define Root            0x0
#define Unevaluated     0x1
#define Entered         0x2
#define Interrupted     0x3
#define Lambda          0x4
#define DoLambda        0x5

/* A tag byte has four interior fields:
 * 7    | 6    | 5    | 4    3    2    1    0
 * free | src? | bool |  tag value          |
 */

/* bit 6 indicates whether there is a SrcPos for the use-position of an expr */
#define HasSrcPos	0x40
#define hasSrcPos(x)  ((x)&HasSrcPos)

/* bit 5 holds a boolean which is used for various purposes in different exps */
#define IsEntered	0x20
#define LocalDef	0x20
#define HasFields	0x20
#define TracedModule	0x20
#define isEntered(x)	((x)&isEntered)
#define localDef(x)	((x)&LocalDef)
#define hasFields(x)	((x)&HasFields)
#define tracedModule(x)	((x)&TracedModule)

/* Tag values: the lower 5 bits of the tag byte */
#define lower5(x)       ((x)&0x1f)

#define Module          0x00
#define SrcPos          0x01

#define ExpApp          0x02
#define ExpValueApp     0x03
#define ExpChar         0x04
#define ExpInt          0x05
#define ExpInteger      0x06
#define ExpRat          0x07
#define ExpRational     0x08
#define ExpFloat        0x09
#define ExpDouble       0x0a
#define ExpValueUse     0x0b
#define ExpConstUse     0x0c
#define ExpConstDef     0x0d
#define ExpGuard        0x0e
#define ExpCase         0x0f
#define ExpIf           0x10
#define ExpFieldUpdate  0x11
#define ExpProjection   0x12
#define ExpHidden       0x13
#define ExpForward      0x14
#define ExpDoStmt       0x15

#define AtomVariable    0x1a
#define AtomConstructor 0x1b
#define AtomAbstract    0x1c

#define ListCons        0x1d

#define eof		0x1f	/* Note: dummy eof value is lower5(-1) */

/* Handy definitions for Booleans */
typedef unsigned int Bool;
#ifndef False
#define False   0
#endif
#ifndef True
#define True    1
#endif

#endif
