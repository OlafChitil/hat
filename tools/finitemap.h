#ifndef _FINITEMAP_H
#define _FINITEMAP_H

#include "checkglib.h"

#ifndef HAVE_GLIB

/* external API */
typedef struct _FM* FiniteMap;
/* old: typedef void* cast; */
typedef uint32_t cast;
typedef int  (*FMComparison) (cast a, cast b);
typedef int  (*FMTraversal)  (cast key, cast value, cast dummy);
typedef void (*FMFreeItem)   (cast key, cast value);
typedef enum { PreOrder, InOrder, PostOrder } Ordering;

FiniteMap	FM_new		(FMComparison cmp, FMFreeItem del);
cast		FM_lookup	(FiniteMap fm, cast key);
void		FM_insert	(FiniteMap fm, cast key, cast value);
void		FM_traverse	(FiniteMap fm, FMTraversal f, Ordering o);
void		FM_destroy	(FiniteMap fm);

/* internals */
typedef struct _Tree* Tree;
struct _Tree {          /* balanced tree implementation of FiniteMap */
  cast key;
  cast value;
  int size;             /* number of non-null branches in this subtree (>=1) */
  Tree left, right;     /* abs(size(left)-size(right)) <= SIZE_RATIO */
};
struct _FM {
  Tree root;
  FMComparison cmp;
  FMFreeItem del;
};

#else	/* use glib instead */

#include <glib.h>

typedef GTree*	FiniteMap;
#define cast		gpointer
#define FMComparison	GCompareFunc
#define FMTraversal	GTraverseFunc
#define FMFreeItem	

#define Ordering	GTraverseType
#define PreOrder	G_PRE_ORDER
#define InOrder		G_IN_ORDER
#define PostOrder	G_POST_ORDER

#define FM_new(cmp,del)		g_tree_new(cmp)
#define FM_lookup(fm,key)	g_tree_lookup(fm,key)
#define FM_insert(fm,key,value)	g_tree_insert(fm,key,value)
#define FM_traverse(fm,f,o)	g_tree_traverse(fm,f,o,(gpointer)0)
#define FM_destroy(fm)		g_tree_destroy(fm)

#endif
#endif
