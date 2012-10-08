#include <stdlib.h>
#include <stdio.h>
#include "finitemap.h"

#ifndef HAVE_GLIB
/* our own implementation of balanced trees, based on Data.FiniteMap */

#define SIZE_RATIO	5	/* max difference in size of l/r subtrees */
#ifndef NULL
#define NULL 0
#endif

/* internal API */
int	Tree_size	(Tree t);
Tree	Tree_new	(cast key, cast value);
cast	Tree_lookup	(Tree t, FMComparison cmp, cast key);
Tree	fixSize		(Tree t);
Tree	Tree_insert	(Tree t, FMComparison cmp, cast key, cast value);
Tree	mkBalTree	(Tree t, Tree l, Tree r);
void	Tree_traverse	(Tree t, FMTraversal f, Ordering o);
void	Tree_destroy	(Tree t, FMFreeItem del);


/* implementation */
FiniteMap
FM_new (FMComparison cmp, FMFreeItem del)
{
  FiniteMap fm;
  fm = (FiniteMap)malloc(sizeof(struct _FM));
  fm->root = (Tree)NULL;
  fm->cmp  = cmp;
  fm->del  = del;
  return fm;
}

Tree
Tree_new (cast key, cast value)
{
  Tree t;
  t = (Tree)malloc(sizeof(struct _Tree));
  t->key   = key;
  t->value = value;
  t->size  = 1;
  t->left  = (cast)NULL;
  t->right = (cast)NULL;
  return t;
}

int
Tree_size (Tree t) { return (t? t->size : 0); }

cast
FM_lookup (FiniteMap fm, cast key)
{
  return Tree_lookup(fm->root,fm->cmp,key);
}

cast
Tree_lookup (Tree t, FMComparison cmp, cast key)
{
  int discrim;
  if (t==NULL) return (cast)NULL;
  discrim = cmp(key,t->key);
  if (discrim==0) {
    return t->value;
  } else if (discrim<0) {
    return Tree_lookup(t->left,cmp,key);
  } else {
    return Tree_lookup(t->right,cmp,key);
  }
}

void
FM_insert (FiniteMap fm, cast key, cast value)
{
  fm->root = Tree_insert(fm->root, fm->cmp, key, value);
}

Tree
Tree_insert (Tree t, FMComparison cmp, cast key, cast value)
{
  int discrim;
  if (t==NULL) return Tree_new(key,value);
  discrim = cmp(key,t->key);
  if (discrim==0) {
    t->value = value;	/* overwrite old value */
    return t;
  } else if (discrim<0) {
    return mkBalTree (t, Tree_insert (t->left, cmp, key, value), t->right);
  } else {
    return mkBalTree (t, t->left, Tree_insert (t->right, cmp, key, value));
  }
}

Tree
fixSize (Tree t)
{
  t->size = Tree_size(t->left) + Tree_size(t->right) + 1;
  return t;
}

Tree
mkBalTree (Tree t, Tree l, Tree r)
{
  int sizeL, sizeR, sizeT;
  sizeL = Tree_size(l);
  sizeR = Tree_size(r);
  sizeT = sizeL+sizeR+1;
  if (sizeL+sizeR < 2) {			/* only one subtree */
    t->left  = l;
    t->right = r;
    t->size  = sizeT;
    return t;
  } else if (sizeR > SIZE_RATIO*sizeL) {	/* right tree too big */
    if (Tree_size(r->left) < 2*Tree_size(r->right)) {
      /* transfer single branch L */
      t->left  = l;
      t->right = r->left;
      r->left  = fixSize(t);
      r->size  = sizeT;
      return r;
    } else {
      /* transfer double L branch of R to root */
      Tree rl;
      rl        = r->left;
      t->left   = l;
      t->right  = rl->left;
      r->left   = rl->right;
      rl->left  = fixSize(t);
      rl->right = fixSize(r);
      rl->size  = sizeT;
      return rl;
    }
  } else if (sizeL > SIZE_RATIO*sizeR) {	/* left tree too big */
    if (Tree_size(l->right) < 2*Tree_size(l->left)) {
      /* transfer single branch R */
      t->right = r;
      t->left  = l->right;
      l->right = fixSize(t);
      l->size  = sizeT;
      return l;
    } else {
      /* transfer double R branch of L to root */
      Tree lr;
      lr        = l->right;
      t->right  = r;
      t->left   = lr->right;
      l->right  = lr->left;
      lr->right = fixSize(t);
      lr->left  = fixSize(l);
      lr->size  = sizeT;
      return lr;
    }
  } else {					/* no imbalance */
    t->left  = l;
    t->right = r;
    t->size  = sizeT;
    return t;
  }
}

void
FM_traverse (FiniteMap fm, FMTraversal f, Ordering o)	/* ordered traversal */
{
  if (fm->root) Tree_traverse (fm->root, f, o);
}

void
Tree_traverse (Tree t, FMTraversal f, Ordering o)
{
  if (o==PreOrder)  (*f)(t->key, t->value, (cast)0);
  if (t->left)  Tree_traverse(t->left, f, o);
  if (o==InOrder)   (*f)(t->key, t->value, (cast)0);
  if (t->right) Tree_traverse(t->right, f, o);
  if (o==PostOrder) (*f)(t->key, t->value, (cast)0);
}

void
FM_destroy (FiniteMap fm)
{
  if (fm->root) Tree_destroy(fm->root, fm->del);
  free(fm);
}

void
Tree_destroy (Tree t, FMFreeItem del)
{
  if (t->left)  Tree_destroy(t->left, del);
  if (t->right) Tree_destroy(t->right, del);
  if (del) { (*del)(t->key, t->value); }
  free(t);
}

#endif

