#include "art.h"
#include "parentset.h"
#include <stdio.h>
#include <stdlib.h>

#ifdef DEBUG
void
showParentSet (ParentSet* ps)
{
  int i;
  fprintf(stderr,"ParentSet { trueParent=0x%x, numOthers=%d, sizeOthers=%d\n",ps->trueParent,ps->numOthers,ps->sizeOthers);
  fprintf(stderr,"          , others=0x%x\n",ps->others);
  for (i=0; i < ps->numOthers; i++) {
    fprintf(stderr,"          , others[%d]=0x%x\n",i,ps->others[i]);
  }
  fprintf(stderr,"          } @ 0x%x\n",ps);
}
#define HIDE(x) x
#else
#define HIDE(X)
#endif

ParentSet*
newParentSet (FileOffset p)
{
  ParentSet* ps = (ParentSet*)malloc(sizeof(ParentSet));
  ps->trueParent = p;
  ps->numOthers  = 0;
  ps->sizeOthers = 0;
  ps->others     = 0;
  return ps;
}

void
extendParentSet (ParentSet* ps, FileOffset p)
{
  if (ps->numOthers == ps->sizeOthers) {
    ps->sizeOthers += CHUNK_OTHER_PARENTS;
    ps->others = (FileOffset*)realloc(ps->others
                                     ,ps->sizeOthers * sizeof(FileOffset));
    if (ps->others==0) fprintf(stderr,"extendParentSet: realloc failed\n");
  }
  HIDE(fprintf(stderr,"extendParentSet: num=%d, size=%d, new p=0x%x\n",ps->numOthers,ps->sizeOthers,p);)
  ps->others[ps->numOthers] = p;
  ps->numOthers += 1;
}

void
freeParentSet (ParentSet* ps)
{
  if (ps->sizeOthers > 0) free(ps->others);
  free(ps);
}

FileOffset
trueParent (ParentSet* ps)
{
  if (ps) return ps->trueParent; else return 0;
}

Bool
elemParentSet (FileOffset p, ParentSet* ps)
{
  int i;
  if (!ps) { return False; }
  if (p == ps->trueParent) { return True; }
  for (i=0; i < ps->numOthers; i++) {
    if (p == ps->others[i]) return True;
  }
  return False;
}

