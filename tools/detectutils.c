/*
 *  Utility functions for hat-detect.
 */
#include <stdio.h>
#include <errno.h>

#ifdef DEBUG
#define HIDE(x) x
#else
#define HIDE(x)
#endif

#include "detectutils.h"

/* Return the next child of any of the parents in the ParentSet.  */
FileOffset
nextChild (ParentSet* ps)
{
  FileOffset node=0;
  /* keep searching one node at a time until a valid child turns up */
  do { node = childSearch(ps); } while (!node);
  return node;
}


/* Skip past one node of the file, returning its position if it is a child
 * of any of the parents in the ParentSet, or 0 otherwise.
 */
FileOffset
childSearch (ParentSet* ps)
{
  unsigned char c; int err;
  FileOffset parent, node = q_position;
  if (hat_interrupted) return 3;
  err = q_fread(&c,sizeof(unsigned char),1,HatFileSeq);
  if (err!=1) {
    return 1;	/* Assume EOF */
  }
  HIDE(fprintf(stderr,"childSearch: 0x%x, %d\n",node,c);)
  switch (lower5(c)) {	/* lower 5 bits identify the TraceType */
    case ExpApp:
    case ExpValueApp:
    case ExpValueUse:
    case ExpConstUse:
    case ExpProjection:
    case ExpConstDef:
    case ExpGuard:
    case ExpCase:
    case ExpIf:
    case ExpFieldUpdate:
    case ExpHidden:
    case ExpForward:
    case ExpDoStmt:
        parent = q_skipNode(c);
        if (elemParentSet(parent,ps)) return node; else return 0;
        break;
    default:
        q_skipNode(c); return 0; break;
  }
}


/* Print a message to stderr if the asserted condition is violated. */
void
assert (Bool cond, char *act)
{
  if (!cond) {
    fprintf(stderr,"Assertion failed: %s at 0x%x\n",act,q_position);
  }
}

/* Look for the file node that corresponds to the definition of Main.main */
FileOffset
findMainUse (Bool findUse)
{
    FileOffset fo;
    FileOffset atom;
    FileOffset def;
    FileOffset use;
    char c;
    char *str;
    
    // We should find the main module at 0x10
    fseek(HatFileSeq,0x10,SEEK_SET); q_position=0x10;
    q_fread(&c,sizeof(char),1,HatFileSeq);
    assert (lower5(c)==Module, "Module tag");
    str = q_readString();
    assert (!strcmp(str,"Main"),"Module is Main");
    
    // The next thing shoult be the atom variable belonging to that module
    q_readString();
    atom = q_position;
    q_fread(&c,sizeof(char),1,HatFileSeq);
    assert (lower5(c)==AtomVariable, "AtomVariable tag");
    fo = q_readFO();
    assert (fo==0x10, "AtomVariable module is Main");
    
    {	/* skip defnpos */
        int x;
        q_fread(&x,sizeof(int),1,HatFileSeq);
    }
    {	/* skip defnpos */
        int x;
        q_fread(&x,sizeof(int),1,HatFileSeq);
    }
    {	/* skip fixity */
        char x;
        q_fread(&x,sizeof(char),1,HatFileSeq);
    }
    
    // Main takes no arguments
    q_fread(&c,sizeof(char),1,HatFileSeq);
    assert (c==0, "AtomVariable has arity 0");
    
    // Make sure the deffinition is main
    str = q_readString();
    assert (!strcmp(str,"main"),"AtomVariable is main");
    
    // Make sure there is a constant definition pointing at main
    def = q_position;
    q_fread(&c,sizeof(char),1,HatFileSeq);
    assert (lower5(c)==ExpConstDef, "ExpConstDef tag");
    q_readFO(); q_readFO();
    fo = q_readFO();
    assert (fo==atom, "ExpConstDef points to AtomVariable main");
    
    // Make sure that main is called
    use = q_position;
    q_fread(&c,sizeof(char),1,HatFileSeq);
    assert (lower5(c)==ExpConstUse, "ExpConstUse tag");
    if (hasSrcPos(c)) q_readFO();
    q_readFO();
    fo = q_readFO();
    assert(fo==def, "ExpConstUse points to ExpConstDef");
    
    if (findUse)
    {
        return use;
    }
    else
    {
        return def;
    }
    
    /* postcondition: q_position points to first node following ExpConstUse */
}


/* Decide whether to ask a question in hat-detect, by determining whether
 * any of the functional identifiers in an expression are untrusted.
 * Originally, we thought that only the 'real' function being applied
 * mattered.  But in a higher-order application like (filter f ...) then
 * we are probably interested in asking the question if f is suspect, even
 * though filter is trusted.  Even this is only an approximation, because
 * (filter f [] = []) is an uninteresting question - but it is too expensive
 * to search for any children of the node, just to rule it out when no 
 * children are found. */
Bool
anySuspect (FileOffset fo)
{
  char c, i;
  FileOffset ptr[20];		/* applications only recorded up to arity 15 */
  FileOffset r;
  if (fo==0) return False;
  freadAt(fo,&c,sizeof(char),1,HatFileRandom);
  switch (lower5(c)) {
    case ExpApp:
	    HIDE(fprintf(stderr,"anysuspect: 0x%x ExpApp\n",fo);)
        if (hasSrcPos(c)) { readFO(); }		/* skip use position */
	readFO(); readFO();			/* skip parent and result */
	ptr[0] = readFO();			/* get fun */
	fread(&c,sizeof(char),1,HatFileRandom);	/* get arity */
	for (i=1; i<=c; i++) ptr[i] = readFO();	/* get args */
	if (anySuspect(ptr[0])) return True;
	for (i=1; i<=c; i++) {
	  if (anySuspect(getResult(ptr[i],True))) return True;
	}
	return False;				/* fall-through case */
	break;
    case ExpProjection:
    case ExpValueUse:
	    HIDE(fprintf(stderr,"anysuspect: 0x%x ExpValueUse\n",fo);)
        if (hasSrcPos(c)) { readFO(); }		/* skip use position */
	readFO();				/* skip parent */
	ptr[0] = readFO();			/* get Atom */
	return anySuspect(ptr[0]);
	break;
    case AtomVariable:
	    HIDE(fprintf(stderr,"anysuspect: 0x%x AtomVariable\n",fo);)
	ptr[0] = readFO();			/* get Module */
	return anySuspect(ptr[0]);
	break;
    case Module:
	    HIDE(fprintf(stderr,"anysuspect: 0x%x Module\n",fo);)
	return tracedModule(c);			/* check trust bit */
	break;
    default:
       	return False;
	break;
  }
}
