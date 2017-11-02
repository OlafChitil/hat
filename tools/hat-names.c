/* This is an implementation of a linear search through the .hat file,
 * building a list of all identifiers found, with application counts.
 */
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include "finitemap.h"
#include "pathutils.h"
#include "art.h"
#include "artutils.h"
#include "hat-names.h"

#define DEBUG 0
#if DEBUG
#define HIDE(x) x
#else
#define HIDE(x)
#endif

/* What we eventually end up with is three flat arrays of information, one for
 * each of globals fns, local fns, and constructors.  These can be passed
 * en masse back to Haskell land for screen presentation.
 */
item **Globals, **Locals, **Constrs;
int GlobalsIdx=0, LocalsIdx=0, ConstrsIdx=0;

/* Return the data to Haskell-land */
FileOffset itemIdent   (item* i) { return i->thispos; }
int        itemArity   (item* i) { return (int)i->arity; }
int        itemUses    (item* i) { return i->uses; }
int        itemPending (item* i) { return i->pending; }
int        itemThunks  (item* i) { return i->thunks; }
item*      getItemPtr  (item **arr, int n) { return arr[n]; }
item**     getGlobals  (void)    { return Globals; }
item**     getLocals   (void)    { return Locals; }
item**     getConstrs  (void)    { return Constrs; }
int        getGlobalsN (void)    { return GlobalsIdx; }
int        getLocalsN  (void)    { return LocalsIdx; }
int        getConstrsN (void)    { return ConstrsIdx; }


/* There are two finitemap structures that associate file pointers
 * with items.
 *
 * The first maps an AtomVariable or AtomConstructor pointer
 * to the information (struct item) we are interested in.
 *
 * The second maps an ExpValueUse, ExpConstUse, or ExpConstDef pointer
 * to the Atom pointer it contains (struct defn).  It also maps any
 * undersaturated application (ExpApp) to the Atom pointer.  The (struct
 * defn) contains a note of the remaining arity so we can tell when full
 * saturation is reached.
 *
 * In the second map, each atom might be the target of several
 * trace pointers, e.g. different usage sites in the source lead to
 * different ExpValueUse nodes, each pointing to the same Atom;
 * e.g.2. an undersaturated ExpApp node (partial application) can
 * also map to an Atom.
 *
 * When searching, when we find:
 *      an Atom -> add it to the first map
 *      an ExpValueUse -> add it to the second map (no increment)
 *      an ExpConstDef -> add it to the second map (no increment)
 *      an ExpConstUse -> add it to the second map (increment item)
 *      an ExpApp -> check arity, if saturated, just increment item
 *                                otherwise add to 2nd map (no increment)
 */
FiniteMap map1, map2 /*, globals=0, locals=0, constrs=0*/ ;
void
map1_insert (FileOffset node, char* id, idkind k, unsigned char arity)
{
  item *it = (item*)NULL;
  it = FM_lookup(map1,(cast)(uintptr_t)node);
  if (!it) {
    it = (item*)malloc(sizeof(item));
    it->name  = id;
    it->kind  = k;
    it->arity = arity;
    it->uses    = 0;
    it->pending = 0;
    it->thunks  = 0;
    it->thispos = node;
    FM_insert(map1,(cast)(uintptr_t)node,(cast)it);
  }
}
defn*
map2_insert (FileOffset usage, FileOffset def, unsigned char ap)
{
  item *it = (item*)NULL;
  it = FM_lookup(map1,(cast)(uintptr_t)def);
  if (it) {
    defn *fn;
    fn = (defn*)malloc(sizeof(defn));
    fn->atom  = def;
    fn->arity = it->arity - ap;
    fn->next  = (defn*)0;
  //if (strcmp(it->name,">=")==0)
  //  fprintf(stderr,"map2: %s at 0x%x (%d)\n",it->name,usage,it->uses);
    FM_insert(map2,(cast)(uintptr_t)usage,(cast)fn);
    return fn;
  } else return (defn*)0;
}

#if 0
/* item_sort() doesn't do any sorting at all.  It folds one item of
 * information into one of three new finite maps (globals,locals,constrs).
 * The new maps are indexed by name rather than file position - if the
 * name is new, a new entry is created; if it exists already, the counts
 * are added together.
 */
int
item_sort (FileOffset node, item *it, void* dummy)
{
  item *already;
  switch (it->kind) {
    case TopId:
      already = FM_lookup(globals,(cast)it->name);
      if (already) {
        it->uses += already->uses;
        it->pending += already->pending;
        it->thunks  += already->thunks;
      } else
        FM_insert(globals,(cast)it->name,(cast)it);
      break;
    case LocalId:
      already = FM_lookup(locals,(cast)it->name);
      if (already) {
        it->uses += already->uses;
        it->pending += already->pending;
        it->thunks  += already->thunks;
      } else
        FM_insert(locals, (cast)it->name,(cast)it);
      break;
    case Construct:
      already = FM_lookup(constrs,(cast)it->name);
      if (already) {
        it->uses += already->uses;
        it->pending += already->pending;
        it->thunks  += already->thunks;
      } else
        FM_insert(constrs,(cast)it->name,(cast)it);
      break;
    default: break;
  }
  return False;
}
#endif

/* item_flatten() takes one item of information in the finite map (map1)
 * and places it into the appropriate flat array (Globals,Local,Constrs).
 */
int
item_flatten (FileOffset node, item *it, void* dummy)
{
  static int globalsz=64, localsz=64, constrsz=64;
  switch (it->kind) {
    case TopId:
      if (GlobalsIdx==0)
          Globals = (item**)malloc(globalsz*sizeof(item*));
      if (GlobalsIdx==globalsz) {
          globalsz *= 2;
          Globals = (item**)realloc(Globals,globalsz*sizeof(item*));
      }
      Globals[GlobalsIdx++] = it;
      break;
    case LocalId:
      if (LocalsIdx==0)
          Locals = (item**)malloc(localsz*sizeof(item*));
      if (LocalsIdx==localsz) {
          localsz *= 2;
          Locals = (item**)realloc(Locals,localsz*sizeof(item*));
      }
      Locals[LocalsIdx++] = it;
      break;
    case Construct:
      if (ConstrsIdx==0)
          Constrs = (item**)malloc(constrsz*sizeof(item*));
      if (ConstrsIdx==constrsz) {
          constrsz *= 2;
          Constrs = (item**)realloc(Constrs,constrsz*sizeof(item*));
      }
      Constrs[ConstrsIdx++] = it;
      break;
    default: break;
  }
  return False;
}

#if 0
/* For printing the variable list with application counts, we need to
 * establish the longest name and the largest count, then columnate
 * the whole list.
 */
#define MAX_IDENT 1024
char	*idents[MAX_IDENT],    *counts[MAX_IDENT];
int	ident_size[MAX_IDENT], count_size[MAX_IDENT];
int	ident_index=0;

int
item_print (char *name, item *it, void* dummy)
{
  if (it->uses || it->pending) {
    if (ident_index >= MAX_IDENT) {
      fprintf(stderr,"out of space for identifiers\n");
      return True;
    }
    idents[ident_index] = name;
    ident_size[ident_index] = strlen(name);
    counts[ident_index] = (char*)malloc(35);
    if (it->pending) {
      snprintf(counts[ident_index],34,"[1;31m%d+[0m[34m%d"
              ,it->pending,it->uses);
      count_size[ident_index] = strlen(counts[ident_index]) - 16;
    } else {
      snprintf(counts[ident_index],34,"%d",it->uses);
      count_size[ident_index] = strlen(counts[ident_index]);
    }
    ident_index++;
  }
  return False;
}
void
columnate (int width)
{
  int i, j, max_count=0, max_ident=0, column_width, columns, lines, me;
  for (i=0; i<ident_index; i++) {
    if (count_size[i] > max_count) max_count=count_size[i];
    if (ident_size[i] > max_ident) max_ident=ident_size[i];
  }
  column_width = max_count + max_ident + 4;
  columns = width / column_width;
  if (ident_index % columns)
       lines = ident_index / columns + 1;
  else lines = ident_index / columns;
  for (i=0; i<lines; i++) {
    for (j=0; j<columns; j++) {
      me = j*lines + i;
      if (me < ident_index) {
        fprintf(stdout,"[34m%*s[0m %-*s"
                      ,max_count,counts[me],max_ident+3,idents[me]);
      }
    }
    fprintf(stdout,"\n");
  }
}

/* (Previously) The main routine called from Haskell.  It gathers
 * all information about identifiers from the file and pretty-prints
 * the data in columns.
 */
void
observableInfo (int width)
{
  int err;

  if (!Globals) {
    fprintf(stdout,"Searching...\n");

    q_position = 0x10;
    fseek(HatFileSeq,q_position,SEEK_SET);

    map1 = FM_new((FMComparison)fileoffset_compare,0);
    map2 = FM_new((FMComparison)fileoffset_compare,0);
    do {
      q_oneNode();
    } while (!feof(HatFileSeq));

    globals = FM_new((FMComparison)strcmp,0);
    locals  = FM_new((FMComparison)strcmp,0);
    constrs = FM_new((FMComparison)strcmp,0);
    FM_traverse(map1,(FMTraversal)item_sort,InOrder);

    FM_traverse(globals,(FMTraversal)item_print,InOrder);
    fprintf(stdout,"[A[K");	/* cursor up and clear-to-eol */
  }

  columnate(width);
  /* note: some data-structure clean-up needed? */
}
#endif

/* The (new) main routine called from Haskell.  It gathers all the
 * counts for identifiers from the file, and dumps the data into
 * global arrays.  Haskell has to get the individual records separately
 * afterwards.
 */
void
collateIdents (void)
{
  q_position = 0x10;
  fseek(HatFileSeq,q_position,SEEK_SET);

  map1 = FM_new((FMComparison)fileoffset_compare,0);
  map2 = FM_new((FMComparison)fileoffset_compare,0);
  do {
    q_oneNode();
  } while (!feof(HatFileSeq));
  FM_traverse(map1,(FMTraversal)item_flatten,InOrder);
//FM_destroy(map1);
//FM_destroy(map2);
}


/* q_oneNode() moves the file pointer past a single node in the file.
 * As a side-effect, if it finds an AtomVariable or AtomConstructor,
 * it adds it to the global structure 'map1'.  If it finds an ExpValueUse
 * or ExpConstDef, it adds an entry in map2 from that usage to the relevant
 * Atom in map1.  If it finds an ExpApp or ExpConstUse, it instead looks
 * up the Atom ptr in map2, then looks up that Atom in map1, and finally
 * increments the usage counter.  However, in the case where an ExpApp
 * is undersaturated (discovered by comparing its arity with the arity
 * stored in map2), rather than incrementing the usage counter, we
 * instead need to add the address of the ExpApp to map2.
 */
void
q_oneNode (void)
{
  char c; int err;
  FileOffset node = q_position;
/*fprintf(stdout,"\n0x%x: ",position); fflush(stdout);*/
  err = q_fread(&c,sizeof(char),1,HatFileSeq);
  if (err!=1) return;
  switch (lower5(c)) {	/* lower 5 bits identify the TraceType */
    case ExpApp:
        if (hasSrcPos(c)) { q_readFO(); }
        q_readFO();		/* skip parent */
        { unsigned char size, next, i;
          FileOffset fun, result; defn *def; item *it;
          result = q_readFO();	/* get result */
          fun = q_readFO();	/* keep fun ptr */
          q_fread(&size,sizeof(unsigned char),1,HatFileSeq);	/* get arity */
          for (i=0; i<size; i++) q_readFO();	/* skip args */
          def = (defn*)FM_lookup(map2,(cast)(uintptr_t)fun);
          if (def) {
            defn *def2;
            it = FM_lookup(map1,(cast)(uintptr_t)def->atom);
            if (it) {
              if (size>=def->arity) {
                if (result==Entered) it->pending += 1;
                else if (result==Unevaluated) it->thunks += 1;
                else it->uses += 1;
              } else if (size < def->arity)
                def2 = map2_insert(node,def->atom,size);
            } else {
              fprintf(stderr,"unknown atom in fun at (ExpApp 0x%x)\n",node);
            }
            if (def->next) {
              it = FM_lookup(map1,(cast)(uintptr_t)def->next->atom);
              if (it) {
                if (size>=def->next->arity) {
                  if (result==Entered) it->pending += 1;
                  else if (result==Unevaluated) it->thunks += 1;
                  else it->uses += 1;
                } else if (size < def->next->arity) {
                  def2->next = (defn*)malloc(sizeof(defn));
                  def2->next->atom  = def->next->atom;
                  def2->next->arity = def->next->arity - size;
                  def2->next->next  = (defn*)0;
                }
              } else {
                fprintf(stderr,"unknown atom in CAF fun at (ExpApp 0x%x)\n",node);
              }
            }
          } else {
       //   fprintf(stderr,"unknown fun at (ExpApp 0x%x)\n",node); 
          }
        } break;
    case ExpValueApp:
        if (hasSrcPos(c)) { q_readFO(); }
        q_readFO();		/* skip parent */
        { unsigned char size, next, i;
          FileOffset fun; defn *def; item *it;
          fun = q_readFO();	/* fun ptr is an Atom ref */
          q_fread(&size,sizeof(unsigned char),1,HatFileSeq);	/* get arity */
          for (i=0; i<size; i++) q_readFO();	/* skip args */
          it = FM_lookup(map1,(cast)(uintptr_t)fun);
          if (it) {
            if (size>=it->arity) {
              it->uses += 1;
              HIDE(fprintf(stderr,"0x%x ExpValueApp: incrementing\n",node);)
            } else if (size < it->arity) {
              map2_insert(node,fun,size);
              HIDE(fprintf(stderr,"0x%x ExpValueApp: partial app\n",node);)
            }
          } else {
            fprintf(stderr,"unknown atom in fun at (ExpValueApp 0x%x)\n",node);
          }
        } break;
    case ExpValueUse:
        if (hasSrcPos(c)) { q_readFO(); }
        q_readFO();		/* skip parent */
        { FileOffset atom; item *it;
          atom = q_readFO();	/* get atom */
          if ((atom!=Lambda)&&(atom!=DoLambda)) {
            it = FM_lookup(map1,(cast)(uintptr_t)atom);
            if (it) {
              map2_insert(node,atom,0);
              if ((it->kind==Construct) && (it->arity==0)) it->uses+=1;
            } else fprintf(stderr,"unknown atom in (ExpValueUse 0x%x)\n",node);
          }
        } break;
    case ExpConstUse:
        if (hasSrcPos(c)) { q_readFO(); }
        q_readFO();		/* skip parent */
        { FileOffset exp; defn *def; item *it;
          exp = q_readFO();	/* get ExpConstDef location */
          def = FM_lookup(map2,(cast)(uintptr_t)exp);
          if (def) {
            defn *def2;
            def2 = map2_insert(node,def->atom,0);
            it = FM_lookup(map1,(cast)(uintptr_t)def->atom);
            if (it) it->uses+=1;
            else fprintf(stderr
                        ,"unknown atom in defn in (ExpConstUse 0x%x)\n",node);
            if (def->next) {
              def2->next = def->next;
            }
          } // else fprintf(stderr,"unknown defn in (ExpConstUse 0x%x)\n",node);
        } break;
    case ExpConstDef:
        { FileOffset atom, result; item *it; defn *def;
          q_readFO();		/* skip parent */
          result = q_readFO();	/* result might be significant */
          atom = q_readFO();	/* get atom */
          it = FM_lookup(map1,(cast)(uintptr_t)atom);
          if (it) def = map2_insert(node,atom,0);
          else fprintf(stderr,"unknown atom in (ExpConstDef 0x%x)\n",node);
          countCAFResult(node,result,def,0,0);
        } break;
    case AtomVariable:
        q_readFO();	/* skip module pointer */
        { int x; q_fread(&x,sizeof(int),1,HatFileSeq); } /* skip line/col */
        { int x; q_fread(&x,sizeof(int),1,HatFileSeq); } /* skip line/col */
        { char x; q_fread(&x,sizeof(char),1,HatFileSeq); } /* skip fixity */
        { unsigned char arity; char *id; idkind k;
          q_fread(&arity,sizeof(unsigned char),1,HatFileSeq);
          id = q_readString();
          k = (localDef(c) ? LocalId : TopId);
          map1_insert(node,id,k,arity);
        }
        break;
    case AtomConstructor:
        q_readFO();	/* skip module pointer */
        { int x; q_fread(&x,sizeof(int),1,HatFileSeq); } /* skip line/col */
        { int x; q_fread(&x,sizeof(int),1,HatFileSeq); } /* skip line/col */
        { char x; q_fread(&x,sizeof(char),1,HatFileSeq); } /* skip fixity */
        { unsigned char arity, tmp; char *id;
          q_fread(&arity,sizeof(unsigned char),1,HatFileSeq);
          id = q_readString();
          if (hasFields(c)) for (tmp=arity;tmp-->0;) q_readFO();
          HIDE(fprintf(stderr,"0x%x AtomConstructor: found %s\n",node,id);)
          map1_insert(node,id,Construct,arity);
        }
        break;
    case AtomAbstract:
        { char* id;
          id = q_readString();
          map1_insert(node,id,Construct,0);
        }
        break;
    default:
        q_skipNode(c);
        break;
  }
}

void
countCAFResult (FileOffset caf, FileOffset value, defn *def, unsigned char arity
               ,FileOffset mostRecentHidden)
{
  unsigned char c;
  HIDE(fprintf(stderr
              ,"countCAF: caf=0x%x, value=0x%x, arity=0x%x, hidden=0x%x\n"
              ,caf,value,arity,mostRecentHidden);)
  if ((value<DoLambda) || (value==caf)) return;
  if (value==mostRecentHidden) return;
  freadAt(value,&c,sizeof(unsigned char),1,HatFileRandom);
  switch (lower5(c)) {  /* lower 5 bits identify the TraceType */
    case ExpApp:
        HIDE(fprintf(stderr,"countCAF: found ExpApp\n");)
        if (hasSrcPos(c)) { readFO(); }
        { unsigned char size;
          FileOffset result, fun;  defn *atom;  item *it;
          readFO();	/* parent */
          result = readFO();
          fun = readFO();
          fread(&size,sizeof(unsigned char),1,HatFileRandom);   /* arity */
          if (fun < caf) {      /* fun already seen in linear scan */
            atom = (defn*)FM_lookup(map2,(cast)(uintptr_t)fun);
            if (atom) {
              it = (item*)FM_lookup(map1,(cast)(uintptr_t)atom->atom);
              if (it) {
                defn *def2;
                def2 = (defn*)malloc(sizeof(defn));
                def->next = def2;
                def2->atom  = atom->atom;
                def2->arity = it->arity - (size+arity);
                def2->next  = (defn*)0;
              }
            }
          } else { /* fun not yet seen; linear scan has not reached it */
            countCAFResult(caf,fun,def,size+arity,mostRecentHidden);
          }
        }
        break;
    case ExpValueUse:
        HIDE(fprintf(stderr,"countCAF: found ExpValueUse\n");)
        if (hasSrcPos(c)) { readFO(); }
        { FileOffset var; item *it;
          readFO();             /* parent */
          var = readFO();       /* atom */
          if ((var==Lambda)||(var==DoLambda)) return;
          if (var < caf) {
            HIDE(fprintf(stderr,"countCAF: var=0x%x < caf=0x%x\n",var,caf);)
            it = (item*)FM_lookup(map1,(cast)(uintptr_t)var);
            HIDE(if (it) fprintf(stderr,"countCAF: var=%s\n",it->name);)
            if (it && (arity<it->arity)) {
              defn *def2;
              HIDE(fprintf(stderr,"countCAF: STORING caf=0x%x var=%s, size=%d\n",caf,it->name,arity);)
              def2 = (defn*)malloc(sizeof(defn));
              def->next = def2;
              def2->atom  = var;
              def2->arity = it->arity - arity;
              def2->next  = (defn*)0;
            }
          } else {
            HIDE(fprintf(stderr,"countCAF: var=0x%x > caf=0x%x\n",var,caf);)
            countCAFResult(caf,var,def,arity,mostRecentHidden);
          }
        }
        break;
    case ExpProjection:
        HIDE(fprintf(stderr,"countCAF: found ExpProjection\n");)
        if (hasSrcPos(c)) { readFO(); }
        { FileOffset result;
          readFO();     /* parent */
          result = readFO();
          countCAFResult(caf,result,def,arity,mostRecentHidden);
        }
        break;
    case ExpHidden:
        HIDE(fprintf(stderr,"countCAF: found ExpHidden\n");)
        { FileOffset result;
          readFO();     /* parent */
          result = readFO();
          countCAFResult(caf,result,def,arity,value);
        }
        break;
    case ExpForward:
        HIDE(fprintf(stderr,"countCAF: found ExpForward\n");)
        { FileOffset result;
          result = readFO();
          countCAFResult(caf,result,def,arity,mostRecentHidden);
        }
        break;
    case AtomVariable:
        readFO();	/* skip module pointer */
        { int x; fread(&x,sizeof(int),1,HatFileRandom);	} /* skip line/col */
        { int x; fread(&x,sizeof(int),1,HatFileRandom);	} /* skip line/col */
        { char x; fread(&x,sizeof(char),1,HatFileRandom); } /* skip fixity */
        { unsigned char size; char *id;
          fread(&size,sizeof(unsigned char),1,HatFileRandom);
          id = readString();
          HIDE(fprintf(stderr,"countCAF: found AtomVariable %s\n",id);)
          if (arity < size) {
            defn *def2;
            HIDE(fprintf(stderr,"countCAF: STORING caf=0x%x var=%s, size=%d\n",caf,id,arity);)
            map1_insert(value,id,TopId,size);
            def2 = (defn*)malloc(sizeof(defn));
            def->next   = def2;
            def2->atom  = value;
            def2->arity = size - arity;
            def2->next  = (defn*)0;
          }
        }
        break;
    default:
        HIDE(fprintf(stderr,"countCAF: found something else\n");)
        break;
  }
  return;
}

