/*
 * Utility functions for hat-observe.
 * ----------------------------------
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include "finitemap.h"
#include "art.h"
#include "artutils.h"
#include "observeutils.h"
#include "ntohl.h"

#define DEBUG 0

#if DEBUG
#define HIDE(x) x
#else
#define HIDE(x)
#endif

/* Some finite maps (i.e. lookup tables):
 * mapAtom2Info    :: FileNode -> Info
 * mapExp2Atom     :: FileNode -> Atom
 * mapContext2Atom :: FileNode -> Atom
 */
FiniteMap mapAtom2Info, mapExp2Atom, mapContext2Atom;

/* Insert into mapAtom2Info */
void
insert_mapAtom2Info (FileOffset atom, char* var, unsigned char arity)
{
  Info *info;
  info = (Info*)malloc(sizeof(Info));
  info->node  = atom;
  info->var   = var;
  info->arity = arity;
  FM_insert(mapAtom2Info,(cast)(uintptr_t)atom,(cast)info);
}
/* Insert into either mapExp2Atom or mapContext2Atom */
void
insert_map2 (FiniteMap map2, FileOffset exp, FileOffset atom
            ,unsigned char arity)
{
  Info *info = (Info*)0;
  info = FM_lookup(mapAtom2Info,(cast)(uintptr_t)atom);
  if (info) {
    Atom *fun;
    fun = (Atom*)malloc(sizeof(Atom));
    fun->atom  = atom;
    fun->arity = info->arity - arity;
    FM_insert(map2,(cast)(uintptr_t)exp,(cast)fun);
  }
}
/* free memory when no longer needed */
void
cleanup_info (FileOffset fo, Info* info) { free(info); }
void
cleanup_atom (FileOffset fo, Atom* atom) { free(atom); }


/* Some global values for an observation search.  Only one search can
 * be active at once.
 */
Bool	o_srcpos, o_context, o_recursive;
char	*o_caller, *o_callee;
char	*o_module;
unsigned	o_linecol;
FileOffset	o_src_fo, o_module_fo;
unsigned char	o_arity=0;	/* oversaturate/override arity */


/* Initialise the global state ready for a new observation to start. */
void
setObserveContext (Bool hascontext, Bool rec, int arity, char* caller)
{
/*struct sigaction sig;*/
  o_srcpos    = False;
  o_context   = hascontext;
  o_recursive = rec;
  o_caller    = strdup(caller);
  o_arity     = (unsigned char)arity;
  q_position  = 0x10;
  fseek(HatFileSeq,q_position,SEEK_SET);
  hat_interrupted = False;
/*sig.sa_handler = ctrlC; sigemptyset(&sig.sa_mask); sig.sa_flags=0; */
/*sigaction(SIGINT,&sig,(void*)0); */
  signal(SIGINT,ctrlC);
  /* Reset the maps here, freeing the tree contents also. */
  if (mapAtom2Info)    FM_destroy(mapAtom2Info);
  if (mapExp2Atom)     FM_destroy(mapExp2Atom);
  if (mapContext2Atom) FM_destroy(mapContext2Atom);
  mapAtom2Info    = FM_new((FMComparison)fileoffset_compare
                          ,(FMFreeItem)cleanup_info);
  mapExp2Atom     = FM_new((FMComparison)fileoffset_compare
                          ,(FMFreeItem)cleanup_atom);
  mapContext2Atom = FM_new((FMComparison)fileoffset_compare
                          ,(FMFreeItem)cleanup_atom);
  HIDE(fprintf(stderr,"setObserveContext: filepos=0x%x\n",ftell(HatFileSeq));)
}

/* Some more initialisation of the global state for a new observation,
 * together with the first search for a matching node.
 */
FileOffset
lookForFirstApp (char* callee)
{
  o_callee = strdup(callee);
  HIDE(fprintf(stderr,"lookForFirstApp: %s\n",o_callee);)
  return nextObservation(0);
}

FileOffset
lookForFirstSrc (int line, int col, char* module)
{
  o_srcpos  = True;
  o_module  = strdup(module);
  o_linecol = htonl((unsigned)(line*10000) + (unsigned)col);
  o_module_fo = o_src_fo = 0;
  HIDE(fprintf(stderr,"lookForFirstSrc: %s %d\n",o_module,o_linecol);)
  return nextObservation(0);
}

/* Search sequentially from the current position for the next matching
 * application.  The argument is not used - it is only there to
 * ensure that the Haskell IO action is executed.
 */
FileOffset
nextObservation (FileOffset seen)
{
  FileOffset node=0;
  if (o_srcpos) {
    do { node = srcSearch(); } while (!node);
  } else {
    do { node = varSearch(); } while (!node);
  }
  HIDE(fprintf(stderr,"nextObservation: 0x%x\n",node);)
  return node;
}

/* varSearch() moves the sequential file pointer past a single node in
 * the file, looking for a particular variable or constructor application.
 *  *  If the node is an AtomVariable matching the var we are
 *     searching for, we record it in the Atom2Info map.
 *  *  If it is an ExpValueUse whose atom value is already in the
 *     Atom2Info map, then we record it in the Exp2Atom map.
 *  *  If the node is an application whose function position
 *     matches the var we are looking for (i.e. it can be found in
 *     the Exp2Atom map), and the arity is correct, we return the
 *     node address.
 *  *  If the application is undersaturated, we simply record the
 *     address in the Exp2Atom map, as for an ExpValueUse.
 *  *  CAFS: If the node is a CAF definition whose Atom matches the var,
 *     we record its address in the Exp2Atom map.  If it is a CAF use
 *     whose definition can be found in the Exp2Atom map, we return the
 *     node address.
 *  *  Constructors are just like vars, except for zero-arity constrs,
 *     which are recorded in as ExpValueUse rather than ExpConstUse.
 *
 * If we were asked to exclude recursive calls, then we additionally keep
 * a record of all calls to this var in the Context2Atom map, and check
 * the parent to decide whether we have found a suitable matching application.
 *
 * Similarly, if the query included a context, then we keep a note of all
 * applications of that context var, and again check the parent to decide
 * whether we have found a match.
 */
FileOffset
varSearch (void)
{
  unsigned char c; int err;
  FileOffset node = q_position;
  HIDE(fprintf(stderr,"\n0x%x: ",q_position);)
  if (hat_interrupted) return 3;
  err = q_fread(&c,sizeof(unsigned char),1,HatFileSeq);
  if (err!=1) return 1;	/* Assume EOF */
  switch (lower5(c)) {  /* lower 5 bits identify the TraceType */
    case ExpApp:
        if (hasSrcPos(c)) { q_readFO(); }
        { unsigned char size, next, i;
          FileOffset parent, result, fun; Atom *atom; Info *it;
          parent = q_readFO();
          result = q_readFO();
          fun = q_readFO();
          q_fread(&size,sizeof(unsigned char),1,HatFileSeq);  /* get arity */
          for (i=0; i<size; i++) q_readFO();    /* skip args */
          /* First check if this is a possible caller context */
          if (o_context) {
            atom = (Atom*)FM_lookup(mapContext2Atom,(cast)(uintptr_t)fun);
            if (atom) {
              it = (Info*)FM_lookup(mapAtom2Info,(cast)(uintptr_t)atom->atom);
              if (it && !strcmp(it->var,o_caller)) {
                insert_map2(mapContext2Atom,node,atom->atom,size);
                HIDE(fprintf(stderr,"App of %s (context) ",it->var);)
              }
            }
          }
          /* Then check if we have found the right callee */
          atom = (Atom*)FM_lookup(mapExp2Atom,(cast)(uintptr_t)fun);
          if (atom) {
            it = (Info*)FM_lookup(mapAtom2Info,(cast)(uintptr_t)atom->atom);
            if (it && !strcmp(it->var,o_callee)) {
              HIDE(fprintf(stderr,"App of %s (callee)",it->var);)
              if (size >= atom->arity) {  /* is not undersaturated */
                if (!o_recursive) {  /* if excluding recursive calls */
                  insert_map2(mapContext2Atom,node,atom->atom,size);
                  atom = (Atom*)FM_lookup(mapContext2Atom,(cast)(uintptr_t)parent);
                  if (atom) {
                    it =(Info*)FM_lookup(mapAtom2Info,(cast)(uintptr_t)atom->atom);
                    if (it && !strcmp(it->var,o_callee)) return 0;
                    else return node;
                  } else return node;
                } else if (o_context) {	 /* if context matters */
                  atom = (Atom*)FM_lookup(mapContext2Atom,(cast)(uintptr_t)parent);
                  if (atom) {
                    it =(Info*)FM_lookup(mapAtom2Info,(cast)(uintptr_t)atom->atom);
                    HIDE(fprintf(stderr,"(context is %s)",it->var);)
                    if (it && !strcmp(it->var,o_caller)) return node;
                  }
                } else return node;  /* no context, no recursive exclusion */
              } else insert_map2(mapExp2Atom,node,atom->atom,size);
            }
          }
        } break;
    case ExpValueApp:
        if (hasSrcPos(c)) { q_readFO(); }
        { unsigned char size, next, i;
          FileOffset parent, fun; Atom *atom; Info *it;
          parent = q_readFO();  /* skip parent */
          fun = q_readFO();     /* fun ptr is an Atom ref */
          q_fread(&size,sizeof(unsigned char),1,HatFileSeq);    /* get arity */
          for (i=0; i<size; i++) q_readFO();    /* skip args */
          it = (Info*)FM_lookup(mapAtom2Info,(cast)(uintptr_t)fun);
          if (it && !strcmp(it->var,o_callee)) {
            if (size >= it->arity) {
              if (o_context) {	/* if context matters */
                atom = (Atom*)FM_lookup(mapContext2Atom,(cast)(uintptr_t)parent);
                if (atom) {
                  it =(Info*)FM_lookup(mapAtom2Info,(cast)(uintptr_t)atom->atom);
                  HIDE(fprintf(stderr,"(context is %s)",it->var);)
                  if (it && !strcmp(it->var,o_caller)) return node;
                }
              } else return node;
            } else insert_map2(mapExp2Atom,node,fun,size);
          }
        } break;
    case AtomConstructor:
    case AtomVariable:
        q_readFO();             /* skip module pointer */
        { int x; q_fread(&x,sizeof(int),1,HatFileSeq); }   /* skip line/col */
        { int x; q_fread(&x,sizeof(int),1,HatFileSeq); }   /* skip line/col */
        { char x; q_fread(&x,sizeof(char),1,HatFileSeq); } /* skip fixity */
        { unsigned char arity, tmp;
          char *id;
          q_fread(&arity,sizeof(unsigned char),1,HatFileSeq);
          id = q_readString();
          if (lower5(c)==AtomConstructor && hasFields(c))
            for (tmp=arity;tmp-->0;) q_readFO();
          HIDE(fprintf(stderr,"%s %s ",(lower5(c)==AtomVariable?"Var":"Con"),id);)
          if (!strcmp(id,o_callee) || !strcmp(id,o_caller)) {
            insert_mapAtom2Info(node,id,((o_arity>arity) ? o_arity : arity));
            HIDE(fprintf(stderr,"(recorded at 0x%x, arity=%d)",node,arity);)
            HIDE(fprintf(stderr,"(o_arity=%d)",node,o_arity);)
          }
        } break;
    case ExpValueUse:
        if (hasSrcPos(c)) { q_readFO(); }
        { FileOffset parent, atom; Info *it;
          parent = q_readFO();	/* get parent */
          atom = q_readFO();	/* get atom */
          if ((atom==Lambda)||(atom==DoLambda)) {
            if (o_context || !o_recursive) {
              Atom *atom;	/* shadows outer scope */
              atom = (Atom*)FM_lookup(mapContext2Atom,(cast)(uintptr_t)parent);
              if (atom) {
                it = (Info*)FM_lookup(mapAtom2Info,(cast)(uintptr_t)atom->atom);
                if (it) {
                  if ( (!o_recursive && !strcmp(it->var,o_callee))
                     || (o_context && !strcmp(it->var,o_caller)) ) {
                    insert_map2(mapContext2Atom,node,atom->atom,0);
                  }
                }
              }
            }
          } else {
            it = (Info*)FM_lookup(mapAtom2Info,(cast)(uintptr_t)atom);
            if (it) {
              if (!strcmp(it->var,o_callee)) {
                insert_map2(mapExp2Atom,node,atom,0);
                if (!o_recursive) insert_map2(mapContext2Atom,node,atom,0);
              }
              if (!strcmp(it->var,o_caller)) {
                insert_map2(mapContext2Atom,node,atom,0);
              }
              HIDE(fprintf(stderr,"ValueUse %s",it->var);)
              if (it->arity==0 && !strcmp(it->var,o_callee)) return node;
            }
          }
        } break;
    case ExpConstDef:
        { FileOffset atom, result; Info *it;
          q_readFO();		/* skip parent */
          result = q_readFO();	/* result might contain desired function */
          atom = q_readFO();	/* get atom */
          it = (Info*)FM_lookup(mapAtom2Info,(cast)(uintptr_t)atom);
          if (it) {
            HIDE(fprintf(stderr,"ConstDef %s",it->var);)
            if (!strcmp(it->var,o_callee))
              insert_map2(mapExp2Atom,node,atom,0);
            else if (o_context && !strcmp(it->var,o_caller))
              insert_map2(mapContext2Atom,node,atom,0);
          //if (!strcmp(it->var,o_callee)) return node;
          } else {
            HIDE(fprintf(stderr,"ConstDef (searchCAF)");)
            searchCAFResult(node,result,0,0);
          }
        } break;
    case ExpConstUse:
        HIDE(fprintf(stderr,"ConstUse ");)
        if (hasSrcPos(c)) { q_readFO(); }
        { FileOffset exp; Atom *def; Info *it;
          q_readFO();		/* skip parent */
          exp = q_readFO();	/* get ExpConstDef location */
          def = (Atom*)FM_lookup(mapExp2Atom,(cast)(uintptr_t)exp);
          if (def) {
            insert_map2(mapExp2Atom,node,def->atom,def->arity);	// allows oversat apps
            it = FM_lookup(mapAtom2Info,(cast)(uintptr_t)def->atom);
            HIDE(if (it) fprintf(stderr,"%s",it->var);)
            if (it && !strcmp(it->var,o_callee)) return node;
          } else {
            def = (Atom*)FM_lookup(mapContext2Atom,(cast)(uintptr_t)exp);
            if (def) {
              it = FM_lookup(mapAtom2Info,(cast)(uintptr_t)def->atom);
              if (it && !strcmp(it->var,o_caller))
                insert_map2(mapContext2Atom,node,def->atom,def->arity);
            }
          }
        } break;
    case ExpGuard:
    case ExpCase:
    case ExpIf:
        if (o_context || !o_recursive) {
          if (hasSrcPos(c)) { q_readFO(); }
          { FileOffset parent; Atom *atom; Info *it;
            parent = q_readFO();
            q_readFO();	/* skip result */
            q_readFO();	/* skip condition */
            atom = (Atom*)FM_lookup(mapContext2Atom,(cast)(uintptr_t)parent);
            if (atom) {
              it = (Info*)FM_lookup(mapAtom2Info,(cast)(uintptr_t)atom->atom);
              if (it) {
                if ( (!o_recursive && !strcmp(it->var,o_callee))
                   || (o_context && !strcmp(it->var,o_caller)) ) {
                  insert_map2(mapContext2Atom,node,atom->atom,0);
                }
              }
            }
          }
        } else q_skipNode(c);
        break;
    default:
        q_skipNode(c);
        break;
  }
  return 0;	/* if we didn't find an application */
}

/* srcSearch() moves the sequential file pointer past a single node in
 * the file, looking for a particular usage position.  Having found the
 * usage position, we then look for any application or value used at that
 * position.
 */
FileOffset
srcSearch (void)
{
  unsigned char c; int err;
  FileOffset node = q_position;
  HIDE(fprintf(stderr,"\n0x%x: ",q_position);)
  if (hat_interrupted) return 3;
  err = q_fread(&c,sizeof(unsigned char),1,HatFileSeq);
  if (err!=1) return 1;	/* Assume EOF */
  if (!o_src_fo) {
    switch (lower5(c)) {  /* lower 5 bits identify the TraceType */
      case Module:
          HIDE(fprintf(stderr,"Module ");)
          if (o_module_fo) q_skipNode(c);
          else {
            char *file;
            q_readString();	/* skip module name */
            file = q_readString();
            HIDE(fprintf(stderr,"File %s ",file);)
            if (!strcmp(file,o_module)) {
              HIDE(fprintf(stderr,"(Got it!)");)
              o_module_fo = node;
            }
          } break;
      case SrcPos:
          HIDE(fprintf(stderr,"SrcPos ");)
          if (!o_module_fo) q_skipNode(c);
          else {
            FileOffset mod; unsigned linecol;
            mod = q_readFO();
            q_fread(&linecol,sizeof(unsigned),1,HatFileSeq);
            { int x; q_fread(&x,sizeof(int),1,HatFileSeq); }/* skip line/col */
            HIDE(fprintf(stderr,"Line/Col %d ",linecol);)
            if (mod==o_module_fo && linecol==o_linecol) {
              HIDE(fprintf(stderr,"(Got it!)");)
              o_src_fo = node;
            }
          } break;
      default:
          q_skipNode(c);
          break;
    }
  } else {
    switch (lower5(c)) {  /* lower 5 bits identify the TraceType */
      case ExpApp:
          HIDE(fprintf(stderr,"ExpApp ");)
          if (hasSrcPos(c)) {
            unsigned char size, i; FileOffset fo;
            fo = q_readFO();
            HIDE(fprintf(stderr,"SrcPos=0x%x ",fo);)
            q_readFO(); q_readFO(); q_readFO();
            q_fread(&size,sizeof(unsigned char),1,HatFileSeq);	/* get arity */
            for (i=0; i<size; i++) q_readFO();	/* skip args */
            if (fo==o_src_fo) {
              HIDE(fprintf(stderr,"(Got it!)");)
              return node;
            }
          } else q_skipNode(c);
          break;
      default:
          q_skipNode(c);
          break;
    }
  }
  return 0;	/* if we didn't find an application at the src location */
}


/* searchCAFResult() takes the result pointer of a CAF and searches the
 * result chain to discover whether the CAF ever reduced to a (partial)
 * application of the function we want to observe.  If so, then the
 * original CAF node should be memoised as if it were the function we
 * want.  The arity to memoise however, is the extent of undersaturatedness
 * of the partial application.
 */
void
searchCAFResult (FileOffset caf, FileOffset value, unsigned char arity
                ,FileOffset mostRecentHidden)
{
  unsigned char c;
  HIDE(fprintf(stderr
              ,"searchCAF: caf=0x%x, value=0x%x, arity=0x%x, hidden=0x%x\n"
              ,caf,value,arity,mostRecentHidden);)
  if ((value<DoLambda) || (value==caf)) return;
  if (value==mostRecentHidden) return;
  freadAt(value,&c,sizeof(unsigned char),1,HatFileRandom);
  switch (lower5(c)) {  /* lower 5 bits identify the TraceType */
    case ExpApp:
        HIDE(fprintf(stderr,"searchCAF: found ExpApp\n");)
        if (hasSrcPos(c)) { readFO(); }
        { unsigned char size;
          FileOffset result, fun;  Atom *atom;  Info *it;
          readFO();
          result = readFO();
          fun = readFO();
          myfread(&size,sizeof(unsigned char),1,HatFileRandom);	/* arity */
          if (fun < caf) {	/* fun already seen in linear scan */
            if (o_context) {
              atom = (Atom*)FM_lookup(mapContext2Atom,(cast)(uintptr_t)fun);
              if (atom) {
                it = (Info*)FM_lookup(mapAtom2Info,(cast)(uintptr_t)atom->atom);
                if (it && !strcmp(it->var,o_caller)) {
                  insert_map2(mapContext2Atom,caf,atom->atom,size);
                }
              }
            }
            atom = (Atom*)FM_lookup(mapExp2Atom,(cast)(uintptr_t)fun);
            if (atom) {
              it = (Info*)FM_lookup(mapAtom2Info,(cast)(uintptr_t)atom->atom);
              if (it && !strcmp(it->var,o_callee)
                     && ((size+arity)<atom->arity)) {
                HIDE(fprintf(stderr,"searchCAF: STORING caf=0x%x var=%s, size=%d\n",caf,it->var,size);)
                insert_map2(mapExp2Atom,caf,atom->atom,size);
              }
            }
          } else { /* fun not yet seen; linear scan has not reached it */
            searchCAFResult(caf,fun,size+arity,mostRecentHidden);
          }
       // if (result!=caf) searchCAFResult(caf,result,arity,mostRecentHidden);
        }
        break;
    case ExpValueUse:
        HIDE(fprintf(stderr,"searchCAF: found ExpValueUse\n");)
        if (hasSrcPos(c)) { readFO(); }
        { FileOffset var; Info *it;
          readFO();		/* parent */
          var = readFO();	/* atom */
          if (var==Lambda) return;
          if (var==DoLambda) return;
          if (var < caf) {
            HIDE(fprintf(stderr,"searchCAF: var=0x%x < caf=0x%x\n",var,caf);)
            it = (Info*)FM_lookup(mapAtom2Info,(cast)(uintptr_t)var);
            HIDE(if (it) fprintf(stderr,"searchCAF: var=%s\n",it->var);)
            if (it && !strcmp(it->var,o_callee) && (arity<it->arity)) {
              HIDE(fprintf(stderr,"searchCAF: STORING caf=0x%x var=%s, size=%d\n",caf,it->var,arity);)
              insert_map2(mapExp2Atom,caf,var,arity);
            }
          } else {
            HIDE(fprintf(stderr,"searchCAF: var=0x%x > caf=0x%x\n",var,caf);)
            searchCAFResult(caf,var,arity,mostRecentHidden);
          }
        }
        break;
    case ExpProjection:
        HIDE(fprintf(stderr,"searchCAF: found ExpProjection\n");)
        if (hasSrcPos(c)) { readFO(); }
        { FileOffset result;
          readFO();	/* parent */
          result = readFO();
          searchCAFResult(caf,result,arity,mostRecentHidden);
        }
        break;
    case ExpHidden:
        HIDE(fprintf(stderr,"searchCAF: found ExpHidden\n");)
        { FileOffset result;
          readFO();	/* parent */
          result = readFO();
          searchCAFResult(caf,result,arity,value);
        }
        break;
    case ExpForward:
        HIDE(fprintf(stderr,"searchCAF: found ExpForward\n");)
        { FileOffset result;
          result = readFO();
          searchCAFResult(caf,result,arity,mostRecentHidden);
        }
        break;
    case AtomVariable:
        readFO();	/* skip module pointer */
        { int x; myfread(&x,sizeof(int),1,HatFileRandom); }   /* skip line/col */
        { int x; myfread(&x,sizeof(int),1,HatFileRandom); }   /* skip line/col */
        { char x; myfread(&x,sizeof(char),1,HatFileRandom); } /* skip fixity */
        { unsigned char size;
          char *id;
          myfread(&size,sizeof(unsigned char),1,HatFileRandom);
          id = readString();
          HIDE(fprintf(stderr,"searchCAF: found AtomVariable %s\n",id);)
          if (!strcmp(id,o_callee) && (arity<size)) {
            insert_mapAtom2Info(value,id,(o_arity>size ? o_arity : size));
              HIDE(fprintf(stderr,"searchCAF: STORING caf=0x%x var=%s, size=%d\n",caf,id,arity);)
            insert_map2(mapExp2Atom,caf,value,arity);
          }
          if (o_context && !strcmp(id,o_caller) && (arity<size)) {
            insert_mapAtom2Info(value,id,size);
            insert_map2(mapContext2Atom,caf,value,arity);
          }
        }
        break;
    default:
        HIDE(fprintf(stderr,"searchCAF: found something else\n");)
        break;
  }
  return;
}

