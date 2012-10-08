/* interface for writing the ART trace */

#ifndef _HAT_C_H
#define _HAT_C_H

#include "art.h"

void hat_Open(char *progname);
void hat_Close(void);
void hat_Error(char* errmsg);
void hat_ErrorExit(char* errmsg, FileOffset trace, int ecode);
void hat_ArithmeticError(int);
void hat_Interrupt(int);
void hat_Abort(char *msg);
void hat_OutputTrace(FileOffset trace, char *output);
Bool hat_Hidden(FileOffset node);

FileOffset
mkRoot(void);

FileOffset
mkModule(char *modname, char *srcfile, Bool traced);

FileOffset
mkSrcPos(FileOffset moduleTraceInfo,int begin,int end);

/* Exp nodes; if use is 0, then the variant without a use field is written */

FileOffset
mkResApp1(FileOffset parent,FileOffset use
         ,FileOffset fun,FileOffset arg1);

FileOffset
mkApp1(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1);

FileOffset
mkApp2(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2);

FileOffset
mkApp3(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3);

FileOffset
mkApp4(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4);

FileOffset
mkApp5(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5);

FileOffset
mkApp6(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6);

FileOffset
mkApp7(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7);

FileOffset
mkApp8(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8);

FileOffset
mkApp9(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9);

FileOffset
mkApp10(FileOffset parent,FileOffset use,FileOffset fun
       ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
       ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
       ,FileOffset arg9,FileOffset arg10);

FileOffset
mkApp11(FileOffset parent,FileOffset use,FileOffset fun
       ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
       ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
       ,FileOffset arg9,FileOffset arg10,FileOffset arg11);

FileOffset
mkApp12(FileOffset parent,FileOffset use,FileOffset fun
       ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
       ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
       ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12);

FileOffset
mkApp13(FileOffset parent,FileOffset use,FileOffset fun
       ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
       ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
       ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12
       ,FileOffset arg13);

FileOffset
mkApp14(FileOffset parent,FileOffset use,FileOffset fun
       ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
       ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
       ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12
       ,FileOffset arg13,FileOffset arg14);

FileOffset
mkApp15(FileOffset parent,FileOffset use,FileOffset fun
       ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
       ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
       ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12
       ,FileOffset arg13,FileOffset arg14,FileOffset arg15);

FileOffset
mkValueApp1(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1);

FileOffset
mkValueApp2(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2);

FileOffset
mkValueApp3(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3);

FileOffset
mkValueApp4(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4);

FileOffset
mkValueApp5(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5);

FileOffset
mkValueApp6(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6);

FileOffset
mkValueApp7(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7);

FileOffset
mkValueApp8(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8);

FileOffset
mkValueApp9(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9);

FileOffset
mkValueApp10(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9,FileOffset arg10);

FileOffset
mkValueApp11(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9,FileOffset arg10,FileOffset arg11);

FileOffset
mkValueApp12(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12);

FileOffset
mkValueApp13(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12
      ,FileOffset arg13);

FileOffset
mkValueApp14(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12
      ,FileOffset arg13,FileOffset arg14);

FileOffset
mkValueApp15(FileOffset parent,FileOffset use,FileOffset fun
      ,FileOffset arg1,FileOffset arg2,FileOffset arg3,FileOffset arg4
      ,FileOffset arg5,FileOffset arg6,FileOffset arg7,FileOffset arg8
      ,FileOffset arg9,FileOffset arg10,FileOffset arg11,FileOffset arg12
      ,FileOffset arg13,FileOffset arg14,FileOffset arg15);

FileOffset
mkChar(FileOffset parent,FileOffset use,char c);

FileOffset
mkInt(FileOffset parent,FileOffset use,int i);

FileOffset
mkInteger(FileOffset parent,FileOffset use,char *i);

FileOffset
mkRat(FileOffset parent,FileOffset use,int num,int denom);

FileOffset
mkRational(FileOffset parent,FileOffset use
          ,char *num,char *denom);

FileOffset
mkFloat(FileOffset parent,FileOffset use,float f);

FileOffset
mkDouble(FileOffset parent,FileOffset use,double d);

FileOffset
mkValueUse(FileOffset parent,FileOffset use,FileOffset value);

FileOffset
mkConstUse(FileOffset parent,FileOffset use,FileOffset con);

FileOffset
mkConstDef(FileOffset context,FileOffset var);

FileOffset
mkGuard(FileOffset parent,FileOffset use,FileOffset cond);

FileOffset
mkCase(FileOffset parent,FileOffset use,FileOffset cond);

FileOffset
mkIf(FileOffset parent,FileOffset use,FileOffset cond);

FileOffset
mkFieldUpdate1(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1);

FileOffset
mkFieldUpdate2(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2);

FileOffset
mkFieldUpdate3(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3);

FileOffset
mkFieldUpdate4(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3
              ,FileOffset binder4,FileOffset bindee4);

FileOffset
mkFieldUpdate5(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3
              ,FileOffset binder4,FileOffset bindee4
              ,FileOffset binder5,FileOffset bindee5);

FileOffset
mkFieldUpdate6(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3
              ,FileOffset binder4,FileOffset bindee4
              ,FileOffset binder5,FileOffset bindee5
              ,FileOffset binder6,FileOffset bindee6);

FileOffset
mkFieldUpdate7(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3
              ,FileOffset binder4,FileOffset bindee4
              ,FileOffset binder5,FileOffset bindee5
              ,FileOffset binder6,FileOffset bindee6
              ,FileOffset binder7,FileOffset bindee7);

FileOffset
mkFieldUpdate8(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3
              ,FileOffset binder4,FileOffset bindee4
              ,FileOffset binder5,FileOffset bindee5
              ,FileOffset binder6,FileOffset bindee6
              ,FileOffset binder7,FileOffset bindee7
              ,FileOffset binder8,FileOffset bindee8);

FileOffset
mkFieldUpdate9(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3
              ,FileOffset binder4,FileOffset bindee4
              ,FileOffset binder5,FileOffset bindee5
              ,FileOffset binder6,FileOffset bindee6
              ,FileOffset binder7,FileOffset bindee7
              ,FileOffset binder8,FileOffset bindee8
              ,FileOffset binder9,FileOffset bindee9);

FileOffset
mkFieldUpdate10(FileOffset parent,FileOffset use
              ,FileOffset arg,FileOffset binder1,FileOffset bindee1
              ,FileOffset binder2,FileOffset bindee2
              ,FileOffset binder3,FileOffset bindee3
              ,FileOffset binder4,FileOffset bindee4
              ,FileOffset binder5,FileOffset bindee5
              ,FileOffset binder6,FileOffset bindee6
              ,FileOffset binder7,FileOffset bindee7
              ,FileOffset binder8,FileOffset bindee8
              ,FileOffset binder9,FileOffset bindee9
              ,FileOffset binder10,FileOffset bindee10);

FileOffset
mkProjection(FileOffset parent,FileOffset use,FileOffset exp);

FileOffset
mkHidden(FileOffset parent);

FileOffset
mkForward(FileOffset result);

FileOffset
mkDoStmt(FileOffset stmt);

/* Atom */

FileOffset
mkLambda(void);

FileOffset
mkDoLambda(void);

FileOffset
mkVariable(FileOffset module,int begin,int end,int fixity,int arity,char *name
	  ,Bool local);

FileOffset
mkConstructor(FileOffset module,int begin,int end,int fixity,int arity
             ,char *name);

FileOffset
mkConstructorWFields(FileOffset module,int begin,int end,int fixity,int arity
                    ,char *name,FileOffset labels[]);

     /* mmmh, need arity greater 30 for Flags of nhc98 ... */

FileOffset
mkAbstract(char *description);

FileOffset
mkListCons(FileOffset elem,FileOffset tail);

/* record that given redex is child of given hidden node */
void recordChild(FileOffset hidden,FileOffset child);


/* Update node that it was entered */

void entResult(FileOffset node,FileOffset use);
void entForward(FileOffset node,FileOffset hidden);

/* Update node with result */

void resResult(FileOffset node,FileOffset result,FileOffset use);
void resForward(FileOffset node,FileOffset result);

/* Implementation of update */
void hat_enter(FileOffset at,FileOffset offset,FileOffset entry);
void hat_reduce(FileOffset at,FileOffset offset,FileOffset entry);
FileOffset hat_topStack(void);
void hat_dumpBuffer(void);
void hat_dumpStack(void); 

#endif
