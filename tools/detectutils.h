/*
 *  Utility functions for hat-detect.
 */
#include "art.h"
#include "parentset.h"
#include "artutils.h"

FileOffset	nextChild	(ParentSet* ps);
FileOffset	childSearch	(ParentSet* ps);
void		assert		(Bool cond, char *act);
FileOffset	findMainUse	(Bool findUse);
Bool		anySuspect	(FileOffset fo);

