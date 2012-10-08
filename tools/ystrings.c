#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* append strings and reserve space for the result string */
char* catStr (char* s1, char* s2, char* s3) {
  int x = strlen (s1) + 1;
  char* H;
  if (s1 == NULL) return NULL;
  if (s2) x = x + strlen (s2);
  if (s3) x = x + strlen (s3);
  H = (char*) malloc(x*sizeof(char));
  strcpy (H,s1);
  if (s2) strcat (H,s2);
  if (s3) strcat (H,s3);
  return H;
}

/* replace string in s with the concatenation of s1,s2 and s3 */
void replaceStr(char** s,char* s1,char *s2,char* s3) {
  char* sneu=catStr(s1,s2,s3);
  if (*s != NULL) free(*s);
  (char*) *s = sneu;
}

