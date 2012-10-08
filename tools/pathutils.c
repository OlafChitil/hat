#include <string.h>

char*
rmext (char* word, char* ext)
{
  if (ext==(char*)0) return word;
  else {
    char *e = ext;
    char *i = strdup(word);
    char *c = i;
    while (*c) c++;
    while (*e) e++;
    while ((*--c==*--e) && e!=ext) ;
    if ((e==ext) && (*c==*e)) *c='\0';
    return i;
  }
}

char*
basename (char* path, char* ext)
{
  char *c = path;
  while (*c) c++;
  while (*c!='/' && c!=path) c--;
  if (c==path) return rmext(path,ext);
  else {
    c++;
    return rmext(strdup(c),ext);
  }
}

char*
dirname (char* path)
{
  char *start, *c;
  start = strdup(path);
  c = start;
  while (*c) c++;
  while (*c!='/' && c!=start) c--;
  if (c==start) {
    return ".";
  } else {
    *c='\0';
    return start;
  }
}

