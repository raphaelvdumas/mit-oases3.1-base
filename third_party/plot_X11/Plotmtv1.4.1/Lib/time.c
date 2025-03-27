#include <time.h>
#include <sys/types.h>
/*
#include <sys/timeb.h>
 */

/* get the local time */
void CNget_localtime(newtime,maxchar)
char newtime[];
int  maxchar;
{
   char      *ctime();
   time_t    time();

   int       i;
   time_t    timeofday;
   char      *loctime;

   /* get the local time */
   timeofday = time((time_t*)0);
   loctime   = ctime(&timeofday);

   /* loctime has a '\n' in it, so delete that character */
   i=0;
   while ((loctime[i] != '\n') && (i<maxchar)) {
      newtime[i] = loctime[i];
      i++;
   }
   newtime[i] = '\0';
}

