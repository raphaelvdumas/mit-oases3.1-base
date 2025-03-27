/*
 * system.c - system commands
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef sun386
#include <sys/utsname.h>
#endif

#define UNKNOWN "unknown"

/*
 * Get the hostname
 */
void CNget_hostname(hostname, maxlen)
char *hostname;
int   maxlen;
{
/*
 * If gethostname() is not available on your system, then
 * change the "ifdef" in the define below to "ifndef", i.e.
 *      #ifdef NO_GETHOSTNAME
 *      becomes
 *      #ifndef NO_GETHOSTNAME
 */
#ifdef NO_GETHOSTNAME
   struct utsname name;
   if (uname(&name) >= 0) {
      /* Architecture */
      (void) strcpy(hostname,name.nodename);
   } else {
      (void) strcpy(hostname,UNKNOWN);
   }
#else
   if (gethostname(hostname, maxlen) < 0)
      (void) strcpy(hostname, UNKNOWN);
#endif
}

/*
 * Get the system/architecture
 */
void CNget_sysname(sysname)
char *sysname;
{
#ifndef sun386
   struct utsname name;

   if (uname(&name) >= 0) {

      /* Architecture */
      (void) strcpy(sysname,name.sysname);

   } else {
      (void) strcpy(sysname,UNKNOWN);
   }
#endif

#ifdef sun386
   /* machine/architecture */
   (void) strcpy(sysname, "sun386");
#endif

}  

