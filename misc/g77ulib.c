/* 
   g77ulib.c

     Collected and Edited by Austin J. Lee

     Tested w/ gcc-2.7.2.1f/Linux-1.3.88
            w/ gcc-2.7.2/Digital UNIX V3.2C (Rev. 148)
	    w/ gcc-2.7.2/IRIX Release 5.3 IP22

     Update on 04/13/97

     List of implemented functions
       besj0       : not tested
       besj1       : not tested
       besjn       : not tested
       besy0       : not tested
       besy1       : not tested
       besyn       : not tested
       dbesj0      : not tested
       dbesj1      : not tested
       dbesjn      : not tested
       dbesy0      : not tested
       dbesy1      : not tested
       dbesyn      : not tested
       chdir       : not tested
       chmod       : tested
       ctime       : tested
       dtime       : tested
       etime       : tested
       fdate       : tested
       flmax       : tested
       flmin       : tested
       dflmax      : tested
       dflmin      : tested
       free        : tested
       gerror      : tested 
       getcwd      : tested
       getgid      : tested
       getlog      : tested
       getnice     : not tested
       getpid      : tested
       getpriority : not tested
       getuid      : tested
       gmtime      : tested
       idate       : tested
       ierrno      : not tested
       itime       : tested
       kill        : tested
       link        : tested
       lnblnk      : tested 
       ltime       : tested
       malloc      : tested
       mclock      : not tested
       nice        : not tested
       perror      : tested
       putc        : tested
       rename      : tested
       secnds      : tested
       setnice     : not tested
       setpriority : not tested
       sleep       : tested
       srand       : not tested
       time        : tested
       umask       : not tested
       unlink      : tested
  
     List of unimplemented functions :
       ierrno : not implemented 
       irand  : not implemented
       rand   : not implemented
       drand  : not implemented
       fchmod : not implemented
       gerror : not implemented
       wait   : not implemented
 
*/

#ifdef __alpha
#define _B64
#endif

#include <unistd.h>
#include <time.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/times.h>
#include <errno.h>
#include <math.h>
#include <sys/resource.h>
#include <float.h>
#include <limits.h>

int lenf77str( char *c, int clen);
void cpyf77str( char **a, char *c, int clen);

struct tb { float usrtime; float systime; };

/*-------------------------------------------------besj0, besj1, besjn*/
float besj0_ ( float *x ) 
{ return (float)j0 ((double)*x); }

float besj1_ ( float *x ) 
{ return (float)j1 ((double)*x); }

float besjn_ ( int *n, float *x ) 
{ return (float)jn (*n,(double)*x); } 


/*-------------------------------------------------besy0, besy1, besyn*/
float besy0_ ( float *x ) 
{ return (float)y0 ((double)*x); }

float besy1_ ( float *x ) 
{ return (float)y1 ((double)*x); }

float besyn_ ( int *n, float *x ) 
{ return (float)yn (*n,(double)*x); }


/*----------------------------------------------dbesj0, dbesj1, dbesjn*/
double dbesj0_ ( double *x ) 
{ return j0 (*x); }

double dbesj1_ ( double *x ) 
{ return j1 (*x); }

double dbesjn_ ( int *n, double *x) 
{ return jn (*n,*x); } 


/*----------------------------------------------dbesy0, dbesy1, dbesyn*/
double dbesy0_ ( double *x ) 
{ return y0 (*x); }

double dbesy1_ ( double *x ) 
{ return y1 (*x); }

double dbesyn_ ( int *n, double *x ) 
{ return yn (*n,*x); }


/*--------------------------------------------------------------chmod*/
int chdir_( char *name, int nl )
{
  int i;
  char *buff;
  i=lenf77str(name,nl);
  buff=(char *)malloc(i*sizeof(char));
  strncat(buff,name,i);
  i=chdir(buff);
  free(buff);
  return i ? errno : 0;
}

/*--------------------------------------------------------------chmod*/
int chmod_( char *name, char *mode, int nl, int ml )
{
  int i,j,ierr;
  char *cmd;
  i=lenf77str(mode,ml);
  j=lenf77str(name,nl);
  cmd=(char *)malloc((i+j+8)*sizeof(char));
  strncat(cmd,"chmod ",6);
  strncat(cmd,mode,i);
  strncat(cmd," ",1);
  strncat(cmd,name,j);
  strncat(cmd,"\0",1);
  ierr=system(cmd);
  free(cmd);
  if(ierr!=-1)
    return 0;
  else
    return -1;
}


/*--------------------------------------------------------------ctime*/
void ctime_( result, length, s )
     char *result;
     int length;
#ifdef _B64
     int *s;
#else
     long *s;
#endif
{
  const time_t ss = (time_t)*s;
  char *rvalue=ctime(&ss);
  int i;
  if(length < 24)
    strncpy(result,rvalue,(size_t)length);
  else
    {
      strncpy(result,rvalue,(size_t)24);
      for(i=24;i<length;i++) result[i]=' ';
    }
}


/*--------------------------------------------------------------dtime*/
float dtime_(dt) struct tb *dt;
{
  struct tms clock;
  static time_t ref_utime=0,ref_stime=0; 
  if( times(&clock) < 0) return -1.0;
  dt->usrtime=(float)(clock.tms_utime-ref_utime)/CLK_TCK;
  dt->systime=(float)(clock.tms_stime-ref_stime)/CLK_TCK;
  ref_utime=clock.tms_utime;
  ref_stime=clock.tms_stime;
  return(dt->usrtime + dt->systime);
}


/*--------------------------------------------------------------etime*/
float etime_(et) struct tb *et;
{	
  struct tms clock;
  if( times(&clock) < 0) return -1.0;
  et->usrtime = (float)(clock.tms_utime)/CLK_TCK;
  et->systime = (float)(clock.tms_stime)/CLK_TCK;
  return(et->usrtime + et->systime);
}


/*--------------------------------------------------------------fdate*/
void fdate_( char *result, int length )
{
  time_t clock=time(NULL);
  char *rvalue=ctime(&clock);
  int i;
  if(length < 24)
    strncpy(result,rvalue,(size_t)length);
  else
    {
      strncpy(result,rvalue,(size_t)length);
      for(i=24;i<length;i++) result[i]=' ';      
    }
}


/*----------------------------------------flmax, flmin, dflmax, dflmin*/
float flmax_( void ) 
{ return (float) FLT_MAX; }

float flmin_( void ) 
{ return (float) FLT_MIN; }

double dflmax_( void ) 
{ return (double) DBL_MAX; }

double dflmin_( void ) 
{ return (double) DBL_MIN; }


/*---------------------------------------------------------------free*/
#ifdef _B64
void free_(char **pointer) 
{ free(*pointer); }
#else
void free_(int *pointer) 
{ free((char *)*pointer); }
#endif


/*-------------------------------------------------------------gerror*/
int gerror_ ( char *str, int  Lstr )
{
  char *errmsg=strerror(errno);
  size_t i=strlen(errmsg);
  if(Lstr>(int)i)
    strncpy(str,errmsg,i);
  else
    strncpy(str,errmsg,(size_t)Lstr);
  return 0;
}


/*-------------------------------------------------------------getcwd*/
int getcwd_( char *s, int slen )
{
  int i;
  getcwd(s,slen);
  for(i=0;i<slen && s[i]; i++);
  for(;i<slen;i++) s[i]=' ';
  return 0;
}


/*-------------------------------------------------------------getgid*/
int getgid_( void ) 
{ return getgid(); }


/*-------------------------------------------------------------getlog*/
int getlog_( char *s, int slen )
{
  int i;
  char *user = getlogin();
  if(!user) user = "";
  for(i=0;i<slen && user[i]; i++) s[i] = user[i];
  for(;i<slen;i++) s[i] = ' ';
  return 0;
}


/*------------------------------------------------------------getnice*/
int getnice_( void ) 
{ return getpriority(0,0); }


/*-------------------------------------------------------------getpid*/
int getpid_( void ) 
{ return getpid(); }


/*--------------------------------------------------------getpriority*/
int getpriority_ ( int *which, int *who ) 
{ return getpriority(*which,*who); }


/*-------------------------------------------------------------getuid*/
int getuid_( void ) 
{ return getuid(); }


/*-------------------------------------------------------------gmtime*/
int gmtime_(s, iarr)
#ifdef _B64
     int *s;
#else
     long *s;
#endif
     int *iarr;
{
  const time_t ss = (time_t)*s;
  struct tm* lt=gmtime(&ss);
  iarr[0]=lt->tm_sec;   /* Seconds after the minute [0-60] */
  iarr[1]=lt->tm_min;   /* Minutes after the hour [0-59]   */
  iarr[2]=lt->tm_hour;  /* Hours since midnight [0-23]     */
  iarr[3]=lt->tm_mday;  /* Day of the month [1-31]         */
  iarr[4]=lt->tm_mon;   /* Months since January [0-11]     */
  iarr[5]=lt->tm_year;  /* Years since 1900                */
  iarr[6]=lt->tm_wday;  /* Days since Sunday [0-6]         */
  iarr[7]=lt->tm_yday;  /* Days since January 1 [0-365]    */
  iarr[8]=lt->tm_isdst; /* Daylight Saving Time flag       */
  return 0;
}


/*--------------------------------------------------------------idate*/
int idate_( int *iarr )
{
  const time_t clock=time(NULL);   /* current time in seconds  */
  struct tm* lt=localtime(&clock); /* date/time structure */
  iarr[0] = lt->tm_mday;	   /* day of month */
  iarr[1] = lt->tm_mon + 1;	   /* month */
  iarr[2] = lt->tm_year + 1900;    /* year */
  return 0;
}


/*-------------------------------------------------------------ierror*/
int ierrno_ ( void ) 
{ return errno; }


/*--------------------------------------------------------------itime*/
int itime_( int *iarr )
{
  const time_t clock=time(NULL);   /* current time in seconds  */
  struct tm* lt=localtime(&clock); /* date/time structure */
  iarr[0] = lt->tm_hour;	   /* hour */
  iarr[1] = lt->tm_min;	           /* minute */
  iarr[2] = lt->tm_sec; 	   /* second */
  return 0;
}


/*---------------------------------------------------------------kill*/
int kill_( int *p, int *s ) 
{ return kill((pid_t)*p,*s); }


/*---------------------------------------------------------------link*/
int link_( char* b, char* a, int blen, int alen )
{
  char *o,*n;
  int olen,nlen,ierr;
  olen=lenf77str(b,blen);
  nlen=lenf77str(a,alen);
  o=(char *)malloc((olen+1)*sizeof(char));
  n=(char *)malloc((nlen+1)*sizeof(char));
  strncpy(o,b,olen);
  strncpy(n,a,nlen);
  o[olen]='\0';
  n[nlen]='\0';
  ierr=link(o,n);
  free(o);
  free(n);    
  return ierr;
}


/*-------------------------------------------------------------lnblnk*/
int lnblnk_( char *c, int clen )
{ 
  int i;
  for(i=clen;i>0 && c[i-1]==' ';i--);
  return i;
}

/*--------------------------------------------------------------ltime*/
int ltime_( s, iarr )
#ifdef _B64
     int *s;
#else
     long *s;
#endif
     int *iarr;
{
  const time_t ss = (time_t)*s;
  struct tm* lt=localtime(&ss);
  iarr[0]=lt->tm_sec;   /* Seconds after the minute [0-60] */
  iarr[1]=lt->tm_min;   /* Minutes after the hour [0-59]   */
  iarr[2]=lt->tm_hour;  /* Hours since midnight [0-23]     */
  iarr[3]=lt->tm_mday;  /* Day of the month [1-31]         */
  iarr[4]=lt->tm_mon;   /* Months since January [0-11]     */
  iarr[5]=lt->tm_year;  /* Years since 1900                */
  iarr[6]=lt->tm_wday;  /* Days since Sunday [0-6]         */
  iarr[7]=lt->tm_yday;  /* Days since January 1 [0-365]    */
  iarr[8]=lt->tm_isdst; /* Daylight Saving Time flag       */
  return 0;
}


/*-------------------------------------------------------------malloc*/
#ifdef _B64
char **malloc_( int *size ) 
{ return (char **)malloc(*size); }
#else
int malloc_( int *size ) 
{ return (int)malloc(*size); }
#endif


/*-------------------------------------------------------------mclock*/
int mclock_( void ) 
{ return clock(); }

/*---------------------------------------------------------------nice*/
int nice_( int *inc) 
{ return nice(*inc); }

/*-------------------------------------------------------------perror*/
void perror_( char* s, int slen )
{
  char* buff;
  int i;
  i=lenf77str(s,slen);
  buff=(char *) malloc((i+1)*sizeof(char));
  strncpy(buff,s,i);
  buff[i]='\0';
  perror(buff);
  free(buff);
}

/*---------------------------------------------------------------putc*/
int putc_( char *c )
{ return putc((int)*c,stdout); }


/*-------------------------------------------------------------rename*/
int rename_( char* b, char* a, int blen, int alen)
{
  char *o,*n;
  int olen,nlen,ierr;
  olen=lenf77str(b,blen);
  nlen=lenf77str(a,alen);
  o=(char *)malloc((olen+1)*sizeof(char));
  n=(char *)malloc((nlen+1)*sizeof(char));
  strncpy(o,b,olen);
  strncpy(n,a,nlen);
  o[olen]='\0';
  n[nlen]='\0';
  ierr=rename(o,n);
  free(o);
  free(n);    
  return ierr;
}

/*-------------------------------------------------------------secnds*/
float secnds_( float *r)
{
  const time_t clock=time(NULL);   /* current time in seconds  */
  struct tm* lt=localtime(&clock); /* date/time structure */
  float f;
  f = (3600.0*((float)lt->tm_hour) + 60.0*((float)lt->tm_min) +
      (float)lt->tm_sec - *r);
  return f;  
}

/*------------------------------------------------------------setnice*/
int setnice_( int *prio)
{ return setpriority(0,0,*prio); }


/*--------------------------------------------------------setpriority*/
int setpriority_ ( int *which, int *who, int *prio)
{ return setpriority(*which, *who, *prio); }


/*--------------------------------------------------------------sleep*/
void sleep_( int *sec )
{ if(*sec > 0) sleep((unsigned int)(*sec)); }


/*--------------------------------------------------------------srand*/
void srand_( int *iseed )
{ srand((unsigned int)(*iseed)); }


/*---------------------------------------------------------------time*/
int time_( void ) 
{ return (int)time(NULL); }


/*--------------------------------------------------------------umask*/
int umask_( int *mask ) 
{ return umask((mode_t) *mask); }


/*-------------------------------------------------------------unlink*/
int unlink_( char* fname, int flen )
{
  char *buff;
  int i,ioerr;
  i=lenf77str(fname,flen);
  buff=(char *)malloc((i+1)*sizeof(char));
  strncpy(buff,fname,i);
  buff[i]='\0';
  ioerr=unlink(buff);
  free(buff);
  return ioerr;
}


/*---------supplement function for fortran character array passing 1 */
int lenf77str( char *c, int clen )
{ 
  int i;
  for(i=clen;i>0 && c[i-1]==' ';i--);
  return i;
}

