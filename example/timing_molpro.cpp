#define FC_FUNC_(name,NAME) name ## _
#include <time.h>
#ifndef _WIN32
#include <sys/times.h>
#endif

#if defined(__APPLE__)
#include <sys/param.h> /* for CLK_TCK */
#define TIMER_TICK	CLK_TCK
#elif defined(_SC_CLK_TCK)
#define TIMER_TICK      sysconf(_SC_CLK_TCK)
#elif defined(HZ)
#define TIMER_TICK      HZ
#else
#define TIMER_TICK      60
#endif

#define SECOND_X      FC_FUNC_(second_x,SECOND_X)
#define TIMING_MOLPRO FC_FUNC_(timing_molpro,TIMING_MOLPRO)
#define WALLCL        FC_FUNC_(wallcl,WALLCL)

#ifndef _WIN32
static clock_t it; struct tms itt;
#endif

extern "C" {
double SECOND_X()
{
#ifdef _WIN32
  return (double) time(NULL);
#else
  it=times(&itt);
  return (double) itt.tms_utime / (double) TIMER_TICK;
#endif
}

double WALLCL()
{
#ifdef _WIN32 /* actually this should probably work everywhere */
  return (double) time(NULL);
#else
  it=times(&itt);
  return (double) it / (double) TIMER_TICK;
#endif
}

void TIMING_MOLPRO(double *cpu, double *sys, double *wall)
{
#ifdef _WIN32
  *wall=*cpu=*sys= WALLCL();
#else
  it=times(&itt);
  *cpu=(double) itt.tms_utime / (double) TIMER_TICK; /* same as SECOND_X() */
  *sys=(double) itt.tms_stime / (double) TIMER_TICK;
  *wall=(double) it / (double) TIMER_TICK; /* same as WALLCL() */
#endif
}
}
