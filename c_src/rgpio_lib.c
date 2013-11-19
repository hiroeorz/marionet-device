/* rgpio_lib.c */

/*  taken from erlan_ale
 *  https://github.com/esl/erlang_ale/blob/master/c_src/gpio_node.c 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/mman.h>
#include <netinet/in.h>
#include <poll.h>
#include <pthread.h>
#include <inttypes.h>

#include "erl_interface.h"
#include "ei.h"

#define BUFSIZE 1000
#define DEBUG 1

#ifdef DEBUG
#define debug(...) printf(__VA_ARGS__)
#else
#define debug(...) ;
#endif

static pthread_t isr_handler_thread;
static int isr_handler_flag;
static int fd_erlang_node;

typedef struct {
   ETERM* pinp;
   void (*isr) (ETERM* pinp, ETERM* pidp, ETERM* modep);
   ETERM* pidp;
   ETERM* modep;
} isr_t;

int
pthread_tryjoin_np(pthread_t thread, void **retval);

void
get_hostname(char* name);

void
strstrip(char *s);

static int
gpio_valfd (int);

void
gpio_init();

ETERM*
gpio_pullup_down(ETERM* pinp, ETERM* modep);

ETERM*
gpio_start_poll (ETERM* pinp, ETERM* pidp, 
		 void (*isr) (ETERM*, ETERM*, ETERM*), ETERM* modep);
void
handle_gpio_interrupt (ETERM* pinp, ETERM* pidp, ETERM* modep);

static void *
isr_handler (void *isr);

int
main(int argc, char **argv) {
  int loop = 1;                         /* Loop flag                       */
  int got;                              /* Result of receive               */
  unsigned char buf[BUFSIZE];           /* Buffer for incomming message    */
  ErlMessage emsg;                      /* Incoming message                */
  char hostname[255];                   /* Node hostname                   */
  char erlang_nodename[255] = "rgpio@"; /* Node name                       */

  /* initialize memory map */
  gpio_init();

  /* Representations of Erlang terms */
  ETERM *msg_type, *fromp, *refp, *tuplep, *fnp, *arg1p, *arg2p, *resp;

  get_hostname(hostname);
  strcat(erlang_nodename, hostname);
  strstrip(erlang_nodename);

  /* initialize erl_interface (once only) */
  erl_init(NULL, 0);

  /* initialize the connection mechanism */
  if (erl_connect_init(1, "rgpio", 0) == -1 )
    erl_err_quit("erl_connect_init");

  if ((fd_erlang_node = erl_connect(erlang_nodename)) < 0)
    erl_err_quit("erl_connect");

  while(loop) {
    got = erl_receive_msg(fd_erlang_node, buf, BUFSIZE, &emsg);
    
    if (got == ERL_TICK) {
      printf("tick\n");
      /* ignore */
    } 
    else if (got == ERL_ERROR) {
      printf("error\n");
      loop = 0;
    }
    else {

      if (emsg.type == ERL_REG_SEND) {
	/* unpack message field */
	msg_type = erl_element(1, emsg.msg);
	fromp = erl_element(2, emsg.msg);
	refp = erl_element(3, emsg.msg);
	tuplep = erl_element(4, emsg.msg);
	fnp = erl_element(1, tuplep);

	if (strncmp(ERL_ATOM_PTR(msg_type), "call", 4) == 0) {
	  /* call expects a msg back */
	  /* always at least one argument so we get that out first*/
	  arg1p = erl_element(2, tuplep); // pin number           

	  if (strncmp(ERL_ATOM_PTR(fnp), "start_poll", 10) == 0) {
	    arg2p = erl_element(3, tuplep);
	    resp = gpio_start_poll(arg1p, fromp, handle_gpio_interrupt, arg2p);
	  }
	  else if (strncmp(ERL_ATOM_PTR(fnp), "pullup_down", 11) == 0) {
	    arg2p = erl_element(3, tuplep);
	    resp = gpio_pullup_down(arg1p, arg2p);
	  }
	  
	  printf("return msg!\n");
	  erl_send(fd_erlang_node, fromp, erl_format("{~w,~w}", refp, resp));
	}

	/* free the term storage used */
	erl_free_term(emsg.from);
	erl_free_term(emsg.msg);
	erl_free_term(fromp);
	erl_free_term(refp);
	erl_free_term(tuplep);
	erl_free_term(fnp);
	erl_free_term(arg1p);
	erl_free_term(arg2p);
	erl_free_term(resp);
      }
    }
  }

  return 0;
}

void
get_hostname(char* name)
{
   FILE *f;
   
   if( !(f = popen("hostname -s", "r")) ){
      perror("Can't execute cmd");
   }
   
   fgets(name, 255, f);

}

void
strstrip( char *s )
{
   char *start;
   char *end;

   // Exit if param is NULL pointer
   if (s == NULL)
      return;

   // Skip over leading whitespace
   start = s;
   while ((*start) && isspace(*start))
      start++;      

   // Is string just whitespace?
   if (!(*start)) 
   {         
      *s = 0x00; // Truncate entire string
      return;     
   }     

   // Find end of string
   end = start;
   while (*end)         
      end++;     

   // Step back from NUL
   end--;      

   // Step backward until first non-whitespace
   while ((end != start) && isspace(*end))         
      end--;     

   // Chop off trailing whitespace
   *(end + 1) = 0x00;

   // If had leading whitespace, then move entire string back to beginning
   if (s != start)
      memmove(s, start, end-start+1);      

   return; 
} 

#define PERI_BASE 0x20000000
#define GPIO_BASE (PERI_BASE + 0x200000)
#define BLOCK_SIZE 4096
#define GPIO_PULLNONE 0x0
#define GPIO_PULLDOWN 0x1
#define GPIO_PULLUP   0x2
static volatile unsigned int *gpio;

void
gpio_init()
{
  int fd;
  void *gpio_map;

  fd = open ("/dev/mem", O_RDWR | O_SYNC);
  if (fd < 0) {
    printf("error: cannot open /dev/mem\n");
    exit(-1);
  }

  gpio_map = mmap(NULL, BLOCK_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED,
		  fd, GPIO_BASE);

  if ((int) gpio_map ==  -1) {
    printf("error: cannot open /dev/mem on the memory\n");
    exit(-1);
  }

  close(fd);
  gpio = (unsigned int *) gpio_map;
}

ETERM*
gpio_pullup_down(ETERM* pinp, ETERM* modep)
{
  int pin = ERL_INT_VALUE(pinp);
  int pullmode = 0;

  if (strncmp(ERL_ATOM_PTR(modep), "pullup", 6) == 0) {
    printf("pullup: %d\n", pin);
    pullmode = GPIO_PULLUP;
  }
  else if (strncmp(ERL_ATOM_PTR(modep), "pulldown", 8) == 0) {
    printf("pulldown: %d\n", pin);
    pullmode = GPIO_PULLDOWN;
  }
  else if (strncmp(ERL_ATOM_PTR(modep), "none", 4) == 0) {
    printf("pullnone: %d\n", pin);
    pullmode = GPIO_PULLNONE;
  }

  gpio[37] = pullmode & 0x3;
  usleep(1);

  gpio[38] = 0x1 << pin;
  usleep(1);

  gpio[37] = 0;
  gpio[38] = 0;

  return erl_format("ok");
}

ETERM*
gpio_start_poll (ETERM* pinp, ETERM* pidp,
		 void (*isr) (ETERM*, ETERM*, ETERM*), ETERM* modep)
{
  /* Details of the ISR */
  isr_t *i = (isr_t *) malloc (sizeof (isr_t));
  i->pinp = erl_format("~w",pinp);
  i->isr = isr;
  i->pidp = erl_format("~w", pidp);
  i->modep = erl_format("~w", modep);

  /* Set isr_handler flag and create thread
     TODO: check for errors using retval */
  isr_handler_flag = 1;
  pthread_create (&isr_handler_thread, NULL, isr_handler, (void *) i);
  pthread_tryjoin_np (isr_handler_thread, NULL);
  fprintf(stderr, "pthtread created\n");
  
  return erl_format("ok");
}

void
handle_gpio_interrupt (ETERM* pinp, ETERM* pidp, ETERM* modep) {
   debug("inside handle_gpio_interrupt\n\r");
   debug("pin: %d, pid_number: %d, mode: %s\n\r",
         ERL_INT_VALUE(pinp), ERL_PID_NUMBER(pidp), ERL_ATOM_PTR(modep));
   
   ETERM* resp = erl_format("{gpio_changed, ~w, ~w}", pinp, modep);
   erl_send(fd_erlang_node, pidp, resp);
}

/* taken from https://github.com/omerk/pihwm/blob/master/lib/pi_gpio.c */       
static void *
isr_handler (void *isr) {
  struct pollfd fdset[1];
  int nfds = 1, gpio_fd, rc;
  char *buf[64];

  isr_t i = *(isr_t *) isr;
  
  if (isr_handler_flag)
    {
      printf ("isr_handler running\n");
      gpio_fd = gpio_valfd (ERL_INT_VALUE(i.pinp));

      if ( gpio_fd == -1) {
	fprintf(stderr, "Unable to open gpio fd\n\r");
	return NULL;
      }

      while (1)
	{
	  memset ((void *) fdset, 0, sizeof (fdset));

	  fdset[0].fd = gpio_fd;
	  fdset[0].events = POLLPRI;

	  rc = poll (fdset, nfds, 10000);	/* Timeout in ms */

	  if (rc < 0)
	    {
	      debug ("\npoll() failed!\n"); 
	      return (void *) -1;
	    }

	  if (rc == 0)
	    {
	      /* debug ("poll() timeout.\n"); */
	      if (isr_handler_flag == 0)
		{
		  debug ("exiting isr_handler (timeout)"); 
		  pthread_exit (NULL);
		}
	    }

	  if (fdset[0].revents & POLLPRI)
	    {
	      /* We have an interrupt! */
	      if (-1 == read (fdset[0].fd, buf, 64))
		{
		  debug ("read failed for interrupt"); 
		  return (void *) -1;
		}

	      (*i.isr) (i.pinp, i.pidp, i.modep);	/* Call the ISR */
	    }

	  fflush (stdout);
	}
    }
  else
    {
      debug ("exiting isr_handler (flag)"); 
      pthread_exit (NULL);
    }

}

static int
gpio_valfd (int pin)
{
  int file;
  char filename[35];
  char *buf[2];
  
  sprintf (filename, "/sys/class/gpio/gpio%d/value", pin);
  file = open (filename, O_RDWR | O_NONBLOCK);
  read (file, buf, 1); /* read to eof */
  
  if (file < 0) {
    return -1;
  }
  else {
    return file;
  }
}

