
#if defined(WIN32)

/* Windows does not have ntohl, so define it here */
#define htonl(x) ntohl(x)
uint32_t ntohl(uint32_t x);

#else

/* Unix does, so just use it */
#include <netinet/in.h>		/* for ntohl() macro */

#endif /* WIN32 */
