#include <sys/ioctl.h>
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <net/if.h>
#include <net/if_dl.h>
#include <net/route.h>
#include <netinet/in.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ifaddrs.h>

#include "help.h"
#include "tun_ioctls.h"

int get_mac(struct ifreq * ifr, int sock, struct tap_info * ti);

static int set_ip(struct ifreq * ifr, int sock, ip4_addr_t ip, ip4_addr_t mask);
static int set_mtu(struct ifreq * ifr, int sock, unsigned int mtu);

int open_tap(ip4_addr_t local_ip, ip4_addr_t local_mask, struct tap_info * ti)
{
    struct ifreq ifr_tap;
    int r = 0;

    int fd = -1;
    int sock = -1;

    int tap_num = 0;
    char name[15];

    do {
        snprintf(name, sizeof(name) - 1, "/dev/tap%d", tap_num++);
    } while (tap_num < 255 && (fd = open(name, O_RDWR)) < 0); 
    
    if (fd < 0)
        return -1;

    memset(&ifr_tap, 0, sizeof(ifr_tap));

    /* setup tap */
    strncpy(ifr_tap.ifr_name, &name[5], IFNAMSIZ);
    
    /*
    if ((ioctl(fd, TUNSIFHEAD, (void *)&ifr_tap)) < 0)
        return -2;
    */
    
    /* setup ip */
    if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
        return -3;

    if (set_ip(&ifr_tap, sock, local_ip, local_mask) < 0)
        return -4;

    if ( ioctl(sock, SIOCGIFFLAGS, &ifr_tap) < 0)
        return -6;

    if ( get_mac(&ifr_tap, sock, ti) < 0)
        return -7;

    ifr_tap.ifr_flags |= IFF_UP;
    ifr_tap.ifr_flags |= IFF_RUNNING;

    if (ioctl(sock, SIOCSIFFLAGS, &ifr_tap) < 0)
        return -8;

    if (set_mtu(&ifr_tap, sock, 1200) < 0)
        return -9;

    ti->desc->desc = fd;

    return fd;
}

static int set_ip(struct ifreq * ifr, int sock, ip4_addr_t ip, ip4_addr_t mask)
{
    /* Setting a single address of an interface is depreciated. Now uses ifaliasreq and it is done in one call */
    struct ifaliasreq ifa;
    struct sockaddr_in *in;
        
    memset(&ifa, 0, sizeof(ifa));
    strcpy(ifa.ifra_name, ifr->ifr_name);
    
    in = (struct sockaddr_in *) &ifa.ifra_addr;
	in->sin_family = AF_INET;
	in->sin_len = sizeof(ifa.ifra_addr);
	in->sin_addr.s_addr = ip;
	
	in = (struct sockaddr_in *) &ifa.ifra_mask;
	in->sin_family = AF_INET;
	in->sin_len = sizeof(ifa.ifra_mask);
	in->sin_addr.s_addr = mask;
    
    if ( ioctl(sock, SIOCSIFADDR, &ifa) < 0) {
        printf("SIOCAIFADDR: %s\n", strerror(errno));
        return -1;
    }

    return 0; 
}

static int set_mtu(struct ifreq * ifr, int sock, unsigned int mtu)
{
    /* Set the MTU of the tap interface */
    ifr->ifr_mtu = mtu; 
    if (ioctl(sock, SIOCSIFMTU, ifr) < 0)  {
        printf("SIOCSIFMTU: %s\n", strerror(errno));
        return -1;
    }

    return 0;
}

void close_tap(union tap_desc * td)
{
    if (0 <= td->desc) 
    {
        close(td->desc);
    }
}

int get_mac(struct ifreq * ifr, int sock, struct tap_info * ti)
{
    struct ifaddrs *ifap;
        
    if (getifaddrs(&ifap) == 0) {
        struct ifaddrs *p;
        for (p = ifap; p; p = p->ifa_next) {
            if (p->ifa_addr->sa_family == AF_LINK && strncmp(((struct sockaddr_dl *) p->ifa_addr)->sdl_data, "tap", 3) == 0) {
            	struct sockaddr_dl *sdp = (struct sockaddr_dl *) p->ifa_addr;
            	memcpy(&(ti->mac), LLADDR(sdp), 6);
            	freeifaddrs(ifap);
                return 0;
            }
        }
        freeifaddrs(ifap);
    }

    printf("Unable to get MAC address.");
    return -1;    
}

int read_tap(union tap_desc * td, char * buf, int len)
{   
    int ret = read(td->desc,buf,len);
    return ret;
}

int write_tap(union tap_desc * td, const char * buf, int len)
{
    int ret = write(td->desc,buf,len);
    return ret;
}
