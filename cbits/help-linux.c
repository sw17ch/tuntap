#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <poll.h>

#include <netinet/ip.h>

#include <sys/ioctl.h>
#include <linux/if.h>
#include <linux/if_tun.h>

#include "help.h"

#define CLEAR(x) \
    memset((x), 0, sizeof(*(x)))

struct tap_desc
{
    int32_t      desc;
    int32_t      sock;
    struct ifreq ifr;
};

tap_desc_t * init_tap()
{
    return (tap_desc_t *) calloc(1,sizeof(tap_desc_t));
}

void finish_tap(tap_desc_t * td)
{
    free(td);
}

int32_t open_tap(tap_desc_t * td, char * name)
{
    char * dev = "/dev/net/tun";

    if (0 == td)
    {
        return -100;
    }
    else if (0 == name)
    {
        return -101;
    }

    int32_t fd = open(dev, O_RDWR);

    if (fd < 0)
    {
        fprintf(stderr,"open(%s): %s\n", name, strerror(errno));
        return -1;
    }
    else
    {
        td->desc = fd;
    }

    CLEAR(&(td->ifr));

    td->sock = socket(AF_INET, SOCK_DGRAM, 0);
    td->ifr.ifr_flags = IFF_TAP | IFF_NO_PI;

    if (*name)
        strncpy(td->ifr.ifr_name, name, IFNAMSIZ);

    if (ioctl(td->desc, TUNSETIFF, (void *)&td->ifr) < 0)
    {
        fprintf(stderr,"TUNSETIFF: %s\n", strerror(errno));
        return -2;
    }

    return 0;
}

int32_t close_tap(tap_desc_t * td)
{
    if (0 <= td->desc)
    {
        close(td->desc);
    }

    return 0;
}

int32_t bring_up_tap(tap_desc_t * td)
{
    td->ifr.ifr_flags |= (IFF_UP | IFF_RUNNING);

    if (ioctl(td->sock, SIOCSIFFLAGS, &td->ifr) < 0)
    {
        fprintf(stderr,"SIOCSIFFLAGS: %s\n", strerror(errno));
        return -3;
    }

    return 0;
}

int32_t set_mtu(tap_desc_t * td, uint32_t mtu)
{
    td->ifr.ifr_mtu = mtu;

    if (ioctl(td->sock, SIOCSIFMTU, &td->ifr) < 0)
    {
        fprintf(stderr,"SIOCSIFMTU: %s\n", strerror(errno));
        return -1;
    }

    return 0;
}

int32_t set_ip(tap_desc_t * td, uint32_t ip)
{
    struct sockaddr_in addr;
    CLEAR(&addr);

    addr.sin_addr.s_addr = ip;
    addr.sin_family = AF_INET;
    memcpy(&td->ifr.ifr_addr, &addr, sizeof(addr));

    if (ioctl(td->sock, SIOCSIFADDR, &td->ifr) < 0)
    {
        fprintf(stderr, "SIOCSIFADDR: %s\n", strerror(errno));
        return -1;
    }

    return 0;
}

int32_t set_mask(tap_desc_t * td, uint32_t mask)
{
    struct sockaddr_in addr;
    CLEAR(&addr);

    addr.sin_addr.s_addr = mask;
    addr.sin_family = AF_INET;
    memcpy(&td->ifr.ifr_addr, &addr, sizeof(addr));

    if ( ioctl(td->sock, SIOCSIFNETMASK, &td->ifr) < 0)
    {
        fprintf(stderr,"SIOCSIFNETMASK: %s\n", strerror(errno));
        return -1;
    }

    return 0;
}

int32_t get_mac(tap_desc_t * td, MACAddr mac)
{
    if (ioctl(td->sock, SIOCGIFHWADDR, &td->ifr) < 0)
    {
        fprintf(stderr,"SIOCGIFHWADDR: %s\n", strerror(errno));
        return -1;
    }
    else
    {
        memcpy(mac,&td->ifr.ifr_hwaddr.sa_data, sizeof(MACAddr));
    }

    return 0;
}

int32_t read_tap(tap_desc_t * td, uint8_t * buf, int32_t len)
{
    struct pollfd p = { td->desc, POLLIN, 0 };

    int32_t ret = poll(&p, 1, 500);

    if (ret == 0)
    {
        return 0;
    }
    else if (ret < 0)
    {
        fprintf(stderr, "ERROR: poll returned %d!!!\n", ret);
        return ret;
    }
    else if (p.revents | POLLIN)
    {
        return read(td->desc,buf,len);
    }
    else
    {
        fprintf(stderr, "ERROR: poll returned, it wasn't a timout, and there "
                        "isn't any data to be read!\n");
        return -100;
    }
}

int32_t write_tap(tap_desc_t * td, const uint8_t * buf, int32_t len)
{
    return write(td->desc,buf,len);
}
