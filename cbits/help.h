#ifndef __NEW__
#define __NEW__ __NEW__

struct tap_desc;
typedef struct tap_desc tap_desc_t;
typedef uint8_t MACAddr[6];

tap_desc_t * init_tap();
void finish_tap(tap_desc_t * td);

int32_t open_tap(tap_desc_t * td, char * name);
int32_t close_tap(tap_desc_t * td);
int32_t bring_up_tap(tap_desc_t * td);
int32_t set_mtu(tap_desc_t * td, uint32_t mtu);
int32_t set_ip(tap_desc_t * td, uint32_t ip);
int32_t set_mask(tap_desc_t * td, uint32_t mask);
int32_t get_mac(tap_desc_t * td, MACAddr mac);

int32_t read_tap(tap_desc_t * td, uint8_t * buf, int32_t len);
int32_t write_tap(tap_desc_t * td, const uint8_t * buf, int32_t len);

#endif /* __NEW__ */
