/* NOTE: most of this code is a direct copy of the qemu tap code
*/

#include <stdio.h>
#include <wchar.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winioctl.h>
#include <ws2tcpip.h>
#include <iphlpapi.h>
#include <io.h>
#include <Fcntl.h>
#include <malloc.h>

//=============
// TAP IOCTLs
//=============

#define TAP_CONTROL_CODE(request,method) \
  CTL_CODE (FILE_DEVICE_UNKNOWN, request, method, FILE_ANY_ACCESS)

#define TAP_IOCTL_GET_MAC               TAP_CONTROL_CODE (1, METHOD_BUFFERED)
#define TAP_IOCTL_GET_VERSION           TAP_CONTROL_CODE (2, METHOD_BUFFERED)
#define TAP_IOCTL_GET_MTU               TAP_CONTROL_CODE (3, METHOD_BUFFERED)
#define TAP_IOCTL_GET_INFO              TAP_CONTROL_CODE (4, METHOD_BUFFERED)
#define TAP_IOCTL_CONFIG_POINT_TO_POINT TAP_CONTROL_CODE (5, METHOD_BUFFERED)
#define TAP_IOCTL_SET_MEDIA_STATUS      TAP_CONTROL_CODE (6, METHOD_BUFFERED)
#define TAP_IOCTL_CONFIG_DHCP_MASQ      TAP_CONTROL_CODE (7, METHOD_BUFFERED)
#define TAP_IOCTL_GET_LOG_LINE          TAP_CONTROL_CODE (8, METHOD_BUFFERED)
#define TAP_IOCTL_CONFIG_DHCP_SET_OPT   TAP_CONTROL_CODE (9, METHOD_BUFFERED)

//=================
// Registry keys
//=================

#define ADAPTER_KEY "SYSTEM\\CurrentControlSet\\Control\\Class\\{4D36E972-E325-11CE-BFC1-08002BE10318}"

#define NETWORK_CONNECTIONS_KEY "SYSTEM\\CurrentControlSet\\Control\\Network\\{4D36E972-E325-11CE-BFC1-08002BE10318}"

//======================
// Filesystem prefixes
//======================

#define USERMODEDEVICEDIR "\\\\.\\Global\\"
#define TAPSUFFIX         ".tap"

#define TAP_COMPONENT_ID "tap0801"

//======================
// Compile time configuration
//======================

//#define DEBUG_TAP_WIN32 1

#define TUN_ASYNCHRONOUS_WRITES 1

#define TUN_BUFFER_SIZE 1560
#define TUN_MAX_BUFFER_COUNT 32
 

#include "help.h"

static OVERLAPPED overlap_read, overlap_write;


int open_tap(ip4_addr_t local_ip, ip4_addr_t local_mask, struct tap_info * ti);

static const IP_ADAPTER_INFO *
get_adapter (const IP_ADAPTER_INFO *ai, DWORD index)
{
  if (ai && index != (DWORD)~0)
    {
      const IP_ADAPTER_INFO *a;

      /* find index in the linked list */
      for (a = ai; a != NULL; a = a->Next)
	{
	  if (a->Index == index)
	    return a;
	}
    }
  return NULL;
}


static void
delete_temp_addresses (DWORD index)
{
  ULONG size = 4096;
  DWORD status;
  IP_ADAPTER_INFO *adapters = (IP_ADAPTER_INFO *) malloc(size);
  if ((status = GetAdaptersInfo (adapters, &size)) != NO_ERROR)
	 return;
   
  const IP_ADAPTER_INFO *a = get_adapter(adapters, index);

  if (a)
  {
    const IP_ADDR_STRING *ip = &a->IpAddressList;
    while (ip)
    {
      
      const DWORD context = ip->Context;

      if ((status = DeleteIPAddress ((ULONG) context)) == NO_ERROR)
      {
        //msg (M_INFO, "Successfully deleted previously set dynamic IP/netmask: %s/%s",  ip->IpAddress.String,    ip->IpMask.String);
      }
      else
      {
        const char *empty = "0.0.0.0";
        if (strcmp (ip->IpAddress.String, empty)
         || strcmp (ip->IpMask.String, empty)) {}
          //msg (M_INFO, "NOTE: could not delete previously set dynamic IP/netmask: %s/%s (status=%u)", ip->IpAddress.String, ip->IpMask.String,         (unsigned int)status);
      }
      ip = ip->Next;
    }
  }
  free(adapters);
}

static int is_tap_win32_dev(const char *guid)
{
    HKEY netcard_key;
    LONG status;
    DWORD len;
    int i = 0;

    status = RegOpenKeyEx(
        HKEY_LOCAL_MACHINE,
        (LPCTSTR) ADAPTER_KEY,
        0,
        KEY_READ,
        (PHKEY)&netcard_key);

    if (status != ERROR_SUCCESS) {
        return FALSE;
    }

    for (;;) {
        char enum_name[256];
        char unit_string[256];
        HKEY unit_key;
        char component_id_string[] = "ComponentId";
        char component_id[256];
        char net_cfg_instance_id_string[] = "NetCfgInstanceId";
        char net_cfg_instance_id[256];
        DWORD data_type;

        len = sizeof (enum_name);
        status = RegEnumKeyEx(
            netcard_key,
            i,
            (LPTSTR) enum_name,
            &len,
            NULL,
            NULL,
            NULL,
            NULL);

        if (status == ERROR_NO_MORE_ITEMS)
            break;
        else if (status != ERROR_SUCCESS) {
            return FALSE;
        }

        snprintf (unit_string, sizeof(unit_string), "%s\\%s",
                  ADAPTER_KEY, enum_name);

        status = RegOpenKeyEx(
            HKEY_LOCAL_MACHINE,
            (LPCTSTR) unit_string,
            0,
            KEY_READ,
            &unit_key);

        if (status != ERROR_SUCCESS) {
            return FALSE;
        } else {
            len = sizeof (component_id);
            status = RegQueryValueEx(
                unit_key,
                (LPCTSTR) component_id_string,
                NULL,
                &data_type,
                component_id,
                &len);

            if (!(status != ERROR_SUCCESS || data_type != REG_SZ)) {
                len = sizeof (net_cfg_instance_id);
                status = RegQueryValueEx(
                    unit_key,
                    (LPCTSTR) net_cfg_instance_id_string,
                    NULL,
                    &data_type,
                    net_cfg_instance_id,
                    &len);

                if (status == ERROR_SUCCESS && data_type == REG_SZ) {
                    if (!strcmp (component_id, TAP_COMPONENT_ID) &&
                        !strcmp (net_cfg_instance_id, guid)) {
                        RegCloseKey (unit_key);
                        RegCloseKey (netcard_key);
                        return TRUE;
                    }
                }
            }
            RegCloseKey (unit_key);
        }
        ++i;
    }

    RegCloseKey (netcard_key);
    return FALSE;
}

static int get_device_guid(
    char *name,
    int name_size,
    char *actual_name,
    int actual_name_size)
{
    LONG status;
    HKEY control_net_key;
    DWORD len;
    int i = 0;
    int stop = 0;

    status = RegOpenKeyEx(
        HKEY_LOCAL_MACHINE,
        (LPCTSTR) NETWORK_CONNECTIONS_KEY,
        0,
        KEY_READ,
        &control_net_key);

    if (status != ERROR_SUCCESS) {
        return -1;
    }

    while (!stop)
    {
        char enum_name[256];
        char connection_string[256];
        HKEY connection_key;
        char name_data[256];
        DWORD name_type;
        const char name_string[] = "Name";

        len = sizeof (enum_name);
        status = RegEnumKeyEx(
            control_net_key,
            i,
            (LPTSTR) enum_name,
            &len,
            NULL,
            NULL,
            NULL,
            NULL);

        if (status == ERROR_NO_MORE_ITEMS)
            break;
        else if (status != ERROR_SUCCESS) {
            return -1;
        }

        snprintf(connection_string,
             sizeof(connection_string),
             "%s\\%s\\Connection",
             NETWORK_CONNECTIONS_KEY, enum_name);

        status = RegOpenKeyEx(
            HKEY_LOCAL_MACHINE,
            (LPCTSTR) connection_string,
            0,
            KEY_READ,
            &connection_key);

        if (status == ERROR_SUCCESS) {
            len = sizeof (name_data);
            status = RegQueryValueEx(
                connection_key,
                (LPTSTR) name_string,
                NULL,
                &name_type,
                name_data,
                &len);

            if (status != ERROR_SUCCESS || name_type != REG_SZ) {
                    return -1;
            }
            else {
                if (is_tap_win32_dev(enum_name)) {
                    snprintf(name, name_size, "%s", enum_name);
                    if (actual_name) {
                        if (strcmp(actual_name, "") != 0) {
                            if (strcmp(name_data, actual_name) != 0) {
                                RegCloseKey (connection_key);
                                ++i;
                                continue;
                            }
                        }
                        else {
                            snprintf(actual_name, actual_name_size, "%s", name_data);
                        }
                    }
                    stop = 1;
                }
            }

            RegCloseKey (connection_key);
        }
        ++i;
    }

    RegCloseKey (control_net_key);

    if (stop == 0)
        return -1;

    return 0;
}

static DWORD get_interface_index (const char *guid)
{
  ULONG index;
  DWORD status;
  wchar_t wbuf[256];
  snwprintf (wbuf, sizeof (wbuf), L"\\DEVICE\\TCPIP_%S", guid);
  wbuf [sizeof(wbuf) - 1] = 0;
  if ((status = GetAdapterIndex (wbuf, &index)) != NO_ERROR)
    return (DWORD)~0;
  else
    return index;
}

static int tap_win32_set_status(HANDLE handle, int status)
{
    unsigned long len = 0;

    return DeviceIoControl(handle, TAP_IOCTL_SET_MEDIA_STATUS,
                &status, sizeof (status),
                &status, sizeof (status), &len, NULL);
}

int open_tap(ip4_addr_t local_ip, ip4_addr_t local_mask, struct tap_info * ti)
{
    // #TODO this function needs some pretty heavy cleanup
    const char *prefered_name = NULL;
      
    DWORD err;
    char device_path[256];
    char device_guid[0x100];
    int rc;
    HANDLE handle;
    DWORD index;
    ULONG ipapi_context;
    ULONG ipapi_instance;
    BOOL bret;
    char name_buffer[0x100] = {0, };
    struct {
        unsigned long major;
        unsigned long minor;
        unsigned long debug;
    } version;
    DWORD len;

    if (prefered_name != NULL)
        snprintf(name_buffer, sizeof(name_buffer), "%s", prefered_name);

    rc = get_device_guid(device_guid, sizeof(device_guid), name_buffer, sizeof(name_buffer));
    if (rc)
        return -1;
    index = get_interface_index(device_guid);

    snprintf (device_path, sizeof(device_path), "%s%s%s",
              USERMODEDEVICEDIR,
              device_guid,
              TAPSUFFIX);

    handle = CreateFile (
        (LPCTSTR) device_path,
        GENERIC_READ | GENERIC_WRITE,
        0,
        0,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_SYSTEM | FILE_FLAG_OVERLAPPED,
        0 );

    if (handle == INVALID_HANDLE_VALUE) {
        return -2;
    }

    bret = DeviceIoControl(handle, TAP_IOCTL_GET_VERSION,
                           &version, sizeof (version),
                           &version, sizeof (version), &len, NULL);

    if (bret == FALSE) {
        CloseHandle(handle);
        return -3;
    }
    
    if(!DeviceIoControl(handle, TAP_IOCTL_GET_MAC, ti->mac, 6, ti->mac, 6, &len, 0)) {
      return -4;
    }
    
    uint32_t ep[4];
    ep[0] = local_ip;
    ep[1] = local_mask;
    ep[2] = 0xFE00000A;
    ep[3] = 0x00FFFFFF;
    if (!DeviceIoControl (handle, TAP_IOCTL_CONFIG_DHCP_MASQ,
			    ep, sizeof (ep),
			    ep, sizeof (ep), &len, NULL))
      return -5;
    
    if (!tap_win32_set_status(handle, TRUE)) {
      return -6;
    }
    
    FlushIpNetTable(index);
    
    overlap_read.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
	  overlap_write.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
	  if (!overlap_read.hEvent || !overlap_write.hEvent) {
  		return -7;
  	}
    
    // delete_temp_addresses (index);
    
    // printf("%x\n", local_ip);
    // if ((err = AddIPAddress (local_ip,
                             // local_mask,
                             // index,
                             // &ipapi_context,
                             // &ipapi_instance)) != NO_ERROR)
    // {
      // printf("AddIPAddress failed! (%d)\n", err);

      // LPVOID lpMsgBuf;

      // if (5010 != err) {
        // if (FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                          // FORMAT_MESSAGE_FROM_SYSTEM |
                          // FORMAT_MESSAGE_IGNORE_INSERTS,
                          // NULL, err,
                          // MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                          // (LPTSTR) & lpMsgBuf, 0, NULL)) {
              // printf("\tError: %s", lpMsgBuf);
              // LocalFree(lpMsgBuf);
        // }
        // return -5;
      // }
    // }
    // if (AddIPAddress (local_ip,
                      // local_mask,
                      // index,
                      // &ipapi_context,
                      // &ipapi_instance) != NO_ERROR)
      // return -7;
    


    
    ti->desc->desc = handle;
    ti->desc->context = ipapi_context;
    
    return 0;
}


void close_tap(union tap_desc * td)
{
  //DeleteIPAddress(td->context);
  CloseHandle(td->desc);
}


/* Read a frame from a tap device. The buffer needs
 * to be at least as large as the MTU of the device. */
int read_tap(union tap_desc * td, char * buf, int len)
{
	DWORD read_size;
	
	ResetEvent(overlap_read.hEvent);
	if (ReadFile(td->desc, buf, len, &read_size, &overlap_read)) {
		return read_size;
	}
	switch (GetLastError()) {
	case ERROR_IO_PENDING:
		WaitForSingleObject(overlap_read.hEvent, INFINITE);
		GetOverlappedResult(td->desc, &overlap_read, &read_size, FALSE);
		return read_size;
		break;
	default:
		break;
	}
	
	return -1;
}

/* Write a frame to a tap device. The frame length
 * must be less than the MTU of the device. */
int write_tap(union tap_desc * td, const char * buf, int len)
{  
  DWORD write_size;
	
	ResetEvent(overlap_write.hEvent);
	if (WriteFile(td->desc,
		buf,
		len,
		&write_size,
		&overlap_write)) {
		return write_size;
	}
	switch (GetLastError()) {
	case ERROR_IO_PENDING:
		WaitForSingleObject(overlap_write.hEvent, INFINITE);
		GetOverlappedResult(td->desc, &overlap_write,
			&write_size, FALSE);
		return write_size;
		break;
	default:
		break;
	}
	
	return -1;
}


