#include "ittnotify.h"
//#include <stdio.h>
 
void fortran_itt_resume()
{
    __itt_resume();
}
 
void fortran_itt_pause()
{
    __itt_pause();
}

__itt_domain* fortran_itt_domain_create(char *name)
{
	__itt_domain* domain;
	domain = __itt_domain_create(name);
	//printf("domain %s : %p\n", name, domain);
	return domain;
}

__itt_string_handle* fortran_itt_string_handle_create(char *name)
{
	__itt_string_handle* handle;
	handle = __itt_string_handle_create(name);
	//printf("handle %s : %p\n", name, handle);
	return handle;
}

void fortran_itt_task_begin(__itt_domain* domain, __itt_string_handle* handle)
{
       //printf("begin %p : %p\n", domain, handle);
	__itt_task_begin(domain, __itt_null, __itt_null, handle);
}

void fortran_itt_task_end(__itt_domain* domain)
{
       //printf("end %p\n", domain);
	__itt_task_end(domain);
}

