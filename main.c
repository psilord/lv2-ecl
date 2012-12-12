#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ecl/ecl.h>
#include "lv2/lv2plug.in/ns/lv2core/lv2.h"

/* the interface to the plugin */
extern const LV2_Descriptor* lv2_descriptor(uint32_t index);

// Drive the plugin interface and see what it does.
int main(int argc, char **argv)  
{
	int i, j;

	int num_descriptors = 10;  // try for ten, resets to whatever we get
	int num_handles_per_descriptor = 3; // fixed.

	const LV2_Descriptor **lv2_desc = 
		malloc(sizeof(LV2_Descriptor*) * num_descriptors);

	// Let's mimic how the plugin will be called.

	for (i = 0; i < num_descriptors; i++) {
		// call the "entry point" to the plugin.
		lv2_desc[i] = lv2_descriptor(i);
		printf("LV2_Descriptor: %p\n", lv2_desc[i]);
		if (lv2_desc[i] == NULL) {
			// Stop whenever the plugin wants us to stop or we hit the
			// maximum number.
			num_descriptors = i;
			printf("Done getting %d descriptors!\n", num_descriptors);
			break;
		}
	}

	// Now, instantiate exactly X objects for each defined plugin

	LV2_Handle **phandles = malloc(sizeof(LV2_Handle) * num_descriptors);

	for (i = 0; i < num_descriptors; i++) {
		phandles[i] = malloc(sizeof(LV2_Handle) * num_handles_per_descriptor);

		// and then ask for X real handles from the specific plugin!
		for (j = 0; j < num_handles_per_descriptor; j++) {
			LV2_Handle *handle = phandles[i];
			handle[j] = 
			lv2_desc[i]->instantiate(lv2_desc[i], 2.4, "/some/path", NULL);
		}
	}


	/* 

	// test execute something out of the library we just initialized
	cl_object num3 = cl_funcall(3,c_string_to_object("doit"),
		MAKE_FIXNUM(10), MAKE_FIXNUM(20));

	cl_princ(1, num3);
	cl_princ(1, c_string_to_object("#\\Newline"));

	*/

	printf("Shutting it all down.\n");
	cl_shutdown();

	return EXIT_SUCCESS;
}
