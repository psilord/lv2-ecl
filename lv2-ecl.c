#include <stdio.h>
#include <stdlib.h>
#include <ecl/ecl.h>

/*
#include "lv2/lv2plug.in/ns/lv2core/lv2.h"

#define AMP_URI "http://lv2plug.in/plugins/eg-amp"

// The LV2_Descriptor for this plugin.
static const LV2_Descriptor descriptor = {
	AMP_URI,
	instantiate,
	connect_port,
	activate,
	run,
	deactivate,
	cleanup,
	extension_data
};

// entry point. 
const LV2_Descriptor*
lv2_descriptor(uint32_t index)
{

	// See about trampolining this too.

	switch (index) {
	case 0:
		return &descriptor;
	default:
		return NULL;
	}
}


// Create a new plugin instance.
static LV2_Handle
instantiate(const LV2_Descriptor*     descriptor,
            double                    rate,
            const char*               bundle_path,
            const LV2_Feature* const* features)
{

	// Initialize the lisp instance.
	cl_boot(argc, argv);

	// Magic name is previously known 
	extern void I_libfoo(cl_object);
	read_VV(OBJNULL, I_libfoo);

	// Call the lisp instantiate function.

	// Alloc an instance here and return it
	return (LV2_Handle) NULL;
}

// Connect a port to a buffer (audio thread, must be RT safe).
static void
connect_port(LV2_Handle instance,
             uint32_t   port,
             void*      data)
{
	
}

// Initialise and prepare the plugin instance for running.
static void
activate(LV2_Handle instance)
{
	// Nothing to do here in this trivial mostly stateless plugin.
}

#define DB_CO(g) ((g) > -90.0f ? powf(10.0f, (g) * 0.05f) : 0.0f)

// Process a block of audio (audio thread, must be RT safe).
static void
run(LV2_Handle instance, uint32_t n_samples)
{
	// Do something
}

// Finish running (counterpart to activate()).
static void
deactivate(LV2_Handle instance)
{
	// Nothing to do here in this trivial mostly stateless plugin.
}

// Destroy a plugin instance (counterpart to instantiate()).
static void
cleanup(LV2_Handle instance)
{
	// do something.
}

// Return extension data provided by the plugin.
static const void*
extension_data(const char* uri)
{
	// This plugin has no extension data.
	return NULL;
}
*/


int main(int argc, char **argv)  
{

	/* execute something out of the library we just initialized */
	cl_object num3 = cl_funcall(3,c_string_to_object("doit"),
		MAKE_FIXNUM(10), MAKE_FIXNUM(20));

	cl_princ(1, num3);
	cl_princ(1, c_string_to_object("#\\Newline"));

	cl_shutdown();

	return EXIT_SUCCESS;
}
