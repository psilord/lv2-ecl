#include <stdio.h>
#include <stdlib.h>
#include <ecl/ecl.h>
#include "lv2/lv2plug.in/ns/lv2core/lv2.h"

#define TRUE 1
#define FALSE 0
// What I return as an index for things in arrays when I can't find a good one.
#define NONE -1

// I choose to use arrays for things instead of hash-tables. This reduces the
// code I have to write, but introduces limitations to the number of existing
// objects.

// There can be this many total descriptors
#define NUM_DESCRIPTORS 1024
// Each descriptr can make this number of instances of something.
#define NUM_INSTANCES 1024

typedef struct _DescAssoc
{
	int initialized;

	// The C descriptor we're going to give to the caller.
	LV2_Descriptor lv2_desc;

	// The association with the lisp version of it.
	cl_object lisp_lv2_desc;

} DescAssoc;

// We need to reverse map an LV2_Handle to the LV2_Description type which
// ultimately produced it.
typedef struct _HandleDescAssoc
{
	int initialized;

	// Which index in the DescAssoc array made this instance?
	int lv2_desc_index;

	// Something we need to associate to which plugin desc which 
	// ultimately produced it.
	LV2_Handle instance;

} HandleDescAssoc;

// prototypes
const LV2_Descriptor* lv2_descriptor(uint32_t index);
static LV2_Handle instantiate(const LV2_Descriptor *descriptor, 
		double rate, const char* bundle_path,
		const LV2_Feature* const* features);
static void connect_port(LV2_Handle instance, uint32_t port, void *data);
static void activate(LV2_Handle instance);
static void run(LV2_Handle instance, uint32_t n_samples);
static void deactivate(LV2_Handle instance);
static void cleanup(LV2_Handle instance);
static const void* extension_data(const char* uri);

static DescAssoc g_plassoc[NUM_DESCRIPTORS];
static HandleDescAssoc g_hdassoc[NUM_DESCRIPTORS * NUM_INSTANCES];
static int g_plassoc_initialized = FALSE;
static int g_hdassoc_initialized = FALSE;

static int g_cl_booted = FALSE;

static void initialize_plugin_internals(void)
{
	int i;

	if (g_plassoc_initialized == FALSE) {
		for (i = 0; i < NUM_DESCRIPTORS; i++) {
			g_plassoc[i].initialized = FALSE;

			// Each desc always points to the same C functions in this api.
			// This basically never changes.
			g_plassoc[i].lv2_desc.instantiate = instantiate;
			g_plassoc[i].lv2_desc.connect_port = connect_port;
			g_plassoc[i].lv2_desc.activate = activate;
			g_plassoc[i].lv2_desc.run = run;
			g_plassoc[i].lv2_desc.deactivate = deactivate;
			g_plassoc[i].lv2_desc.cleanup = cleanup;
			g_plassoc[i].lv2_desc.extension_data = extension_data;
		}
	}

	if (g_hdassoc_initialized == FALSE) {
		for (i = 0; i < NUM_DESCRIPTORS * NUM_INSTANCES; i++) {
			g_hdassoc[i].initialized = FALSE;
		}
		g_hdassoc_initialized = TRUE;

	}
}

static void initialize_ecl(void) 
{
	int fake_argc = 1;
	char *fake_argv[] = {"InternalPlugin", NULL};

	if (g_cl_booted == FALSE) {
		// Initialize ECL
		cl_boot(fake_argc, fake_argv);

		// Use the UFFI package before we do anything.
		cl_use_package(1, (cl_find_package(ecl_make_keyword("UFFI"))));

		// Initalize the Common Lisp plugin library.
		// Magic name is previously known 
		extern void I_libfoo(cl_object);
		read_VV(OBJNULL, I_libfoo);

		g_cl_booted = TRUE;
	}
}

// search the g_plassoc table and return me an index into the
// AssocDesc association array, or 
// 
static int allocate_new_lv2_descriptor(void)
{
	int i;

	for (i = 0; i < NUM_DESCRIPTORS; i++) {
		if (g_plassoc[i].initialized == FALSE) {
			g_plassoc[i].initialized = TRUE;
			return i;
		}
	}

	return NONE;
}


// C entry point first called by application. 
const LV2_Descriptor*
lv2_descriptor(uint32_t index)
{
	initialize_plugin_internals();
	initialize_ecl();

	// Trampoline this request to the ECL side, then unpack the resultant
	// return value into something suitable for the caller.

	cl_object obj = 
		cl_funcall(2, c_string_to_object("lv2-descriptor"),
			MAKE_FIXNUM(index));

	printf("C: lv2_descriptor got from lisp:");
	cl_pprint(1, obj);
	cl_princ(1, c_string_to_object("#\\Newline"));

	// Convert obj to something meaningful and return it


	return (LV2_Descriptor*) NULL;
}

#define AMP_URI "http://lv2plug.in/plugins/eg-amp"

// Create a new plugin instance.
static LV2_Handle
instantiate(const LV2_Descriptor*     descriptor,
            double                    rate,
            const char*               bundle_path,
            const LV2_Feature* const* features)
{


	// Call the lisp instantiate function.

	// Find a free instance in the array and fill it, returning the
	// pointer to the LV2_Descriptor in it.
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

int main(int argc, char **argv)  
{
	const LV2_Descriptor *lv2_desc = NULL;

	// Let's mimic how the plugin will be called.

	// This is the first call into the plugin, it is here that we 
	// initialize ECL and invoke the Lisp function of the same name.
	lv2_desc = lv2_descriptor(0);


	/* execute something out of the library we just initialized */
	cl_object num3 = cl_funcall(3,c_string_to_object("doit"),
		MAKE_FIXNUM(10), MAKE_FIXNUM(20));

	cl_princ(1, num3);
	cl_princ(1, c_string_to_object("#\\Newline"));

	cl_shutdown();

	return EXIT_SUCCESS;
}
