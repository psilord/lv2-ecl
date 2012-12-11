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
	LV2_Handle handle;

	// The handle
	cl_object lisp_handle;

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
static void associate_lv2_and_lisp_descs(int index, cl_object lisp_obj);
static LV2_Descriptor* get_lv2_desc_address(int index);

// dumb global variables
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
		g_plassoc_initialized = TRUE;
	}

	if (g_hdassoc_initialized == FALSE) {
		for (i = 0; i < NUM_DESCRIPTORS * NUM_INSTANCES; i++) {
			g_hdassoc[i].initialized = FALSE;
			g_hdassoc[i].lv2_desc_index = NONE;
			g_hdassoc[i].handle = NULL;
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
			printf("Picked index: %d\n", i);
			return i;
		}
	}

	return NONE;
}

static void associate_lv2_and_lisp_descs(int index, cl_object lisp_obj)
{
	/* XXX bitwise copy, can I do that to a cl_object correctly? */
	g_plassoc[index].lisp_lv2_desc = lisp_obj;
}

// given an index, return the address of the lv2 descriptor from there.
static LV2_Descriptor* get_lv2_desc_address(int index)
{
	return &g_plassoc[index].lv2_desc;
}

// given a lv2_desc pointer, return the index associated with it.
static int get_lv2_desc_index(const LV2_Descriptor *lv2_desc)
{
	int i;

	for (i = 0; i < NUM_DESCRIPTORS; i++) {
		if (&g_plassoc[i].lv2_desc == lv2_desc) {
			return i;
		}
	}

	return NONE;
}


// C entry point first called by application. 
const LV2_Descriptor*
lv2_descriptor(uint32_t index)
{
	int desc_index;

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

	if (obj == Cnil) {
		// Don't make an association in this case.
		return NULL;
	}

	// Associate the lisp description with a C address of a real LV2_Descriptor
	desc_index = allocate_new_lv2_descriptor();
	associate_lv2_and_lisp_descs(desc_index, obj);

	// The application will use this to talk about this specific plugin.
	return get_lv2_desc_address(desc_index);
}

#define AMP_URI "http://lv2plug.in/plugins/eg-amp"

// Create a new plugin instance.
static LV2_Handle
instantiate(const LV2_Descriptor*     descriptor,
            double                    rate,
            const char*               bundle_path,
            const LV2_Feature* const* features)
{
	int lv2_desc_index;

	
	// lookup the DescAssoc with this descripter pointer
	lv2_desc_index = get_lv2_desc_index(descriptor);

	printf("Instantiate for LV2_Desc called:\n"
		"\tdesc_index = %d\n"
		"\tdescriptor = %p\n"
		"\trate = %f\n"
		"\tbundle path = %s\n"
		"\tfeatures array = %p\n", 
		lv2_desc_index, descriptor, rate, bundle_path, features);

	// Call the mirrored lisp instantiate function on the associated
	// lisp version of the lv2-descriptor. This is going to return a
	// cl_object that represents an LV2_Handle.

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

// Drive the plugin interface and see what it does.
int main(int argc, char **argv)  
{
	int i, j;

	int num_descriptors = 10;  // try for ten, resets to whatever we get
	int num_handles_per_descriptor = 10; // fixed.

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



static LV2_Handle instantiate(const LV2_Descriptor *descriptor, 
		double rate, const char* bundle_path,
		const LV2_Feature* const* features);


