#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ecl/ecl.h>
#include "lv2/lv2plug.in/ns/lv2core/lv2.h"
#include "lv2-ecl-internal.h"

// move to lisp side.
#define AMP_URI "http://lv2plug.in/plugins/eg-amp"

static DescAssoc g_plassoc[NUM_DESCRIPTORS];
static HandleDescAssoc g_hdassoc[NUM_INSTANCES];
static int g_plassoc_initialized = FALSE;
static int g_hdassoc_initialized = FALSE;

static int g_cl_booted = FALSE;

// Take a string, and return a malloced string which is what was passed in,
// but with the literal " " quotes around it. You must free the memory
// when done with it.
static char* stringify(const char *string)
{
	char *str = NULL;
	
	str = calloc(strlen(string) + 2, sizeof(char));
	strcpy(str, "\"");
	strcat(str, string);
	strcat(str, "\"");
	return str;
}

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
		for (i = 0; i < NUM_INSTANCES; i++) {
			g_hdassoc[i].initialized = FALSE;
			g_hdassoc[i].lv2_desc_index = NONE;

			// initialize the handle to a unique value, it doesn't matter what
			// the bit pattern is here, only that they are unique from all the
			// rest. Yes, I know (void*)0 is NULL, but it doesn't matter.
			g_hdassoc[i].handle = (void*)i;
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

static DescAssoc* get_desc_assoc_at_index(int index)
{
	return &g_plassoc[index];
}


// return an index to a usable HandleDescAssoc structure.
static int allocate_new_lv2_handle(void)
{
	int i;

	for (i = 0; i < NUM_INSTANCES; i++) {
		if (g_hdassoc[i].initialized == FALSE) {
			g_hdassoc[i].initialized = TRUE;
			printf("Picked index: %d\n", i);
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


// Create a new plugin instance.
static LV2_Handle
instantiate(const LV2_Descriptor*     descriptor,
            double                    rate,
            const char*               bundle_path,
            const LV2_Feature* const* features)
{
	char *real_string = NULL;
	int lv2_desc_index;
	DescAssoc *da = NULL;

	// lookup the DescAssoc with this descripter pointer
	lv2_desc_index = get_lv2_desc_index(descriptor);
	da = get_desc_assoc_at_index(lv2_desc_index);

	printf("Instantiate for LV2_Desc called:\n"
		"\tdesc_index = %d\n"
		"\tdescriptor = %p\n"
		"\trate = %f\n"
		"\tbundle path = %s\n"
		"\tfeatures array = %p\n", 
		lv2_desc_index, descriptor, rate, bundle_path, features);

	// Call the mirrored lisp instantiate function from the associated
	// lisp version of the lv2-descriptor. This is going to return a
	// cl_object that represents an LV2_Handle.

	// first, get the function we need to call from the right lisp lv2
	// descriptor.
	cl_object lisp_instantiate_function = 
		cl_funcall(2,
			c_string_to_object("lv2-instantiate"),
			da->lisp_lv2_desc);

	
	// Then call it, passing the usual stuff. lisp will return to us the
	// handle it wants back.
	real_string = stringify(bundle_path);
	cl_object lisp_handle =
		cl_funcall(5,
			lisp_instantiate_function,
			da->lisp_lv2_desc,
			ecl_make_double_float(rate),
			c_string_to_object(real_string),
			Cnil);
	free(real_string);

	// Associate the lisp_handle with a duck handle we're going to give to the
	// TODO

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
	ecl_import_current_thread(ECL_NIL, ECL_NIL);

	// stuff

	ecl_release_current_thread();
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
	ecl_import_current_thread(ECL_NIL, ECL_NIL);
	// Do something
	ecl_release_current_thread();
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
