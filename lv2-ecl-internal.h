#ifndef _LV2_ECL_H
#define _LV2_ECL_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
#define NUM_INSTANCES (1024 * NUM_DESCRIPTORS)

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

	// When we finally return an instance handle back to the host, it'll be
	// this one as it is uniquely initialized per structure. Later, we'll use
	// it to remap the handle back to the lisp_handle and the lv2_desc_index.
	LV2_Handle handle;

	// The real handle as returned from the lisp plguin.
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

#endif 
