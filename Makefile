CC = gcc
ECL = ecl

# name of the created program
TARGET = de

DRIVER_CSRCS = main.c

PLUGIN_CSRCS = lv2-ecl.c
PLUGIN_LSRCS = plugin.lisp amp.lisp

TARGET_LISP_LIB = foo.a
TARGET_LISP_LIB_INIT = I_libfoo
TARGET_LIB = lib$(TARGET_LISP_LIB)

# Flags I wish to define on the compile line.
DEF_FLAGS = -g -Wall -fPIC `ecl-config --cflags`

# Where are the API header files I'm using?
INCLUDEPATH = -I/usr/local/include  -I/usr/local/include/lv2

# Where to find various libraries.
LINKPATH = -L /usr/local/lib `ecl-config --ldflags`

# Various libraries I'm going to need...
LIBS = $(TARGET_LIB) `ecl-config --libs`

###################################################################
# Generally you don't want to mess with stuff below this line...
###################################################################

CFLAGS = $(DEF_FLAGS) $(INCLUDEPATH)

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

DRIVER_OBJS := $(patsubst %.c,%.o,$(DRIVER_CSRCS))

PLUGIN_COBJS := $(patsubst %.c,%.o,$(PLUGIN_CSRCS))
PLUGIN_LOBJS := $(patsubst %.lisp,%.o,$(PLUGIN_LSRCS))
PLUGIN_OBJS := $(PLUGIN_COBJS) $(PLUGIN_LOBJS)

$(TARGET): .autodepfile $(DRIVER_OBJS) $(TARGET_LIB)
	$(CC) $(CFLAGS) $(LINKPATH) $(DRIVER_OBJS) $(LIBS) -o $(TARGET)

$(TARGET_LIB): $(PLUGIN_COBJS) $(PLUGIN_LSRCS)
	./compile-lisp.lsh $(TARGET_LISP_LIB) $(TARGET_LISP_LIB_INIT) $(PLUGIN_LSRCS)
	ar rs $(TARGET_LIB) $(PLUGIN_COBJS)

.PHONY: clean
clean:
	- rm -f $(TARGET) $(TARGET_LIB) $(DRIVER_OBJS) $(PLUGIN_OBJS) core aout gmon.out .depfile .autodepfile

# This is set up with the dependancies and file name such that make doesn't
# try and rebuild the depfile for stuff like make clean...
.autodepfile: $(DRIVER_CSRCS) $(PLUGIN_CSRCS)
	$(CC) -MM $(CFLAGS) $^ > .depfile
	touch .autodepfile

# Include any created dependancies...
-include .depfile



