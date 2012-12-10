CC = gcc
ECL = ecl

# Add source files in here that I want to compile...
SRCS = lv2-ecl.c 

LSRCS = plugin.lisp amp.lisp

# name of the created program
TARGET = de

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

%.o : %.lisp
	$(ECL) -s -compile $< -o $@

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

OBJS := $(patsubst %.c,%.o,$(SRCS)) $(patsubst %.lisp,%.o,$(LSRCS))

$(TARGET): .autodepfile $(TARGET_LIB) $(OBJS)
	$(CC) $(CFLAGS) $(LINKPATH) $(OBJS) $(LIBS) -o $(TARGET)

$(TARGET_LIB): $(LSRCS)
	./compile-lisp.lsh $(TARGET_LISP_LIB) $(TARGET_LISP_LIB_INIT) $(LSRCS)

.PHONY: clean
clean:
	- rm -f $(TARGET) $(TARGET_LIB) core a.out $(OBJS) gmon.out .depfile .autodepfile

# This is set up with the dependancies and file name such that make doesn't
# try and rebuild the depfile for stuff like make clean...
.autodepfile: $(SRCS)
	$(CC) -MM $(CFLAGS) $^ > .depfile
	touch .autodepfile

# Include any created dependancies...
-include .depfile



