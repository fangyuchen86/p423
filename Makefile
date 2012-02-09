#----------------------------------------------------------------------
# File Makefile	
# 	
# Created 10 Jan 2012 by Chris Frisz
# Modified 8 Feb 2012 by Samuel Waggoner
# 	
# This Makefile is intended for use with CSCI-P423 and runs the the
# load_and_test.ss file. It may be extended to do other things as you
# may need.
#----------------------------------------------------------------------

#-- Variables --#
SC=scheme
TESTS=tests.ss
LIBS=load_and_test.ss
ENV=$(SC) $(LIBS)



#-- Rules --#

# The main point of this file is to run the tests
all : env

# Run the testing on the compiler
env : $(LIBS)
	$(ENV)

test : $(TESTS)
	cat $(TESTS) | $(ENV)

test-all :
	echo "(test-all)" | $(ENV)