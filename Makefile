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
TESTS=tests.ss
ENV=env.ss

#-- Rules --#

# The main point of this file is to create the compiler environment
all : env

# Create the compiler environment
env : $(ENV)
	scheme $(ENV)

# Run custom tests on the compiler
tests : $(TESTS)
	cat $(TESTS) | scheme $(ENV)

# Run the testing on the compiler
test-all :
	echo "(test-all)" | scheme $(ENV)