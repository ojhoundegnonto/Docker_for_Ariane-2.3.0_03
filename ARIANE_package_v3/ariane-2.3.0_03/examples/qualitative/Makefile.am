all: namelist go.sh

namelist:
	sed "s%../../data%${prefix}/examples/data%g" namelist_check > namelist

go.sh:
	sed "s%/your_dir/bin%${prefix}/bin%g" src_go.sh > go.sh

clean:
	rm -f namelist go.sh
	rm -fr results

##
SUBDIRS		= . validation
examplesdir     = $(prefix)/examples/qualitative
examples_DATA  	= go.sh namelist initial_positions
EXTRA_DIST      = src_go.sh namelist_check initial_positions quali_check.sh
TESTS           = quali_check.sh
#TESTS_ENVIRONMENT = $(SHELL)
##
