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
examplesdir     = $(prefix)/examples/quantitative
examples_DATA  	= go.sh namelist segments segrid region_limits
EXTRA_DIST      = src_go.sh namelist_check segments segrid quant_check.sh region_limits
TESTS           = quant_check.sh
#TESTS_ENVIRONMENT = $(SHELL)
##
