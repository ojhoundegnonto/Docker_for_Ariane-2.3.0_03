SUFFIXES = .F90 .f90 .o .mod

.f90.mod:
	$(FC) $(FCFLAGS) -c $<

.f90.o:
	$(FC) $(FCFLAGS) -c -o $@ $<

clean-local:
	-rm -f *.mod *.o *__genmod.f90

mod_precision.mod:
	$(FC) -cpp $(CPPFLAGS) $(FCFLAGS) -c mod_precision.F90 

##
SUBDIRS		= .
bin_PROGRAMS  	= ariane
bin_SCRIPTS 	= mod_criter0.f90 mod_criter1.f90 mod_criter2.f90
##
##
ariane_SOURCES 	=                      \
	mod_precision.F90              \
	mod_configure.f90              \
	mod_lun.f90                    \
	mod_cst.f90                    \
	mod_namelist.f90               \
	mod_memory.f90                 \
	mod_flags.f90                  \
	mod_which_type.f90             \
	mod_print_criters.f90          \
	mod_reducmem.f90               \
	mod_netcdf.f90                 \
	mod_netcdf_write_fast.f90      \
	mod_input_grid.f90             \
	mod_seq.f90                    \
	mod_w_comput.f90               \
	mod_rhostp.f90                 \
	mod_sigma.f90                  \
	mod_posin.f90                  \
	mod_fx.f90                     \
	mod_fy.f90                     \
	mod_fz.f90                     \
	mod_input_data_main.f90        \
	mod_netcdf_output.f90          \
	mod_b2c_grid_save.f90          \
	mod_b2c_grid_interpolation.f90 \
	mod_input_data.f90             \
	mod_init_particules.f90        \
	mod_stats.f90                  \
	mod_stati.f90                  \
	mod_zinter.f90                 \
	mod_orca.f90                   \
	mod_txt.f90                    \
	mod_output_data.f90            \
	mod_trajec_subs.f90            \
	mod_criter0.f90                \
	mod_criter1.f90                \
	mod_criter2.f90                \
	mod_quant.f90                  \
	mod_save_netcdf.f90            \
	posini.f90                     \
	posini_seq.f90                 \
	trajec.f90                     \
	trajec_seq.f90                 \
	ariane.f90 
##
## noinst_LIBRARIES = netcdf.a
##

