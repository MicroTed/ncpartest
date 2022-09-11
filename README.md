This example expects the netcdf libraries to be built with pnetcdf support, but can probably be adapted to work without it.

 example compile:
 mpif90 -o x.comnc -I/Users/Shared/opt/local/mpich332/netcdf474p/include commas_netcdf.f90 -L/Users/Shared/opt/local/mpich332/netcdf474p/lib -lnetcdff -lnetcdf -lhdf5_fortran -lhdf5hl_fortran -lhdf5_hl -lhdf5 -lz -lpnetcdf
 set values in 'namelist.input' (see below)
 mpirun -np N x.comnc (where N = nprocx*nprocy)

The namelist mainly controls the number of processors (nprocx, nprocy) and whether to use the HDF5 interface (parallelio_type = 1) or the pnetcdf interface (parallelio_type = 2 )

&params
  nx = 40,
  ny = 40,
  nprocx = 2,
  nprocy = 2,
!  regfile = 'registry.zvd'
  regfile = 'registry.take'
  parallelio_type = 1
/

regfile = 'registry' file, which has a list of variables (0D, 1D, 2D, 3D) with attributes (time dependence and other)



