#define MPI
#define NC4
!     This is part of the netCDF package.  Copyright 2006 University
!     Corporation for Atmospheric Research/Unidata.  See COPYRIGHT
!     file for conditions of use.

!     This is a very simple example which writes a 2D array of sample
!     data. To handle this in netCDF we create two shared dimensions,
!     "x" and "y", and a netCDF variable, called "data".

!     This example demonstrates the netCDF Fortran 90 API. This is
!     part of the netCDF tutorial, which can be found at:
!     http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-tutorial
      
!     Full documentation of the netCDF Fortran 90 API can be found at:
!     http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90

!     $Id: simple_xy_nc4_wr.f90,v 1.6 2010/04/06 19:32:09 ed Exp $
! example compile
! mpif90 -o x.comnc -I/Users/Shared/opt/local/mpich332/netcdf474p/include commas_netcdf.f90 -L/Users/Shared/opt/local/mpich332/netcdf474p/lib -lnetcdff -lnetcdf -lhdf5_fortran -lhdf5hl_fortran -lhdf5_hl -lhdf5 -lz -lpnetcdf
! set values in 'namelist.input'
! mpirun -np 4 x.comnc

!-------------------------------------------------------------------------------
!
!
!
!
!
!
!
!-------------------------------------------------------------------------------

MODULE GRID_MODULE

 implicit none

 logical DEBUG          ; parameter(DEBUG          = .false.)
 logical FAILsoEXIT     ; parameter(FAILsoEXIT     = .true. )
 integer lun2           ; parameter(lun2           = 81     )
 integer lundbg         ; parameter(lundbg         = 0      )
 logical overwrite_file ; parameter(overwrite_file = .true. )
 logical copy_variable  ; parameter(copy_variable  = .true. )

 integer name_length    ; parameter(name_length    = 10)
 integer type_length    ; parameter(type_length    =  5)
 integer unit_length    ; parameter(unit_length    = 15)
 integer desc_length    ; parameter(desc_length    =255)

 TYPE VARIABLE

  character(LEN=name_length)   name
  character(LEN=type_length)   type
  integer                      tdepend
  integer                      dim
  integer                      ng
  integer                      istag
  integer                      jstag
  integer                      kstag
  integer                      index
  integer                      pdef            ! positive definite variable
  integer                      dyntype         ! dynamics type (flag)
  integer                      phytype         ! mixing type   (flag)
  integer                      buotype         ! whether variable used in buoyancy calculation
  character(LEN=unit_length)   unit
  character(LEN=desc_length)   description
  integer                      basedim
  character(LEN=desc_length)   field
  character(LEN=desc_length)   positions
  integer                      binindex
  integer                      numbins

  integer                   :: int  
  real                      :: flt
  real, allocatable         :: flt1d(:) 
  real, allocatable         :: flt2d(:,:) 
  real, allocatable         :: tmp3d(:,:,:) 
  real, pointer             :: flt3d(:,:,:) 

  real, allocatable         :: base1d(:) 
  real, allocatable         :: base2d(:,:) 
  real, allocatable         :: base3d(:,:,:) 

 END TYPE VARIABLE

 TYPE XYZ4D
  real, allocatable         :: flt4d(:,:,:,:) 
 END TYPE XYZ4D
 
 TYPE ATTRIBUTE
  character(LEN=20)             name
  character(LEN=3)              type
  integer                       int
  real                          flt
  character(LEN=desc_length) :: str 
 END TYPE ATTRIBUTE

 TYPE GRID

  TYPE (ATTRIBUTE), allocatable, dimension(:) :: attr
  TYPE (XYZ4D) :: xyz3d
  TYPE (VARIABLE),  allocatable, dimension(:) :: var

 END TYPE GRID

 TYPE MAXMIN
  character(LEN=name_length) name
  real                       max,  min
  integer                    imax, imin
  integer                    jmax, jmin
  integer                    kmax, kmin
 END TYPE MAXMIN
 
 INTERFACE GET_ATTRIBUTE
    module procedure GET_ATTRIBUTE_, GET_ATTRIBUTE_INT, GET_ATTRIBUTE_FLT, GET_ATTRIBUTE_STR
 END INTERFACE
 
 INTERFACE SET_ATTRIBUTE
    module procedure SET_ATTRIBUTE_, SET_ATTRIBUTE_INT, SET_ATTRIBUTE_FLT, SET_ATTRIBUTE_STR
 END INTERFACE
 
 INTERFACE SET_VARIABLE
    module procedure SET_VARIABLE_,        &
                     SET_VARIABLE_INT,     &
                     SET_VARIABLE_FLT
 END INTERFACE
    
 INTERFACE GET_VARIABLE
    module procedure GET_VARIABLE_,        &
                     GET_VARIABLE_INT,     &
                     GET_VARIABLE_FLT,     &
                     GET_VARIABLE_FLT1D,   &
                     GET_VARIABLE_FLT2D,   &
                     GET_VARIABLE_FLT3D

 END INTERFACE GET_VARIABLE

CONTAINS

! #include "src/grid_stat.F90"
#include "grid_variable.F90"
#include "grid_attribute.F90"

END MODULE GRID_MODULE

! ###############################################################################

MODULE FILE_MODULE

 implicit none
 
   logical :: restart_separate = .true. ! whether to write separate restart files; 
                                        ! for ensembles only (must have ne > 1)
   logical :: writing_restart = .false.
   integer :: ienkf           = 0
!   character(LEN = 25) :: runname
!   character(LEN = 60) :: path
   integer             :: lun                     = -1           ! output unit
   
   integer             :: membernumber

   integer             :: historyoutput = 1      ! 0=netcdf for each MPI tile; 1=single netcdf, 2=fortran binary
   integer             :: historyinput  = -1     ! 0=netcdf for each MPI tile; 1=single netcdf, 2=fortran binary
   integer             :: restartformat = 1      ! 0=netcdf for each MPI tile; 1=single netcdf, 2=fortran binary
   logical             :: parallelio = .true.
   logical             :: parallel_compress = .false.
   logical             :: parallel_compress_on = .false.
   integer             :: parallelio_in = -1   ! used for restarting from a netcdf4 file but output will be pnetcdf
   integer             :: netcdfversion = 4
   integer             :: parallelio_type = 1  ! 1=netcdf4/hdf5; 2=pnetcdf
   integer             :: nclibversion = 0
   integer             :: pnetcdf_flag = 0 ! used for setting file mode with pnetcdf depending on netcdf version
   integer             :: shuffle = 1, deflate= 1, deflate_level = 2
   integer             :: chunkalg = 2
   integer             :: onedoutput = 0  ! for outputting a column of data from 2D runs

   
   INTEGER           ::  ihttima = 6
   CHARACTER(len= 6) ::  timfmt  = '(i6.6)'
   character(len=40) ::  filehead          ! base name for history files
   integer           ::  lfilehead         ! number of non-blank characters in filehead
   character(len=60) ::  hdrcfs = CHAR(0)  ! storage directory
   integer           ::  lhdrcfs = 0       ! length of hdrcfs

END MODULE FILE_MODULE
!  
!===============================================================================
MODULE GRIDIO_MODULE

 USE GRID_MODULE
 USE NETCDF
 USE FILE_MODULE
! USE COMMASMPI_MODULE

 implicit none
 
 logical :: DEBUG_IO       = .true.
 logical :: write_attributes = .true.
! logical DEBUG_IO       ; parameter(DEBUG_IO       = .true.)
! #if defined (NC4) && defined (MPI)
! integer, parameter :: NF90_INDEPENDENT = 0
! integer, parameter :: NF90_COLLECTIVE = 1
!These next two are now defined in netcdf as of version 4.1.2 (or maybe earlier),
!so comment out if you get a compile error.
! integer, parameter :: NF90_MPIIO = 2*4096, NF90_PNETCDF=0
! integer, parameter :: NF90_MPIPOSIX = 4*4096
 integer, parameter :: nf90_iotype = NF90_MPIIO !  NF90_MPIPOSIX
! integer, parameter :: nf90_iotype = NF90_MPIPOSIX
 integer, parameter :: INDCOL = NF90_COLLECTIVE
! integer, parameter :: INDCOL = NF90_INDEPENDENT

 
 character(len=155),private :: f(1000)
 integer           ,private :: numlines = 0

CONTAINS

! ######################################################################
!-------------------------------------------------------------------------------
! 
!
!         <<<<<<<<<<<<<<<  GRID_DEFINE_FROM_FILE  >>>>>>>>>>>>>>>>>>>>  
!  
!
! Written by Lou Wicker, Nov/Dec 2005
!  
!-------------------------------------------------------------------------------

  SUBROUTINE GRID_DEFINE_FROM_FILE(gd,filename)
  
!  SUBROUTINE GRID_DEFINE_FROM_FILE(gd)
  
!   USE PARAM_MODULE

   implicit none
   
   character(LEN=*) filename
   integer, parameter :: maxvars = 1000
   character(LEN=255) lines(maxvars),line
   type(GRID) :: gd

   integer nvar, nattr, n, count, fend
   integer ibeg, iend
   character(LEN=10) dummy_char
   character(LEN=type_length) type
   integer i,iadd,istat
   integer :: ng = 3
   
   open(unit=51, file=filename, form='formatted')
   rewind(51)
     DO i = 1,maxvars
       read(51,'(132a)',iostat=istat) f(i)
!       write(*,'(132a)') f(i)
       IF ( f(i)(1:3) .eq. 'EOF' ) THEN
         exit
       ENDIF
     ENDDO
    close(51)


   i = 0
   
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file

 i = i + 1
   read(f(i),*) nattr
   IF( DEBUG .or. DEBUG_IO ) write(6,*) 'DEFINE_GRID_FROM_FILE:  NATTR = ', nattr
   allocate(gd%attr(nattr))

 i = i + 1
   read(f(i),*) nvar
!   IF( DEBUG .or. DEBUG_IO ) write(6,*) 'DEFINE_GRID_FROM_FILE:  NVAR  = ', nvar

 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file

! Read in grid attributes

101 format(1x,a,2x,a,2x,i3,2x,g15.5)

   DO n = 1,nattr

  i = i + 1
   read(f(i),101) gd%attr(n)%name, gd%attr(n)%type
    gd%attr(n)%str(1:desc_length) = ' '            ! Did this because non-initialized strings get garbage put in them...

    IF( DEBUG .or. DEBUG_IO ) write(6,*) 'DEFINE_GRID_FROM_FILE:  ATTR  = ', gd%attr(n)%name

   ENDDO
   
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file
 i = i + 1
   read(f(i),*) dummy_char 				! this is just to skip a comment line in the file

! Read in grid variables (we have omitted the "index" parameter - that is set later...)

102 format(1x,a,2x,i1,2x,a,2x,8(i2,2x),a,2x,a)

    n = 0
    fend = 0

    DO n = 1,maxvars
   i = i + 1
   read(f(i),'(a)') line
     IF ( line(1:3) .ne. 'EOF' ) THEN
!       write(6,'(i3,a)') n, line(1:50)
       lines(n) = line
     ELSE
       nvar = n - 1
       fend = 1
       EXIT
     ENDIF
    
    ENDDO
    
    IF ( fend .ne. 1 ) THEN
      write(0,*) 'DEFINE_GRID_FROM_FILE: Too many variables! Current max is ',maxvars
      STOP
    ENDIF

   IF( DEBUG .or. DEBUG_IO ) write(6,*) 'DEFINE_GRID_FROM_FILE:  NVAR  = ', nvar
   allocate(gd%var(nvar))

   DO n = 1,nvar

!  i = i + 1
!   read(f(i),102) gd%var(n)%name,    &
    read(lines(n),102)              &
                 gd%var(n)%name,    &
                 gd%var(n)%dim,     &
!                 gd%var(n)%ng,      &
                 gd%var(n)%type,    &
                 gd%var(n)%tdepend, &
                 gd%var(n)%istag,   &
                 gd%var(n)%jstag,   &
                 gd%var(n)%kstag,   &
                 gd%var(n)%pdef,    &
                 gd%var(n)%dyntype, &
                 gd%var(n)%phytype, &
                 gd%var(n)%buotype, &
                 gd%var(n)%unit,    &
                 gd%var(n)%description
                 
!                 IF ( gd%var(n)%type == 'nxyz4d' ) THEN
!                   gd%var(n)%nbins = gd%var(n)%dim
!                   gd%var(n)%dim  = 4
!                   iadd = gd%var(n)%nbins
!                 ELSE
!                   gd%var(n)%nbins = 0
                   iadd = 1
!                 ENDIF

    
    IF ( gd%var(n)%dim .eq. 3 ) THEN ! .or. gd%var(n)%type(3:4) .eq. '2d' ) THEN
      IF ( gd%var(n)%tdepend .ge. 1 ) THEN
        gd%var(n)%field = trim(gd%var(n)%name)//', scalar, series'
      ELSE
        gd%var(n)%field = trim(gd%var(n)%name)//', scalar'
      ENDIF
    ELSE
      gd%var(n)%field = CHAR(0)
      gd%var(n)%positions = CHAR(0)
    ENDIF
    
    IF( n .eq. 1 ) THEN
      type = gd%var(1)%type
      count = 1 
    ENDIF

    IF( type .ne. gd%var(n)%type ) THEN
      type = gd%var(n)%type
      count = 1
    ENDIF
    
    IF ( gd%var(n)%dim .eq. 3 .or. gd%var(n)%dim .eq. 4 ) THEN
      gd%var(n)%basedim = 1
      
    ELSE
      gd%var(n)%basedim = 0
    ENDIF

    if (gd%var(n)%dim .eq. 0) then
     gd%var(n)%ng = 0 
    else
     gd%var(n)%ng = ng
    endif
    
    IF ( gd%var(n)%dim .ge. 3 ) THEN

     gd%var(n)%positions = ' '

    ENDIF

    gd%var(n)%index = count
    count = count + iadd

    IF( DEBUG .or. DEBUG_IO ) write(6,*) 'DEFINE_GRID_FROM_FILE:  VAR  = ', gd%var(n)%name, &
                                                                            gd%var(n)%type, &
                                                                            gd%var(n)%index
   ENDDO


  END SUBROUTINE GRID_DEFINE_FROM_FILE

! ######################################################################
! ######################################################################

SUBROUTINE DEFINE_NCDF_VAR(ncid,dim_id,var,dimvals)

#ifdef MPI
  USE mpi
#endif
  USE NETCDF 
  
  implicit none

  integer ncid, dim_id(0:8), dimvals(0:8), sizes(1:4)
  type(variable) :: var

  integer, allocatable :: dimids(:)
  integer dim, nid, status, i
  integer, allocatable :: chunksizes(:) , extend_increments(:)
  logical isinteger
  character(20) :: tmpstring

  INTEGER :: mpi_error_code

!-------------------------------------------------------------------------------
! Determine dims -> if dim == 0, write out the scalar 

  dim = var%dim + var%tdepend

  IF( dim == 0 ) THEN

    IF( DEBUG_IO ) write(0,*) 'DEFINE_NCDF_VAR 0D: var%name ',var%name, ncid, nid
    IF( var%type(1:5) == 'icnst' ) THEN
      status = NF90_DEF_VAR(ncid, var%name, NF90_INT, nid)
      isinteger = .true.
    ELSE
      status = NF90_DEF_VAR(ncid, var%name, NF90_FLOAT, nid)
      isinteger = .false.
    ENDIF
    IF( DEBUG_IO ) write(0,*) 'DEFINE_NCDF_VAR: var%name,isinteger ', var%name,isinteger

    IF(status /= NF90_NOERR) write(6,*) 'DEFINE_NCDF_VAR:  Error creating variable: ', var%name
    if (status /= nf90_noerr) call handle_err(status)

!-------------------------------------------------------------------------------
! Else we are writing out a 1, 2, 3D array - Use a trick about the type labeling to cut down
!      on the number of conditionals

  ELSE

    IF( DEBUG_IO ) write(0,*) 'DEFINE_NCDF_VAR 2: var%name, dim ',var%name, dim, var%type, var%tdepend
    allocate(dimids(dim))
    
    dimids(:) = 0

    IF( var%tdepend /= 0 ) dimids(dim) = dim_id(0)

    IF( var%type(1:1) == 'x' ) THEN               ! x1d set
      IF( var%istag == 1) THEN 
        dimids(1) = dim_id(2)
        sizes(1) = dimvals(2)
      ELSE
        dimids(1) = dim_id(1)
        sizes(1) = dimvals(1)
      ENDIF
    ENDIF

    IF( var%type(1:1) == 'y' ) THEN               ! y1d set
      IF( var%jstag == 1) THEN 
        dimids(1) = dim_id(4)
      ELSE
        dimids(1) = dim_id(3)
      ENDIF
    ENDIF

    IF( var%type(1:1) == 'z' ) THEN               ! z1d set
      IF( var%kstag == 1) THEN 
        dimids(1) = dim_id(6)
      ELSE
        dimids(1) = dim_id(5)
      ENDIF
    ENDIF

    IF( var%type(2:2) == 'y' ) THEN               ! xy2d set
      IF( var%jstag == 1) THEN 
        dimids(2) = dim_id(4)
        sizes(2) = dimvals(4)
      ELSE
        dimids(2) = dim_id(3)
        sizes(2) = dimvals(3)
      ENDIF
    ENDIF

    IF( var%type(2:2) == 'z' ) THEN               ! xz2d & yz2d set
      IF( var%kstag == 1) THEN 
        dimids(2) = dim_id(6)
      ELSE
        dimids(2) = dim_id(5)
      ENDIF
    ENDIF

    IF( var%type(2:2) == '2' ) THEN               ! x2d & y2d & z2d set
        dimids(2) = dimids(1)
        dimids(1) = dim_id(8)
    ENDIF

    IF( var%type(3:3) == 'z' ) THEN               ! xyz3d set
      IF( var%kstag == 1) THEN 
        dimids(3) = dim_id(6)
        sizes(3) = dimvals(6)
      ELSE
        dimids(3) = dim_id(5)
        sizes(3) = dimvals(5)
      ENDIF
    ENDIF

! Write out information

    IF( DEBUG_IO ) write(0,*) 'DEFINE_NCDF_VAR: var%name, dimids ',var%name, dimids

    IF( var%type(1:5) == 'icnst' ) THEN
      IF( DEBUG_IO ) write(0,*) 'DEFINE_NCDF_VAR INT 1: var%name ',var%name, ncid
      status = NF90_DEF_VAR(ncid, var%name, NF90_INT, dimids, nid)
      isinteger = .true.
      IF( DEBUG_IO ) write(0,*) 'DEFINE_NCDF_VAR INT: var%name, dimids, nid ',var%name, dimids, nid
#ifdef NC4
     IF ( netcdfversion .eq. 4 .and. parallelio_type /= 2) THEN
!        status = NF90_DEF_VAR_CHUNKING(ncid, nid, NF90_CHUNKED, (/ 300 /) )
!        if (status /= nf90_noerr) call handle_err(status)
     ENDIF
#endif
    ELSE
      status = NF90_DEF_VAR(ncid, var%name, NF90_real, dimids, nid)
      isinteger = .false.

#if defined (NC4)
      IF( netcdfversion .eq. 4 .and. var%type(1:5) == 'rcnst' ) THEN
        status = NF90_DEF_VAR_CHUNKING(ncid, nid, NF90_CHUNKED, (/ 300 /) )
        if (status /= nf90_noerr) call handle_err(status)
      ENDIF

     IF ( netcdfversion .eq. 4 .and. parallelio_type /= 2) THEN !{
        IF( DEBUG_IO ) write(0,*) 'DEFINE_NCDF_VAR NC4 setup: var%name ',var%name

      IF( var%type(3:3) == 'z' .or. var%type(1:2) == 'xy' ) THEN               ! xyz3d set
!      IF( var%type(3:3) == 'z' ) THEN               !{ xyz3d set
        allocate( chunksizes(4) )
!        allocate( extend_increments(4) ) 
        chunksizes(1:3) = sizes(1:3)
        
        IF ( var%type(3:3) == 'z' ) THEN
!        IF ( sizes(2) > 2 .and. nprock == 1 ) THEN ! not 2D and not tiled in vertical
        IF ( sizes(2) > 2 ) THEN ! not 2D and not tiled in vertical
          IF ( 4*(sizes(3)/4) == sizes(3) ) THEN
          chunksizes(3)   = sizes(3)/4
          ELSE
          chunksizes(3)   = sizes(3)/4 + 1
          ENDIF
        ENDIF
        ELSE
          chunksizes(3)   = 1
        ENDIF 
        chunksizes(4)   = 1
!        extend_increments(1:4) = chunksizes(1:4)
#ifdef MPI
        ! send chunk sizes from rank 0 so that all have the same value
         IF ( parallelio ) THEN
         CALL MPI_Bcast(chunksizes, 4, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_error_code)
         ENDIF
         
#endif
!         IF ( var%name == 'TH' ) THEN
!           write(0,*) 'rank, TH chunksizes: ',my_rank,chunksizes
!         ENDIF
         IF ( (.not. parallelio) .or. ( parallel_compress .and. var%type(3:3) == 'z'  ) ) THEN
!         IF ( (.not. parallelio) .or. ( parallel_compress .and. .false. ) ) THEN
         IF ( DEBUG_IO ) write(0,*) 'DEFINE_NCDF_VAR  chunksizes = ',chunksizes(:)
!        IF ( var%type(3:3) == 'z' ) THEN
        status = NF90_DEF_VAR_CHUNKING(ncid, nid, NF90_CHUNKED, chunksizes)
        IF (status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error setting chunking for: ', var%name
!        ENDIF
         ENDIF
!        deallocate( extend_increments ) 

!        IF ( (.not. parallelio) .or. ( var%name(1:1) == 'Q' .and. parallel_compress .and. var%type(3:3) == 'z') ) THEN   
        IF ( (.not. parallelio) .or. ( parallel_compress .and. var%type(3:3) == 'z') ) THEN   
!        IF ( (.not. parallelio) .or. ( parallel_compress .and. .false.) ) THEN   
         IF ( DEBUG_IO ) write(0,*) 'set deflate for ',var%name,var%type,shuffle, deflate, deflate_level
         status = NF90_DEF_VAR_DEFLATE(ncid, nid, shuffle, deflate, deflate_level)
!         write(0,*) 'deflate, deflate_level, shuffle = ',deflate, deflate_level, shuffle
         IF (status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error setting deflate for: ', var%name
        ENDIF

        deallocate( chunksizes )
        ENDIF!}
        
     ELSEIF ( netcdfversion .eq. 4 .and. parallelio .and. parallelio_type /= 2 ) THEN ! } {
        IF( DEBUG_IO ) write(0,*) 'DEFINE_NCDF_VAR NC4 parallel setup: var%name ',var%name
      IF( var%type(3:3) == 'z' ) THEN               ! xyz3d set
        allocate( chunksizes(4) )
!        allocate( extend_increments(4) ) 
       chunksizes(1:3) = sizes(1:3)
        IF ( sizes(2) > 2 ) THEN ! not 2D
          IF ( 4*(sizes(3)/4) == sizes(3) ) THEN
          chunksizes(3)   = sizes(3)/4
          ELSE
          chunksizes(3)   = sizes(3)/4 + 1
          ENDIF
        ENDIF
!        extend_increments(1:4) = chunksizes(1:4)

#ifdef MPI
        ! send chunk sizes from rank 0 so that all have the same value
         IF ( parallelio ) THEN
         CALL MPI_Bcast(chunksizes, 4, MPI_INTEGER, 0, MPI_COMM_WORLD, mpi_error_code)
         ENDIF
         
#endif
        
!        write(0,*) 'DEFINE_NCDF_VAR: sizes parallel = ',var%name,chunksizes(:)
!        write(0,*) 'DEFINE_NCDF_VAR: var%kstag = ',var%kstag,dimvals(5),dimvals(6)
        status = NF90_DEF_VAR_CHUNKING(ncid, nid, NF90_CHUNKED, chunksizes)
        IF (status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error setting chunking for: ', var%name
        deallocate( chunksizes )
!        deallocate( extend_increments ) 
      ENDIF

     ENDIF !}
#endif
    ENDIF

    IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable: ', var%name
    if (status /= nf90_noerr) call handle_err(status)

    deallocate(dimids)

  ENDIF

! DEFINE ATTRIBUTES

  IF ( write_attributes ) THEN
  
  status = NF90_PUT_ATT(ncid, nid, "type", var%type)
  IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute long_name: ', var%name

  status = NF90_PUT_ATT(ncid, nid, "long_name", var%description)
  IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute long_name: ', var%name

  status = NF90_PUT_ATT(ncid, nid, "units",     var%unit)
  IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute units:     ', var%name

!  IF ( .not. ( parallelio .and. parallelio_type == 1 )  ) THEN

  status = NF90_PUT_ATT(ncid, nid, "index",     var%index)
  IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute index:     ', var%name

!  ENDIF


  status = NF90_PUT_ATT(ncid, nid, "posdef",    var%pdef)
  IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute posdef:    ', var%name

  status = NF90_PUT_ATT(ncid, nid, "istag",      var%istag)
  IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute istag:     ', var%name

  status = NF90_PUT_ATT(ncid, nid, "jstag",      var%jstag)
  IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute jstag:     ', var%name

  status = NF90_PUT_ATT(ncid, nid, "kstag",      var%kstag)
  IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute kstag:     ', var%name

  status = NF90_PUT_ATT(ncid, nid, "dyntype",   var%dyntype)
  IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute dyntype:   ', var%name

  status = NF90_PUT_ATT(ncid, nid, "phytype",   var%phytype)
  IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute phytype:   ', var%name

  status = NF90_PUT_ATT(ncid, nid, "buotype",   var%buotype)
  IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute buotype:   ', var%name

  status = NF90_PUT_ATT(ncid, nid, "basedim",   var%basedim)
  IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute basedim:   ', var%name

  IF ( var%field .ne. CHAR(0) ) THEN
    status = NF90_PUT_ATT(ncid, nid, "field",   var%field)
    IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute field:   ', var%name
  ENDIF
  
  IF ( var%positions .ne. CHAR(0) ) THEN
    status = NF90_PUT_ATT(ncid, nid, "positions",   var%positions)
    IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute positions:   ', var%name
  ENDIF

!  ENDIF ! true/false
  
! for CF conventions and VAPOR import
  IF ( var%name == 'XC' .or. var%name == 'XE' ) THEN
    status = NF90_PUT_ATT(ncid, nid, "axis",   'X')
    IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute axis:   ', var%name
  ENDIF

  IF ( var%name == 'YC' .or. var%name == 'YE' ) THEN
    status = NF90_PUT_ATT(ncid, nid, "axis",   'Y')
    IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute axis:   ', var%name
  ENDIF

  IF ( var%name == 'ZC' .or. var%name == 'ZE' ) THEN
    status = NF90_PUT_ATT(ncid, nid, "axis",   'Z')
    IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute axis:   ', var%name
  ENDIF
  
  ENDIF ! write_attributes

  IF ( historyoutput == -1 .and. var%dim .gt. 0 ) THEN
  IF ( isinteger ) THEN
    status = NF90_PUT_ATT(ncid, nid, "_FillValue",   0)
  ELSE
    status = NF90_PUT_ATT(ncid, nid, "_FillValue",   0.0)
  ENDIF
  IF(status /= NF90_NOERR) print *,'DEFINE_NCDF_VAR:  Error creating variable attribute _FillValue:   ', var%name
  ENDIF
  

END SUBROUTINE DEFINE_NCDF_VAR

! ################################################

! ################################################
!     This subroutine handles errors by printing an error message and
!     exiting with a non-zero status.
! ################################################
  subroutine handle_err(errcode)
!    use netcdf
    implicit none
    integer, intent(in) :: errcode
    
    if(errcode /= nf90_noerr) then
       print *, 'Error: ', trim(nf90_strerror(errcode))
!       CALL commasmpi_abort()
       stop "Stopped"
    endif
  end subroutine handle_err

! ################################################
! ################################################

END MODULE GRIDIO_MODULE




program commas_netcdf
  use netcdf
  use mpi
  use grid_module
  use gridio_module
  use file_module
  implicit none

  character (len = *), parameter :: FILE_NAME = "nctest.nc"
  integer, parameter :: NDIMS = 3
  integer :: nprocx = 2, nprocy = 2
  integer :: nproc
  integer :: NX = 20, NY =  20, NZ = 40, NZE
  integer :: nxend, nyend, nzend
!  integer, parameter :: nxend = 60, nyend = 60, nzend = 50
!  integer, parameter :: NX = nxend/nprocx, NY = nyend/nprocx, NZ = nzend
  integer, parameter :: numvar = 450
  integer :: iproc, jproc
  integer :: ncid, nid, varid, dimids(NDIMS+1), axisid(7) ! varid2,varid3,varid4,varid5,varid6
  integer :: varids(numvar)
  character(len=40) :: varnames(numvar)
  character(len=3) :: numstr
  integer :: x_dimid, y_dimid, z_dimid, time_dimid
  integer :: xe_dimid, ye_dimid, ze_dimid
  real,allocatable :: data_out(:,:,:), data_out2(:,:,:)
  integer :: chunks(ndims+1)
!  integer :: deflate_level
  integer :: x, y, i,j,k,n, time,ntime
  integer cmode, status
!  integer, parameter :: nf90_iotype = NF90_MPIIO !  NF90_MPIPOSIX
!  integer, parameter :: pnetcdf_flag = NF90_PNETCDF
!  integer, parameter :: INDCOL = NF90_COLLECTIVE
!  logical, parameter :: DEBUG_IO = .false.
!  integer :: parallelio_type = 1
  INTEGER :: my_comm, my_info
  integer p, my_rank, ierr
  integer start(NDIMS+1), count(NDIMS+1)
!  real :: xc(nxend), yc(nyend), zc(nzend), ze(nzend+1)
  real,allocatable :: xc(:), xe(:), yc(:), ye(:), zc(:), ze(:)
  real :: dx = 500., dy = 500, dz = 400.
  integer, parameter :: ivardef = 2
  integer :: NXE, NYE
  integer dim_id(0:8),dimvals(0:8)
  integer istag,jstag,kstag
  integer itile,jtile,ktile, nproci, nprocj, nprock
  integer :: mpi_error_code

  TYPE(GRID) :: gd
!  character(100) :: regfile = 'registry.tak' ! fail
!  character(100) :: regfile = 'registry.tak.3donly' ! success
!  character(100) :: regfile = 'registry.tak.0d3d' ! fail
!  character(100) :: regfile = 'registry.tak.1d2d3d' ! fail
!  character(100) :: regfile = 'registry.tak.0d1d3d' ! fail
!  character(100) :: regfile = 'registry.take.3d' ! success
!  character(100) :: regfile = 'registry.take.0d3d' ! fails if any 0d var is time dependent
!  character(100) :: regfile = 'registry.take.2d3d' ! success, even if any 2d var is NOT time dependent
  character(100) :: regfile = 'registry.take.2d3d' ! 
  integer        :: istat

  namelist /params/ &
      nprocx, nprocy, &
      nx, ny,         &
      regfile,        &
      parallel_compress, &
      parallelio_type,  &
      write_attributes

      call MPI_Init(ierr)
      call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
      call MPI_Comm_size(MPI_COMM_WORLD, p, ierr)

          my_comm = MPI_COMM_WORLD
          my_info = MPI_INFO_NULL


      open(15,file='namelist.input',status='old',form='formatted',action='read')
      rewind(15)
      read(15,NML=params,iostat=istat)
      IF ( istat /= 0 ) THEN
        write(0,*) 'PROBLEM WITH PARAMS namelist: not found or bad token'
      ELSE
        IF ( my_rank == 0 ) THEN
          write(6, nml=params)
        ENDIF
      ENDIF
      close(15)

    
    nproc = nprocx*nprocy
    NZE = nz + 1
    nxend = NX*nprocx+1
    nyend = NY*nprocy+1
    nzend = NZ+1
    
!     There must be 4 procs for this test.
      if (p .ne. nproc) then
         print *, 'This test program must be run on ',nproc,' processors.'
         stop 2
      endif
      
!   parallelio_type = 1
  ! get position in grid
  ! 15 16 17 18 19 
  ! 10 11 12 13 14
  ! 5  6  7  8  9
  ! 0  1  2  3  4

  
   DO j = 0,nprocy-1
   DO i = 0,nprocx-1
    n = nprocx*j + i
    IF ( my_rank == n ) THEN
      iproc = i
      jproc = j
    ENDIF
   ENDDO
   ENDDO
   
   itile = nx
   jtile = ny
   ktile = nz
   nproci = nprocx
   nprocj = nprocy
   nprock = 1
   parallel_compress = .false.
   parallelio = .true.
   
   allocate( xc(nxend) )
   allocate( xe(nxend) )
   allocate( yc(nyend) )
   allocate( ye(nyend) )
   allocate( zc(nzend) )
   allocate( ze(nzend) )
   
   IF ( iproc == nprocx - 1 ) THEN
     nxe = nx + 1
   ELSE
     nxe = nx
   ENDIF

   IF ( jproc == nprocy - 1 ) THEN
     nye = ny + 1
   ELSE
     nye = ny
   ENDIF

   call grid_define_from_file(gd, regfile)
   
  ! write(0,*) 'my_rank,iproc,jproc = ',my_rank,iproc,jproc
    
    
  ! Create some pretend data. If this wasn't an example program, we
  ! would have some real data to write, for example, model output.
!  do k = 1, nz
!  do j = 1, NY
!     do i = 1, NX
!        data_out(i, j, k) = (my_rank+1)*( (j - 1) * NY + (i - 1) )
!     end do
!  end do
!  end do
  
  DO i = 1,nxend
    xc(i) = (i - 0.5)*dx
  ENDDO
  DO j = 1,nyend
    yc(j) = (j - 0.5)*dy
  ENDDO
  DO k = 1,nzend
    zc(k) = (k - 0.5)*dz
  ENDDO

  DO i = 1,nxend
    xe(i) = (i - 1)*dx
  ENDDO
  DO j = 1,nyend
    ye(j) = (j - 1)*dy
  ENDDO
  DO k = 1,nzend
    ze(k) = (k - 1)*dz
  ENDDO
  
     pnetcdf_flag = nf90_64bit_offset

   IF ( my_rank == 0 ) write(0,*) 'create file'
   
   IF ( parallelio_type == 1 ) THEN ! HDF5 parallel
     cmode  = ior(NF90_NETCDF4,nf90_iotype)
    cmode = IOR(nf90_netcdf4, nf90_classic_model) 
    cmode = IOR(cmode, nf90_mpiio) 
    netcdfversion = 4
   ELSEIF ( parallelio_type == 2 ) THEN ! pnetcdf
     cmode  = ior(ior(pnetcdf_flag,NF90_MPIIO),nf90_64bit_offset)
     netcdfversion = 3
   ENDIF
!       cmode = ior(IOR(nf90_CLOBBER, nf90_PNETCDF)

  ! Always check the return code of every netCDF function call. In
  ! this example program, wrapping netCDF calls with "call check()"
  ! makes sure that any return which is not equal to nf90_noerr (0)
  ! will print a netCDF error message and exit.

  ! Create the netCDF file. The nf90_clobber parameter tells netCDF to
  ! overwrite this file, if it already exists.
!  call check( nf90_create(FILE_NAME, nf90_netcdf4, ncid) )
   IF( parallelio_type == 1 ) THEN
     IF ( DEBUG_IO ) write(0,*) 'hdf5 create, rank = ',my_rank
     status = nf90_create(FILE_NAME, cmode, ncid, comm = my_comm, info = my_info)
   ELSE
     IF ( DEBUG_IO ) write(0,*) 'pnetcdf create, rank = ',my_rank
    status = nf90_create(FILE_NAME, cmode, ncid, comm = my_comm, info = my_info)
!    status = NF90_CREATE_PAR(filename(ibeg:iend),cmode,my_comm,my_info,ncid)
   ENDIF
   call check(status)

!   IF ( my_rank == 0 ) write(0,*) 'define dims'

  
  ! Define the dimensions. NetCDF will hand back an ID for each. 
!  status     = NF90_DEF_DIM(ncid, 'TIME', NF90_UNLIMITED, time_dimid)
  ntime = 1
!  status     = NF90_DEF_DIM(ncid, 'TIME', ntime, time_dimid)
!  IF(status /= NF90_NOERR) print *,'GRID_DEFINE_NETCDF:  Error unlimited dimension'

  status     = NF90_DEF_DIM(ncid, 'TIME', NF90_UNLIMITED, dim_id(0))
  IF(status /= NF90_NOERR) print *,'GRID_DEFINE_NETCDF:  Error unlimited dimension'

!  call check( nf90_def_dim(ncid, "XC", NXend-1, x_dimid) )
!  call check( nf90_def_dim(ncid, "YC", NYend-1, y_dimid) )
!  call check( nf90_def_dim(ncid, "ZC", NZend-1, z_dimid) )

!  call check( nf90_def_dim(ncid, "XE", NXend, xe_dimid) )
!  call check( nf90_def_dim(ncid, "YE", NYend, ye_dimid) )
!  call check( nf90_def_dim(ncid, "ZE", NZend, ze_dimid) )

  status     = NF90_DEF_DIM(ncid, 'XC', nxend-1, dim_id(1))
  IF(status /= NF90_NOERR) print *,'GRID_DEFINE_NETCDF:  Error defining nxend-1'
  status     = NF90_DEF_DIM(ncid, 'XE', nxend,   dim_id(2))
  IF(status /= NF90_NOERR) print *,'GRID_DEFINE_NETCDF:  Error defining nxend'
  
  status     = NF90_DEF_DIM(ncid, 'YC', nyend-1, dim_id(3))
  IF(status /= NF90_NOERR) print *,'GRID_DEFINE_NETCDF:  Error defining nyend-1'
  status     = NF90_DEF_DIM(ncid, 'YE', nyend,   dim_id(4))
  IF(status /= NF90_NOERR) print *,'GRID_DEFINE_NETCDF:  Error defining nyend'
  
  status     = NF90_DEF_DIM(ncid, 'ZC', nzend-1, dim_id(5))
  IF(status /= NF90_NOERR) print *,'GRID_DEFINE_NETCDF:  Error defining nzend-1'
  status     = NF90_DEF_DIM(ncid, 'ZE', nzend,   dim_id(6))
  IF(status /= NF90_NOERR) print *,'GRID_DEFINE_NETCDF:  Error defining nzend'

  IF ( nproci > 1 ) THEN
  dimvals(1) = itile
  ELSE
  dimvals(1) = itile
  ENDIF
  dimvals(2) = itile

  IF ( nprocj > 1 ) THEN
  dimvals(3) = jtile
  ELSE
  dimvals(3) = jtile
  ENDIF
  dimvals(4) = jtile

  IF ( nprock > 1 ) THEN
  dimvals(5) = ktile
  ELSE
  dimvals(5) = ktile-1
  ENDIF
  dimvals(6) = ktile
  

!   IF ( .false. ) THEN
!   call check( nf90_def_var(ncid, 'XC', NF90_real, (/dim_id(1)/), nid) )
!     status = NF90_PUT_ATT(ncid, nid, "units",   'meters')
!     status = NF90_PUT_ATT(ncid, nid, "axis",   'X')
!     axisid(1) = nid
!   call check( nf90_def_var(ncid, 'YC', NF90_real, (/dim_id(3)/), nid) )
!     status = NF90_PUT_ATT(ncid, nid, "units",   'meters')
!     status = NF90_PUT_ATT(ncid, nid, "axis",   'Y')
!     axisid(2) = nid
!     
!   call check( nf90_def_var(ncid, 'ZC', NF90_real, (/dim_id(5)/), nid) )
!     status = NF90_PUT_ATT(ncid, nid, "units",   'meters')
!     status = NF90_PUT_ATT(ncid, nid, "axis",   'Z')
!     axisid(3) = nid
! 
!   call check( nf90_def_var(ncid, 'XE', NF90_real, (/dim_id(2)/), nid) )
!     status = NF90_PUT_ATT(ncid, nid, "units",   'meters')
!     status = NF90_PUT_ATT(ncid, nid, "axis",   'X')
!     axisid(4) = nid
!   call check( nf90_def_var(ncid, 'YE', NF90_real, (/dim_id(4)/), nid) )
!     status = NF90_PUT_ATT(ncid, nid, "units",   'meters')
!     status = NF90_PUT_ATT(ncid, nid, "axis",   'Y')
!     axisid(5) = nid
!     
!   call check( nf90_def_var(ncid, 'ZE', NF90_real, (/dim_id(6)/), nid) )
!     status = NF90_PUT_ATT(ncid, nid, "units",   'meters')
!     status = NF90_PUT_ATT(ncid, nid, "axis",   'Z')
!     axisid(6) = nid
! 
!   call check( nf90_def_var(ncid, 'TIME', NF90_real, (/ dim_id(0) /), nid) )
!     status = NF90_PUT_ATT(ncid, nid, "units",   'seconds')
!     axisid(7) = nid
!     
!   ENDIF

  ! The dimids array is used to pass the IDs of the dimensions of
  ! the variables. Note that in fortran arrays are stored in
  ! column-major format.
  ! Define the variable. The type of the variable in this case is
  ! NF90_INT (4-byte integer). Optional parameters chunking, shuffle,
  ! and deflate_level are used.
  chunks(1) = Nx
  chunks(2) = Ny
  chunks(3) = nz/4
  chunks(4) = 1
  deflate_level = 2
  
! 
!   DO n = 1,numvar
!   
!   
!   
!   istag = 0
!   jstag = 0
!   kstag = 0
!   IF ( n > 3 ) THEN
!   dimids =  (/ dim_id(1), dim_id(3), dim_id(5), dim_id(0) /)
!   ELSEIF ( n == 1 ) THEN ! u
!   istag = 1
!   dimids =  (/ dim_id(2), dim_id(3), dim_id(5), dim_id(0) /)
!   ELSEIF ( n == 2 ) THEN ! v
!   jstag = 1
!   dimids =  (/ dim_id(1), dim_id(4), dim_id(5), dim_id(0) /)
!   ELSEIF ( n == 3 ) THEN ! w
!   kstag = 1
!   dimids =  (/ dim_id(1), dim_id(3), dim_id(6), dim_id(0) /)
!   ENDIF
!   
!  
!   
!   
!   
!   write(numstr,'(i3.3)') n
!   varnames(n) = 'data'//numstr(1:3)
!   IF ( ivardef == 1 ) then
!   call check( nf90_def_var(ncid, varnames(n), NF90_real, dimids, varids(n), &
!        chunksizes = chunks, shuffle = .TRUE., deflate_level = deflate_level) )
!   ELSE
!   
!    call check( nf90_def_var(ncid, varnames(n), NF90_real, dimids, varids(n)) )
!    call check( NF90_DEF_VAR_CHUNKING(ncid, varids(n), NF90_CHUNKED, chunks) )
!    call check( NF90_DEF_VAR_DEFLATE(ncid, varids(n),1, 1, deflate_level) )
!   
!   ENDIF
!     status = NF90_PUT_ATT(ncid, varids(n), "units",   'count')
!     status = NF90_PUT_ATT(ncid, varids(n), "index",     n)
!     i = -1
!     status = NF90_PUT_ATT(ncid, varids(n), "posdef",     i)
!     i = 0
!     status = NF90_PUT_ATT(ncid, varids(n), "istag",     istag)
!     i = 0
!     status = NF90_PUT_ATT(ncid, varids(n), "jstag",     jstag)
!     i = 0
!     status = NF90_PUT_ATT(ncid, varids(n), "kstag",     kstag)
!    ENDDO


  DO n = 1,size(gd%var)
  
  IF( DEBUG_IO ) write(0,*) 'GRID_DEFINE_NETCDF: DEFINING VARIABLE ', gd%var(n)%name,n,size(gd%var), my_rank
  
#ifdef MPI
    CALL MPI_BARRIER(my_comm, mpi_error_code) ! nf90_PNETCDF
#endif
  
  CALL DEFINE_NCDF_VAR(ncid,dim_id,gd%var(n),dimvals)
  
#ifdef MPI
    IF( DEBUG_IO .and. .false.) THEN
    write(0,*) 'GRID_DEFINE_NETCDF: AFTER DEFINING VARIABLE ', gd%var(n)%name, my_rank
!    CALL MPI_BARRIER(my_comm, mpi_error_code) ! nf90_PNETCDF
  status = NF90_ENDDEF(NCID)
  IF(status /= NF90_NOERR) THEN
   write(0,*) 'DEFINE_NETCDF:  Error ending define mode 1,rank', status, NF90_STRERROR(status), my_rank
   call commasmpi_abort()
  ENDIF
    status = NF90_REDEF(NCID)
  IF(status /= NF90_NOERR) write(0,*) 'DEFINE_NETCDF:  Error entering redefine mode 1,rank', status, NF90_STRERROR(status), my_rank
   ELSEIF( DEBUG_IO) THEN
    write(0,*) 'GRID_DEFINE_NETCDF: AFTER DEFINING VARIABLE ', gd%var(n)%name, my_rank
   ENDIF
#endif

  ENDDO

   
  ! End define mode. This tells netCDF we are done defining metadata.
   IF( DEBUG_IO) THEN
    write(0,*) 'GRID_DEFINE_NETCDF: enddef '
   ENDIF
  call check( nf90_enddef(ncid) )
   IF( DEBUG_IO) THEN
    write(0,*) 'GRID_DEFINE_NETCDF: enddef 2'
   ENDIF


! Testing code...
!    write(0,*) 'close file after define ',my_rank
!    status=NF90_CLOSE(NCID)
    
!    CALL MPI_BARRIER(my_comm, ierr)
    
!    write(0,*) 'open file after define ',my_rank
!     cmode  = ior(NF90_WRITE,nf90_iotype)
!    status = NF90_OPEN_PAR(file_name,cmode,my_comm,my_info,ncid)

  ! Write the pretend data to the file. Although netCDF supports
  ! reading and writing subsets of data, in this case we write all the
  ! data in one operation.

!   time = 0
!   status = NF90_VAR_PAR_ACCESS(ncid,axisid(7),INDCOL)
!   call check( nf90_put_var(ncid, axisid(7), time, (/1/) )) !, count=(/1/) ) )
!   status = NF90_VAR_PAR_ACCESS(ncid,axisid(1),INDCOL)
!   call check( nf90_put_var(ncid, axisid(1), xc, start=(/1/), count=(/nxend-1/) ) )
!   status = NF90_VAR_PAR_ACCESS(ncid,axisid(2),INDCOL)
!   call check( nf90_put_var(ncid, axisid(2), yc, start=(/1/), count=(/nyend-1/) ) )
!   status = NF90_VAR_PAR_ACCESS(ncid,axisid(3),INDCOL)
!   call check( nf90_put_var(ncid, axisid(3), zc, start=(/1/), count=(/nzend-1/) ) )
! 
!   status = NF90_VAR_PAR_ACCESS(ncid,axisid(4),INDCOL)
!   call check( nf90_put_var(ncid, axisid(4), xe, start=(/1/), count=(/nxend/) ) )
!   status = NF90_VAR_PAR_ACCESS(ncid,axisid(5),INDCOL)
!   call check( nf90_put_var(ncid, axisid(5), ye, start=(/1/), count=(/nyend/) ) )
!   status = NF90_VAR_PAR_ACCESS(ncid,axisid(6),INDCOL)
!   call check( nf90_put_var(ncid, axisid(6), ze, start=(/1/), count=(/nzend/) ) )

!     Determine what part of the variable will be written for this
!     processor. It's a checkerboard decomposition.
      count(1) = NX
      count(2) = NY
      count(3) = NZ
      count(4) = 1
      start(3) = 1
      start(4) = 1
      if (my_rank .eq. 0) then
         start(1) = 1
         start(2) = 1
      else if (my_rank .eq. 1) then
         start(1) = NX + 1
         start(2) = 1
      else if (my_rank .eq. 2) then
         start(1) = 1
         start(2) = NY + 1
      else if (my_rank .eq. 3) then
         start(1) = NX + 1
         start(2) = NY + 1
      endif
      
      start(1) = NX*iproc + 1
      start(2) = NY*jproc + 1
      
      

!   IF ( .false. ) THEN
!   DO n = 1,numvar
! 
!   istag = 0
!   jstag = 0
!   kstag = 0
!   IF ( n > 3 ) THEN
! !  dimids =  (/ dim_id(1), dim_id(3), dim_id(5), dim_id(0) /)
!   ELSEIF ( n == 1 .and. iproc == nprocx - 1  ) THEN ! u
!   istag = 1
! !  dimids =  (/ dim_id(2), dim_id(3), dim_id(5), dim_id(0) /)
!   ELSEIF ( n == 2 .and. jproc == nprocy - 1 ) THEN ! v
!   jstag = 1
! !  dimids =  (/ dim_id(1), dim_id(4), dim_id(5), dim_id(0) /)
!   ELSEIF ( n == 3 ) THEN ! w
!   kstag = 1
! !  dimids =  (/ dim_id(1), dim_id(3), dim_id(6), dim_id(0) /)
!   ENDIF
!   
!       count(1) = NX+istag
!       count(2) = NY+jstag
!       count(3) = NZ+kstag
! 
!  ! write(0,*) 'write: n,count = ',n,count(1),count(2),count(3)
!   allocate( data_out2(count(1),count(2),count(3)) )
!   do k = 1, nz+kstag
!   do j = 1, NY+jstag
!      do i = 1, NX+istag
!         data_out2(i, j, k) = (float(n)/float(numvar))*(my_rank+1)*( (j - 1) * (NY+jstag) + (i - 1) )
!      end do
!   end do
!   end do
! 
!    status = NF90_VAR_PAR_ACCESS(ncid,varids(n),INDCOL)
! 
!   call check( nf90_put_var(ncid, varids(n), data_out2, start=start, count=count) )
!   
!   deallocate( data_out2 )
!   
!   ENDDO
!   ENDIF 
  
  ! Close the file. This frees up any internal netCDF resources
  ! associated with the file, and flushes any buffers.
  call check( nf90_close(ncid) )

  print *, '*** SUCCESS writing example file ', FILE_NAME, '!'

  call MPI_Finalize(ierr)

contains
  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop 2
    end if
  end subroutine check  
end program commas_netcdf

