!===============================================================================
! Part of f90rad3, the Fortran module for dealing with GPR data files in the
! RAD3 format.
!
! Andy Nowacki <andy.nowacki@bristol.ac.uk>
!
! See the file LICENCE for licence details.
!===============================================================================

module f90rad3

   use, intrinsic :: iso_c_binding, only: C_SHORT

   implicit none

   ! Precision selectors
   integer, parameter, private :: int4 = selected_int_kind(9)
   integer, parameter, private :: sp = selected_real_kind(6,37)
   integer, parameter, private :: dp = selected_real_kind(15,307)
   integer, parameter, private :: rs = sp
   integer, parameter, private :: rad3int = C_SHORT ! Traces are stored as shorts

   ! Length of character strings
   integer, parameter, private :: RAD3_FILENAME_LEN = 250, &
                                  RAD3_CHAR_LEN = 80, &
                                  RAD3_COMMENT_LEN = 250
   ! Other private parameters used
   integer, parameter, private :: RAD3_RAD_FILE_LEN = 38 ! No lines in .rad file

   ! Logical units for I/O
   integer, parameter, private :: ISTDERR = 0, ISTDIN = 5, ISTDOUT = 6, &
                                  IIN = 10, IOUT = 11

   ! Private variables modified by routines
   logical, private :: verbose = .false.

   ! Helper routines not exposed
   private :: rad3_error, &
              rad3_warning, &
              rad3_allocate, &
              rad3_verbose, &
              rad3_check_file_name, &
              check_ncf

   ! Type defining a section (collection of traces)
   type rad3trace
      character(len=RAD3_FILENAME_LEN) :: file
      real(rs) :: delta ! Sampling frequency
      ! Header part; not all fields needed
      integer :: n  ! number of samples
      real(rs) :: frequency
      integer :: frequency_steps
      real(rs) :: signal_position
      integer :: raw_signal_position
      integer :: distance_flag
      integer :: time_flag
      integer :: program_flag
      integer :: external_flag
      real(rs) :: time_interval
      real(rs) :: distance_interval
      character(len=RAD3_CHAR_LEN) :: operator_name
      character(len=RAD3_CHAR_LEN) :: customer_name
      character(len=RAD3_CHAR_LEN) :: site_name
      character(len=RAD3_CHAR_LEN) :: antennas
      character(len=RAD3_CHAR_LEN) :: antenna_orientation
      real(rs) :: antenna_separation
      character(len=RAD3_COMMENT_LEN) :: comment
      real(rs) :: timewindow
      integer :: stacks
      integer :: stack_exponent
      real(rs) :: stacking_time
      integer :: last_trace
      real(rs) :: stop_position
      real(rs) :: system_calibration
      real(rs) :: start_position
      integer :: short_flag
      integer :: intermediate_flag
      integer :: long_flag
      integer :: preprocessing
      integer :: high
      integer :: low
      real(rs) :: fixed_increment
      integer :: fixed_moves_up
      integer :: fixed_moves_down
      real(rs) :: fixed_position
      real(rs) :: wheel_calibration
      integer :: positive_direction
      ! Trace/Section part.  First dimension is point, second trace number
      real(rs), dimension(:,:), allocatable :: tr
      real(rs), dimension(:), allocatable :: x    ! Shot offset
      real(rs), dimension(:), allocatable :: twtt ! Travel times
   end type rad3trace

   ! Type defining a collection of traces which form a 3D volume
   type rad3volume
      real(rs) :: dx, dy, dt
      integer :: nx, ny, nt
      real(rs), dimension(:), allocatable :: x, y, twtt ! x along trace
      real(rs), dimension(:,:,:), allocatable :: amp
   end type rad3volume

contains

!===============================================================================
subroutine rad3_load(file, tr)
!===============================================================================
! Load a .rd3 file.  Supply the filename without the extension.  A .rad file with
! the same name must be in the same directory as the filename supplied.
   character(len=*), intent(in) :: file
   type(rad3trace), intent(inout) :: tr
   integer(rad3int), dimension(:), allocatable :: temp_trace
   logical :: exists
   integer :: i

   call rad3_check_file_name(file)
   ! Get header information
   call rad3_read_rad_file(trim(file)//'.rad', tr)
   ! Allocate memory for section
   if (allocated(tr%tr)) deallocate(tr%tr)
   allocate(tr%tr(tr%n, tr%last_trace))
   allocate(temp_trace(tr%n))
   ! Check file exists
   inquire(file=trim(file)//'.rd3', exist=exists)
   if (.not.exists) &
      call rad3_error('rad3_load: File "'//trim(file)//'.rad3" does not exist')
   open(IIN, file=trim(file)//'.rd3', access='direct', recl=rad3int*tr%n)
   do i = 1, tr%last_trace
      read(IIN, rec=i) temp_trace
      tr%tr(:,i) = real(temp_trace, kind=rs)
   enddo
   close(IIN)
   deallocate(temp_trace)
end subroutine rad3_load
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_load_volume(infile, vol)
!===============================================================================
! Read in a collection of traces forming a volume.  They must be equally spaced
! in the dimension perpendicular to the x orientation.
! Supply:
!  file:  Name of file containing on each line:
!     filename, y_offset, x_offset
   type(rad3volume), intent(inout) :: vol
   character(len=*), intent(in) :: infile
   integer :: nfiles
   character(len=RAD3_FILENAME_LEN), dimension(:), allocatable :: files
   real(rs) :: xmin, xmax
   real(rs), dimension(:), allocatable :: x0

   call rad3_delete_volume(vol)
   call read_input
   call load_files

   deallocate(x0, files)

contains
   subroutine read_input()
      integer :: ifile, iostat
      character(len=250) :: arg
      logical :: exists
      inquire(file=infile, exist=exists)
      if (.not.exists) &
         call rad3_error('rad3_load_volume: File "'//trim(infile)//'" does not exist')
      open(IIN, file=infile)
      ! Count number of files/traces
      ifile = 0
      do
         if (verbose) write(ISTDERR,'(a,i0)') 'rad3_load_volume: Reading line ',ifile+1
         read(IIN,'(a)',iostat=iostat) arg
         if (iostat < 0) then
            nfiles = ifile
            vol % ny = nfiles
            if (verbose) write(ISTDERR,'(a,i0)') 'rad3_load_volume: Total traces = ',nfiles
            exit
         endif
         if (iostat > 0) &
            call rad3_error('rad3_load_volume: Problem reading file "'//trim(infile)//'"')
         ifile = ifile + 1
      enddo
      allocate(files(nfiles))
      allocate(vol % y(nfiles))
      allocate(x0(nfiles))
      ! Read parameters for each file
      rewind(IIN)
      do ifile = 1, nfiles
         if (verbose) write(ISTDERR,'(a,i0)') 'rad3_load_volume: Reading data from line ',ifile
         read(IIN,*,iostat=iostat) files(ifile), vol%y(ifile), x0(ifile)
         if (iostat /= 0) &
            call rad3_error('rad3_load_volume: Problem getting values from file "'//trim(infile)//'"')
      enddo
      vol % dy = abs(vol%y(2) - vol%y(1))
      close(IIN)
   end subroutine read_input

   subroutine load_files()
      type(rad3trace) :: tr
      integer :: i, ix1, ix2
      ! Get box dimensions
      do i = 1, nfiles
         call rad3_read_rad_file(trim(files(i))//'.rad', tr)
         if (i == 1) then
            xmin = x0(i)
            xmax = x0(i) + tr%x(tr%last_trace)
            vol % dx = tr%distance_interval
            vol % dt = tr%delta
            vol % nt = tr%n
            allocate(vol%twtt(vol%nt))
            vol%twtt = tr%twtt
            if (verbose) write(ISTDERR,'(a,2(1x,f0.6))') 'rad3_load_volume: xmin,xmax =',xmin,xmax
         endif
         if (x0(i) + tr%x(tr%last_trace) > xmax) &
            xmax = x0(i) + tr%x(tr%last_trace)
         if (x0(i) < xmin) xmin = x0(i)
         call rad3_delete(tr)
      enddo
      vol % nx = nint((xmax - xmin)/tr%distance_interval) + 1
      if (verbose) write(ISTDERR,'(a,3(1x,i0))') 'rad3_load_volume: nx,ny,nt =',vol%nx,vol%ny,vol%nt
      allocate(vol%x(vol%nx))
      ! Fill in array of x values
      do i = 1, vol%nx
         vol % x(i) = xmin + real(i-1)*vol%dx
      enddo
      allocate(vol%amp(vol%nx,vol%ny,vol%nt))
      vol % amp = 0.
      ! Read traces into correct position
      do i = 1, nfiles
         call rad3_load(files(i), tr)
         ix1 = nint((x0(i) - xmin)/tr%distance_interval) + 1
         ix2 = ix1 + tr%last_trace - 1
         vol%amp(ix1:ix2,i,:) = transpose(tr%tr(:,1:tr%last_trace))
         call rad3_delete(tr)
      enddo
   end subroutine load_files

end subroutine rad3_load_volume
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_save(tr, file)
!===============================================================================
! Save a trace as a RAD3-format file, including the header file.
! file should be the name of the file without the suffix.
   type(rad3trace), intent(in) :: tr
   character(len=*), intent(in) :: file
   integer :: i
   real(rs) :: scale
   call rad3_check_file_name(file)
   ! Normalise trace to fit into short int
   scale = real(huge(1_rad3int))/maxval(abs(tr%tr))
   ! Save binary part
   open(IOUT, file=trim(file)//'.rd3', access='direct', recl=rad3int*tr%n)
   do i = 1, tr%last_trace
      write(IOUT, rec=i) int(tr%tr(:,i)*scale, kind=rad3int)
   enddo
   close(IOUT)
   ! Save ASCII header
   call rad3_write_rad_file(tr, file)
end subroutine rad3_save
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_save_netcdf(tr, file, history)
!===============================================================================
! Save a trace to a NetCDF file
! Optionally supply a string (history) which details the command used to produce
! the NetCDF file (e.g., what filtering used)
   use netcdf, only: nf90_create, nf90_def_dim, nf90_def_var, nf90_enddef, &
      nf90_put_var, nf90_close, NF90_CLOBBER, NF90_FLOAT, nf90_noerr, nf90_strerror, &
      NF90_GLOBAL, nf90_put_att
   type(rad3trace), intent(in) :: tr
   character(len=*), intent(in) :: file
   character(len=*), intent(in), optional :: history
   integer :: ncid, x_dimid, twtt_dimid, x_varid, twtt_varid, tr_varid

   call rad3_check_exists(tr)

   ! Create file
   call check_ncf(nf90_create(trim(file), NF90_CLOBBER, ncid))

   ! Define dimensions
   call check_ncf(nf90_def_dim(ncid, 'x',    tr%last_trace, x_dimid))
   call check_ncf(nf90_def_dim(ncid, 'twtt', tr%n,          twtt_dimid))
   ! Set variables for coordinates
   call check_ncf(nf90_def_var(ncid, 'x',    NF90_FLOAT, x_dimid,    x_varid))
   call check_ncf(nf90_def_var(ncid, 'twtt', NF90_FLOAT, twtt_dimid, twtt_varid))
   call check_ncf(nf90_put_att(ncid, x_varid,    'units', 'm'))
   call check_ncf(nf90_put_att(ncid, twtt_varid, 'units', 'ns'))
   call check_ncf(nf90_put_att(ncid, x_varid,    'long_name', 'Distance along profile'))
   call check_ncf(nf90_put_att(ncid, twtt_varid, 'long_name', 'Two-way travel time'))
   call check_ncf(nf90_put_att(ncid, x_varid,    'actual_range', &
      (/tr%x(1), tr%x(tr%last_trace)/)))
   call check_ncf(nf90_put_att(ncid, twtt_varid, 'actual_range', &
      (/tr%twtt(1), tr%twtt(tr%n)/)))
   call check_ncf(nf90_def_var(ncid, 'amplitude', NF90_FLOAT, (/x_dimid, twtt_dimid/), &
      tr_varid))
   ! Set variables for amplitude
   call check_ncf(nf90_put_att(ncid, tr_varid, 'long_name', 'Radar amplitude'))
   call check_ncf(nf90_put_att(ncid, tr_varid, 'actual_range', &
      (/minval(tr%tr), maxval(tr%tr)/)))

   ! Add comments about data
   call check_ncf(nf90_put_att(ncid, NF90_GLOBAL, 'title', &
      'Converted from RAD3 by f90rad3'))
   call check_ncf(nf90_put_att(ncid, NF90_GLOBAL, 'comment', &
      'Operator: '// trim(tr%operator_name) // '; ' // &
      'Customer: '// trim(tr%customer_name) // '; ' // &
      'Site: ' // trim(tr%site_name) // '; ' // &
      'Antennas: ' // trim(tr%antennas) // '; ' // &
      'Antenna orientation: ' // trim(tr%antenna_orientation) // '; ' // &
      'Comments: ' // trim(tr%comment)))
   if (present(history)) then
      call check_ncf(nf90_put_att(ncid, NF90_GLOBAL, 'history', trim(history)))
   endif
   ! Finish data description
   call check_ncf(nf90_enddef(ncid))

   ! Fill structure
   call check_ncf(nf90_put_var(ncid, x_varid,    tr%x))
   call check_ncf(nf90_put_var(ncid, twtt_varid, tr%twtt))
   call check_ncf(nf90_put_var(ncid, tr_varid,   transpose(tr%tr)))

   ! Finalise file
   call check_ncf(nf90_close(ncid))

end subroutine rad3_save_netcdf
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_save_volume_netcdf(v, file, history)
!===============================================================================
! Save a RAD3 volume to a 3D NetCDF file
! Optionally supply a string (history) to detail the command used to produce
! the NetCDF file (e.g., what filtering used)
   use netcdf, only: nf90_create, nf90_def_dim, nf90_def_var, nf90_enddef, &
      nf90_put_var, nf90_close, NF90_CLOBBER, NF90_FLOAT, nf90_noerr, nf90_strerror, &
      NF90_GLOBAL, nf90_put_att
   type(rad3volume), intent(in) :: v
   character(len=*), intent(in) :: file
   character(len=*), intent(in), optional :: history
   integer :: ncid, x_dimid, y_dimid, twtt_dimid, &
              x_varid, y_varid, twtt_varid, amp_varid

   call rad3_check_volume_exists(v)
   ! Create file
   call check_ncf(nf90_create(trim(file), NF90_CLOBBER, ncid))

   ! Define dimensions
   call check_ncf(nf90_def_dim(ncid, 'x',    v%nx, x_dimid))
   call check_ncf(nf90_def_dim(ncid, 'y',    v%ny, y_dimid))
   call check_ncf(nf90_def_dim(ncid, 'twtt', v%nt, twtt_dimid))
   ! Set variables for coordinates
   call check_ncf(nf90_def_var(ncid, 'x',    NF90_FLOAT, x_dimid,    x_varid))
   call check_ncf(nf90_def_var(ncid, 'y',    NF90_FLOAT, y_dimid,    y_varid))
   call check_ncf(nf90_def_var(ncid, 'twtt', NF90_FLOAT, twtt_dimid, twtt_varid))
   call check_ncf(nf90_put_att(ncid, x_varid,    'units', 'm'))
   call check_ncf(nf90_put_att(ncid, y_varid,    'units', 'm'))
   call check_ncf(nf90_put_att(ncid, twtt_varid, 'units', 'ns'))
   call check_ncf(nf90_put_att(ncid, x_varid,    'long_name', 'Distance along lines'))
   call check_ncf(nf90_put_att(ncid, y_varid,    'long_name', 'Distance across lines'))
   call check_ncf(nf90_put_att(ncid, twtt_varid, 'long_name', 'Two-way travel time'))
   call check_ncf(nf90_put_att(ncid, x_varid,    'actual_range', (/v%x(1), v%x(v%nx)/)))
   call check_ncf(nf90_put_att(ncid, y_varid,    'actual_range', (/v%y(1), v%y(v%ny)/)))
   call check_ncf(nf90_put_att(ncid, twtt_varid, 'actual_range', &
      (/v%twtt(1), v%twtt(v%nt)/)))
   call check_ncf(nf90_def_var(ncid, 'amplitude', NF90_FLOAT, &
      (/x_dimid, y_dimid, twtt_dimid/), amp_varid))
   ! Set variables for amplitude
   call check_ncf(nf90_put_att(ncid, amp_varid, 'long_name', 'Radar amplitude'))
   call check_ncf(nf90_put_att(ncid, amp_varid, 'actual_range', &
      (/minval(v%amp), maxval(v%amp)/)))

   ! Add comments about data
   call check_ncf(nf90_put_att(ncid, NF90_GLOBAL, 'title', &
      'Converted from RAD3 by f90rad3'))
!    call check_ncf(nf90_put_att(ncid, NF90_GLOBAL, 'comment', &
!       'Operator: '// trim(tr%operator_name) // '; ' // &
!       'Customer: '// trim(tr%customer_name) // '; ' // &
!       'Site: ' // trim(tr%site_name) // '; ' // &
!       'Antennas: ' // trim(tr%antennas) // '; ' // &
!       'Antenna orientation: ' // trim(tr%antenna_orientation) // '; ' // &
!       'Comments: ' // trim(tr%comment)))
   if (present(history)) then
      call check_ncf(nf90_put_att(ncid, NF90_GLOBAL, 'history', trim(history)))
   endif
   ! Finish data description
   call check_ncf(nf90_enddef(ncid))

   ! Fill structure
   call check_ncf(nf90_put_var(ncid, x_varid,    v%x))
   call check_ncf(nf90_put_var(ncid, y_varid,    v%y))
   call check_ncf(nf90_put_var(ncid, twtt_varid, v%twtt))
   call check_ncf(nf90_put_var(ncid, amp_varid,  v%amp))

   ! Finalise file
   call check_ncf(nf90_close(ncid))

end subroutine rad3_save_volume_netcdf
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_info(tr)
!===============================================================================
! Write some useful information about a rad3 file to stdout
   type(rad3trace), intent(in) :: tr
   call rad3_check_exists(tr)
   write(ISTDOUT,'(a,f0.8)') 'delta = ', tr % delta
   write(ISTDOUT,'(a,i0)')   'npts  = ', tr % n
   write(ISTDOUT,'(a,f0.8)') 'dx    = ', tr % distance_interval
   write(ISTDOUT,'(a,f0.8)') 'xmin  = ', tr % x(1)
   write(ISTDOUT,'(a,f0.8)') 'xmax  = ', tr % x(tr%last_trace)
   write(ISTDOUT,'(a,f0.8)') 'tmin  = ', tr % twtt(1)
   write(ISTDOUT,'(a,f0.8)') 'tmax  = ', tr % twtt(tr%n)
   write(ISTDOUT,'(a,i0)')   'xpts  = ', tr % last_trace
   write(ISTDOUT,'(a,f0.8)') 'amin  = ', minval(tr % tr)
   write(ISTDOUT,'(a,f0.8)') 'amax  = ', maxval(tr % tr)
end subroutine rad3_info
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_dump(tr)
!===============================================================================
! Send the values of a profile out as x,t,amplitude triplets to stdout
   implicit none
   type(rad3trace), intent(in) :: tr
   integer :: ix, it
   call rad3_check_exists(tr)
   do ix=1,tr%last_trace
      do it=1,tr%n
         write(ISTDOUT,*) tr%x(ix), tr%twtt(it), tr%tr(it,ix)
      enddo
   enddo
end subroutine rad3_dump
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_dump_volume(v)
!===============================================================================
! Send the values of a volume out as x,y,t,amplitude
   type(rad3volume), intent(in) :: v
   integer :: ix, iy, it
   write(0,'(a,i0.0,1x,i0.0,1x,i0.0)') 'Dimensions of volume (nx,ny,nt): ', &
      v%nx, v%ny, v%nt
   write(0,'(a,6(f0.6,1x))') 'Limits of volume (x1,x2,y1,y2,t1,t2): ', &
      v%x(1), v%x(v%nx), v%y(1), v%y(v%ny), v%twtt(1), v%twtt(v%nt)
   write(0,'(a,f0.4)') 'Maximum amplitude: ', maxval(abs(v%amp))
   write(0,'(a,f0.6,2(1x,f0.6))') 'Grid spacing (x,y,t): ', v%dx, v%dy, v%dt
   do ix = 1, v%nx
      do iy = 1, v%ny
         do it = 1, v%nt
            write(*,*) v%x(ix), v%y(iy), v%twtt(it), v%amp(ix,iy,it)
         enddo
      enddo
   enddo
end subroutine rad3_dump_volume
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_dump_slice(v, twtt)
!===============================================================================
! Send the values of a given timeslice of a volume to stdout as x,y,amplitude
   type(rad3volume), intent(in) :: v
   real(rs), intent(in) :: twtt
   integer :: ix, iy, it
   it = nint((twtt - v%twtt(1))/v%dt)
   write(0,'(a,i0.0,1x,i0.0,1x,i0.0)') 'Dimensions of volume (nx,ny,nt): ', &
      v%nx, v%ny, v%nt
   write(0,'(a,6(f0.6,1x))') 'Limits of volume (x1,x2,y1,y2,t1,t2): ', &
      v%x(1), v%x(v%nx), v%y(1), v%y(v%ny), v%twtt(1), v%twtt(v%nt)
   write(0,'(a,f0.4)') 'Maximum amplitude: ', maxval(abs(v%amp(:,:,it)))
   write(0,'(a,f0.6,2(1x,f0.6))') 'Grid spacing (x,y,t): ', v%dx, v%dy, v%dt
   do ix = 1, v%nx
      do iy = 1, v%ny
            write(*,*) v%x(ix), v%y(iy), v%amp(ix,iy,it)
      enddo
   enddo
end subroutine rad3_dump_slice
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_read_rad_file(file, tr)
!===============================================================================
! Read the .rad file containing header information
! Filename must include the extension.
   character(len=*), intent(in) :: file
   type(rad3trace), intent(inout) :: tr
   character(len=300) :: tempstr, field, value
   integer :: i, iostat
   logical :: exists
   inquire(file=file, exist=exists)
   if (.not.exists) &
      call rad3_error('rad3_read_rad_file: File "'//trim(file)//'" does not exist')
   open(IIN, file=file, iostat=iostat)
   if (iostat /= 0) &
      call rad3_error('rad3_read_rad_file: Problem opening file "'//trim(file)//'"')
   i = 1
   do while (i <= RAD3_RAD_FILE_LEN)
      read(IIN, '(a)') tempstr
      field = tempstr(1:index(tempstr, ':')-1)
      value = tempstr(index(tempstr, ':')+1:len_trim(tempstr))
      select case (field)
         case('SAMPLES')
            read(value,*) tr % n
         case('FREQUENCY')
            read(value,*) tr % frequency
         case('FREQUENCY STEPS')
            read(value,*) tr % frequency_steps
         case('SIGNAL POSITION')
            read(value,*) tr % signal_position
         case('RAW SIGNAL POSITION')
            read(value,*) tr % raw_signal_position
         case('DISTANCE FLAG')
            read(value,*) tr % distance_flag
         case('TIME FLAG')
            read(value,*) tr % time_flag
         case('PROGRAM FLAG')
            read(value,*) tr % program_flag
         case('EXTERNAL FLAG')
            read(value,*) tr % external_flag
         case('TIME INTERVAL')
            read(value,*) tr % time_interval
         case('DISTANCE INTERVAL')
            read(value,*) tr % distance_interval
         case('OPERATOR')
            tr % operator_name = trim(value)
         case('CUSTOMER')
            tr % customer_name = trim(value)
         case('SITE')
            tr % site_name = trim(value)
         case('ANTENNAS')
            tr % antennas = trim(value)
         case('ANTENNA ORIENTATION')
            tr % antenna_orientation = trim(value)
         case('ANTENNA SEPARATION')
            read(value,*) tr % antenna_separation
         case('COMMENT')
            read(value,*) tr % comment
         case('TIMEWINDOW')
            read(value,*) tr % timewindow
         case('STACKS')
            read(value,*) tr % stacks
         case('STACK EXPONENT')
            read(value,*) tr % stack_exponent
         case('STACKING TIME')
            read(value,*) tr % stacking_time
         case('LAST TRACE')
            read(value,*) tr % last_trace
         case('STOP POSITION')
            read(value,*) tr % stop_position
         case('SYSTEM CALIBRATION')
            read(value,*) tr % system_calibration
         case('START POSITION')
            read(value,*) tr % start_position
         case('SHORT FLAG')
            read(value,*) tr % short_flag
         case('INTERMEDIATE FLAG')
            read(value,*) tr % intermediate_flag
         case('LONG FLAG')
            read(value,*) tr % long_flag
         case('PREPROCESSING')
            read(value,*) tr % preprocessing
         case('HIGH')
            read(value,*) tr % high
         case('LOW')
            read(value,*) tr % low
         case('FIXED INCREMENT')
            read(value,*) tr % fixed_increment
         case('FIXED MOVES UP')
            read(value,*) tr % fixed_moves_up
         case('FIXED MOVES DOWN')
            read(value,*) tr % fixed_moves_down
         case('FIXED POSITION')
            read(value,*) tr % fixed_position
         case('WHEEL CALIBRATION')
            read(value,*) tr % wheel_calibration
         case('POSITIVE DIRECTION')
            read(value,*) tr % positive_direction
         case default
            call rad3_error('rad3_read_rad_file: Unknown field: '//trim(field))
      end select
      if (i == RAD3_RAD_FILE_LEN) exit
      i = i + 1
   enddo

   close(IIN)

   if (i /= RAD3_RAD_FILE_LEN) &
      call rad3_error('rad3_read_rad_file: Incorrect number of fields in rad file')

   ! Compute derived values in rad3 type and fill distance and time arrays
   tr % delta = tr%timewindow / (tr%n - 1)
   call rad3_allocate(tr%x, tr%last_trace)
   do i = 1, tr%last_trace
      tr % x(i) = real(i-1) * tr%distance_interval
   enddo

   call rad3_allocate(tr%twtt, tr%n)
   do i = 1, tr%n
      tr % twtt(i) = real(i-1) * tr%delta
   enddo
end subroutine rad3_read_rad_file
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_write_rad_file(tr, file)
!===============================================================================
! Save the ASCII header file.  Supply filename without the .rad extension.
   type(rad3trace), intent(in) :: tr
   character(len=*), intent(in) :: file
   call rad3_check_exists(tr)
   call rad3_check_file_name(file)
   open(IOUT, file=trim(file)//'.rad')
   write(IOUT, '(a,i0)')   'SAMPLES:', tr % n
   write(IOUT, '(a,f0.6)') 'FREQUENCY:', tr % frequency
   write(IOUT, '(a,i0)')   'FREQUENCY STEPS:', tr % frequency_steps
   write(IOUT, '(a,f0.6)') 'SIGNAL POSITION:', tr % signal_position
   write(IOUT, '(a,i0)')   'RAW SIGNAL POSITION:', tr % raw_signal_position
   write(IOUT, '(a,i0)')   'DISTANCE FLAG:', tr % distance_flag
   write(IOUT, '(a,i0)')   'TIME FLAG:', tr % time_flag
   write(IOUT, '(a,i0)')   'PROGRAM FLAG:', tr % program_flag
   write(IOUT, '(a,i0)')   'EXTERNAL FLAG:', tr % external_flag
   write(IOUT, '(a,f0.6)') 'TIME INTERVAL:', tr % time_interval
   write(IOUT, '(a,f0.6)') 'DISTANCE INTERVAL:', tr % distance_interval
   write(IOUT, '(a,a)')    'OPERATOR:', trim(tr % operator_name)
   write(IOUT, '(a,a)')    'CUSTOMER:', trim(tr % customer_name)
   write(IOUT, '(a,a)')    'SITE:', trim(tr % site_name)
   write(IOUT, '(a,a)')    'ANTENNAS:', trim(tr % antennas)
   write(IOUT, '(a,a)')    'ANTENNA ORIENTATION:', trim(tr % antenna_orientation)
   write(IOUT, '(a,f0.6)') 'ANTENNA SEPARATION:', tr % antenna_separation
   write(IOUT, '(a,a)')    'COMMENT:', trim(tr % comment)
   write(IOUT, '(a,f0.6)') 'TIMEWINDOW:', tr % timewindow
   write(IOUT, '(a,i0)')   'STACKS:', tr % stacks
   write(IOUT, '(a,i0)')   'STACK EXPONENT:', tr % stack_exponent
   write(IOUT, '(a,f0.6)') 'STACKING TIME:', tr % stacking_time
   write(IOUT, '(a,i0)')   'LAST TRACE:', tr % last_trace
   write(IOUT, '(a,f0.6)') 'STOP POSITION:', tr % stop_position
   write(IOUT, '(a,f0.10)')'SYSTEM CALIBRATION:', tr % system_calibration
   write(IOUT, '(a,f0.6)') 'START POSITION:', tr % start_position
   write(IOUT, '(a,i0)')   'SHORT FLAG:', tr % short_flag
   write(IOUT, '(a,i0)')   'INTERMEDIATE FLAG:', tr % intermediate_flag
   write(IOUT, '(a,i0)')   'LONG FLAG:', tr % long_flag
   write(IOUT, '(a,i0)')   'PREPROCESSING:', tr % preprocessing
   write(IOUT, '(a,i0)')   'HIGH:', tr % high
   write(IOUT, '(a,i0)')   'LOW:', tr % low
   write(IOUT, '(a,f0.6)') 'FIXED INCREMENT:', tr % fixed_increment
   write(IOUT, '(a,i0)')   'FIXED MOVES UP:', tr % fixed_moves_up
   write(IOUT, '(a,i0)')   'FIXED MOVES DOWN:', tr % fixed_moves_down
   write(IOUT, '(a,f0.6)') 'FIXED POSITION:', tr % fixed_position
   write(IOUT, '(a,f0.6)') 'WHEEL CALIBRATION:', tr % wheel_calibration
   write(IOUT, '(a,i0)')   'POSITIVE DIRECTION:', tr % positive_direction
   close(IOUT)
end subroutine rad3_write_rad_file
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_remove_mean(tr)
!===============================================================================
! Remove the mean trace from each shot
   type(rad3trace), intent(inout) :: tr
   real(dp), dimension(tr%n) :: mean
   integer :: i
   call rad3_verbose('rad3_remove_mean: Removing mean from trace')
   mean = 0
   do i = 1, tr%last_trace
      mean(1:tr%n) = mean(1:tr%n) + &
         real(tr%tr(1:tr%n,i), kind=dp)/real(tr%last_trace, kind=dp)
   enddo
   do i = 1, tr%last_trace
      tr%tr(1:tr%n,i) = tr%tr(1:tr%n,i) - real(mean(1:tr%n), kind=rs)
   enddo
end subroutine rad3_remove_mean
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_volume_remove_mean(v)
!===============================================================================
! Remove the mean trace from the volume.  Only operates on points with data,
! i.e., where the trace is not zeroed.
   type(rad3volume), intent(inout) :: v
   real(rs), dimension(v%nt) :: mean
   integer :: ipts, ix, iy
   call rad3_verbose('rad3_volume_remove_mean: Removing mean trace from volume')
   mean = 0
   ipts = 0
   do iy = 1, v%ny
      do ix = 1, v%nx
         if (maxval(abs(v%amp(ix,iy,:))) > 1) then
            mean = mean + v%amp(ix,iy,:)
            ipts = ipts + 1
         endif
      enddo
   enddo
   mean = mean/real(ipts)
   do iy = 1, v%ny
      do ix = 1, v%nx
         if (maxval(abs(v%amp(ix,iy,:))) > 1) v%amp(ix,iy,:) = v%amp(ix,iy,:) - mean
      enddo
   enddo
end subroutine rad3_volume_remove_mean
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_gain(tr, start_samp, lin_gain, exp_gain)
!===============================================================================
! Apply time-varying gain to a trace of the form
!    A(t) = A0(t)*lin_gain + t*exp(exp_gain*t),
! starting at sample number start_samp
   type(rad3trace), intent(inout) :: tr
   integer, intent(in) :: start_samp
   real(dp), intent(in) :: lin_gain, exp_gain
   integer :: it
   if (start_samp < 1 .or. start_samp > tr%n) &
      call rad3_error('rad3_gain: Start sample must be between 1 and nsamples')
   do it = start_samp, tr%n
      tr%tr(it,:) = real(tr%tr(it,:) * &
         (lin_gain*tr%twtt(it-start_samp+1) + &
          exp(exp_gain*1.e-3_dp*tr%twtt(it-start_samp+1))), kind=rs)
   enddo
end subroutine rad3_gain
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_volume_gain(tr, start_samp, lin_gain, exp_gain)
!===============================================================================
! Apply time-varying gain to a trace of the form
!    A(t) = A0(t)*lin_gain + t*exp(exp_gain*t),
! starting at sample number start_samp
   type(rad3volume), intent(inout) :: tr
   integer, intent(in) :: start_samp
   real(dp), intent(in) :: lin_gain, exp_gain
   integer :: it
   if (start_samp < 1 .or. start_samp > tr%nt) &
      call rad3_error('rad3_volume_gain: Start sample must be between 1 and nsamples')
   do it = start_samp, tr%nt
      tr%amp(:,:,it) = real(tr%amp(:,:,it) * &
         (lin_gain*tr%twtt(it-start_samp+1) + &
          exp(exp_gain*1.e-3_dp*tr%twtt(it-start_samp+1))), kind=rs)
   enddo
end subroutine rad3_volume_gain
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_delete(tr)
!===============================================================================
   type(rad3trace), intent(inout) :: tr
   if (allocated(tr%x)) deallocate(tr%x)
   if (allocated(tr%tr)) deallocate(tr%tr)
   if (allocated(tr%twtt)) deallocate(tr%twtt)
end subroutine rad3_delete
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_delete_volume(tr)
!===============================================================================
   type(rad3volume), intent(inout) :: tr
   if (allocated(tr%x)) deallocate(tr%x)
   if (allocated(tr%y)) deallocate(tr%y)
   if (allocated(tr%twtt)) deallocate(tr%twtt)
   if (allocated(tr%amp)) deallocate(tr%amp)
   tr % dx = 0.
   tr % dy = 0.
   tr % dt = 0.
end subroutine rad3_delete_volume
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_allocate(a, n)
!===============================================================================
   real(rs), intent(inout), dimension(:), allocatable :: a
   integer, intent(in) :: n
   if (allocated(a)) then
      if (size(a) /= n) deallocate(a)
   else
      allocate(a(n))
   endif
end subroutine rad3_allocate
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_check_file_name(file)
!===============================================================================
! Makes sure that a file does not contain the extension
   character(len=*), intent(in) :: file
   if (len_trim(file) > 5) then
      if (file(len_trim(file)-3:len_trim(file)) == '.rd3' .or. &
          file(len_trim(file)-3:len_trim(file)) == '.rad') &
         call rad3_warning('rad3_check_file_name: Supplied filename includes extension.  Do not supply.')
   endif
end subroutine rad3_check_file_name
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_check_exists(tr)
!===============================================================================
   type(rad3trace), intent(in) :: tr
   if (.not.allocated(tr%tr) .or. .not.allocated(tr%x) .or. &
      .not.allocated(tr%twtt)) &
      call rad3_error('rad3_check_exists: Trace does not exist')
end subroutine rad3_check_exists
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_check_volume_exists(v)
!===============================================================================
   type(rad3volume), intent(in) :: v
   if (.not.allocated(v%x) .or. .not.allocated(v%y) .or. &
      .not.allocated(v%twtt) .or. .not.allocated(v%amp)) &
      call rad3_error('rad3_check_volume_exists: Trace does not exist')
end subroutine rad3_check_volume_exists
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_error(str)
!===============================================================================
   character(len=*), intent(in) :: str
   write(ISTDERR,'(a)') 'f90rad3: Error: '//str
   stop
end subroutine rad3_error
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_warning(str)
!===============================================================================
   character(len=*), intent(in) :: str
   write(ISTDERR,'(a)') 'f90rad3: Warning: '//str
end subroutine rad3_warning
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_set_verbose()
!===============================================================================
! Make the module verbose
   verbose = .true.
end subroutine rad3_set_verbose
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_verbose(str)
!===============================================================================
   character(len=*), intent(in) :: str
   if (verbose) write(ISTDERR,'(a)') 'f90rad3: '//str
end subroutine rad3_verbose
!-------------------------------------------------------------------------------

!===============================================================================
subroutine check_ncf(status)
!===============================================================================
   use netcdf, only: nf90_noerr, nf90_strerror
   integer, intent(in) :: status
   if (status /= nf90_noerr) &
      call rad3_error('rad3_save_netcdf: '//trim(nf90_strerror(status)))
end subroutine check_ncf
!-------------------------------------------------------------------------------

end module f90rad3
