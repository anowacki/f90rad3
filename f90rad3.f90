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
   integer, parameter, private :: IERR = 0, ISTDIN = 5, ISTDOUT = 6, &
                                  IIN = 10, IOUT = 11

   ! Helper routines not exposed
   private :: rad3_error, &
              rad3_warning

   ! Type defining a section (collection of traces)
   type rad3trace
      character(len=RAD3_FILENAME_LEN) :: file
      real(rs) :: delta ! Sampling frequency
      ! Header part; not all fields needed
      integer :: n  ! number of samples
      real(rs) :: frequency
      real(rs) :: frequency_steps
      real(rs) :: signal_position
      real(rs) :: raw_signal_position
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
      integer(rad3int), dimension(:,:), allocatable :: tr
      real(rs), dimension(:), allocatable :: x    ! Shot offset
      real(rs), dimension(:), allocatable :: twtt ! Travel times
   end type rad3trace

contains

!===============================================================================
subroutine rad3_load(file, tr)
!===============================================================================
! Load a .rd3 file.  Supply the filename without the extension.  A .rad file with
! the same name must be in the same directory as the filename supplied.
   character(len=*), intent(in) :: file
   type(rad3trace), intent(inout) :: tr
   logical :: exists
   integer :: i

   if (file(len_trim(file)-3:len_trim(file)) == '.rd3') &
      call rad3_warning('rad3_load: Supplied filename includes extension.  Do not supply.')
   ! Get header information
   call rad3_read_rad_file(trim(file)//'.rad', tr)
   ! Allocate memory for section
   if (allocated(tr%tr)) deallocate(tr%tr)
   allocate(tr%tr(tr%n, tr%last_trace))
   ! Check file exists
   inquire(file=trim(file)//'.rd3', exist=exists)
   if (.not.exists) &
      call rad3_error('rad3_load: File "'//trim(file)//'.rad3" does not exist')
   open(IIN, file=trim(file)//'.rd3', access='direct', recl=rad3int*tr%n)
   do i = 1, tr%last_trace
      read(IIN, rec=i) tr%tr(:,i)
   enddo
   close(IIN)
end subroutine rad3_load
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_info(tr)
!===============================================================================
   implicit none
   type(rad3trace), intent(in) :: tr
   write(ISTDOUT,'(a,f0.8)') 'delta = ', tr % delta
   write(ISTDOUT,'(a,i0.0)') 'npts  = ', tr % n
   write(ISTDOUT,'(a,f0.8)') 'dx    = ', tr % distance_interval
   write(ISTDOUT,'(a,f0.8)') 'xmax  = ', tr % x(tr%last_trace)
   write(ISTDOUT,'(a,f0.8)') 'tmax  = ', tr % twtt(tr%n)
!    write(ISTDOUT,'(a,i0.0)') 'Amin  = ', minval(tr%tr)
!    write(ISTDOUT,'(a,i0.0)') 'Amax  = ', maxval(tr%tr)
end subroutine rad3_info
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_dump(tr)
!===============================================================================
! Send the values of a profile out as x,t,amplitude triplets to stdout
   implicit none
   type(rad3trace), intent(in) :: tr
   integer :: ix, it
   do ix=1,tr%last_trace
      do it=1,tr%n
         write(ISTDOUT,*) tr%x(ix), tr%twtt(it), tr%tr(it,ix)
      enddo
   enddo
end subroutine rad3_dump
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
subroutine rad3_error(str)
!===============================================================================
   character(len=*), intent(in) :: str
   write(IERR,'(a)') 'f90rad3: Error: '//str
   stop
end subroutine rad3_error
!-------------------------------------------------------------------------------

!===============================================================================
subroutine rad3_warning(str)
!===============================================================================
   character(len=*), intent(in) :: str
   write(IERR,'(a)') 'f90rad3: Warning: '//str
end subroutine rad3_warning
!-------------------------------------------------------------------------------

end module f90rad3
