!===============================================================================
program rad3slice
!===============================================================================
! Takes a time slice of a set of files in the current folder
! Files must be listed in a rad3slice.in file, with the format
!     file y_coord x0_coord
! Provide a sample time to display

   use f90rad3

   implicit none
   
   type(rad3volume) :: v
   real :: twtt
   character(len=250) :: infile = 'rad3slice.in'
   logical :: rmean = .false.

   call rad3_set_verbose

   call get_args
   call rad3_load_volume(infile, v)
   if (rmean) call rad3_volume_remove_mean(v)
   call rad3_dump_slice(v, twtt)

contains
   
   !============================================================================
   subroutine get_args()
   !============================================================================
      character(len=250) :: arg
      integer :: iarg, narg
      narg = command_argument_count()
      if (narg < 1) call usage
      call get_command_argument(narg, arg)
      read(arg, *) twtt
      iarg = 1
      do while (iarg < narg)
         call get_command_argument(iarg, arg)
         select case (arg)
            case('-f')
               call get_command_argument(iarg+1, infile)
               iarg = iarg + 2
            case('-rmean')
               rmean = .true.
               iarg = iarg + 1
            case default
               call usage
         end select
      enddo
   end subroutine get_args
   !----------------------------------------------------------------------------

   !============================================================================
   subroutine usage()
   !============================================================================
      write(0,'(a)') 'Usage: rad3slice (options) [twtt]', &
                     'Writes a timeslice as x,y,z to stdout', &
                     'Reads info from the file rad3slice.in by default', &
                     'Options:', &
                     '   -f [infile] : Read info from infile [rad3slice.in]', &
                     '   -rmean      : Remove mean trace'
      stop
   end subroutine usage
   !----------------------------------------------------------------------------

end program rad3slice
!-------------------------------------------------------------------------------