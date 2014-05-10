!===============================================================================
program rad3toncf
!===============================================================================
! Convert a rad3 file to NetCDF format.  Supply filename without extension.
! By default, writes to file.nc.  

   use f90rad3

   implicit none
   type(rad3trace) :: tr
   character(len=250) :: infile, outfile, command
   real(8) :: lin_gain, exp_gain
   integer :: start_samp
   logical :: rmean = .false., gain = .false., outfile_set = .false.

   call get_args

   call rad3_load(infile, tr)

   if (rmean) call rad3_remove_mean(tr)
   if (gain) call rad3_gain(tr, start_samp, lin_gain, exp_gain)

   call rad3_save_netcdf(tr, outfile, history=command)

contains
   subroutine usage()
      write(0,'(a)') &
         'Usage: rad3toncf (options) [infile]', &
         'Do not supply the .rd3 extension', &
         'Options:', &
         '   -gain [start_samp lin_gain exp_gain]' ,&
         '                 : Apply gain to trace [none]', &
         '   -o [outfile]  : Send output to outfile [infile.nc]', &
         '   -rmean        : Remove mean trace [no not remove]', &
         '   -v            : Verbose output sent to stderr'
      stop
   end subroutine usage

   subroutine get_args()
      character(len=250) :: arg
      integer :: iarg, narg
      narg = command_argument_count()
      if (narg < 1) call usage
      call get_command_argument(narg, infile)
      iarg = 1
      do while (iarg < narg)
         call get_command_argument(iarg, arg)
         select case(arg)
            case('-gain')
               call get_command_argument(iarg+1, arg)
               read(arg,*) start_samp
               call get_command_argument(iarg+2, arg)
               read(arg,*) lin_gain
               call get_command_argument(iarg+3, arg)
               read(arg,*) exp_gain
               gain = .true.
               iarg = iarg + 4
            case('-o')
               call get_command_argument(iarg+1, outfile)
               outfile_set = .true.
               iarg = iarg + 2
            case('-rmean')
               rmean = .true.
               iarg = iarg + 1
            case('-v')
               call rad3_set_verbose
               iarg = iarg + 1
            case default
               call usage
         end select
      enddo
      if (.not.outfile_set) outfile = trim(infile) // '.nc'
      call get_command(command)
   end subroutine get_args

end program rad3toncf
!-------------------------------------------------------------------------------
