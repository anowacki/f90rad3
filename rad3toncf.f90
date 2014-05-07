!===============================================================================
program rad3toncf
!===============================================================================
! Convert a rad3 file to NetCDF format.  Supply filename without extension.
! By default, writes to file.nc.  

   use f90rad3

   implicit none
   type(rad3trace) :: tr
   character(len=250) :: infile, outfile

   call get_args

   call rad3_load(infile, tr)
   call rad3_save_netcdf(tr, outfile)

contains
   subroutine usage()
      write(0,'(a)') &
         'Usage: rad3toncf [infile] (outfile)', &
         'Do not supply the .rd3 extension', &
         'Default output is to file.nc'
      stop
   end subroutine usage

   subroutine get_args()
      if (command_argument_count() == 1) then
         call get_command_argument(1, infile)
         outfile = trim(infile) // '.nc'
      else if (command_argument_count() == 2) then
         call get_command_argument(1, infile)
         call get_command_argument(2, outfile)
      else
         call usage
      endif
   end subroutine get_args
end program rad3toncf
!-------------------------------------------------------------------------------
