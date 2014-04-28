program rad3info
   use f90rad3
   implicit none
   type(rad3trace) :: tr
   character(len=250) :: file

   if (command_argument_count() /= 1) call usage

   call get_command_argument(1, file)

   call rad3_read_rad_file(trim(file)//'.rad', tr)
   call rad3_info(tr)
   
contains
   subroutine usage()
      implicit none
      write(0,'(a)') &
         'Usage: rad3info [rad3 file without .rad3 extension]', &
         '   .rad file of same name must be in same directory', &
         '   Writes some information about a rad3 file.'
      stop
   end subroutine usage
end program rad3info
