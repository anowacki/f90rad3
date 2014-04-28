program rad3dump
   use f90rad3
   implicit none
   type(rad3trace) :: tr
   character(len=250) :: file
   integer :: itrace, it

   if (command_argument_count() /= 1) call usage

   call get_command_argument(1, file)

   call rad3_load(file, tr)
   call rad3_dump(tr)

contains
   subroutine usage()
      implicit none
      write(0,'(a)') &
         'Usage: rad3dump [rad3 file without .rad3 extension]', &
         '   .rad file of same name must be in same directory', &
         '   Writes twtt,a to stdout'
      stop
   end subroutine usage
end program rad3dump
