program rad3dump
   use f90rad3
   implicit none
   type(rad3trace) :: tr
   character(len=250) :: file
   integer :: start_samp
   real(8) :: lin_gain, exp_gain
   logical :: remove_mean = .false., &
              gain = .false.

   call get_options

   call rad3_load(file, tr)

   ! Filters
   if (remove_mean) call rad3_remove_mean(tr)
   if (gain) call rad3_gain(tr, start_samp, lin_gain, exp_gain)

   call rad3_dump(tr)

contains
   subroutine get_options()
      character(len=250) :: arg
      integer :: iarg, narg
      iarg = 1
      narg = command_argument_count()
      if (narg == 0) call usage
      call get_command_argument(narg, file)
      do while (iarg < narg)
         call get_command_argument(iarg, arg)
         select case (arg)
            case('-gain')
               gain = .true.
               call get_command_argument(iarg+1, arg)
               read(arg,*) start_samp
               call get_command_argument(iarg+2, arg)
               read(arg,*) lin_gain
               call get_command_argument(iarg+3, arg)
               read(arg,*) exp_gain
               iarg = iarg + 4
            case('-rmean')
               remove_mean = .true.
               iarg = iarg + 1
            case('-v')
               call rad3_set_verbose
               iarg = iarg + 1
            case default
               call usage
         end select
      enddo
   end subroutine

   subroutine usage()
      write(0,'(a)') &
         'Usage: rad3dump (options) [rad3 file without .rad3 extension]', &
         '   .rad file of same name must be in same directory', &
         '   Writes twtt,a to stdout', &
         'Options:', &
         '   -gain [start_sample linear_gain exponentail_gain]', &
         '           : Apply time-varying gain to sample', &
         '   -rmean  : Remove the mean trace from all traces', &
         '   -v      : Verbose output sent to stderr'
      stop
   end subroutine usage
end program rad3dump
