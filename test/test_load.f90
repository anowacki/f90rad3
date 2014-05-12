!===============================================================================
! Part of f90rad3, the Fortran module for dealing with GPR data files in the
! RAD3 format.
!
! Andy Nowacki <andy.nowacki@bristol.ac.uk>
!
! See the file LICENCE for licence details.
!===============================================================================

program test_load

   use f90rad3

   implicit none

   type(rad3trace) :: tr
   integer, parameter :: nt = 888, nx = 2  ! test_data parameters
   real(8), dimension(nt,nx) :: a
   character(250) :: file1 = 'short_data'

   call rad3_load(file1, tr)
   if (size(tr%tr, 1) /= nt .or. size(tr%tr, 2) /= nx) then
      write(*,*) 'Test failed (loaded array has unexpected size)'
      stop
   endif

   call load_array
   
   if (any(abs(tr%tr - a) > 1.)) then
      write(*,*) 'Test failed (loaded arrays has unexpected values)'
   else
      write(*,*) 'Test passed'
   endif

contains

   subroutine load_array()
      include 'test_load_load_array.f90'
   end subroutine load_array

end program test_load
