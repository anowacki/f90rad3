program test_save
   
   use f90rad3
   
   implicit none
   
   type(rad3trace) :: tr1, tr2
   character(250) :: file1 = 'test_data', &  ! No extension!
                     file2 = 'test_data_out'
   call rad3_load(file1, tr1)
   call rad3_save(tr1, file2)
   call rad3_load(file2, tr2)
   if (any(abs(tr1%tr - tr2%tr) > 1.)) then
      write(*,*) 'Test failed'
   else
      write(*,*) 'Test passed'
   endif
   
end program test_save
