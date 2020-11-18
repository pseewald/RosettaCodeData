!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Sun May 19 23:14:14
!
!a=./F && make $a && $a < unixdict.txt
!f95 -Wall -ffree-form F.F -o F
!101
!110010
!10001100101000
!
!Compilation finished at Sun May 19 23:14:14
!
!
!   tobin=: -.&' '@":@#:
!   tobin 5
!101
!   tobin 50
!110010
!   tobin 9000
!10001100101000

program bits
   implicit none
   integer, dimension(3) :: a
   integer :: i
   data a/5, 50, 9000/
   do i = 1, 3
      call s(a(i))
   end do

contains

   subroutine s(a)
      integer, intent(in) :: a
      integer :: i
      if (a .eq. 0) then
         write (6, '(a)') '0'
         return
      end if
      do i = 31, 0, -1
         if (btest(a, i)) exit
      end do
      do while (0 .lt. i)
         if (btest(a, i)) then
            write (6, '(a)', advance='no') '1'
         else
            write (6, '(a)', advance='no') '0'
         end if
         i = i - 1
      end do
      if (btest(a, i)) then
         write (6, '(a)') '1'
      else
         write (6, '(a)') '0'
      end if
   end subroutine s

end program bits
