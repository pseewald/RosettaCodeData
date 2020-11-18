subroutine gcd_bin(value, u, v)
   integer, intent(out) :: value
   integer, intent(inout) :: u, v
   integer :: k, t

   u = abs(u)
   v = abs(v)
   if (u < v) then
      t = u
      u = v
      v = t
   end if
   if (v == 0) then
      value = u
      return
   end if
   k = 1
   do while ((mod(u, 2) == 0) .and. (mod(v, 2) == 0))
      u = u/2
      v = v/2
      k = k*2
   end do
   if ((mod(u, 2) == 0)) then
      t = u
   else
      t = -v
   end if
   do while (t /= 0)
      do while ((mod(t, 2) == 0))
         t = t/2
      end do
      if (t > 0) then
         u = t
      else
         v = -t
      end if
      t = u - v
   end do
   value = u*k
end subroutine gcd_bin
