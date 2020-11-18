subroutine gcd_iter(value, u, v)
   integer, intent(out) :: value
   integer, intent(inout) :: u, v
   integer :: t

   do while (v /= 0)
      t = u
      u = v
      v = mod(t, v)
   end do
   value = abs(u)
end subroutine gcd_iter
