module op3pt_mod
	use mat_mod
	implicit none
	public op3pt
	private produce, bounded
contains
	subroutine op3pt()
		type(matcrs) :: a
		double precision, allocatable, target :: e(:)
		integer, allocatable, target :: idx(:), col(:)
		integer :: i, n, d(3)
		d = (/-1, 0, 1/)
		read *, n
		a%n = n
		a%m = 3*(n-2) + 2*2
		call init_matcrs(a, e, idx, col)
		a%e = 1
		do i=1, n
			call produce(i, n, d, a%idx, a%col)
		end do
		call print_matcrs_array(a)
	end subroutine
	subroutine produce(i, n, d, idx, col)
		integer, intent(in) :: i, n, d(:)
		integer, intent(out) :: idx(0:), col(:)
		integer :: ii, nn, cc
		nn = i
		cc = idx(nn-1)
		do ii=1, size(d)
			if(bounded(i+d(ii), n)) then
				cc = cc + 1
				col(cc) = i+d(ii)
			end if
		end do
		idx(nn) = cc
	end subroutine
	logical function bounded(i, n) result(a)
		integer, intent(in) :: i, n
		a = i>=1 .and. i<=n
	end function
end module
