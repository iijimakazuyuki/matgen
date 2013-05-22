module op3pt_mod
	use matcrs_mod
	implicit none
	public op3pt
	private produce, bounded
contains
	subroutine op3pt(n)
		integer, intent(in) :: n
		
		type(matcrs) :: a
		double precision, allocatable :: e(:)
		integer, allocatable :: row(:), col(:)
		integer :: i, d(3)
		d = (/-1, 0, 1/)
		a%n = n
		a%m = n
		a%k = 3*(n-2) + 2*2
		call init_matcrs(a)
		a%e = 1
		do i=1, n
			call produce(i, n, d, a%row, a%col)
		end do
		call print_matcrs_array(a)
	end subroutine
	subroutine produce(i, n, d, row, col)
		integer, intent(in) :: i, n, d(:)
		integer, intent(out) :: row(0:), col(:)
		integer :: ii, nn, cc
		nn = i
		cc = row(nn-1)
		do ii=1, size(d)
			if(bounded(i+d(ii), n)) then
				cc = cc + 1
				col(cc) = i+d(ii)
			end if
		end do
		row(nn) = cc
	end subroutine
	logical function bounded(i, n) result(a)
		integer, intent(in) :: i, n
		a = i>=1 .and. i<=n
	end function
end module
