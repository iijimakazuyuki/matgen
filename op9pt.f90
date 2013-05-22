module op9pt_mod
	use matcrs_mod
	implicit none
	public op9pt
	private map, produce, bounded
contains
	subroutine op9pt(n)
		integer, intent(in) :: n
		
		type(matcrs) :: a
		double precision, allocatable :: e(:)
		integer, allocatable :: row(:), col(:)
		integer :: i, j, d(3)
		d = (/-1, 0, 1/)
		a%n = n*n
		a%m = n*n
		!“à•”A4•ÓA4‹÷
		a%k = 9*(n*n - 4*n + 4) + 6*(4*n - 2*4) + 4*4
		call init_matcrs(a)
		a%e = 1
		do i=1, n
			do j=1, n
				call produce(i,j,n,d,a%row,a%col)
			end do
		end do
		call print_matcrs_array(a)
	end subroutine
	integer function map(i, j, n)
		integer, intent(in) :: i, j, n
		map = (i-1)*n + j
	end function
	subroutine produce(i, j, n, d, row, col)
		integer, intent(in) :: i, j, n, d(:)
		integer, intent(out) :: row(0:), col(:)
		integer :: ii, jj, nn, cc
		nn = map(i, j, n)
		cc = row(nn-1)
		do ii=1, size(d)
			do jj=1, size(d)
				if(bounded(i+d(ii), j+d(jj), n)) then
					cc = cc + 1
					col(cc) = map(i+d(ii), j+d(jj), n)
				end if
			end do
		end do
		row(nn) = cc
	end subroutine
	logical function bounded(i, j, n) result(a)
		integer, intent(in) :: i, j, n
		a = i>=1 .and. i<=n .and. j>=1 .and. j<=n
	end function
end module
