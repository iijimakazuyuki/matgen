module op27pt_mod
	use matcrs_mod
	implicit none
	public op27pt
	private map, produce, bounded
contains
	subroutine op27pt(n)
		integer, intent(in) :: n
		
		type(matcrs) :: a
		double precision, allocatable, target :: e(:)
		integer, allocatable, target :: row(:), col(:)
		integer :: i, j, k, d(3)
		d = (/-1, 0, 1/)
		a%n = n*n*n
		a%m = n*n*n
		!“à•”A6–ÊA12•ÓA8Šp
		a%k = 27*(n*n*n - 6*n*n + 12*n - 8) + 18*(6*n*n - 2*12*n + 3*8) + 12*(12*n - 3*8) + 8*8
		call init_matcrs(a)
		a%e = 1
		do i=1, n
			do j=1, n
				do k=1, n
					call produce(i, j, k, n, d, a%row, a%col)
				end do
			end do
		end do
		call print_matcrs_array(a)
	end subroutine
	integer function map(i, j, k, n)
		integer, intent(in) :: i, j, k, n
		map = (i-1)*n*n + (j-1)*n + k
	end function
	subroutine produce(i, j, k, n, d, row, col)
		integer, intent(in) :: i, j, k, n, d(:)
		integer, intent(out) :: row(0:), col(:)
		integer :: ii, jj, kk, nn, cc
		nn = map(i, j, k, n)
		cc = row(nn-1)
		do ii=1, size(d)
			do jj=1, size(d)
				do kk=1, size(d)
					if(bounded(i+d(ii), j+d(jj), k+d(kk), n)) then
						cc = cc + 1
						col(cc) = map(i+d(ii), j+d(jj), k+d(kk), n)
					end if
				end do
			end do
		end do
		row(nn) = cc
	end subroutine
	logical function bounded(i, j, k, n) result(a)
		integer, intent(in) :: i, j, k, n
		a = i>=1 .and. i<=n .and. j>=1 .and. j<=n .and. k>=1 .and. k<=n
	end function
end module
