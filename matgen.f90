program matgen
	use op3pt_mod
	use op9pt_mod
	use op27pt_mod
	implicit none
	
	integer :: entry, n
	read *, entry, n
	if(entry == 3) then
		call op3pt(n)
	else if(entry == 9) then
		call op9pt(n)
	else if(entry == 27) then
		call op27pt(n)
	end if
end program