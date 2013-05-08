program matgen
	use op3pt_mod
	use op9pt_mod
	use op27pt_mod
	implicit none
	
	integer :: entry
	read *, entry
	if(entry == 3) then
		call op3pt()
	else if(entry == 9) then
		call op9pt()
	else if(entry == 27) then
		call op27pt()
	end if
end program