Program my_3facto
 implicit none
  integer n,factn,i,mf
   print*, 'please enter n'
  read(*,*)n
  call  factoriel(n,factn)
  call  modfac(factn,mf)
   print*,'n!=', factn
 End program
!***********************************************
 SUBROUTINE factoriel(n,factn)
   integer n,factn,i,fact
    IF ( n < 0) THEN
		print*, 'factorial is singular for negative integers' 
	   ELSE IF (   n > 0) THEN
       	 fact = 1.0
        do i = 2, n
          fact = fact * i
        enddo
	   factn=fact
    End if
   RETURN
END
!***********************************************
 SUBROUTINE modfac(factn,mf)
   integer factn,mf,J
	 IF (mod(factn,10) > 0) THEN
     	print*, 'f(n)=' , mod(factn,10) 
	   ELSE IF (  mod(factn,10) .eq. 0) THEN
    	  j=1
		  mf=factn/(10**j)
	  do while (mod(mf,10) .eq. 0)       
            mf=factn/(10**j) 
            j = j + 1
          end do  
		  if (  mod(mf,10) > 0) THEN
		  print*, 'f(n)=' , mod(mf,10)
		  end if 
	END IF   
   RETURN
END
