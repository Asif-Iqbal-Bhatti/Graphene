PROGRAM initialization
! Building a FCC lattice
IMPLICIT NONE

INTEGER, PARAMETER :: d = 8 !  dimension of the box in angstrom
INTEGER, PARAMETER :: Ntot = 8*d*d
REAL*8, DIMENSION (Ntot,3)  :: pos
REAL*8, PARAMETER	:: a = 3.89 ! in angstrom
INTEGER	:: i,j,k

 OPEN (UNIT = 10, FILE = "slab.ascii", STATUS = "replace")
 !build the lattice
 k=1
 !first layer
do i=0,d-1
   do j=0,d-1
		pos(k,1)=a*i
		pos(k,2)=a*j
		pos(k,3)=0
		
		pos(k+1,1)=a*(i+0.5)
		pos(k+1,2)=a*(j+0.5)
		pos(k+1,3)=0
		
		k=k+2
	end do
end do
!second layer
do i=0,d-1
   do j=0,d-1
		pos(k,1)=a*i
		pos(k,2)=a*(j+0.5)
		pos(k,3)=a*0.5
        	pos(k+1,1)=a*(i+0.5)
		pos(k+1,2)=a*j
		pos(k+1,3)=a*0.5
        
		k=k+2
	end do
end do
!third layer
do i=0,d-1
   do j=0,d-1
		pos(k,1)=a*i
		pos(k,2)=a*j
		pos(k,3)=a
        
		pos(k+1,1)=a*(i+0.5)
		pos(k+1,2)=a*(j+0.5)
		pos(k+1,3)=a
        
		k=k+2
	end do
end do
!fourth layer
do i=0,d-1
   do j=0,d-1
		pos(k,1)=a*i
		pos(k,2)=a*(j+0.5)
		pos(k,3)=a*1.5
       
		pos(k+1,1)=a*(i+0.5)
		pos(k+1,2)=a*j
		pos(k+1,3)=a*1.5
       
		k=k+2
	end do
end do  
  do k= 1, Ntot
     write(10,'(a,3(3x,F18.10))') 'C', (pos(k,i),i=1,3)
  enddo
 
 close(10)
 
 
END PROGRAM initialization
