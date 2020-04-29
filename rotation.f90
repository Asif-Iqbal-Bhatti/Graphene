 PROGRAM rotation
 IMPLICIT NONE
!  This program reads n points from a data file arrays x, y, z.
 INTEGER            :: n, i, io
 INTEGER, PARAMETER :: nmax = 10000, MAX_PATH_LEN = 100, u = 1
 REAL               :: x(nmax), y(nmax), z(nmax), x1(nmax), y2(nmax), z3(nmax), SIND
 REAL, PARAMETER    :: omega = 60. ! angle between lattice vectors
 REAL, PARAMETER    :: alpha = -6.4 ! change rotation by alpha amount
 REAL,  PARAMETER   :: p1 = 1., p2 = 1.     ! scqling fqctors for lattice vectors
 REAL, PARAMETER    :: q = 1.     ! ratio btw original lattice vectors for hexognal
                                                          
  OPEN (u, FILE = "iridium.ascii", STATUS ='OLD', ACTION = 'READ') ! Open the data file
!!!!!!!!!!!!!!!!!!!! Read the number of points!!!!!!!!!!!!!!!!!!!!!!!
  n = 0
  DO
    READ(u,*,IOSTAT = io) 
    IF (io .ne. 0) EXIT
    n = n + 1
  ENDDO
  rewind(u)

  PRINT*, "Number of lines are", n      
  IF (n.GT.nmax) then
    WRITE(*,*) 'Error: n = ', n, 'is larger than nmax =', nmax
    GOTO 9999
  ENDIF
!!!!!!!!!!!!!!!!!!  Loop over the data points!!!!!!!!!!!!!!!!
  DO  i = 1, n
    READ(u,*) x(i), y(i), z(i)
!!!!!!!!!!!!!!!!!write(*,*) x(i), y(i), z(i)!!!!!!!!!!!!!!
  ENDDO
  CLOSE(u)
  !!!!!!!!!!!!!!!!here is the rotation by angle alpha!!!!!!!!!!!  
  DO i = 1, n
    x1(i) = (1/SIND(omega))*(p1*SIND(omega - alpha)*x(i) + q*p1*SIND(alpha)*y(i))  
    y2(i) = (1/SIND(omega))*((-p2/q)*SIND(alpha)*x(i) + p2*SIND(omega + alpha)*y(i))
  ENDDO

  OPEN (UNIT = 10, FILE = "rotation.ascii", STATUS='REPLACE', ACTION='WRITE')
   write(10,*) n
   write(10,*)
  DO  i= 1, n
    WRITE(10,'(a,4(2x,F12.6))')   'Ir', x1(i), y2(i), z3(i)
  ENDDO

 CLOSE (10)
 9999 STOP
 END PROGRAM rotation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   REAL FUNCTION SIND(x)
    IMPLICIT NONE
    REAL, INTENT(IN) :: x
    REAL  :: Degreetoradian
    SIND = SIN(Degreetoradian(x))

   END FUNCTION SIND

   REAL FUNCTION Degreetoradian(degree)
    IMPLICIT NONE
    REAL, INTENT(IN)  :: degree
     Real, PARAMETER    :: PI = 3.1415926
    Degreetoradian = degree * PI/180.
  END FUNCTION Degreetoradian

 



