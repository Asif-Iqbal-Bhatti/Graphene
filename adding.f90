 PROGRAM adding
 IMPLICIT NONE
!  This program reads n points from a data file arrays x, y, z.
 INTEGER            :: n, i, io
 INTEGER, PARAMETER :: nmax = 1000, MAX_PATH_LEN = 100, u = 1
 REAL               :: x(nmax), y(nmax), z(nmax)
 REAL, PARAMETER    :: g = 2.714583
 !      Open the data file
  OPEN (u, FILE = "iridium.ascii", STATUS ='OLD', ACTION = 'READ')
!          Read the number of points
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
!  Loop over the data points
  DO  i= 1, n
    READ(u,*) x(i), y(i), z(i)
    !write(*,*) x(i), y(i), z(i)
  ENDDO
  CLOSE(u)
  OPEN (UNIT = 10, FILE = "iridium.ascii", STATUS='REPLACE', ACTION='WRITE')
  
  DO  i= 1, n
    WRITE(10,'(a,4(2x,F12.6))')   'Ir', x(i), y(i) + g, z(i)
  ENDDO
 CLOSE (10)

 9999 STOP
 END PROGRAM adding


!program count_lines
!   implicit none
!   integer:: n
!   character(len=60):: cmd
!    cmd = "cat file_name.dat | grep '[^ ]' | wc -l > nlines.txt"
!    call system(cmd)
!    open(1,file='nlines.txt')
!    read(1,*) n
!    print*, "Number of lines are", n 
!    cmd = 'rm nlines.txt'
!    call system(cmd)
!end program 
