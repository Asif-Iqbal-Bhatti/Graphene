program benzene
 implicit none
 integer :: i,ibase, nbase, ix, iy, iz, l 
 integer, allocatable, dimension(:) :: n
 real(kind=8),dimension(3) :: pos
 real(kind=8), allocatable,dimension(:,:) :: base, vec, trans


  ! Everything defined in Angstroms

  nbase = 4 ! 2 atoms in unit cell for orthognol 4

  allocate(base(3,nbase))
  allocate(vec(3,3)) 
  allocate(trans(3,3)) 
  allocate(n(3))

  ! Specify super-cell
  n(1) = 4
  n(2) = 3
  n(3) = 1

  !Specify basis (could replace this by read in file) position of the atoms in the unit cell
  base(1,1) = 0.0  ! Three coord are needed for the pos of atom
  base(2,1) = 0.0  ! 
  base(3,1) = 0.0   !
  
  base(1,2) = 1.42
  base(2,2) = 0.0
  base(3,2) = 0.0
  
  base(1,3) = 1.42 + 0.71
  base(2,3) = (1.42 * SQRT(3.0)) / 2.
  base(3,3) = 0.0
  
  base(1,4) = 1.42*2 + 0.71
  base(2,4) = (1.42 * SQRT(3.0)) / 2.
  base(3,4) = 0.0
  
l = 0

! Specify lattice vectors (could also read in file)
  vec(1,1) = 4.278  !x 
  vec(2,1) = 0.0
  vec(3,1) = 0.0
  vec(1,2) = 0.0
  vec(2,2) = SQRT(3.0) * 1.42 !y
  vec(3,2) = 0.0
  vec(1,3) = 0.0
  vec(2,3) = 0.0
  vec(3,3) = 10.0    !z


 OPEN (UNIT = 1, FILE = "graphene_ortho.ascii", STATUS = "replace")
 write(1,*) n(1)*n(2)*n(3)*nbase
   write(1,*)
  do ix = 1, n(1)
     trans(1,1) = (ix-1) * vec(1,1)
     trans(2,1) = (ix-1) * vec(2,1)
     trans(3,1) = (ix-1) * vec(3,1)

     do iy = 1, n(2)
         trans(1,2) = (iy-1) * vec(1,2)
         trans(2,2) = (iy-1) * vec(2,2)
         trans(3,2) = (iy-1) * vec(3,2)
         do iz = 1, n(3)
            trans(1,3) = (iz-1) * vec(1,3)
            trans(2,3) = (iz-1) * vec(2,3)
            trans(3,3) = (iz-1) * vec(3,3)
            do  ibase = 1, nbase
                pos(1) = base(1,ibase) + trans(1,1) + trans(1,2) + trans(1,3)
                pos(2) = base(2,ibase) + trans(2,1) + trans(2,2) + trans(2,3)
                pos(3) = base(3,ibase) + trans(3,1) + trans(3,2) + trans(3,3)

               write(*,'(a,3(2x,F12.6))') 'C', (pos(i),i=1,3)
               write(1,'(a,3(2x,F12.6))') 'C', (pos(i),i=1,3)
           end do
        end do
     end do
  end do
  close(1)
  deallocate( base)
  deallocate(vec)
  deallocate(trans)
  deallocate(n)

end program
