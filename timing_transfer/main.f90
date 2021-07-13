program main
  
  use mpi_f08
  use module_some_mpi
  implicit none
  integer, parameter            :: sender=0, receiver=1
  integer                       :: length, rank, nproc, ierr, i, k
  type(doublet), allocatable    :: vector_of_doublet(:)
  
  ! timing
  real(8)                       :: t0, t1

  Call initialize_MPI(rank, nproc, ierr)
  
  if (rank==0) write(*,*) "start test of transfer "
  
  Call initialize_mpi_doublet_and_triplet() 
  
  ! here you could start a loop and record the time for the transfer as a function of length

!  length =1000000; allocate(vector_of_doublet(1:length))
!      do i=1, length
!    vector_of_doublet(i)%g=[1,2,3,5,5,6]
!    vector_of_doublet(i)%z=dcmplx(1,1)
!  enddo ! i                                                                                                                                                  

!  do i=1,4
!     t0 = MPI_Wtime()
!     length = i**2 * int(10.d0**i)
!     Call send_and_receive_vector_of_doublet(sender, receiver, length, vector_of_doublet)
  !     t1 = MPI_Wtime()


  do i=1,9
!    length = 10**i
    length = int(10.d0**i)
    allocate(vector_of_doublet(1:length))
    do k=1, length
    vector_of_doublet(i)%g=[1,2,3,5,5,6]
    vector_of_doublet(i)%z=dcmplx(1,1)
    enddo !k
    t0 = MPI_Wtime()
        Call send_and_receive_vector_of_doublet(sender, receiver, length, vector_of_doublet)
    t1 = MPI_Wtime()
    deallocate(vector_of_doublet) 


  if (rank==0) write(*,*) "length, nproc, time_of_transfer, nproc =  ", length, nproc, t1 - t0

  enddo ! i                                                                                                                                                  


   call MPI_Finalize()
  
  ! as far as I can see, one needs fairly large values of "length" to get 
  ! time of transfer ~ length
  

end program main
