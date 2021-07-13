! Author : Dietrich Foerster 
! contact: dietrich.foerster@u-bordeaux.fr
! date : 10/6/2021


module module_some_mpi

use mpi_f08

type(MPI_Datatype)  :: mpi_triplet, mpi_doublet

type :: triplet
integer   :: m1
integer   :: m2
complex(8)::z
end type 

type :: doublet
integer   :: g(1:6)
complex(8)::z
end type 


contains


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine receive_vector_of_triplet(sender, receiver, length, vector_of_triplet)
use mpi_f08
! extern
integer                       :: sender, receiver, length
type(triplet)                  :: vector_of_triplet(1:length)
! intern
integer                       :: rank, ierr
integer, parameter            :: tag=100
type(MPI_Datatype)            :: mpi_triplet_vector
type(MPI_Status)              :: msgstatus
call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
if (rank == receiver) then
  Call MPI_Type_contiguous(length, mpi_triplet, mpi_triplet_vector, ierr)
  Call MPI_Type_commit(mpi_triplet_vector, ierr)
  call MPI_RECV(vector_of_triplet,1,mpi_triplet_vector,sender,tag,MPI_COMM_WORLD, msgstatus,ierr)
  Call MPI_TYPE_FREE(mpi_triplet_vector,ierr)
endif ! rank == receiver
end subroutine receive_vector_of_triplet



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine send_and_receive_vector_of_triplet(sender, receiver, length, vector_of_triplet)
use mpi_f08
! extern
integer                       :: sender, receiver, length
type(triplet)                 :: vector_of_triplet(1:length)
! intern
integer                       :: ierr, rank
integer, parameter            :: tag=100
type(MPI_Datatype)            :: mpi_triplet_vector
type(MPI_Status)              :: msgstatus

if (sender /=receiver) then

  call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
  Call MPI_Type_contiguous(length, mpi_triplet, mpi_triplet_vector, ierr)
  Call MPI_Type_commit(mpi_triplet_vector, ierr)
  
  if (rank == sender) then  
     Call MPI_SEND(vector_of_triplet,1,mpi_triplet_vector,receiver,tag,MPI_COMM_WORLD,ierr)
  endif ! rank == sender

  if (rank==receiver) then
     Call MPI_RECV(vector_of_triplet,1,mpi_triplet_vector,sender,tag,MPI_COMM_WORLD, msgstatus,ierr)
  endif ! rank==receiver  

  Call MPI_TYPE_FREE(mpi_triplet_vector,ierr)
  
endif !  sender /=receiver

end subroutine send_and_receive_vector_of_triplet




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine send_vector_of_triplet(sender, receiver, length, vector_of_triplet)
use mpi_f08
! extern
integer                       :: sender, receiver, length
type(triplet)                 :: vector_of_triplet(1:length)
! intern
integer                       :: ierr, rank
integer, parameter            :: tag=100
type(MPI_Datatype)            :: mpi_triplet_vector
call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
if (rank == sender) then
  Call MPI_Type_contiguous(length, mpi_triplet, mpi_triplet_vector, ierr)
  Call MPI_Type_commit(mpi_triplet_vector, ierr)
  Call MPI_SEND(vector_of_triplet,1,mpi_triplet_vector,receiver,tag,MPI_COMM_WORLD,ierr)
  Call MPI_TYPE_FREE(mpi_triplet_vector,ierr)
endif ! rank == sender
end subroutine send_vector_of_triplet


subroutine send_and_receive_vector_of_doublet(sender, receiver, length, vector_of_doublet)
use mpi_f08
implicit none
! extern
integer                       :: sender, receiver, length
type(doublet)                 :: vector_of_doublet(1:length)
! intern
integer                       :: ierr, rank
integer, parameter            :: tag=100
type(MPI_Datatype)            :: mpi_doublet_vector
type(MPI_Status)              :: msgstatus

if (receiver /=sender) then

  Call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
  Call MPI_Type_contiguous(length, mpi_doublet, mpi_doublet_vector, ierr)
  Call MPI_Type_commit(mpi_doublet_vector, ierr)
  
  if (rank == sender) then
     Call MPI_SEND(vector_of_doublet,1,mpi_doublet_vector,receiver,tag,MPI_COMM_WORLD,ierr)
  endif ! rank == sender  

  if (rank == receiver) then
    call MPI_RECV(vector_of_doublet,1,mpi_doublet_vector,sender,tag,MPI_COMM_WORLD, msgstatus,ierr)
  endif !
   
  Call MPI_TYPE_FREE(mpi_doublet_vector,ierr)
  
endif ! receiver /=sender 

end subroutine send_and_receive_vector_of_doublet




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine send_vector_of_doublet(sender, receiver, length, vector_of_doublet)
use mpi_f08
! extern
integer                       :: sender, receiver, length
type(doublet)                 :: vector_of_doublet(1:length)
! intern
integer                       :: ierr, rank
integer, parameter            :: tag=100
type(MPI_Datatype)            :: mpi_doublet_vector
call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
if (rank == sender) then
  Call MPI_Type_contiguous(length, mpi_doublet, mpi_doublet_vector, ierr)
  Call MPI_Type_commit(mpi_doublet_vector, ierr)
  Call MPI_SEND(vector_of_doublet,1,mpi_doublet_vector,receiver,tag,MPI_COMM_WORLD,ierr)
  Call MPI_TYPE_FREE(mpi_doublet_vector,ierr)
endif ! rank == sender
end subroutine send_vector_of_doublet




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine receive_vector_of_doublet(sender, receiver, length, vector_of_doublet)
use mpi_f08
! extern
integer                       :: sender, receiver, length
type(doublet)                :: vector_of_doublet(1:length)
! intern
integer                       :: rank, ierr
integer, parameter            :: tag=100
type(MPI_Datatype)            :: mpi_doublet_vector
type(MPI_Status)              :: msgstatus
call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
if (rank == receiver) then
  Call MPI_Type_contiguous(length, mpi_doublet, mpi_doublet_vector, ierr)
  Call MPI_Type_commit(mpi_doublet_vector, ierr)
  call MPI_RECV(vector_of_doublet,1,mpi_doublet_vector,sender,tag,MPI_COMM_WORLD, msgstatus,ierr)
  Call MPI_TYPE_FREE(mpi_doublet_vector,ierr)
endif ! rank == receiver
end subroutine receive_vector_of_doublet



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine initialize_MPI(myrank,nproc, ierr)
use mpi_f08
call MPI_INIT(ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD,myrank,ierr)
CALL MPI_COMM_SIZE (MPI_COMM_WORLD, nproc, ierr) 
end subroutine initialize_MPI


subroutine initialize_mpi_doublet_and_triplet()
use mpi_f08
implicit none
Call initialize_mpi_type_doublet() 
Call initialize_mpi_type_triplet() 
end subroutine initialize_mpi_doublet_and_triplet

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine initialize_mpi_type_doublet()
! purpose : MPI type corresponding to type doublet {g(1:6),z}
! type   :: doublet; integer   :: g(1:6); complex(8)::z; end type doublet
use mpi_f08
implicit none
! intern
integer                                      :: ierr 
type(doublet)                                :: doublet_data
type(MPI_Datatype)                           :: temporary_mpi_type_doublet
integer                                      :: block_lengths_of_doublet(2)
type(MPI_Datatype)                           :: types_in_doublet(2)
integer(kind=MPI_ADDRESS_KIND)               :: displacements_in_doublet(2),addresses_in_doublet(2)
integer(kind=MPI_ADDRESS_KIND)               :: lb,extent

! creating the MPI datatype "mpi_type_doublet" corresponding to the scalar type doublet
types_in_doublet=[MPI_INTEGER, MPI_DOUBLE_COMPLEX]
block_lengths_of_doublet =[6,1]
call MPI_GET_ADDRESS(doublet_data%g,addresses_in_doublet(1),ierr)
call MPI_GET_ADDRESS(doublet_data%z,addresses_in_doublet(2),ierr)

! Calculation of displacements relative to the start address in triplet
displacements_in_doublet(:)=addresses_in_doublet(:) - addresses_in_doublet(1)

! creation of the MPI type "temporary_mpi_type_doublet"
call MPI_TYPE_CREATE_STRUCT(2,block_lengths_of_doublet,displacements_in_doublet,types_in_doublet,&
                            & temporary_mpi_type_doublet, ierr)

! necessary according to the MPI report 
Call MPI_TYPE_GET_EXTENT(temporary_mpi_type_doublet, LB, EXTENT, IERR)

! I do not know whether this MPI_TYPE_CREATE_RESIZED is necessary
call MPI_TYPE_CREATE_RESIZED(temporary_mpi_type_doublet,lb,extent,mpi_doublet,ierr)

! Validation of the MPI datatype "mpi_doublet"
call MPI_TYPE_COMMIT(mpi_doublet,ierr)

end subroutine initialize_mpi_type_doublet





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine initialize_mpi_type_triplet()
! purpose : create MPI type corresponding to type triplet {m1,m2,z}
! type :: triplet; integer   :: m1; integer   :: m2; complex(8)::z; end type triplet
use mpi_f08
implicit none
! intern
integer                                      :: ierr 
type(triplet)                                :: triplet_data
type(MPI_Datatype)                           :: temporary_mpi_type_triplet
integer                                      :: block_lengths_of_triplet(3)
type(MPI_Datatype)                           :: types_in_triplet(3)
integer(kind=MPI_ADDRESS_KIND)               :: displacements_in_triplet(3),addresses_in_triplet(3)
integer(kind=MPI_ADDRESS_KIND)               :: lb,extent

! creating the MPI datatype "mpi_triplet" corresponding to the scalar type triplet
types_in_triplet=[MPI_INTEGER, MPI_INTEGER, MPI_DOUBLE_COMPLEX]
block_lengths_of_triplet =[1,1,1]
call MPI_GET_ADDRESS(triplet_data%m1,addresses_in_triplet(1),ierr)
call MPI_GET_ADDRESS(triplet_data%m2,addresses_in_triplet(2),ierr)
call MPI_GET_ADDRESS(triplet_data%z ,addresses_in_triplet(3),ierr)

! Calculation of displacements relative to the start address in triplet
displacements_in_triplet(:)=addresses_in_triplet(:) - addresses_in_triplet(1)

! creation of the MPI type "temporary_mpi_type_triplet"
call MPI_TYPE_CREATE_STRUCT(3,block_lengths_of_triplet,displacements_in_triplet,types_in_triplet,temporary_mpi_type_triplet, ierr)

! necessary according to the MPI report 
Call MPI_TYPE_GET_EXTENT(temporary_mpi_type_triplet, LB, EXTENT, IERR)

! I do not know whether this MPI_TYPE_CREATE_RESIZED is necessary
call MPI_TYPE_CREATE_RESIZED(temporary_mpi_type_triplet,lb,extent,mpi_triplet,ierr)

! Validation of the MPI datatype "mpi_type_triplet"
call MPI_TYPE_COMMIT(mpi_triplet,ierr)

end subroutine initialize_mpi_type_triplet


end module module_some_mpi
