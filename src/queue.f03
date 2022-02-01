module xxmodulebase___queue_ftl

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Reference : http://www.cplusplus.com/reference/queue/queue
! Synopsis  : Queue (FIFO) container template
!             Limitations with repsect to STL C++
!              - No emplace functions.
!              - No swap functions.
!
! License   : This file is part of the Fortran Template Library (FTL).
!
!             FTL is free software: you can redistribute it and/or modify
!             it under the terms of the GNU Lesser General Public License as
!             published by the Free Software Foundation, either version 3 of
!             the License, or (at your option) any later version.
!
!             FTL is distributed in the hope that it will be useful,
!             but WITHOUT ANY WARRANTY; without even the implied warranty of
!             MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!             See the GNU Lesser General Public License for more details.
!
!             You should have received a copy of the GNU Lesser General Public
!             License along with FTL.  
!             If not, see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_object
  use xxuse__

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public xxtypebase___queue_ftl, xxconstructor___queue_ftl

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! Queue node type
  type t_node
    private

!   The element data instance
    class(xxtypebase__),  pointer :: element => null()

!   Pointer to the next mode in the queue (null if last)
    type(t_node), pointer :: next    => null()

  end type t_node


! Linked queue container type
  type, extends(t_object) :: xxtypebase___queue_ftl
    private

!     The number of nodes in the queue
      integer :: count   = 0

!     The first node
      type(t_node), pointer :: first => null()

!     The last node
      type(t_node), pointer :: last => null()

    contains

!     Insertion and removal
      procedure :: push => queue_push
      procedure :: pop => queue_pop
      procedure :: clear => queue_clear

!     Access
      procedure :: front => queue_front
      procedure :: back => queue_back

!     Query
      procedure :: size => queue_size
      procedure :: empty => queue_empty

!     Conversion
      procedure :: array => queue_array

!     Assignment
      generic :: assignment(=) => queue_assign_from_queue, &
                                  queue_assign_from_array
      procedure :: queue_assign_from_queue
      procedure, private :: queue_assign_from_array

!     Destructor
      final :: queue_

  end type xxtypebase___queue_ftl


! Constructor interface
  interface xxconstructor___queue_ftl
    module procedure queue_default
    module procedure queue_copy
    module procedure queue_copy_from_array
  end interface xxconstructor___queue_ftl

!---End of declaration of module variables--------------------------------------

contains

! Default constructor
function queue_default( ) result(res)

! The result queue
  type(xxtypebase___queue_ftl) :: res

! Initialise
  res%first => null()
  res%last => null()
  res%count = 0

end function queue_default


! Copy constructor
function queue_copy( this ) result(res)

! The input queue
  type(xxtypebase___queue_ftl), intent(in) :: this

! The result queue
  type(xxtypebase___queue_ftl) :: res

! Copy the queue
  res = this

end function queue_copy


! Copy constructor from array
function queue_copy_from_array( a ) result(res)

! The input array
  class(xxtypebase__), dimension(:), intent(in) :: a

! The result queue
  type(xxtypebase___queue_ftl) :: res

! Copy the queue
  res = a

end function queue_copy_from_array


! Destructor
subroutine queue_( this )

! The queue
  type(xxtypebase___queue_ftl), intent(inout) :: this

! Clear the queue
  if( this%count > 0 ) call this%clear()

end subroutine queue_


! Add a node to the end of the queue
subroutine queue_push( this, element )

! The queue
  class(xxtypebase___queue_ftl), intent(inout) :: this

! The element
  class(xxtypebase__), intent(in) :: element

! Check if queue already contains elements
  if( associated(this%last) ) then

!   Allocate new node
    allocate( this%last%next )

!   Reasign new node pointers
    this%last => this%last%next

  else

!   Allocate memory for first node
    allocate(this%first)

!   Assign pointers
    this%last => this%first

  end if

! Copy the element into its queue position
  call element_assign_pointer( this%last%element, element )

! Increase counter
  this%count = this%count + 1

end subroutine queue_push


! Remove node from the beginning of the queue
subroutine queue_pop( this )

! The queue
  class(xxtypebase___queue_ftl), intent(inout) :: this

! Local node pointer
  type(t_node), pointer :: node

! Check that the queue is not empty
  if( associated(this%first) ) then

!   Destroy data element in the first queue node
    deallocate( this%first%element )

!   Check if there is more than one node
    if( associated(this%first%next) ) then

!     More than one node in the queue; remove the first one
      node => this%first
      this%first => this%first%next
      deallocate( node )

    else

!     Only one node in queue; remove it
      deallocate( this%first )
      this%last  => null()

    end if

!   Decrease counter
    this%count = this%count - 1

  end if

end subroutine queue_pop


! Return the element in the first (next to extract) node in the queue
pure function queue_front( this ) result(res)

! The queue
  class(xxtypebase___queue_ftl), target, intent(in) :: this

! Pointer to the element in the first node in the queue
  class(xxtypebase__), allocatable :: res

! Assign the return value
  call element_assign_allocatable( res, this%first%element )

end function queue_front


! Returns the element in the last (last to extract) node in the queue
pure function queue_back( this ) result(res)

! The queue
  class(xxtypebase___queue_ftl), target, intent(in) :: this

! Pointer to the element in the first node in the queue
  class(xxtypebase__), allocatable :: res

! Assign the return value
  call element_assign_allocatable( res, this%last%element )

end function queue_back


! Destroy a queue
subroutine queue_clear( this )

! The queue
  class(xxtypebase___queue_ftl), intent(inout) :: this

! Local node pointers
  type(t_node), pointer :: del, next

! Check if empty queue
  if( associated( this%last ) ) then

!   Initialise queue navigation
    del => this%first

!   Navigate the queue
    do while( associated(del) )

!     Save pointer to next node
      next => del%next

!     Destroy data element in the current queue node
      deallocate( del%element )

!     Deallocate the queue node
      deallocate( del )

!     Irerate
      del => next

    end do

  end if

! Reinitialise queue pointers
  this%first => null()
  this%last => null()

! Reset counter
  this%count = 0

end subroutine queue_clear


! Return total length of queue
pure function queue_size( this ) result(res)

! The queue
  class(xxtypebase___queue_ftl), intent(in) :: this

! The queue size
  integer :: res

! Assign the return value
  res = this%count

end function queue_size


! Test whether queue is empty
pure function queue_empty( this ) result(res)

! The queue
  class(xxtypebase___queue_ftl), intent(in) :: this

! The queue empty status
  logical :: res

! Asign the return value
  res = ( this%count == 0 )

end function queue_empty


! Create a queue from an exisiting queue (assignment operator)
subroutine queue_assign_from_queue( this, other )

! The output queue
  class(xxtypebase___queue_ftl), intent(out) :: this

! The input queue
  type(xxtypebase___queue_ftl), intent(in) :: other

! Local node pointers
  type(t_node), pointer :: lptr

! Initialise navigation pointer
  lptr => other%first

! Loop on the queue
  do while( associated(lptr) )

!   Add element to the output queue
    call this%push( lptr%element )

!   Iterate
    lptr => lptr%next

  end do

end subroutine queue_assign_from_queue


! Create a queue from an array (assignment operator)
subroutine queue_assign_from_array( this, array )

! The output queue
  class(xxtypebase___queue_ftl), intent(out) :: this

! The input array
  class(xxtypebase__), dimension(:), intent(in) :: array

! Local counter
  integer :: i

! Loop on the input array
  do i = 1, size(array)

!   Add element to the output queue
    call this%push( array(i) )

  end do

end subroutine queue_assign_from_array


! Create an array (allocatabe) of elements from a queue
function queue_array( this ) result(res)

! The input queue
  class(xxtypebase___queue_ftl), intent(in) :: this

! The returned array of elements (unallocated if memory failure)
  class(xxtypebase__), allocatable, dimension(:) :: res

! Local node pointers
  type(t_node), pointer :: lptr

! Local counter
  integer :: i

! Memory allocation status
  integer :: status

! Allocate memory for returned array
  allocate( res( this%size() ), stat=status )
  if( status == 0 ) then

!   Initialise navigation pointer
    lptr => this%first

!   Loop on the elements
    do i = 1, this%size()
      res(i) = lptr%element
      lptr => lptr%next
    end do

  end if

end function queue_array


! Implement the assignment between two elements (contained in the container node)
! Centralises the implementation allowing the handling of polymorphism (store parent classes pointing derived clasess)
! at the time thta allows the invocation of assignment operators in the cases when the element implements it
pure subroutine element_assign_pointer( left, right )

! Element to be allocated and assigned (pointer interface)
  class(xxtypebase__), pointer, intent(inout) :: left

! Source element
  class(xxtypebase__), intent(in) :: right

! Allocate first. Use mold to allow polymorphic object storage through parent class
  allocate( left, mold=right )

! Assign explicitly to allow invoking the assignment operator if available
  left = right

end subroutine element_assign_pointer


! Implement the assignment between two elements (contained in the container node)
! Centralises the implementation allowing the handling of polymorphism (store parent classes pointing derived clasess)
! at the time thta allows the invocation of assignment operators in the cases when the element implements it
pure subroutine element_assign_allocatable( left, right )

! Element to be allocated and assigned (allocatable interface)
  class(xxtypebase__), allocatable, intent(inout) :: left

! Source element
  class(xxtypebase__), intent(in) :: right

! Allocate first. Use mold to allow polymorphic object storage through parent class
  allocate( left, mold=right )

! Assign explicitly to allow invoking the assignment operator if available
  left = right

end subroutine element_assign_allocatable

end module xxmodulebase___queue_ftl
