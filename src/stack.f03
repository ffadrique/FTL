module xxmodulebase___stack_ftl

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Reference : http://www.cplusplus.com/reference/queue/queue
! Synopsis  : Stack (LIFO) container template
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

  public xxtypebase___stack_ftl, xxconstructor___stack_ftl

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! Stack node type
  type t_node
    private

!   The element data instance
    class(xxtypebase__), pointer :: element => null()

!   Pointer to the down node in the stack (null if bottom)
    type(t_node),  pointer :: pdown => null()

  end type t_node


! Linked stack container type
  type, extends(t_object) :: xxtypebase___stack_ftl
    private

!   The number of nodes in the stack
    integer :: count   = 0

!   The top node
    type(t_node), pointer :: ptop => null()

!   The bottom node
    type(t_node), pointer :: pbottom => null()

    contains

!     Insertion and removal
      procedure :: push => stack_push
      procedure :: pop => stack_pop
      procedure :: clear => stack_clear

!     Access
      procedure :: top => stack_top
      procedure :: bottom => stack_bottom

!     Query
      procedure :: size => stack_size
      procedure :: empty => stack_empty

!     Conversion
      procedure :: array => stack_array

!     Assignment
      generic :: assignment(=) => stack_assign_from_stack, &
                                  stack_assign_from_array
      procedure :: stack_assign_from_stack
      procedure, private :: stack_assign_from_array

!     Destructor
      final :: stack_

  end type xxtypebase___stack_ftl


! Constructor interface
  interface xxconstructor___stack_ftl
    module procedure stack_default
    module procedure stack_copy
    module procedure stack_copy_from_array
  end interface xxconstructor___stack_ftl

!---End of declaration of module variables--------------------------------------

contains

! Default constructor
function stack_default( ) result(res)

! The result stack
  type(xxtypebase___stack_ftl) :: res

! Initialise
  res%ptop => null()
  res%pbottom => null()
  res%count = 0

end function stack_default


! Copy constructor
function stack_copy( stack ) result(res)

! The input stack
  type(xxtypebase___stack_ftl), intent(in) :: stack

! The result stack
  type(xxtypebase___stack_ftl) :: res

! Copy the stack
  res = stack

end function stack_copy


! Copy constructor from array
function stack_copy_from_array( a ) result(res)

! The input array
  class(xxtypebase__), dimension(:), intent(in) :: a

! The result stack
  type(xxtypebase___stack_ftl) :: res

! Copy the stack
  res = a

end function stack_copy_from_array


! Destructor
subroutine stack_( this )

! The stack
  type(xxtypebase___stack_ftl), intent(inout) :: this

! Clear the stack
  if( this%count > 0 ) call this%clear()

end subroutine stack_


! Add a node to the top of the stack
subroutine stack_push( this, element )

! The stack
  class(xxtypebase___stack_ftl), intent(inout) :: this

! The element
  class(xxtypebase__), intent(in) :: element

! Local node pointer
  type(t_node), pointer :: node

! Allocate the new node
  allocate( node )

! Check if stack already contains elements
  if( associated(this%ptop) ) then

!   Reasign new node pointers
    node%pdown => this%ptop
    this%ptop => node

  else

!   Assign pointers
    this%ptop => node
    this%pbottom => this%ptop

  end if

! Copy the element into its stack position
  call element_assign_pointer( node%element, element )

! Increase counter
  this%count = this%count + 1

end subroutine stack_push


! Remove node from the beginning of the stack
subroutine stack_pop( this )

! The stack
  class(xxtypebase___stack_ftl), intent(inout) :: this

! Local node pointer
  type(t_node), pointer :: node

! Check that the stack is not empty
  if( associated(this%ptop) ) then

!   Destroy data element in the top stack node
    deallocate( this%ptop%element )

!   Check if there is more than one node
    if( associated(this%ptop%pdown) ) then

!     More than one node in the stack; remove the top one
      node => this%ptop
      this%ptop => this%ptop%pdown
      deallocate( node )

    else

!     Only one node in stack; remove it
      deallocate( this%ptop )
      this%pbottom => null()

    end if

!   Decrease counter
    this%count = this%count - 1

  end if

end subroutine stack_pop


! Return the element in the top (down to extract) node in the stack
pure function stack_top( this ) result(res)

! The stack
  class(xxtypebase___stack_ftl), target, intent(in) :: this

! Pointer to the element in the top node in the stack
  class(xxtypebase__), allocatable :: res

! Assign the return value
  call element_assign_allocatable( res, this%ptop%element )

end function stack_top


! Return the element in the bottom (bottom to extract) node in the stack
pure function stack_bottom( this ) result(res)

! The stack
  class(xxtypebase___stack_ftl), target, intent(in) :: this

! Pointer to the element in the bottom node in the stack
  class(xxtypebase__), allocatable :: res

! Assign the return value
  call element_assign_allocatable( res, this%pbottom%element )

end function stack_bottom


! Destroy a stack
subroutine stack_clear( this )

! The stack
  class(xxtypebase___stack_ftl), intent(inout) :: this

! Local node pointers
  type(t_node), pointer :: del, down


! Check if empty stack
  if( associated( this%pbottom ) ) then

!   Initialise stack navigation
    del => this%ptop

!   Navigate the stack
    do while( associated(del) )

!     Save pointer to down node
      down => del%pdown

!     Destroy data element in the current stack node
      deallocate( del%element )

!     Deallocate the stack node
      deallocate( del )

!     Irerate
      del => down

    end do

  end if

! Reinitialise stack pointers
  this%ptop => null()
  this%pbottom => null()

! Reset counter
  this%count = 0

end subroutine stack_clear


! Return total length of stack
pure function stack_size( this ) result(res)

! The stack
  class(xxtypebase___stack_ftl), intent(in) :: this

! The stack size
  integer :: res

! Assign the return value
  res = this%count

end function stack_size


! Test whether stack is empty
pure function stack_empty( this ) result(res)

! The stack
  class(xxtypebase___stack_ftl), intent(in) :: this

! The stack empty status
  logical :: res

! Asign the return value
  res = ( this%count == 0 )

end function stack_empty


! Create a stack from an exisiting stack (assignment operator)
subroutine stack_assign_from_stack( this, other )

! The output stack
  class(xxtypebase___stack_ftl), intent(out) :: this

! The input stack
  type(xxtypebase___stack_ftl), intent(in) :: other

! Local node pointers
  type(t_node), pointer :: lptr, dptr

! Check if the input stack has elements
  if( associated(other%ptop) ) then

!   Initialise navigation pointer
    lptr => other%pbottom

!   Loop on the stack
    do while( .not. associated(lptr,other%ptop) )

!     Add element to the output stack
      call this%push( lptr%element )

!     Loop searching for the current pointer location
      dptr => other%ptop
      do while( .not. associated(dptr%pdown,lptr) )
        dptr => dptr%pdown
      end do

!     Iterate
      lptr => dptr

    end do

!   Add the top element
    call this%push(other%ptop%element)

  end if

end subroutine stack_assign_from_stack


! Create a stack from an array (assignment operator)
subroutine stack_assign_from_array( this, array )

! The output stack
  class(xxtypebase___stack_ftl), intent(out) :: this

! The input array
  class(xxtypebase__), dimension(:), intent(in) :: array

! Local counter
  integer :: i

! Loop on the input array
  do i = 1, size(array)

!   Add element to the output stack
    call this%push( array(i) )

  end do

end subroutine stack_assign_from_array


! Create an array (allocatable) of elements from a stack (bottom to top)
function stack_array( this ) result(res)

! The input stack
  class(xxtypebase___stack_ftl), intent(in) :: this

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
    lptr => this%ptop

!   Loop on the elements
    do i = this%size(), 1, -1
      res(i) = lptr%element
      lptr => lptr%pdown
    end do

  end if

end function stack_array


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

! Assign explicitly to allow invoking the assignment operator if implemented in the element
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

! Assign explicitly to allow invoking the assignment operator if implemented in the element
  left = right

end subroutine element_assign_allocatable

end module xxmodulebase___stack_ftl
