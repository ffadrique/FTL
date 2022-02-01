module xxmodulebase___slist_ftl

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Reference : http://www.cplusplus.com/reference/list
! Synopsis  : Single linked list container template
!             Despite of the STL forward_list, this container has been built
!             to mimic the list functionality in the understanding of the
!             limitations of slist with respect to reverse iteration.
!             Limitations with repsect to STL C++
!              - No reverse iteration.
!              - No constant iterators.
!              - Max size is dummy (dummy value not computed from architecture).
!              - No emplace functions.
!              - Splice can append lists with an extension on the STL C++
!                interface that emulates the past-last-element with null iterator
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

  public xxtypebase___slist_ftl, xxconstructor___slist_ftl
  public xxtypebase___slist_ftl_iterator

  public distance, swap

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! List node type
  type t_slist_node
    private

!   The element data instance
    class(xxtypebase__), pointer :: element => null()

!   Pointer to the next mode in the list (null if last)
    type(t_slist_node),  pointer :: next    => null()

  end type t_slist_node


! Single linked list container type
  type, extends(t_object) :: xxtypebase___slist_ftl
    private

!   The number of nodes in the list
    integer :: count = 0

!   The first node
    type(t_slist_node), pointer :: first => null()

!   The last node
    type(t_slist_node), pointer :: last  => null()

    contains

!     Assign content
      generic :: assignment(=) => slist_assign_from_slist
      procedure :: slist_assign_from_slist

!     Iterators
      procedure :: begin => slist_begin
      procedure :: end => slist_end

!     Capacity
      procedure :: empty => slist_empty
      procedure :: size => slist_size
      procedure, nopass :: max_size => slist_max_size

!     Element access
      procedure :: front => slist_front
      procedure :: back => slist_back

!     Modifiers
      generic :: assign => slist_assign_from_slist, &
                           slist_assign_from_range, &
                           slist_assign_from_fill, &
                           slist_assign_from_array
      procedure, private :: slist_assign_from_range
      procedure, private :: slist_assign_from_fill
      procedure, private :: slist_assign_from_array
      procedure :: push_front => slist_push_front
      procedure :: pop_front => slist_pop_front
      procedure :: push_back => slist_push_back
      procedure :: pop_back => slist_pop_back
      generic :: insert => slist_insert_single, &
                           slist_insert_fill, &
                           slist_insert_range, &
                           slist_insert_array
      procedure, private :: slist_insert_single
      procedure, private :: slist_insert_fill
      procedure, private :: slist_insert_range
      procedure, private :: slist_insert_array
      generic :: erase => slist_erase_single, &
                          slist_erase_range
      procedure, private :: slist_erase_single
      procedure, private :: slist_erase_range
      procedure :: swap => slist_swap
      procedure :: resize => slist_resize
      procedure :: clear => slist_clear

!     Operations
      generic :: splice => slist_splice_slist, &
                           slist_splice_range, &
                           slist_splice_single
      procedure, private :: slist_splice_slist
      procedure, private :: slist_splice_range
      procedure, private :: slist_splice_single
      procedure, private :: slist_splice_nodes
      procedure :: remove => slist_remove
      procedure :: remove_if => slist_remove_if
      procedure :: unique => slist_unique
      procedure :: merge => slist_merge
      procedure :: sort => slist_sort
      procedure, private :: quick_sort
      procedure :: reverse => slist_reverse

!     Selection
      procedure :: binary_search => slist_binary_search
      procedure :: select => slist_select
      procedure :: at => slist_at_get

!     Conversion
      procedure :: array => slist_array

!     Destructor
      final :: slist_

  end type xxtypebase___slist_ftl


! Constructor interface
  interface xxconstructor___slist_ftl
    module procedure slist_default
    module procedure slist_fill
    module procedure slist_range
    module procedure slist_copy
    module procedure slist_copy_from_array
  end interface xxconstructor___slist_ftl


! Interface to provide user comparison functions
  abstract interface
    pure function comparison( from_list, reference ) result(res)
      use xxuse__
      class(xxtypebase__), intent(in) :: from_list
      class(xxtypebase__), intent(in) :: reference
      logical :: res
    end function comparison
  end interface

! Interface to provide predicate algorithm to the contained element
  abstract interface
    pure function predicate( a ) result(res)
      use xxuse__
      class(xxtypebase__), intent(in) :: a
      logical :: res
    end function predicate
  end interface

! Interface to provide binary predicate algorithm to the contained elements
  abstract interface
    pure function binary_predicate( a, b ) result(res)
      use xxuse__
      class(xxtypebase__), intent(in) :: a
      class(xxtypebase__), intent(in) :: b
      logical :: res
    end function binary_predicate
  end interface


! Single linked list iterator type
  type, extends(t_object) :: xxtypebase___slist_ftl_iterator
    private

!     Pointer to the referenced node
!     Iterator has no defined constructor
!     Iterator is constructed by defaul using this attribute initialisation
      type(t_slist_node), pointer :: node => null()

!     Pointer to the containing list; required for reverse navigation operations
      type(xxtypebase___slist_ftl), pointer :: parent => null()

    contains

!     Access
      procedure :: get_element => slist_iterator_get_element
      procedure :: set_element => slist_iterator_set_element
      procedure :: get_element_ptr => slist_iterator_get_element_ptr

!     Navigation
      procedure :: next => slist_iterator_next
      procedure :: previous => slist_iterator_previous
      procedure :: associated => slist_iterator_associated
      procedure :: nullify => slist_iterator_nullify
      procedure :: distance => slist_iterator_distance
      procedure :: swap => slist_iterator_swap_iterators

!     Assignment
      generic :: assignment(=) => slist_iterator_assign
      procedure, private :: slist_iterator_assign

!     Comparison operators
      generic :: operator(==) => slist_iterator_equal
      procedure, private :: slist_iterator_equal
      generic :: operator(/=) => slist_iterator_not_equal
      procedure, private :: slist_iterator_not_equal

  end type xxtypebase___slist_ftl_iterator


! Interfaces for procedures not bound to type
  interface distance
    module procedure slist_iterator_distance
  end interface distance
  interface swap
    module procedure slist_iterator_swap_iterators
  end interface swap

!---End of declaration of module variables--------------------------------------

contains

! (1) empty container constructor (default constructor)
!     Constructs an empty container, with no elements.
function slist_default() result( res )

! The result list
  type(xxtypebase___slist_ftl) :: res

! Initialise
  res%first => null()
  res%last => null()
  res%count = 0

end function slist_default


! (2) fill constructor
!     Constructs a container with n elements.
!     Each element is a copy of val.
function slist_fill( n, val ) result( res )

! The number of elements
  integer, intent(in) :: n

! The element to use to fill the list
  class(xxtypebase__), intent(in) :: val

! The result list
  type(xxtypebase___slist_ftl) :: res

! Assign input to output
  call res%assign( n, val )

end function slist_fill


! (3) range constructor
!     Constructs a container with as many elements as the range (first,last),
!     with each element constructed from its corresponding element in that range,
!     in the same order.
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
function slist_range( first, last ) result( res )

! Iterator to first node to insert
  class(xxtypebase___slist_ftl_iterator), intent(in) :: first

! Iterator to last node to insert
  class(xxtypebase___slist_ftl_iterator), intent(in) :: last

! The result list
  type(xxtypebase___slist_ftl) :: res

! Assign input to output
  call res%assign( first, last )

end function slist_range


! (4) copy constructor
!     Constructs a container with a copy of each of the elements in other,
!     in the same order.
function slist_copy( other ) result( res )

! The input list
  type(xxtypebase___slist_ftl), intent(in) :: other

! The result list
  type(xxtypebase___slist_ftl) :: res

! Assign input to output
  call res%assign( other )

end function slist_copy


! Copy constructor from array
function slist_copy_from_array( val ) result(res)

! The input array
  class(xxtypebase__), dimension(:), intent(in) :: val

! The result list
  type(xxtypebase___slist_ftl) :: res

! Assign input to output
  call res%assign( val )

end function slist_copy_from_array


! Assign content
! Assigns new contents to the container, replacing its current contents, and
! modifying its size accordingly.
subroutine slist_assign_from_slist( this, other )

! The output list
  class(xxtypebase___slist_ftl), intent(out) :: this

! The input list
  class(xxtypebase___slist_ftl), intent(in) :: other

! Local node pointers
  type(t_slist_node), pointer :: lptr

! Initialise navigation pointer
  lptr => other%first

! Loop on the list
  do while( associated(lptr) )

!   Add element to the output list
    call this%push_back( lptr%element )

!   Iterate
    lptr => lptr%next

  end do

end subroutine slist_assign_from_slist


! List destructor
! Destroys the container object.
pure subroutine slist_( this )

! The list
  type(xxtypebase___slist_ftl), intent(inout) :: this

! Clear the list
  if( this%count > 0 ) call this%clear()

end subroutine slist_


! Return iterator to beginning
! Returns an iterator pointing to the first element in the list container.
! If the container is empty, the returned iterator value shall not be dereferenced.
function slist_begin( this ) result(res)

! The list
  class(xxtypebase___slist_ftl), target, intent(in) :: this

! Pointer to beginning of the list
  type(xxtypebase___slist_ftl_iterator) :: res

! Return pointer to first node in the list
  res%node => this%first
  res%parent => this

end function slist_begin


! Return iterator to end
! Returns an iterator referring to the last element in the list container.
! If the container is empty, the returned iterator value shall not be dereferenced.
function slist_end( this ) result(res)

! The list
  class(xxtypebase___slist_ftl), target, intent(in) :: this

! Pointer to end of the list
  type(xxtypebase___slist_ftl_iterator) :: res

! Return pointer to the last node in the liss
  res%node => this%last
  res%parent => this

end function slist_end


! Test whether container is empty
! Returns whether the list container is empty (i.e. whether its size is 0).
! This function does not modify the container in any way.
pure function slist_empty( this ) result(res)

! The list
  class(xxtypebase___slist_ftl), intent(in) :: this

! The list empty status
  logical :: res

! Assign the return value
  res = ( this%count == 0 )

end function slist_empty


! Return size
! Returns the number of elements in the list container.
pure function slist_size( this ) result(res)

! The list
  class(xxtypebase___slist_ftl), intent(in) :: this

! The list size
  integer :: res

! Assign the return value
  res = this%count

end function slist_size


! Return maximum size
! Returns the maximum number of elements that the list container can hold.
! This is the maximum potential size the container can reach due to known system
! or library implementation limitations, but the container is by no means
! guaranteed to be able to reach that size: it can still fail to allocate
! storage at any point before that size is reached.
pure function slist_max_size() result(res)

! The list size
  integer :: res

! Assign the return value (dummy from C++)
  res = 1073741823

end function slist_max_size


! Access first element
! Returns a reference to the first element in the list container.
! Calling this function on an empty container causes undefined behaviour.
pure function slist_front( this ) result(res)

! The list
  class(xxtypebase___slist_ftl), intent(in) :: this

! Pointer to the element in the first node in the list
  class(xxtypebase__), allocatable :: res

! Assign the return value
  call element_assign_allocatable( res, this%first%element )

end function slist_front


! Access last element
! Returns a reference to the last element in the list container.
! Calling this function on an empty container causes undefined behaviour.
pure function slist_back( this ) result(res)

! The list
  class(xxtypebase___slist_ftl), intent(in) :: this

! Pointer to the element in the last node in the list
  class(xxtypebase__), allocatable :: res

! Assign the return value
  call element_assign_allocatable( res, this%last%element )

end function slist_back


! Assign new content to container
! Assigns new contents to the list container, replacing its current contents,
! and modifying its size accordingly.
! (1), the new contents are elements constructed from each of the elements in the
!      range between first and last, in the same order.
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
subroutine slist_assign_from_range( this, first, last )

! The output list
  class(xxtypebase___slist_ftl), intent(out) :: this

! Iterator to first node to insert
  class(xxtypebase___slist_ftl_iterator), intent(in) :: first

! Iterator to last node to insert
  class(xxtypebase___slist_ftl_iterator), intent(in) :: last

! Local iterator
  type(xxtypebase___slist_ftl_iterator) :: it

! Initialise navigation pointer
  it = first

! Loop on the list
  do while( it%associated() )

!   Add this element
    call this%push_back( it%node%element )

!   Check if this was the last element
    if( it == last ) exit

!   Iterate
    it = it%next()

  end do

end subroutine slist_assign_from_range


! Assign new content to container
! Assigns new contents to the list container, replacing its current contents,
! and modifying its size accordingly.
! (2), the new contents are n elements, each initialized to a copy of val.
subroutine slist_assign_from_fill( this, n, val )

! The output list
  class(xxtypebase___slist_ftl), intent(out) :: this

! The number of elements
  integer, intent(in) :: n

! The element to used to populate the container
  class(xxtypebase__), intent(in) :: val

! Local variables
  integer :: i

! Loop on the number of elements
  do i = 1, n

!   Add this element
    call this%push_back( val )

  end do

end subroutine slist_assign_from_fill


! Assign a list from an array
subroutine slist_assign_from_array( this, val )

! The output list
  class(xxtypebase___slist_ftl), intent(out) :: this

! The input array
  class(xxtypebase__), dimension(:), intent(in) :: val

! Local counter
  integer :: i

! Loop on the input array
  do i = 1, size(val)

!   Add element to the output list
    call this%push_back( val(i) )

  end do

end subroutine slist_assign_from_array


! Insert element at beginning
! Inserts a new element at the beginning of the list, right before its current
! first element. The content of val is copied (or moved) to the inserted element.
! This effectively increases the container size by one.
subroutine slist_push_front( this, val )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! The element
  class(xxtypebase__), intent(in) :: val

! Local node pointer
  type(t_slist_node), pointer :: node

! Allocate the new node
  allocate( node )

! Check if list already contains elements
  if( associated(this%last) ) then

!   Reasgign new node pointers
    node%next => this%first
    this%first => node

  else

!   Assign pointers
    this%first => node
    this%last => this%first

  end if

! Copy the element into its list position
  call element_assign_pointer( this%first%element, val )

! Increase counter
  this%count = this%count + 1

end subroutine slist_push_front


! Delete first element
! Removes the first element in the list container,
! effectively reducing its size by one.
! This destroys the removed element.
pure subroutine slist_pop_front( this )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! Local node pointer
  type(t_slist_node), pointer :: node

! Check that the list is not empty
  if( associated(this%first) ) then

!   Destroy data element in the first list node
    deallocate( this%first%element )

!   Check if there is more than one node
    if( .not. associated(this%first%next) ) then

!     Only one node in list; remove it
      deallocate( this%first )
      this%last  => null()

    else

!     More than one node in the list; remove the first one
      node => this%first
      this%first => this%first%next
      deallocate( node )

    end if

!   Decrease counter
    this%count = this%count - 1

  end if

end subroutine slist_pop_front


! Add element at the end
! Adds a new element at the end of the list container, after its current
! last element. The content of val is copied (or moved) to the new element.
! This effectively increases the container size by one.
subroutine slist_push_back( this, val )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! The element
  class(xxtypebase__), intent(in) :: val

! Check if list already contains elements
  if( associated(this%last) ) then

!   Allocate new node
    allocate( this%last%next )

!   Reassign new node pointers
    this%last => this%last%next

  else

!   Allocate memory for first node
    allocate(this%first)

!   Assign pointers
    this%last => this%first

  end if

! Assign node after last
  this%last%next => null()

! Copy the element into its list position
  call element_assign_pointer( this%last%element, val )

! Increase counter
  this%count = this%count + 1

end subroutine slist_push_back


! Delete last element
! Removes the last element in the list container,
! effectively reducing the container size by one.
! This destroys the removed element.
pure subroutine slist_pop_back( this )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! Local node pointer
  type(t_slist_node), pointer :: node

! Check that the list is not empty
  if( associated(this%first) ) then

!   Destroy data element in the last list node
    deallocate( this%last%element )

!   Check if there is more than one node
    if( .not. associated(this%first%next) ) then

!     Only one node in list; remove it
      deallocate( this%last )
      this%first  => null()

    else

!     More than one node in the list; look for the last-but-one node
      call previous_node( this%last, this%first, node )

!     Remove the last node
      deallocate( node%next )

!     Reassign last list pointer
      this%last => node

    end if

!   Decrease counter
    this%count = this%count - 1

  end if

end subroutine slist_pop_back


! Insert elements
! The container is extended by inserting new elements before the element at
! the specified position.
! This effectively increases the list size by one.
! Iterator remains associated to the node in input
! Note that this operation is expensive as the node before the insertion
! position must be located since the input node knows nothing about
! its predecesor
function slist_insert_single( this, iterator, val ) result(res)

! The list
  class(xxtypebase___slist_ftl), target, intent(inout) :: this

! Iterator to node used as reference for insertion
  class(xxtypebase___slist_ftl_iterator), intent(in) :: iterator

! The element
  class(xxtypebase__), intent(in) :: val

! Iterator to the inserted element
  type(xxtypebase___slist_ftl_iterator) :: res

! Local node pointers
  type(t_slist_node), pointer :: new
  type(t_slist_node), pointer :: node

! Allocate new node
  allocate( new )

! Check if input iterator points to first element
  if( associated(iterator%node,this%first) ) then

!   Associate pointers
    new%next => this%first
    this%first => new

  else

!   Look for the node before the insertion node
    call previous_node( iterator%node, this%first, node )

!   Associate pointers
    new%next => node%next
    node%next => new

  end if

! Copy the element into its list position
  call element_assign_pointer( new%element, val )

! Increase counter
  this%count = this%count + 1

! Return the iterator to the inserted element
  res%node => new
  res%parent => this

end function slist_insert_single


! Insert elements
! The container is extended by inserting new elements before the element at
! the specified position.
! This effectively increases the list size by n.
! Iterator remains associated to the node in input
! Note that this operation is expensive as the node before the insertion
! position must be located since the input node knows nothing about
! its predecesor
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
function slist_insert_fill( this, iterator, n, val ) result(res)

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! Iterator to node used as reference for insertion
  class(xxtypebase___slist_ftl_iterator), intent(in) :: iterator

! The number of times to insert the element
  integer, intent(in) :: n

! The element
  class(xxtypebase__), intent(in) :: val

! Iterator to the inserted element
  type(xxtypebase___slist_ftl_iterator) :: res

! Local variables
  integer :: i
  type(xxtypebase___slist_ftl_iterator) :: it

! Insert the first element to store the iterator
  res = this%insert( iterator, val )

! Insert the rest of the element
  do i = 2, n

!   ***** Provisionally use the insert_single function       *****
!   ***** This may be optimised for speed in further versions *****
    it = this%insert( iterator, val )

  end do

end function slist_insert_fill


! Insert elements
! The container is extended by inserting new elements before the element at
! the specified position.
! This effectively increases the list size by the number of element in (first,last].
! Iterator remains associated to the node in input
! This assumes that the range (first,last) is an actual connected range,
! i.e. it is possible to navigate from first to last, otherwise the resulting
! list is corrupted.
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
function slist_insert_range( this, iterator, first, last ) result(res)

! The list
  class(xxtypebase___slist_ftl), target, intent(inout) :: this

! Iterator to node used as reference for insertion
  type(xxtypebase___slist_ftl_iterator), intent(in) :: iterator

! Iterator to first node to insert
  class(xxtypebase___slist_ftl_iterator), intent(in) :: first

! Iterator to last node to insert
  class(xxtypebase___slist_ftl_iterator), intent(in) :: last

! Iterator to the inserted element
  type(xxtypebase___slist_ftl_iterator) :: res

! Local nodes
  type(t_slist_node), pointer :: inode
  type(t_slist_node), pointer :: node
  type(t_slist_node), pointer :: previous, nextnode
  type(xxtypebase___slist_ftl_iterator) :: it

! Check the insertion point position
  if( associated(iterator%node,this%first) ) then

!   Inserting before the first node
    previous => null()

  else

!   Locate the node previous to insertion
!   The insertion process takes place always between previous and iterator
    previous => this%first
    do while(.not. associated(previous%next,iterator%node) )
      previous => previous%next
    end do

  end if

! Allocate first node to keep it in return
  allocate(node)
  res%node => node
  res%parent => this

! Initialise input node navigation
  inode => first%node
  it = iterator

! Navigate the input nodes
! Exits on last%next or null
  do

!   Keep the next node to preserve it after pointer assignment
    nextnode => inode%next

!   Assign forward pointers
    if( associated(it%node,this%first) ) then

!     Inserting at the beginning of the list
      this%first => node

    else

!     Inserting in an arbitrary position
      previous%next => node

    end if
    node%next => it%node

!   Copy the element into its list position
    call element_assign_pointer( node%element, inode%element )

!   Increase counter
    this%count = this%count + 1

!   Iterate
    inode => nextnode
    if(  .not. associated(inode) ) exit
    if( associated(inode,last%node%next) ) exit

!   Previous node is now the just inserted node
    previous => node

!   Allocate new node
!   In each iteration, the control of the memory behind node is passed
!   to the node within the list. Allocation creates a new node each time.
    allocate( node )

  end do

end function slist_insert_range


! Insert elements
! The container is extended by inserting new elements before the element at
! the specified position.
! This effectively increases the list size by the size of the array.
! Iterator remains associated to the node in input
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
function slist_insert_array( this, iterator, val ) result(res)

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! Iterator to node used as reference for insertion
  class(xxtypebase___slist_ftl_iterator), intent(in) :: iterator

! The element
  class(xxtypebase__), dimension(:), intent(in) :: val

! Iterator to the inserted element
  type(xxtypebase___slist_ftl_iterator) :: res

! Local variables
  integer :: i
  type(xxtypebase___slist_ftl_iterator) :: it

! Insert the first element to store the iterator
  res = this%insert( iterator, val(1) )

! Loop on the rest of elements
  do i = 2, size(val)

!   ***** Provisionally use the insert_single function       *****
!   ***** This may be optimised for speed in further versions *****
    it = this%insert( iterator, val(i) )

  end do

end function slist_insert_array


! Erase elements
! Removes from the list container either a single element (position)
! This effectively reduces the container size by one element, which is destroyed.
! Input iterator returns not associated
! Note that this operation is expensive as the node before the insertion
! position must be located since the input node knows nothing about
! its predecesor
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
subroutine slist_erase_single( this, iterator )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! Iterator to node to remove
  class(xxtypebase___slist_ftl_iterator), intent(inout) :: iterator

! Local node pointers
  type(t_slist_node), pointer :: previous


! Check that there are elements in the list
  if( associated(this%first) ) then

!   Check if input iterator points to first element
    if( associated(iterator%node,this%first) ) then

!     Associate pointers
      this%first => iterator%node%next

    else

!     Look for the node before the input node
      call previous_node( iterator%node, this%first, previous )

!     Associate pointers
      previous%next => iterator%node%next

    end if

!   Check if last node has been removed
    if( .not. associated(this%first) ) then
      this%last => null()
    end if

!   Destroy data element in the list node
    deallocate( iterator%node%element )

!   Deallocate list element
    deallocate( iterator%node )

!   Decrease counter
    this%count = this%count - 1

  end if

end subroutine slist_erase_single


! Erase elements
! Removes from the list container either a range of elements (first,last).
! This effectively reduces the container size by the number of elements removed,
! which are destroyed.
! Note that this operation is expensive as the node before the insertion
! position must be located since the input node knows nothing about
! its predecesor
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
subroutine slist_erase_range( this, first, last )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! Iterator to first node to remove
  class(xxtypebase___slist_ftl_iterator), intent(in) :: first

! Iterator to last node to remove
  class(xxtypebase___slist_ftl_iterator), intent(in) :: last

! Local node pointers
  type(t_slist_node), pointer :: node, nodenext, lastnodenext

! Check that there are elements in the list
  if( associated(this%first) ) then

!   Assign forward pointers
    if( associated(first%node,this%first) ) then
      this%first => last%node%next
    else
      call previous_node( first%node, this%first, node )
      node%next => last%node%next
    end if

!   Initialise node navigation in the deleted range
!   Nullify last%next to be used as loop exit check
    node => first%node
    lastnodenext => last%node%next

!   Loop on the nodes in the deleted range
    do

!     Keep next node before deallocating
      nodenext => node%next

!     Destroy data element in the list node
      deallocate( node%element )

!     Deallocate list element
      deallocate( node )

!     Decrease counter
      this%count = this%count - 1

!     Iterate
      node => nodenext
      if( .not. associated(node) ) exit
      if( associated(node,lastnodenext) ) exit

    end do

  end if

end subroutine slist_erase_range


! Swap content
! Exchanges the content of the container by the content of other, which is another list of the same type.
! Sizes may differ.
! After the call to this member function, the elements in this container are
! those which were in other before the call,
! and the elements of other are those which were in this.
! All iterators, references and pointers remain valid for the swapped objects.
pure subroutine slist_swap( this, other )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! The list to swap
  type(xxtypebase___slist_ftl), intent(inout) :: other

! Local variables
  type(t_slist_node), pointer :: tmpnode
  integer :: n

! Swap the first node
  tmpnode => this%first
  this%first => other%first
  other%first => tmpnode

! Swap the last node
  tmpnode => this%last
  this%last => other%last
  other%last => tmpnode

! Swap the list sizes
  n = this%count
  this%count = other%count
  other%count = n

end subroutine slist_swap


! Change size
! Resizes the container so that it contains n elements.
! If n is smaller than the current container size, the content is reduced to its
! first n elements, removing those beyond (and destroying them).
! If n is greater than the current container size, the content is expanded by
! inserting at the end as many elements as needed to reach a size of n.
! If val is specified, the new elements are initialized as copies of val,
! otherwise, they are value-initialized.
subroutine slist_resize( this, n, val )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! The number of elements in the resulting list
  integer, intent(in) :: n

! The element to use to initialise traling elements
  class(xxtypebase__), optional, target, intent(in) :: val

! Local variables
  integer :: i
  class(xxtypebase__), pointer :: init
  class(xxtypebase__), allocatable, target :: default

! List size is greater than requested size
  if( this%count < n ) then

!   Check if initialisation value has been given
    if( present(val) ) then
      init => val
    else
      allocate( default, mold=this%first%element )
      init => default
    end if

!   Loop on the number of element to add
    do i = this%count + 1, n
      call this%push_back(init)
    end do

! List size is smaller than requested size
  else if( this%count > n ) then

!   Loop on the number of element to remove
    do i = this%count, n + 1, -1
      call this%pop_back()
    end do

! List size is the same as the requested size
  else
  endif

end subroutine slist_resize


! Clear content
! Removes all elements from the list container (which are destroyed)
! and leaving the container with a size of 0
pure subroutine slist_clear( this )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! Local node pointers
  type(t_slist_node), pointer :: del, next

! Check if empty list
  if( associated( this%last ) ) then

!   Initialise list navigation
    del => this%first

!   Navigate the list
    do while( associated(del) )

!     Save pointer to next node
      next => del%next

!     Destroy data element in the current list node
      deallocate( del%element )

!     Deallocate the list node
      deallocate( del )

!     Irerate
      del => next

    end do

  end if

! Reinitialise list pointers
  this%first => null()
  this%last => null()

! Reset counter
  this%count = 0

end subroutine slist_clear


! Get reference to element at given position
function slist_at_get( this, idx ) result(res)

! The list
  class(xxtypebase___slist_ftl), intent(in) :: this

! The position in the list
  integer, intent(in) :: idx

! The object to replace
  class(xxtypebase__), pointer :: res

! Local variables
  integer :: i
  type(t_slist_node), pointer :: node

! Look for the node
  node => this%first
  do i = 2, idx
    if( .not. associated(node) ) exit
    node => node%next
  end do

! Check node found
  if( associated(node) ) then
    res => node%element
  else
    res => null()
  end if

end function slist_at_get


! Transfer elements from list to list
! Transfers elements from source into the container, inserting them at position.
! This effectively inserts those elements into the container and removes them from source,
! altering the sizes of both containers.
! The operation does not involve the construction or destruction of any element.
! (1) transfers all the elements of source into the container.
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
subroutine slist_splice_slist( this, position, source )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! The position in list to insert the elements
  class(xxtypebase___slist_ftl_iterator), intent(in) :: position

! The source list
  class(xxtypebase___slist_ftl), intent(inout) :: source

! Call the generic splice function
  call this%slist_splice_nodes( position, source, source%first, source%last )

end subroutine slist_splice_slist


! Transfer elements from list to list
! Transfers elements from source into the container, inserting them at position.
! This effectively inserts those elements into the container and removes them from source,
! altering the sizes of both containers.
! The operation does not involve the construction or destruction of any element.
! (2) transfers only the element pointed by iterator from x into the container.
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
subroutine slist_splice_single( this, position, source, it )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! The position in list to insert the elements
  class(xxtypebase___slist_ftl_iterator), intent(in) :: position

! The source list
  class(xxtypebase___slist_ftl), intent(inout) :: source

! The element position in source
  class(xxtypebase___slist_ftl_iterator), intent(in) :: it

! Call the generic splice function
  call this%slist_splice_nodes( position, source, it%node, it%node )

end subroutine slist_splice_single


! Transfer elements from list to list
! Transfers elements from source into the container, inserting them at position.
! This effectively inserts those elements into the container and removes them from source,
! altering the sizes of both containers.
! The operation does not involve the construction or destruction of any element.
! (3) transfers the range (first,last) from source into the container.
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
subroutine slist_splice_range( this, position, source, first, last )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! The position in list to insert the elements
  class(xxtypebase___slist_ftl_iterator), intent(in) :: position

! The source list
  class(xxtypebase___slist_ftl), intent(inout) :: source

! The first position in source to retrieve elements
  class(xxtypebase___slist_ftl_iterator), intent(in) :: first

! The last position in source to retrieve elements
  class(xxtypebase___slist_ftl_iterator), intent(in) :: last

! Call the generic splice function
  call this%slist_splice_nodes( position, source, first%node, last%node )

end subroutine slist_splice_range


! Transfer elements from list to list
! Actual implementation of splcie at node level
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
!
! Splice as in C++ cannot append lists because the end iterator returns the
! poisiton of the last-element and not the past-last-element.
! To circumvent this problem, provision of position=null identfied insert after
! the end of the list )after last element)
subroutine slist_splice_nodes( this, position, source, first, last )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! The position in list to insert the elements
  class(xxtypebase___slist_ftl_iterator), intent(in) :: position

! The source list
  class(xxtypebase___slist_ftl), intent(inout) :: source

! The first position in source to retrieve elements
  type(t_slist_node), pointer, intent(in) :: first

! The last position in source to retrieve elements
  type(t_slist_node), pointer, intent(in) :: last

! Local variables
  type(t_slist_node), pointer :: it, sfirst, slast
  type(t_slist_node), pointer :: sposition
  integer :: ncount

! Count the number of nodes in range
  ncount = slist_nodes_distance(first,last) + 1

! Preserve pointers to first and last in source
  sfirst => first
  slast => last

! Delete nodes from source
! Check position of first transferred node
  if( .not. associated(first,source%first) ) then

!   Check position of last transferred node
    if( .not. associated(last,source%last) ) then

!     Fisrt transeferred node is not the first node in source
!     Last transeferred node is not the last node in source
      call previous_node( first, source%first, it )
      it%next => last%next

    else

!     Fisrt transeferred node is not the first node in source
!     Last trasnferred node is the last node in source
      call previous_node( first, source%first, it )
      it%next => null()
      source%last => it

    end if

  else

!   Check position of last transferred node
    if( associated(last%next) ) then

!     Fisrt transeferred node is the first node in source
!     Last transeferred node is not the last node in source
      source%first => last%next

    else

!     Fisrt transeferred node is the first node in source
!     Last transeferred node is the last node in source
!     This results in the source being an empty list
      source%first => null()
      source%last => null()

    end if

  end if

! Recompute number of elements in source
  source%count = source%count - ncount

! Insert elements in this
! Check position iterator association (detect append case)
  if( position%associated() ) then

!   Get reference to the position node
    sposition => position%node

!   Check position of insertion points in this
    if( .not. associated(sposition,this%first) ) then

!     Position is not the first in this
      call previous_node( sposition, this%first, it )
      it%next => sfirst

    else

!     Position is the first in this
      this%first => sfirst

    end if

!   Insert elements in this
!   Link the last in source to this
    slast%next => sposition

  else

!   Position id after last in this (null iterator in input)
    this%last%next => sfirst

    this%last => slast
    slast%next => null()

  end if

! Recompute number of elements in this
  this%count = this%count + ncount

end subroutine slist_splice_nodes


! Remove elements with specific value
! Removes from the container all the elements that compare equal to val.
! This calls the destructor of these objects and reduces the container
! size by the number of elements removed.
subroutine slist_remove( this, val )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! The value to use as comparison for the removal
  class(xxtypebase__), intent(in) :: val

! Local iterators
  type(xxtypebase___slist_ftl_iterator) :: it, itnext

! Check that there are elements in the list
  if( associated(this%first) ) then

!   Initialise iteration
    it = this%begin()

!   Loop on the iterator range
    do while( it%associated() )

!     Keep next iterator in case current is removed
      itnext = it%next()

!     Check and remove
      if( it%node%element == val ) then
        call this%erase(it)
      end if

!     Iterate
      it = itnext

    end do

  end if

end subroutine slist_remove


! Remove elements fulfilling condition
! Removes from the container all the elements for which Predicate pred returns true.
! This calls the destructor of these objects and reduces the container size by the
! number of elements removed.
! The function calls pred(i%node%element) for each element (where i is an iterator
! to that element). Any of the elements in the list for which this returns true,
! are removed from the container.
subroutine slist_remove_if( this, pred )

! The list
  class(xxtypebase___slist_ftl), intent(inout) :: this

! THe predicate to use for removal selection
  procedure(predicate) :: pred

! Local variables
  type(xxtypebase___slist_ftl_iterator) :: it, itnext

! Check that there are elements in the list
  if( associated(this%first) ) then

!   Initialise iteration
    it = this%begin()

!   Loop on the iterator range
    do while( it%associated() )

!     Preserve next in case that current is deleted
      itnext = it%next()

!     Check and remove
      if( pred( it%node%element ) ) then
        call this%erase(it)
      end if

!     Iterate
      it = itnext

    end do

  end if

end subroutine slist_remove_if


! Remove duplicate values
! (1) removes all but the first element from every consecutive group of equal
!     elements in the container.
! (2) takes as argument a specific comparison function that determine the "uniqueness"
!     of an element. In fact, any behavior can be implemented (and not only an equality
!     comparison), but notice that the function will call binary_pred(*i,*j)) for all
!     pairs of elements (where i and j are iterators to the elements) and remove j from
!     the list if the predicate returns true.
subroutine slist_unique( this, bpred )

! The list
  class(xxtypebase___slist_ftl), target, intent(inout) :: this

! The interface for the binary predicate (optional)
! If the operator is not provided, then operator(==) is assumed
  procedure(binary_predicate), optional :: bpred

! Local variables
  type(t_slist_node), pointer :: outer, inner, innernext
  logical :: check
  type(xxtypebase___slist_ftl_iterator) :: it

! Outer-loop on the nodes
  outer => this%first
  do while( associated(outer) )

!   Initialise inner loop
    inner => outer%next

!   Inner-loop on the nodes
    do while( associated(inner) )

!     Compare the inner and the outer elements
      if( present( bpred ) ) then
        check = bpred( outer%element, inner%element )
      else
        check = ( outer%element == inner%element )
      end if

!     Check inner element equal to outer
      if( check ) then

!       Preserve the next pointer before removing current
        innernext => inner%next

!       Remove innetr element
        it%node => inner
        call this%erase(it)

!       Iterate inner loop
        inner => innernext

      else

!       Iterate inner loop
        inner => inner%next

      end if

    end do

!   Iterate outer loop
    outer => outer%next

  end do

end subroutine slist_unique


! Merge sorted lists
! Merges other into the list by transferring all of its elements at their respective
! ordered positions into the container (both containers shall already be ordered).
! This effectively removes all the elements in other (which becomes empty), and inserts
! them into their ordered position within container (which expands in size by the
! number of elements transferred).
! The operation is performed without constructing nor destroying any element:
! they are transferred, no matter whether other is an lvalue or an rvalue, or whether
! the value_type supports move-construction or not.
! (1) Each element of other is inserted at the position that corresponds to its value according
!     to the strict weak ordering defined by operator < or comp. The resulting order of
!     equivalent elements is stable (i.e., equivalent elements preserve the relative order
!     they had before the call, and existing elements precede those equivalent inserted from other).
! (2) Take a specific predicate (comp) to perform the comparison operation between elements.
!     This comparison shall produce a strict weak ordering of the elements (i.e., a consistent
!     transitive comparison, without considering its reflexiveness).
! This function requires that the list containers have their elements already ordered by
! value (or by comp) before the call.
! The function does nothing if (other == this).
subroutine slist_merge( this, other, comp )

! The list
  class(xxtypebase___slist_ftl), target, intent(inout) :: this

! The list to merge
  type(xxtypebase___slist_ftl), target, intent(inout) :: other

! Comparison function (optional)
  procedure(comparison), optional :: comp

! Local variables
  type(t_slist_node), pointer :: node, nodenext
  type(t_slist_node), pointer :: xnode, xnodenext
  type(t_slist_node), pointer :: it
  logical :: insert

! Check if the same list is input
! Use addresses of first element to decide on same list
  if( .not. associated( this%first, other%first ) ) then

!   Initialise the nodex in other (this is the outer loop)
    xnode => other%first

!   Initialise navigation in this list (this is the inner loop)
!   Each step fo the outer loop shall start the inner loop in the lasp postions
!   in the list (assuming tha both lists are ordered) not to iterate over the
!   whole list from the beginning in each iteration of the outer loop.
    node => this%first

!   Loop on the nodes in other
    outer : do while( associated(xnode) )

!     Preserve node before it is moved
      xnodenext => xnode%next

!     Loop on the nodes in this
      inner : do while( associated(node) )

!       Preserve node before it is moved
        nodenext => node%next

!       Check if this node is the insertion point
        if( present(comp) ) then
          insert = comp( xnode%element, node%element )
        else
          insert = ( xnode%element < node%element )
        end if

!       Check if this element must be inserted
        if( insert ) then

!         Assign forward pointers
          if( associated(node,this%first) ) then
            this%first => xnode
          else
            call previous_node( node, this%first, it )
            it%next => xnode
          end if
          xnode%next => node

!         Exit the inner loop
          exit inner

        end if

!       Iterate the inner loop
        node => nodenext

!       Check past last node
!       If this point is reached with node=null this means that
!       tHe remaining elements in other are past the end of this
        if( .not. associated(node) ) then

!         Append the other node at the end of this list
          this%last%next => xnode
          this%last => xnode
          xnode%next => null()

        end if

      end do inner

!     Iterate the outer loop
      xnode => xnodenext

    end do outer

!   Compute sizes
    this%count = this%count + other%count

!   Reset other
    other%count = 0
    other%first => null()
    other%last => null()

  end if

end subroutine slist_merge


! Sort elements in container
! Sorts the elements in the list, altering their position within the container.
! (1) The sorting is performed by applying an algorithm that uses operator < to compare elements
! (2) The sorting is performed by applying an algorithm that uses comp to compare elements.
!     This comparison shall produce a strict weak ordering of the elements (i.e., a
!     consistent transitive comparison, without considering its reflexiveness).
! The resulting order of equivalent elements is stable: i.e., equivalent elements
! preserve the relative order they had before the call.
! The entire operation does not involve the construction, destruction or copy of any element
! object. Elements are moved within the container.
subroutine slist_sort( this, compare )

! The list to sort
  class(xxtypebase___slist_ftl), intent(inout) :: this

! The interface for the comparison operator (optional)
! If the operator is not provided, then operator(<) is assumed
  procedure(comparison), optional :: compare

! Call the sorting routine
  call this%quick_sort( this%first, this%last, this%count, compare )

end subroutine slist_sort


! Sorting subroutine (Quick-sort method) algorithm kernel
recursive subroutine quick_sort( this, left, right, size, compare )

! The list to sort
  class(xxtypebase___slist_ftl), intent(inout) :: this

! Quick-sort partition left position
  type(t_slist_node), pointer :: left

! Quick-sort partition right position
  type(t_slist_node), pointer :: right

! The number of node between left and right (including selves)
  integer, intent(in) :: size

! The interface for the comparison operator (optional)
! If the operator is not provided, then operator(<) is assumed
  procedure(comparison), optional :: compare

! Counters
  integer :: i

! Partition size
  integer :: lsize

! The result of the element comparison
  logical :: sortby

! Local pointers
  type(t_slist_node), pointer :: last, ptr, last_prev
  class(xxtypebase__), pointer :: ref, ele

! Check for single element list
  if( size > 1 ) then

!   Initialise partition sizes
    lsize = 0

!   Look for an element in the middle of the list
    ptr => left
    do i = 1, size / 2
      ptr => ptr%next
    end do
    ref => ptr%element

!   Move the reference element to the beginning of the list
    call swap_nodes( left, ptr )
    last => left
    last_prev => null()

!   Generate partition
    ptr => left%next
    do

!     Get this element
      ele => ptr%element

!     Check element
      if( present( compare ) ) then
        sortby = compare( ele, ref )
      else
        sortby = ( ele < ref )
      end if
      if( sortby ) then

!       Swap element
        last_prev => last
        last => last%next
        call swap_nodes( last, ptr )
        lsize = lsize + 1

      end if

!     Next element
      ptr => ptr%next

!     Check exit conditions
      if( .not. associated(ptr) ) exit
      if( associated(ptr,right%next) ) exit

    end do

!   Restore the partition element
    call swap_nodes( left, last )

!   Sort partitions
    call this%quick_sort( left,      last_prev, lsize,        compare )
    call this%quick_sort( last%next, right,     size-lsize-1, compare )

  end if

end subroutine quick_sort


! Swap two nodes in a list
pure subroutine swap_nodes( node1, node2 )

! Iterator to first element
  type(t_slist_node), intent(inout) :: node1

! Iterator to second element
  type(t_slist_node), intent(inout) :: node2

! Intermediate pointer
  class(xxtypebase__), pointer :: tmp

! Swap the pointers to the data elements
  tmp           => node1%element
  node1%element => node2%element
  node2%element => tmp

end subroutine swap_nodes


! Swap two nodes in a list pointed by iterators
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
pure subroutine slist_iterator_swap_iterators( iter1, iter2 )

! Iterator to first element
  class(xxtypebase___slist_ftl_iterator), intent(inout) :: iter1

! Iterator to second element
  class(xxtypebase___slist_ftl_iterator), intent(inout) :: iter2

! Local variables
  type(xxtypebase___slist_ftl), pointer :: ltmp

! Swap the nodes pointed by the iterators
  call swap_nodes( iter1%node, iter2%node )
  ltmp => iter1%parent
  iter1%parent => iter2%parent
  iter2%parent => iter1%parent

end subroutine slist_iterator_swap_iterators


! Reverse the order of elements in a list
! Note that this operation is expensive as the element swapped at the end
! of the list has to be located every time as the list cannot be navigated
! simultaneously forward and backward (backward requires locating)
pure subroutine slist_reverse( this )

! The list to reverse
  class(xxtypebase___slist_ftl), intent(inout) :: this

! Local node pointers
  type(t_slist_node), pointer :: lptr, rptr, last

! Loop simultaneously from left to right and from right to left
  lptr => this%first
  rptr => this%last
  last => rptr

! Loop until both pointer are identical (odd number of elements)
! or until the two pointers cross (even number of elements)
  do

!   Check pointers
    if( associated( lptr, rptr ) ) exit
    if( associated( rptr%next, lptr ) ) exit

!   Swapt elements
    call swap_nodes( lptr, rptr )

!   Iterate backward navigation
!   This must be before forward iteration as it uses lptr to start up search
    call previous_node( last, lptr, rptr )
    last => rptr

!   Iterate forward navigation
    lptr => lptr%next

  end do

end subroutine slist_reverse


! Binary search subroutine (assumes ascending sorted list) front-end
function slist_binary_search( this, item, isless, isgreater ) result(res)

! The list to search
  class(xxtypebase___slist_ftl), target, intent(in) :: this

! The element to look for
  class(xxtypebase__), intent(in) :: item

! If the operator is not provided, then operator(<) is assumed
  procedure(comparison), optional :: isless

! If the operator is not provided, then operator(>) is assumed
  procedure(comparison), optional :: isgreater

! The iterator to the list element (not associated if not found)
  type(xxtypebase___slist_ftl_iterator) :: res

! Call the sorting routine
  res = recursive_binary_search( this%first, this%last, this%count, item, isless, isgreater )
  res%parent => this

end function slist_binary_search


! Binary search subroutine (assumes ascending sorted list) algorithm kernel
recursive function recursive_binary_search( left, right, size, item, isless, isgreater ) result(res)

! Quick-sort partition left position
  type(t_slist_node), target, intent(in) :: left

! Quick-sort partition right position
  type(t_slist_node), intent(in) :: right

! The number of node between left and right (including selves)
  integer, intent(in) :: size

! The element to look for
  class(xxtypebase__), intent(in) :: item

! If the operator is not provided, then operator(<) is assumed
  procedure(comparison), optional :: isless

! If the operator is not provided, then operator(>) is assumed
  procedure(comparison), optional :: isgreater

! The iterator to the list element (not associated if not found)
  type(xxtypebase___slist_ftl_iterator) :: res

! Counters
  integer :: i

! The result of the element comparison
  logical :: check

! Local pointers
  type(t_slist_node),  pointer :: ptr, prev
  class(xxtypebase__), pointer :: ref

! Check for single element list
  if( size > 0 ) then

!   Look for an element in the middle of the list
    ptr => left
    prev => null()
    do i = 1, size / 2
      prev => ptr
      ptr => ptr%next
    end do
    ref => ptr%element

!   Check element
    if( present( isless ) ) then
      check = isless( item, ref )
    else
      check = ( item < ref )
    end if
    if( check ) then

!     Invoke search for lower partition
      res = recursive_binary_search( left, prev, size / 2, item, isless, isgreater )

    else

!     Check element
      if( present( isgreater ) ) then
        check = isgreater( item, ref )
      else
        check = ( item > ref )
      end if
      if( check ) then

!       Invoke search for lower partition
        res = recursive_binary_search( ptr%next, right, size - size / 2 - 1, item, isless, isgreater )

      else

!       Element found
        res%node => ptr

      end if

    end if

  else

!   Not found
    res%node => null()

  end if

end function recursive_binary_search


! Search a list following certain criteria
function slist_select( this, reference, bpred ) result(res)

! The list to search selected items
  class(xxtypebase___slist_ftl), intent(in) :: this

! The reference element to use as selecting pattern
  class(xxtypebase__), intent(in) :: reference

! The interface for the comparison operator
  procedure(binary_predicate), optional :: bpred

! The output list contatining the selected elements
  type(xxtypebase___slist_ftl) :: res

! Local node pointer
  type(t_slist_node), pointer :: node

! Local variables
  logical :: flag

! Initialise list navigation
  node => this%first

! Loop in the list
  do while( associated(node) )

!   Decide whether element matches the criterion
    if( present(bpred) ) then
      flag = bpred( node%element, reference )
    else
      flag = ( node%element == reference )
    end if
    if( flag ) then
      call res%push_back( node%element )
    end if

!   Retrieve next element
    node => node%next

  end do

end function slist_select


! Create an array (allocatabe) of elements from a list
function slist_array( this ) result(res)

! The input list
  class(xxtypebase___slist_ftl), intent(in) :: this

! The returned array of elements (unallocated if memory failure)
  class(xxtypebase__), allocatable, dimension(:) :: res

! Local node pointers
  type(t_slist_node), pointer :: lptr

! Local counter
  integer :: i

! Memory allocation status
  integer :: status

! Allocate memory for returned array
  allocate( res( this%size() ), mold=this%first%element, stat=status )
  if( status == 0 ) then

!   Initialise navigation pointer
    lptr => this%first

!   Loop on the elements
    do i = 1, this%size()
      res(i) = lptr%element
      lptr => lptr%next
    end do

  end if

end function slist_array


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











! Return iterator to next node in the list
function slist_iterator_next( this ) result(res)

! The list iterator
  class(xxtypebase___slist_ftl_iterator), intent(in) :: this

! Pointer to next node in the list
  type(xxtypebase___slist_ftl_iterator) :: res

! Return pointer to next node
  res%node => this%node%next
  res%parent => this%parent

end function slist_iterator_next


! Return iterator to previous node in the list
! Note that this operation is expensive as the node before current
! position must be located since the input node knows nothing about
! its predecesor
function slist_iterator_previous( this ) result(res)

! The list iterator
  class(xxtypebase___slist_ftl_iterator), intent(in) :: this

! The previous node in the list
  type(xxtypebase___slist_ftl_iterator) :: res

! Local node pointer
  type(t_slist_node), pointer :: node


! Check if input iterator points to first element
  if( associated(this%node,this%parent%first) ) then

!   Nullify pointer to node
    node => null()

  else

!   Look for the node before the input node
    call previous_node( this%node, this%parent%first, node )

  end if

! Return pointer to previous node
  res%node => node
  res%parent => this%parent

end function slist_iterator_previous


! Return the distance (number of elements) between two iterators
! The target node must be reacheabke from this using the next operator
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
function slist_iterator_distance( this, target ) result(res)

! The first list iterator
  class(xxtypebase___slist_ftl_iterator), intent(in) :: this

! The final list iterator
  class(xxtypebase___slist_ftl_iterator), intent(in) :: target

! The number of elements between the iterators
  integer :: res

! Call the general nodes distance function
  res = slist_nodes_distance( this%node, target%node )

end function slist_iterator_distance


! Return the distance (number of elements) between two nodes
! The target node must be reacheabke from this using the next pointer
function slist_nodes_distance( origin, target ) result(res)

! The first node
  type(t_slist_node), pointer, intent(in) :: origin

! The final node
  type(t_slist_node), pointer, intent(in) :: target

! The number of elements between the iterators
  integer :: res

! Local variables
  type(t_slist_node), pointer :: node

! Count the number of nodes in range
  node => origin
  res = 0
  do while( .not. associated(node,target) .and. associated(node) )

!   Count this node
    res = res + 1

!   Iterate
    node => node%next

  end do

! Check end node asscoiation
! Return -1 if end of list was hit without finding the target
  if( .not. associated(node) ) then
    res = -1
  end if

end function slist_nodes_distance


! Return the association status of an iterator
pure function slist_iterator_associated( this ) result(res)

! The list iterator
  class(xxtypebase___slist_ftl_iterator), intent(in) :: this

! The association status
  logical :: res

! Return the association status
  res = associated( this%node )

end function slist_iterator_associated


! Nullify an iterator
pure subroutine slist_iterator_nullify( this )

! The list iterator
  class(xxtypebase___slist_ftl_iterator), intent(inout) :: this

! Nullify pointer
  this%node => null()

end subroutine slist_iterator_nullify


! Get an elememnt in the node pointed by the iterator
pure function slist_iterator_get_element( this ) result(res)

! The iterator
  class(xxtypebase___slist_ftl_iterator), intent(in) :: this

! The returned pointer to element
  class(xxtypebase__), allocatable :: res

! Return the data element pointed by the iterator
  call element_assign_allocatable( res, this%node%element )

end function slist_iterator_get_element


! Get a pointer to the data elememnt in the node pointed by the iterator
function slist_iterator_get_element_ptr( this ) result(res)

! The iterator
  class(xxtypebase___slist_ftl_iterator), intent(in) :: this

! The returned pointer to element
  class(xxtypebase__), pointer :: res

! Return the data element pointed by the iterator
  res => this%node%element

end function slist_iterator_get_element_ptr


! Set the element in the node pointed by the iterator
pure subroutine slist_iterator_set_element( this, val )

! The iterator
  class(xxtypebase___slist_ftl_iterator), intent(inout) :: this

! The element to be assigned
  class(xxtypebase__), intent(in) :: val

! Copy the element into its list position
  if( associated(this%node%element) ) deallocate( this%node%element )
  call element_assign_pointer( this%node%element, val )

end subroutine slist_iterator_set_element


! Create an interator from an existing iterator (assignment operator)
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
subroutine slist_iterator_assign( left, right )

! The output iterator
  class(xxtypebase___slist_ftl_iterator), intent(out) :: left

! The input iterator
  class(xxtypebase___slist_ftl_iterator), intent(in) :: right

! Associate iterator
  left%node => right%node
  left%parent => right%parent

end subroutine slist_iterator_assign


! Compare two iterators (equality)
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
pure function slist_iterator_equal( left, right ) result(res)

! The first iterator
  class(xxtypebase___slist_ftl_iterator), intent(in) :: left

! The second iterator
  class(xxtypebase___slist_ftl_iterator), intent(in) :: right

! The comparison result
  logical :: res

! Compute result by pointer association check
  res = associated( left%node, right%node )

end function slist_iterator_equal


! Compare two iterator (inequality)
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
pure function slist_iterator_not_equal( left, right ) result(res)

! The first iterator
  class(xxtypebase___slist_ftl_iterator), intent(in) :: left

! The second iterator
  class(xxtypebase___slist_ftl_iterator), intent(in) :: right

! The comparison result
  logical :: res

! Compute result by pointer association check
  res = .not. associated( left%node, right%node )

end function slist_iterator_not_equal


! Locate the node previous to a given one
pure subroutine previous_node( target, origin, previous )

! The input node whoe previous one is to be searched
  type(t_slist_node), pointer :: target

! The node from which the target node is to be searched
  type(t_slist_node), pointer :: origin

! The node previous to target
  type(t_slist_node), pointer :: previous

! Look for the node
  previous => origin
  do while( .not. associated(previous%next,target) )
    previous => previous%next
  end do

end subroutine previous_node

end module xxmodulebase___slist_ftl
