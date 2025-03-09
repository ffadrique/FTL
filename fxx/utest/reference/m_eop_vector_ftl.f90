module m_eop_vector_ftl

!-------------------------------------------------------------------------------
! Copyright : 2025, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Reference : http://www.cplusplus.com/reference/vector
!             http://www.cplusplus.com/reference/list
! Synopsis  : Vector (dynamic) container template
!             Limitations with respect to STL C++
!              - No reverse iteration.
!              - No constant iterators.
!              - Max size is dummy (dummy value not computed from architecture).
!              - No emplace functions.
!              - No operator [].
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
  use m_eop

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_eop_vector_ftl, eop_vector_ftl
  public t_eop_vector_ftl_iterator

  public distance, swap

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! Vector node type
  type t_node
    private

!   The element data instance
    type(t_eop), pointer :: element => null()

  end type t_node


! Vector container type
  type, extends(t_object) :: t_eop_vector_ftl
    private

!     The number of nodes in the vector
      integer :: count = 0

!     The internal array
      type(t_node), dimension(:), allocatable :: data

    contains

!     Assign content
      generic :: assignment(=) => vector_assign_from_vector
      procedure :: vector_assign_from_vector

!     Destructor
      final :: vector_

!     Iterators
      procedure :: begin => vector_begin
      procedure :: end => vector_end

!     Capacity
      procedure :: empty => vector_empty
      procedure :: size => vector_size
      procedure, nopass :: max_size => vector_max_size
      procedure :: capacity => vector_capacity
      
!     Element access
      procedure :: front => vector_front
      procedure :: back => vector_back

!     Modifiers
      generic :: assign => vector_assign_from_vector, &
                           vector_assign_from_range, &
                           vector_assign_from_fill, &
                           vector_assign_from_array
      procedure, private :: vector_assign_from_range
      procedure, private :: vector_assign_from_fill
      procedure, private :: vector_assign_from_array
      procedure :: push_front => vector_push_front
      procedure :: pop_front => vector_pop_front
      procedure :: push_back => vector_push_back
      procedure :: pop_back => vector_pop_back
      generic :: insert => vector_insert_single, &
                           vector_insert_single_at, &
                           vector_insert_fill, &
                           vector_insert_fill_at, &
                           vector_insert_range, &
                           vector_insert_range_at, &
                           vector_insert_array, &
                           vector_insert_array_at
      procedure, private :: vector_insert_single
      procedure, private :: vector_insert_single_at
      procedure, private :: vector_insert_fill
      procedure, private :: vector_insert_fill_at
      procedure, private :: vector_insert_range
      procedure, private :: vector_insert_range_at
      procedure, private :: vector_insert_array
      procedure, private :: vector_insert_array_at
      generic :: erase => vector_erase_single, &
                          vector_erase_single_at, &
                          vector_erase_range, &
                          vector_erase_range_at
      procedure, private :: vector_erase_single
      procedure, private :: vector_erase_single_at
      procedure, private :: vector_erase_range
      procedure, private :: vector_erase_range_at
      generic :: swap => vector_swap_index, &
                         vector_swap
      procedure, private :: vector_swap_index
      procedure, private :: vector_swap
      procedure :: resize => vector_resize
      procedure :: clear => vector_clear

!     Operations
      generic :: splice => vector_splice_vector, &
                           vector_splice_range, &
                           vector_splice_single
      procedure, private :: vector_splice_vector
      procedure, private :: vector_splice_range
      procedure, private :: vector_splice_single
      procedure, private :: vector_splice_nodes
      procedure :: remove => vector_remove
      procedure :: remove_if => vector_remove_if
      procedure :: unique => vector_unique
      procedure :: merge => vector_merge
      procedure :: sort => vector_sort
      procedure, private :: quick_sort
      procedure :: reverse => vector_reverse

!     Selection
      procedure :: binary_search => vector_binary_search
      procedure :: select => vector_select
      procedure :: at => vector_at_get

!     Element access
      procedure :: get_element => vector_element_from_index
      procedure :: get_element_ptr => vector_element_ptr_from_index
      procedure :: set_element => vector_element_to_index

!     Conversion
      procedure :: array => vector_array

!     Memory allocation management
      procedure :: reserve => vector_reserve
      procedure, private :: realloc => vector_realloc

  end type t_eop_vector_ftl

! Reference vector size parameters
  integer, parameter :: vector_base_capacity          = 100
  real,    parameter :: vector_base_capacity_increase = 1.5


  ! Constructor interface
  interface eop_vector_ftl
    module procedure vector_default
    module procedure vector_fill
    module procedure vector_range
    module procedure vector_copy
    module procedure vector_copy_from_array
  end interface eop_vector_ftl


! Interface to provide user comparison functions
  abstract interface
    pure function comparison( from_vector, reference ) result(res)
      use m_eop
      type(t_eop), intent(in) :: from_vector
      type(t_eop), intent(in) :: reference
      logical :: res
    end function comparison
  end interface

! Interface to provide predicate algorithm to the contained element
  abstract interface
    pure function predicate( a ) result(res)
      use m_eop
      type(t_eop), intent(in) :: a
      logical :: res
    end function predicate
  end interface

! Interface to provide binary predicate algorithm to the contained elements
  abstract interface
    pure function binary_predicate( a, b ) result(res)
      use m_eop
      type(t_eop), intent(in) :: a
      type(t_eop), intent(in) :: b
      logical :: res
    end function binary_predicate
  end interface


! Vector iterator type
  type, extends(t_object) :: t_eop_vector_ftl_iterator
    private

!   The element index within the array (iterator not initialised if 0)
    integer :: idx = 0

!   Pointer to the container vector
    type(t_eop_vector_ftl), pointer :: parent => null()

    contains

!     Access
      procedure :: get_element => vector_iterator_get_element
      procedure :: set_element => vector_iterator_set_element
      procedure :: get_element_ptr => vector_iterator_get_element_ptr

!     Navigation
      procedure :: next => vector_iterator_next
      procedure :: previous => vector_iterator_previous
      procedure :: associated => vector_iterator_associated
      procedure :: nullify => vector_iterator_nullify
      procedure :: index => vector_iterator_index
      procedure :: distance => vector_iterator_distance
      procedure :: swap => vector_iterator_swap_iterators


!     Assignment
      generic :: assignment(=) => vector_iterator_assign
      procedure, private :: vector_iterator_assign

!     Comparison operators
      generic :: operator(==) => vector_iterator_equal
      procedure, private :: vector_iterator_equal
      generic :: operator(/=) => vector_iterator_not_equal
      procedure, private :: vector_iterator_not_equal

  end type t_eop_vector_ftl_iterator


! Interfaces for procedures not bound to type
  interface distance
    module procedure vector_iterator_distance
  end interface distance
  interface swap
    module procedure vector_iterator_swap_iterators
  end interface swap

!---End of declaration of module variables--------------------------------------

contains

! (1) empty container constructor (default constructor)
!     Constructs an empty container, with no elements.
function vector_default() result( res )

! The result vector
  type(t_eop_vector_ftl) :: res

! Initialise counter
  res%count = 0

end function vector_default


! (2) fill constructor
!     Constructs a container with n elements.
!     Each element is a copy of val.
function vector_fill( n, val ) result( res )

! The number of elements
  integer, intent(in) :: n

! The element to use to fill the vector
  type(t_eop), intent(in) :: val

! The result vector
  type(t_eop_vector_ftl) :: res

! Assign input to output
  call res%assign( n, val )

end function vector_fill


! (3) range constructor
!     Constructs a container with as many elements as the range (first,last),
!     with each element constructed from its corresponding element in that range,
!     in the same order.
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
function vector_range( first, last ) result( res )

! Iterator to first node to insert
  class(t_eop_vector_ftl_iterator), intent(in) :: first

! Iterator to last node to insert
  class(t_eop_vector_ftl_iterator), intent(in) :: last

! The result vector
  type(t_eop_vector_ftl) :: res

! Assign input to output
  call res%assign( first, last )

end function vector_range


! (4) copy constructor
!     Constructs a container with a copy of each of the elements in other,
!     in the same order.
function vector_copy( other ) result( res )

! The input vector
  type(t_eop_vector_ftl), intent(in) :: other

! The result vector
  type(t_eop_vector_ftl) :: res

! Assign input to output
  call res%assign( other )

end function vector_copy


! Copy constructor from array
function vector_copy_from_array( val ) result(res)

! The input array
  type(t_eop), dimension(:), intent(in) :: val

! The result vector
  type(t_eop_vector_ftl) :: res

! Assign input to output
  call res%assign( val )

end function vector_copy_from_array


! Assign content
! Assigns new contents to the container, replacing its current contents, and
! modifying its size accordingly.
subroutine vector_assign_from_vector( this, other )

! The output vector
  class(t_eop_vector_ftl), intent(out) :: this

! The input vector
  type(t_eop_vector_ftl), intent(in) :: other

! Local variables
  integer :: i

! Check if source is allocated
  if( other%count > 0 ) then

!   Allocate memory
    allocate(this%data( size(other%data) ))

!   Copy the vector
    do i = 1, other%count
      allocate( this%data(i)%element, source=other%data(i)%element )
    end do

 !   Set counter
     this%count = other%count

  end if

end subroutine vector_assign_from_vector


! Vector destructor
! Destroys the container object.
pure subroutine vector_( this )

! The vector
  type(t_eop_vector_ftl), intent(inout) :: this

! Clear the vector
  call this%clear()

end subroutine vector_


! Return iterator to beginning
! Returns an iterator pointing to the first element in the vector container.
! If the container is empty, the returned iterator value shall not be dereferenced.
function vector_begin( this ) result(res)

! The vector
  class(t_eop_vector_ftl), target, intent(in) :: this

! Pointer to beginning of the vector
  type(t_eop_vector_ftl_iterator) :: res

! Return pointer to first node in the vector
  if( this%count > 0 ) then
    res%idx = 1
  else
    res%idx = 0
  end if

! Assign the parent container
  res%parent => this

end function vector_begin


! Return iterator to end
! Returns an iterator referring to the last element in the vector container.
! If the container is empty, the returned iterator value shall not be dereferenced.
function vector_end( this ) result(res)

! The vector
  class(t_eop_vector_ftl), target, intent(in) :: this

! Pointer to end of the vector
  type(t_eop_vector_ftl_iterator) :: res

! Return pointer to the last stored node in the vector
  res%idx = this%count

!  Assign the parent container
   res%parent => this

end function vector_end


! Test whether container is empty
! Returns whether the vector container is empty (i.e. whether its size is 0).
! This function does not modify the container in any way.
pure function vector_empty( this ) result(res)

! The vector
  class(t_eop_vector_ftl), intent(in) :: this

! The vector empty status
  logical :: res

! Assign the return value
  res = ( this%count == 0 )

end function vector_empty


! Return size
! Returns the number of elements in the vector container.
pure function vector_size( this ) result(res)

! The vector
  class(t_eop_vector_ftl), intent(in) :: this

! The vector size
  integer :: res

! Assign the return value
  res = this%count

end function vector_size


! Return maximum size
! Returns the maximum number of elements that the vector container can hold.
! This is the maximum potential size the container can reach due to known system
! or library implementation limitations, but the container is by no means
! guaranteed to be able to reach that size: it can still fail to allocate
! storage at any point before that size is reached.
pure function vector_max_size() result(res)

! The vector size
  integer :: res

! Assign the return value (dummy from C++)
  res = 1073741823

end function vector_max_size


! Return size of allocated storage capacity
! Returns the size of the storage space currently allocated for the vector, 
! expressed in terms of elements.
! This capacity is not necessarily equal to the vector size. 
! It can be equal or greater, with the extra space allowing to accommodate 
! for growth without the need to reallocate on each insertion.
! Notice that this capacity does not suppose a limit on the size of the vector. 
! When this capacity is exhausted and more is needed, it is automatically expanded
! by the container (reallocating it storage space). 
! The capacity of a vector can be explicitly altered by calling member vecto::reserve.
pure function vector_capacity( this ) result(res)

! The vector
  class(t_eop_vector_ftl), intent(in) :: this

! The vector capacity
  integer :: res

! Assign the return value (dummy from C++)
  if( allocated(this%data) ) then
    res = size(this%data)
  else
    res = 0
  end if

end function vector_capacity


! Access first element
! Returns a reference to the first element in the vector container.
! Calling this function on an empty container causes undefined behaviour.
pure function vector_front( this ) result(res)

! The vector
  class(t_eop_vector_ftl), intent(in) :: this

! Pointer to the element in the first node in the vector
  type(t_eop), allocatable :: res

! Assign the return value
  call element_assign_allocatable( res, this%data(1)%element )

end function vector_front


! Access last element
! Returns a reference to the last element in the vector container.
! Calling this function on an empty container causes undefined behaviour.
pure function vector_back( this ) result(res)

! The vector
  class(t_eop_vector_ftl), intent(in) :: this

! Pointer to the element in the last node in the vector
  type(t_eop), allocatable :: res

! Assign the return value
  call element_assign_allocatable( res, this%data(this%count)%element )

end function vector_back


! Assign new content to container
! Assigns new contents to the vector container, replacing its current contents,
! and modifying its size accordingly.
! (1), the new contents are elements constructed from each of the elements in the
!      range between first and last, in the same order.
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
subroutine vector_assign_from_range( this, first, last )

! The output vector
  class(t_eop_vector_ftl), intent(out) :: this

! Iterator to first node to insert
  class(t_eop_vector_ftl_iterator), intent(in) :: first

! Iterator to last node to insert
  class(t_eop_vector_ftl_iterator), intent(in) :: last

! Local variables
  type(t_eop_vector_ftl_iterator) :: it
  integer :: i, n

! Allocate the vector storage
  n = distance( first, last ) + 1
  call this%reserve(n)

! Initialise navigation pointer
  it = first
  i = 1

! Loop on the vector
  do while( it%associated() )

!   Add this element
    allocate( this%data(i)%element, source=it%parent%data(it%idx)%element )

!   Check if this was the last element
    if( it == last ) exit

!   Iterate
    it = it%next()
    i = i + 1

  end do

! Set the counter
  this%count = n

end subroutine vector_assign_from_range


! Assign new content to container
! Assigns new contents to the vector container, replacing its current contents,
! and modifying its size accordingly.
! (2), the new contents are n elements, each initialized to a copy of val.
subroutine vector_assign_from_fill( this, n, val )

! The output vector
  class(t_eop_vector_ftl), intent(out) :: this

! The number of elements
  integer, intent(in) :: n

! The element to used to populate the container
  type(t_eop), intent(in) :: val

! Local variables
  integer :: i

! Allocate the vector storage
  call this%reserve(n)

! Add the element to the vector, the prescribed number of times
  do i = 1, n
    call element_assign_pointer( this%data(i)%element, val )
  end do

! Set the counter
  this%count = n

end subroutine vector_assign_from_fill


! Assign a vector from an array
subroutine vector_assign_from_array( this, val )

! The output vector
  class(t_eop_vector_ftl), intent(out) :: this

! The input array
  type(t_eop), dimension(:), intent(in) :: val

! Local variables
  integer :: i, n

! Allocate the vector storage
  n = size(val)
  call this%reserve(n)

! Copy the vector
  do i = 1, n
    call element_assign_pointer( this%data(i)%element, val(i) )
  end do

! Set the counter
  this%count = n

end subroutine vector_assign_from_array


! Insert element at beginning
! Inserts a new element at the beginning of the vector, right before its current
! first element. The content of val is copied (or moved) to the inserted element.
! This effectively increases the container size by one.
subroutine vector_push_front( this, val )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! The element
  type(t_eop), intent(in) :: val

! Local variables
  integer :: i

! Check for allocation status and initialize if required
  call this%realloc()

! Move all elements one position forward
  do i = this%count, 1, -1
    this%data(i+1)%element => this%data(i)%element
  end do

! Copy the element into the array
  call element_assign_pointer( this%data(1)%element, val )

! Increase counter
  this%count = this%count + 1

end subroutine vector_push_front


! Delete first element
! Removes the first element in the vector container,
! effectively reducing its size by one.
! This destroys the removed element.
pure subroutine vector_pop_front( this )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! Local counter
  integer :: i

! Check if the vector is already empty
  if( this%count > 0 ) then

!   Destroy data element in the first vector node
    deallocate( this%data(1)%element )

!   Shift all elements one position backwards
    do i = 1, this%count - 1
      this%data(i)%element => this%data(i+1)%element
    end do

!   Decrease counter
    this%count = this%count - 1

  end if

end subroutine vector_pop_front


! Add element at the end
! Adds a new element at the end of the vector container, after its current
! last element. The content of val is copied (or moved) to the new element.
! This effectively increases the container size by one.
subroutine vector_push_back( this, val )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! The element
  type(t_eop), intent(in) :: val

! Check for allocation status and initialize if required
  call this%realloc()

! Increase counter
  this%count = this%count + 1

! Copy the element into the array
  call element_assign_pointer( this%data(this%count)%element, val )

end subroutine vector_push_back


! Delete last element
! Removes the last element in the vector container,
! effectively reducing the container size by one.
! This destroys the removed element.
pure subroutine vector_pop_back( this )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! Check if the vector is already empty
  if( this%count > 0 ) then

!   Destroy data element in the last vector node
    deallocate( this%data(this%count)%element )

!   Decrease counter
    this%count = this%count - 1

  end if

end subroutine vector_pop_back


! Insert elements
! The container is extended by inserting new elements before the element at
! the specified position.
! This effectively increases the vector size by one.
! Iterator remains associated to the node in input
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
function vector_insert_single( this, iterator, val ) result(res)

! The vector
  class(t_eop_vector_ftl), target, intent(inout) :: this

! Iterator to node used as reference for insertion
  class(t_eop_vector_ftl_iterator), intent(in) :: iterator

! The element
  type(t_eop), intent(in) :: val

! Iterator to the inserted element
  type(t_eop_vector_ftl_iterator) :: res

! Insert by index
  res%idx = this%insert( iterator%idx, val )

! Set pointer to parent
  res%parent => this

end function vector_insert_single


! Insert elements
! The container is extended by inserting new elements before the element at
! the specified position (by absolute index)
! This effectively increases the vector size by one.
! Iterator remains associated to the node in input
function vector_insert_single_at( this, index, val ) result(res)

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! Index to element used as reference for insertion
  integer, intent(in) :: index

! The element
  type(t_eop), intent(in) :: val

! The index in this of the inserted element
  integer :: res

! Counter
  integer :: i

! Check that index is within bounds; else do nothing
  if( index >= 1 .and. index <= this%count ) then

!   Check for allocation status and initialize if required
    call this%realloc()

!   Make room for the inserted element. Move pointers, not memory
    do i = this%count, index, -1
      this%data(i+1)%element => this%data(i)%element
    end do

!   Allocate memory for the new element
    allocate( this%data(index)%element, mold=val )

!   Copy element
    call element_assign_pointer( this%data(index)%element, val )

!   Increase vector size
    this%count = this%count + 1

!   Set the return index
    res = index

  end if

end function vector_insert_single_at


! Insert elements
! The container is extended by inserting new elements before the element at
! the specified position.
! This effectively increases the vector size by n.
! Iterator remains associated to the node in input
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
function vector_insert_fill( this, iterator, n, val ) result(res)

! The vector
  class(t_eop_vector_ftl), target, intent(inout) :: this

! Iterator to node used as reference for insertion
  class(t_eop_vector_ftl_iterator), intent(in) :: iterator

! The number of times to insert the element
  integer, intent(in) :: n

! Iterator to the inserted element
  type(t_eop_vector_ftl_iterator) :: res

! The element
  type(t_eop), intent(in) :: val

! Insert by index
  res%idx = this%insert( iterator%idx, n, val )

! Set pointer to parent
  res%parent => this

end function vector_insert_fill


! Insert elements
! The container is extended by inserting new elements before the element at
! the specified position (by absolute index)
! This effectively increases the vector size by n.
! Iterator remains associated to the node in input
function vector_insert_fill_at( this, index, n, val ) result(res)

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! Index to element used as reference for insertion
  integer, intent(in) :: index

! The number of times to insert the element
  integer, intent(in) :: n

! The element
  type(t_eop), intent(in) :: val

! Index in this to the start of the inserted section
  integer :: res

! Counter
  integer :: i, j

! Check that index is within bounds; else do nothing
  if( index >= 1 .and. index <= this%count ) then

!   Check for allocation status
    call this%reserve( this%count + n )

!   Make room for the inserted elements. Move pointers, not memory
    do i = this%count, index, -1
      this%data(i+n)%element => this%data(i)%element
    end do

!   Loop on the number of elements to insert
    do i = 1, n

!     Allocate memory for the new element and copy
      j = index + i - 1
      call element_assign_pointer( this%data(j)%element, val )

    end do

!   Increase vector size
    this%count = this%count + n

!   Set the return index
    res = index

  end if

end function vector_insert_fill_at


! Insert elements
! The container is extended by inserting new elements before the element at
! the specified position.
! This effectively increases the vector size by the number of element in (first,last].
! Iterator remains associated to the node in input
! This assumes that the range (first,last) is an actual connected range,
! i.e. it is possible to navigate from first to last, otherwise the resulting
! vector is corrupted.
function vector_insert_range( this, iterator, first, last ) result(res)

! The vector
  class(t_eop_vector_ftl), target, intent(inout) :: this

! Iterator to node used as reference for insertion
  type(t_eop_vector_ftl_iterator), intent(in) :: iterator

! Iterator to first node to insert
  type(t_eop_vector_ftl_iterator), intent(in) :: first

! Iterator to last node to insert
  type(t_eop_vector_ftl_iterator), intent(in) :: last

! Iterator to the inserted section
  type(t_eop_vector_ftl_iterator) :: res

! Insert by index
  res%idx = this%insert( iterator%idx, first, last )

! Set pointer to parent
  res%parent => this

end function vector_insert_range


! Insert elements
! The container is extended by inserting new elements before the element at
! the specified position (by absolute index)
! This effectively increases the vector size by the number of element in (first,last].
! Iterator remains associated to the node in input
! This assumes that the range (first,last) is an actual connected range,
! i.e. it is possible to navigate from first to last, otherwise the resulting
! vector is corrupted.
function vector_insert_range_at( this, index, first, last ) result(res)

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! Index to element used as reference for insertion
  integer, intent(in) :: index

! Iterator to first node to insert
  type(t_eop_vector_ftl_iterator), intent(in) :: first

! Iterator to last node to insert
  type(t_eop_vector_ftl_iterator), intent(in) :: last

! Index in this to the start of the inserted section
  integer :: res

! Local nodes
  integer :: inode, ifirst, ilast
  integer :: i, n

! Check that index is within bounds; else do nothing
  if( index >= 1 .and. index <= this%count ) then

!   Check the number of nodes to insert
    n = distance( first, last ) + 1

!   Check for allocation status and initialize if required
    call this%reserve( this%count + n )

!   Make room for the inserted elements. Move pointers, not memory
    do i = this%count, index, -1
      this%data(i+n)%element => this%data(i)%element
    end do

!   Initialise input node navigation
    ifirst = first%idx
    ilast = last%idx
    i = 1

!   Navigate the input nodes
    do inode = ifirst, ilast

!     Allocate memory for the new element and copy
      call element_assign_pointer( this%data(index+i-1)%element, first%parent%data(inode)%element )

!     Iterate
      i = i + 1

    end do

!   Increase vector size
    this%count = this%count + n

!   Set the return index
    res = index

  end if

end function vector_insert_range_at


! Insert elements
! The container is extended by inserting new elements before the element at
! the specified position.
! This effectively increases the vector size by the size of the array.
! Iterator remains associated to the node in input
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
function vector_insert_array( this, iterator, val ) result(res)

! The vector
  class(t_eop_vector_ftl), target, intent(inout) :: this

! Iterator to node used as reference for insertion
  class(t_eop_vector_ftl_iterator), intent(in) :: iterator

! The element
  type(t_eop), dimension(:), intent(in) :: val

! Iterator to the inserted element
  type(t_eop_vector_ftl_iterator) :: res

! Insert by index
  res%idx = this%insert( iterator%idx, val )

! Assign pointer to parent
  res%parent => this

end function vector_insert_array


! Insert elements
! The container is extended by inserting new elements before the element at
! the specified position (by absolute index)
! This effectively increases the vector size by n.
! Iterator remains associated to the node in input
function vector_insert_array_at( this, index, val ) result(res)

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! Index to element used as reference for insertion
  integer, intent(in) :: index

! The element
  type(t_eop), dimension(:), intent(in) :: val

! Index in this to the start of the inserted section
  integer :: res

! Counter
  integer :: i, j, n

! Check that index is within bounds; else do nothing
  if( index >= 1 .and. index <= this%count ) then

!   Compute the number of elements
    n = size(val)

!   Check for allocation status and initialize if required
    call this%reserve( this%count + n )

!   Make room for the inserted elements. Move pointers, not memory
    do i = this%count, index, -1
      this%data(i+n)%element => this%data(i)%element
    end do

!   Loop on the number of elements to insert
    do i = 1, n

!     Allocate memory for the new element nd copy
      j = index + i - 1
      call element_assign_pointer( this%data(j)%element, val(i) )

    end do

!   Increase vector size
    this%count = this%count + n

!   Set the return index
    res = index

  end if

end function vector_insert_array_at


! Erase elements
! Removes from the vector container either a single element (position)
! This effectively reduces the container size by one element, which is destroyed.
! Input iterator returns not associated
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
subroutine vector_erase_single( this, iterator )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! Iterator to node to remove
  class(t_eop_vector_ftl_iterator), intent(inout) :: iterator

! Erase by index
  call this%erase( iterator%idx )

end subroutine vector_erase_single


! Erase elements
! Removes from the vector container either a single element (position by absolute index)
! This effectively reduces the container size by one element, which is destroyed.
! Input iterator returns not associated
subroutine vector_erase_single_at( this, index )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! Index to element erase
  integer, intent(in) :: index

! Local variables
  integer :: i, j

! Check that there are elements in the vector
  if( this%count > 0 ) then

!   Check that index is within bounds; else do nothing
    if( index >= 1 .and. index <= this%count ) then

!     Delete the element
      deallocate( this%data(index)%element )

!     Reallocate the elements. Move pointers, not memory
      do i = index, this%count
        j = i + 1
        if( j <= size(this%data) ) then
          this%data(i)%element => this%data(j)%element
        else
          this%data(i)%element => null()
        end if
      end do

!     Nullify the last element
      this%data(this%count)%element => null()

!     Decrease counter
      this%count = this%count - 1

    end if

  end if

end subroutine vector_erase_single_at


! Erase elements
! Removes from the vector container either a range of elements (first,last).
! This effectively reduces the container size by the number of elements removed,
! which are destroyed.
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
subroutine vector_erase_range( this, first, last )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! Iterator to first node to remove
  type(t_eop_vector_ftl_iterator), intent(in) :: first

! Iterator to last node to remove
  class(t_eop_vector_ftl_iterator), intent(in) :: last

! Erase using indexes
  call this%erase( first%idx, last%idx )

end subroutine vector_erase_range


! Erase elements
! Removes from the vector container either a range of elements (first,last).
! This effectively reduces the container size by the number of elements removed,
! which are destroyed.
subroutine vector_erase_range_at( this, ifirst, ilast )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! Index to first node to remove
  integer, intent(in) :: ifirst

! Index to last node to remove
  integer, intent(in) :: ilast

! Local variables
  integer :: i, n, m

! Check that indexes are within bounds; else do nothing
  if( ifirst >= 1 .and. ifirst <= this%count ) then
    if( ilast >= 1 .and. ilast <= this%count ) then

!     Compute the number of nodes to remove
      n = ilast - ifirst + 1

!     Compute the number of nodes after ilast
      m = this%count - ilast

!     Delete the elements
      do i = ifirst, ilast
        deallocate( this%data(i)%element )
      end do

!     Reallocate the elements at end of vector. Move pointers, not memory
      do i = 1, m
        this%data(ifirst+i-1)%element => this%data(ilast+i)%element
      end do

!     Nullify the last elements
      do i = ifirst + m + 1, this%count
        this%data(i)%element => null()
      end do

!     Decrease counter
      this%count = this%count - n

    end if
  end if

end subroutine vector_erase_range_at


! Swap content
! Exchanges the content of the container by the content of other, which is another vector of the same type.
! Sizes may differ.
! After the call to this member function, the elements in this container are
! those which were in other before the call,
! and the elements of other are those which were in this.
! All iterators, references and pointers remain valid for the swapped objects.
pure subroutine vector_swap( this, other )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! The vector to swap
  type(t_eop_vector_ftl), intent(inout) :: other

! Local variables
  type(t_eop), pointer :: tmpe
  integer :: i, n

! Check for required reallocations
! Note that only one of the arrays requires reallocaiton; the other one should fit
  if( this%count > size(other%data) ) then
    call other%reserve(this%count)
  else if( other%count > size(this%data) ) then
    call this%reserve(other%count)
  end if

! Swap on the common part of the nodes (move pointers, not memory)
  n = min( this%count, other%count )
  do i = 1, n
    tmpe => this%data(i)%element
    this%data(i)%element => other%data(i)%element
    other%data(i)%element => tmpe
  end do

 ! Swap the extra part of the nodes in the largest vector
   if(this%count > other%count ) then
    do i = n + 1, this%count
      other%data(i)%element => this%data(i)%element
      this%data(i)%element => null()
    end do
   else
    do i = n + 1, other%count
      this%data(i)%element => other%data(i)%element
      other%data(i)%element => null()
    end do
  end if

! Swap the vector sizes
  n = this%count
  this%count = other%count
  other%count = n

end subroutine vector_swap


! Change size
! Resizes the container so that it contains n elements.
! If n is smaller than the current container size, the content is reduced to its
! first n elements, removing those beyond (and destroying them).
! If n is greater than the current container size, the content is expanded by
! inserting at the end as many elements as needed to reach a size of n.
! If val is specified, the new elements are initialized as copies of val,
! otherwise, they are value-initialized.
subroutine vector_resize( this, n, val )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! The number of elements in the resulting vector
  integer, intent(in) :: n

! The element to use to initialise traling elements
  type(t_eop), optional, target, intent(in) :: val

! Local variables
  integer :: i
  type(t_eop), pointer :: init
  type(t_eop), allocatable, target :: default

! Vector size is greater than requested size
  if( this%count < n ) then

!   Check if initialisation value has been given
    if( present(val) ) then
      init => val
    else
      allocate( default, mold=this%data(1)%element )
      init => default
    end if

!   Loop on the number of element to add
    do i = this%count + 1, n
      call this%push_back(init)
    end do

! Vector size is smaller than requested size
  else if( this%count > n ) then

!   Loop on the number of element to remove
    do i = this%count, n + 1, -1
      call this%pop_back()
    end do

! Vector size is the same as the requested size
  else
  endif

end subroutine vector_resize


! Clear content
! Removes all elements from the vector container (which are destroyed)
! and leaving the container with a size of 0
pure subroutine vector_clear( this )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! Local variables
  integer :: i

! Verify vector status
  if( allocated( this%data ) ) then

!   Deallocate all elements
    do i = 1, this%count
      deallocate( this%data(i)%element )
    end do

!   Deallocate the vector internal array
    deallocate( this%data )

  end if

! Reset counter
  this%count = 0

end subroutine vector_clear


! Get reference to element at given position
function vector_at_get( this, index ) result(res)

! The vector
  class(t_eop_vector_ftl), intent(in) :: this

! The position in the vector
  integer, intent(in) :: index

! The object to replace
  type(t_eop), pointer :: res

! Check that indexes are within bounds; else do nothing
  if( index >= 1 .and. index <= this%count ) then
    res => this%data(index)%element
  else
    res => null()
  end if

end function vector_at_get


! Get the value pointed by the index
pure function vector_element_from_index( this, index ) result(res)

! The vector
  class(t_eop_vector_ftl), intent(in) :: this

! The index
  integer, intent(in) :: index

! The returned element
  type(t_eop), allocatable :: res

! Check that index is within bounds
  if( index >= 1 .and. index <= this%count ) then

!   Get the element
    allocate( res, source=this%data(index)%element )

  end if

end function vector_element_from_index


! Get the value pointed by the index
function vector_element_ptr_from_index( this, index ) result(res)

! The vector
  class(t_eop_vector_ftl), target, intent(in) :: this

! The index
  integer, intent(in) :: index

! The returned element
  type(t_eop), pointer :: res

! Check that index is within bounds
  if( index >= 1 .and. index <= this%count ) then

!   Get the element pointer
    res => this%data(index)%element

  end if

end function vector_element_ptr_from_index


! Set the value pointed by the index
! Provide destructor is the replaced element has complex memory components
subroutine vector_element_to_index( this, index, element )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! The index
  integer, intent(in) :: index

! The element
  type(t_eop), intent(in) :: element

! Check that index is within bounds; else do nothing
  if( index >= 1 .and. index <= this%count ) then

!   Set the element
    if( associated(this%data(index)%element) ) deallocate(this%data(index)%element)
    allocate( this%data(index)%element, source=element )

  end if

end subroutine vector_element_to_index


! Transfer elements from vector to vector
! Transfers elements from source into the container, inserting them at position.
! This effectively inserts those elements into the container and removes them from source,
! altering the sizes of both containers.
! The operation does not involve the construction or destruction of any element.
! (1) transfers all the elements of source into the container.
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
subroutine vector_splice_vector( this, position, source )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! The position in vector to insert the elements
  class(t_eop_vector_ftl_iterator), intent(in) :: position

! The source vector
  class(t_eop_vector_ftl), intent(inout) :: source

! Call the generic splice function
  call this%vector_splice_nodes( position, source, 1, source%count )

! Deallocate memory in source
  deallocate(source%data)

end subroutine vector_splice_vector


! Transfer elements from vector to vector
! Transfers elements from source into the container, inserting them at position.
! This effectively inserts those elements into the container and removes them from source,
! altering the sizes of both containers.
! The operation does not involve the construction or destruction of any element.
! (2) transfers only the element pointed by iterator from x into the container.
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
subroutine vector_splice_single( this, position, source, it )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! The position in vector to insert the elements
  class(t_eop_vector_ftl_iterator), intent(in) :: position

! The source vector
  class(t_eop_vector_ftl), intent(inout) :: source

! The element position in source
  class(t_eop_vector_ftl_iterator), intent(in) :: it

! Call the generic splice function
  call this%vector_splice_nodes( position, source, it%idx, it%idx )

! Deallocate memory in source if source array is empty
  if( source%count == 0 ) deallocate(source%data)

end subroutine vector_splice_single


! Transfer elements from vector to vector
! Transfers elements from source into the container, inserting them at position.
! This effectively inserts those elements into the container and removes them from source,
! altering the sizes of both containers.
! The operation does not involve the construction or destruction of any element.
! (3) transfers the range (first,last) from source into the container.
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
subroutine vector_splice_range( this, position, source, first, last )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! The position in vector to insert the elements
  class(t_eop_vector_ftl_iterator), intent(in) :: position

! The source vector
  class(t_eop_vector_ftl), intent(inout) :: source

! The first position in source to retrieve elements
  class(t_eop_vector_ftl_iterator), intent(in) :: first

! The last position in source to retrieve elements
  class(t_eop_vector_ftl_iterator), intent(in) :: last

! Call the generic splice function
  call this%vector_splice_nodes( position, source, first%idx, last%idx )

! Deallocate memory in source if source array is empty
  if( source%count == 0 ) deallocate(source%data)

end subroutine vector_splice_range


! Transfer elements from vector to vector
! Actual implementation of splcie at node level
subroutine vector_splice_nodes( this, position, source, ifirst, ilast )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! The position in vector to insert the elements
  class(t_eop_vector_ftl_iterator), intent(in) :: position

! The source vector
  class(t_eop_vector_ftl), intent(inout) :: source

! The first position in source to retrieve elements
  integer, intent(in) :: ifirst

! The last position in source to retrieve elements
  integer, intent(in) :: ilast

! Local variables
  integer :: i, j, n
  integer :: iposition

! Check the input iterator
  if( position%associated() ) then

!   Normal insertion
    iposition = position%idx

  else

!   Append at end of this
    iposition = this%count + 1

  end if

! Check source contents
  if( ifirst >= 1 .and. ifirst <= source%count ) then
    if( ilast >= 1 .and. ilast <= source%count ) then

!     Count the number of nodes in range
      n = ilast - ifirst + 1

!     Raallocate memory in container
      call this%reserve( this%count + n )

!     Make room for the inserted elements. Move pointers, not memory
      do i = this%count, iposition, -1
        this%data(i+n)%element => this%data(i)%element
      end do

!     Transfer elements from source
      do i = 1, n
        this%data(iposition+i-1)%element => source%data(ifirst+i-1)%element
      end do

!     Recompute number of elements in this
      this%count = this%count + n

!     Remove the nodes from source
      do i = ifirst, source%count
        j = i + n
        if( j <= size(source%data)) then
          source%data(i)%element => source%data(j)%element
        else
          source%data(i)%element => null()
        end if
      end do

!     Nullify tralining elements in source
      do i = source%count, source%count - n + 1, -1
        source%data(i)%element => null()
      end do

!     Recompute number of elements in source
      source%count = source%count - n

    end if
  end if

end subroutine vector_splice_nodes


! Remove elements with specific value
! Removes from the container all the elements that compare equal to val.
! This calls the destructor of these objects and reduces the container
! size by the number of elements removed.
subroutine vector_remove( this, val )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! The value to use as comparison for the removal
  type(t_eop), intent(in) :: val

! Local variables
  integer :: i

! Check that there are elements in the vector
  if( this%count > 0 ) then

!   Loop on the nodes
    i = 1
    do

!     Check removal
      if( this%data(i)%element == val ) then

!       Remove
        call this%erase(i)
      else

!       Iterate if not removal only
        i = i + 1

      end if

!     Checkt exit condition
      if( i > this%count ) exit

    end do

  end if

end subroutine vector_remove


! Remove elements fulfilling condition
! Removes from the container all the elements for which Predicate pred returns true.
! This calls the destructor of these objects and reduces the container size by the
! number of elements removed.
! The function calls pred(i%get_element()) for each element (where i is an iterator
! to that element). Any of the elements in the vector for which this returns true,
! are removed from the container.
subroutine vector_remove_if( this, pred )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! THe predicate to use for removal selection
  procedure(predicate) :: pred

! Local variables
  integer :: i

! Check that there are elements in the vector
  if( this%count > 0 ) then

!   Loop on the nodes
    i = 1
    do

!     Check removal
      if( pred( this%data(i)%element ) ) then

!       Remove
        call this%erase(i)
      else

!       Iterate if not removal only
        i = i + 1

      end if

!     Checkt exit condition
      if( i > this%count ) exit

    end do

  end if

end subroutine vector_remove_if


! Remove duplicate values
! (1) removes all but the first element from every consecutive group of equal
!     elements in the container.
! (2) takes as argument a specific comparison function that determine the "uniqueness"
!     of an element. In fact, any behavior can be implemented (and not only an equality
!     comparison), but notice that the function will call binary_pred(*i,*j)) for all
!     pairs of elements (where i and j are iterators to the elements) and remove j from
!     the vector if the predicate returns true.
subroutine vector_unique( this, bpred )

! The vector to sort
  class(t_eop_vector_ftl), intent(inout) :: this

! The interface for the binary predicate (optional)
! If the operator is not provided, then operator(==) is assumed
  procedure(binary_predicate), optional :: bpred

! Local variables
  integer :: iout, iin
  logical :: check

! Outer-loop on the nodes
  iout = 1
  do while( iout <= this%count )

!   Inner-loop on the nodes
    iin = iout + 1
    do while( iin <= this%count )

!     Compare the inner and the outer elements
      if( present( bpred ) ) then
        check = bpred( this%data(iout)%element, this%data(iin)%element )
      else
        check = ( this%data(iout)%element == this%data(iin)%element )
      end if

!     Remove inner element if equal to outer
      if( check ) then
        call this%erase( iin )
      end if

!     Iterate inner loop
      iin = iin + 1

    end do

!   Iterate outer loop
    iout = iout + 1

  end do

end subroutine vector_unique


! Merge sorted vectors
! Merges other into the vector by transferring all of its elements at their respective
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
! This function requires that the vector containers have their elements already ordered by
! value (or by comp) before the call.
! The function does nothing if (other == this).
subroutine vector_merge( this, other, comp )

! The vector
  class(t_eop_vector_ftl), target, intent(inout) :: this

! the vector to swap
  type(t_eop_vector_ftl), target, intent(inout) :: other

! Comparison function (optional)
  procedure(comparison), optional :: comp

! Local variables
  integer :: iin, iout
  integer :: i, j
  logical :: insert

! Check if the same vector is input
! Use addresses of first element to decide on same vector
  if( .not. associated( this%data(1)%element, other%data(1)%element ) ) then

!   Reserve size for the merge
    call this%reserve( this%count + other%count )
  
!   Initialise the nodes in other (this is the outer loop)
    outer : do iout = 1, other%count

!     Initialise navigation in this vector (this is the inner loop)
!     Each step of the outer loop shall start the inner loop in the last postions
!     in the vector (assuming tha both vectors are ordered) not to iterate over the
!     whole vector from the beginning in each iteration of the outer loop.
      inner : do iin = 1, this%count

!       Check if this node is the insertion point
        if( present(comp) ) then
          insert = comp( other%data(iout)%element, this%data(iin)%element )
        else
          insert = ( other%data(iout)%element < this%data(iin)%element )
        end if

!       Check if this element must be inserted
        if( insert ) then

!         Insert the node
          do i = this%count, iin, -1
            j = i + 1
            if( j <= size(this%data) ) then
              this%data(j)%element => this%data(i)%element
            end if
          end do
          this%data(iin)%element => other%data(iout)%element

!         Update the counter
          this%count = this%count + 1

!         Exit the inner loop
          exit

        end if

      end do inner

!     Check past last node
!     If this point is reached with node=null this means that
!     the remaining elements in other are past the end of this
      if( iin > this%count ) then

!       Append the other node at the end of this vector
        this%count = this%count + 1
        this%data(this%count)%element => other%data(iout)%element

      end if

    end do outer

!   Reset other
    deallocate( other%data )
    other%count = 0

  end if

end subroutine vector_merge


! Sort elements in container
! Sorts the elements in the vector, altering their position within the container.
! (1) The sorting is performed by applying an algorithm that uses operator < to compare elements
! (2) The sorting is performed by applying an algorithm that uses comp to compare elements.
!     This comparison shall produce a strict weak ordering of the elements (i.e., a
!     consistent transitive comparison, without considering its reflexiveness).
! The resulting order of equivalent elements is stable: i.e., equivalent elements
! preserve the relative order they had before the call.
! The entire operation does not involve the construction, destruction or copy of any element
! object. Elements are moved within the container.
subroutine vector_sort( this, compare )

! the vector to sort
  class(t_eop_vector_ftl), intent(inout) :: this

! The interface for the comparison operator (optional)
! If the operator is not provided, then operator(<) is assumed
  procedure(comparison), optional :: compare

! Call the sorting routine
  call this%quick_sort( 1, this%count, compare )

end subroutine vector_sort


! Sorting subroutine (Quick-sort method) algorithm kernel
recursive subroutine quick_sort( this, left, right, compare )

! Array of indices
  class(t_eop_vector_ftl), intent(inout) :: this

! Quick-sort partition left index
  integer, intent(in) :: left

! Quick-sort partition right index
  integer, intent(in) :: right

! The interface for the comparison operator (optional)
! If the operator is not provided, then operator(<) is assumed
  procedure(comparison), optional :: compare

! Counters
  integer :: i, last

! The result of the element comparison
  logical :: sortby

! Return if array contains fewer than 2 elements
  if( left < right ) then

!   Move partition element to the beginning of the array
    call this%swap( left, ( left + right ) / 2 )
    last = left

!   Partition
    do i = left + 1, right

!     Check element
      if( present( compare ) ) then
        sortby = compare( this%data(left)%element, &
                          this%data(i)%element )
      else
        sortby = ( this%data(left)%element < this%data(i)%element )
      end if

!     Swap elements
      if( .not. sortby ) then
        last = last + 1
        call  this%swap( last, i )
      end if

    end do

!   Restore partition element
    call this%swap( left, last )
    call this%quick_sort( left, last - 1, compare )
    call this%quick_sort( last + 1, right, compare )

  end if

end subroutine quick_sort


! Swap two elements in a vector  from their indexes
pure subroutine vector_swap_index( this, index1, index2 )

! The vector to have the elementes swapped
  class(t_eop_vector_ftl), intent(inout) :: this

! Index to first element
  integer, intent(in) :: index1

! Index to second element
  integer, intent(in) :: index2

! Local pointer
  type(t_eop), pointer :: tmp_ptr

! Swap elements; swap pointers, not memory
  tmp_ptr => this%data(index1)%element
  this%data(index1)%element => this%data(index2)%element
  this%data(index2)%element => tmp_ptr

end subroutine vector_swap_index


! Swap two nodes in a vector pointed by iterators
pure subroutine vector_iterator_swap_iterators( iter1, iter2 )

! Iterator to first element
  class(t_eop_vector_ftl_iterator), intent(inout) :: iter1

! Iterator to second element
  type(t_eop_vector_ftl_iterator), intent(inout) :: iter2


! Swap the element by their indexes
  call iter1%parent%swap( iter1%idx, iter2%idx )

end subroutine vector_iterator_swap_iterators


! Reverse the order of elements in a vector
pure subroutine vector_reverse( this )

! The vector to reverse
  class(t_eop_vector_ftl), intent(inout) :: this

! Intermediate indexes
  integer :: lidx, ridx

! Loop simultaneously from left to right and from right to left
  lidx = 1
  ridx = this%count

! Loop until both indexes are identical (odd number of elements)
! or until the two indexes cross (even number of elements)
  do

!   Check indexes
    if( ridx <= lidx ) exit

!   Swapt elements
    call this%swap( lidx, ridx )

!   Iterate
    ridx = ridx - 1
    lidx = lidx + 1

  end do

end subroutine vector_reverse


! Binary search subroutine (assumes ascending sorted  vector) front-end
function vector_binary_search( this, item, isless, isgreater ) result(res)

! The vector to search
  class(t_eop_vector_ftl), target, intent(in) :: this

! The element to look for
  type(t_eop), intent(in) :: item

! The iterator to the vector element (not associated if not found)
  type(t_eop_vector_ftl_iterator) :: res

! If the operator is not provided, then operator(<) is assumed
  procedure(comparison), optional :: isless

! If the operator is not provided, then operator(>) is assumed
  procedure(comparison), optional :: isgreater

! Call the sorting routine
  res%idx = recursive_binary_search( this, 1, this%count, item, isless, isgreater )

! Set pointer to parent container
  res%parent => this

end function vector_binary_search


! Binary search subroutine (assumes ascending sorted vector) algorithm kernel
recursive function recursive_binary_search( this, left, right, item, isless, isgreater ) result(res)

! The vector to search
  type(t_eop_vector_ftl), intent(in) :: this

! Quick-sort partition left position
  integer, intent(in) :: left

! Quick-sort partition right position
  integer, intent(in) :: right

! The element to look for
  type(t_eop), intent(in) :: item

! If the operator is not provided, then operator(<) is assumed
  procedure(comparison), optional :: isless

! If the operator is not provided, then operator(>) is assumed
  procedure(comparison), optional :: isgreater

! The index to the vector element (0 if not found)
  integer :: res

! The result of the element comparison
  logical :: check

! Local pointers
  integer :: mid
  type(t_eop), pointer :: ref

! Check if still elements to search
  if( right < left ) then

!   Not found
    res = 0

  else

!   Compute next partition point
    mid = ( left + right ) / 2
    ref => this%data(mid)%element

!   Check element
    if( present( isless ) ) then
      check = isless( item, ref )
    else
      check = ( item < ref )
    end if
    if( check ) then

!     Invoke search for lower partition
      res = recursive_binary_search( this, left, mid-1, item, isless, isgreater )

    else

!     Check element
      if( present( isgreater ) ) then
        check = isgreater( item, ref )
      else
        check = ( item > ref )
      end if
      if( check ) then

!       Invoke search for upper partition
        res = recursive_binary_search( this, mid+1, right, item, isless, isgreater )

      else

!       Element found
        res = mid

      end if

    end if

  end if

end function recursive_binary_search


! Search a vector following certain criteria
function vector_select( this, reference, bpred ) result(res)

! The vector to search selecting items
  class(t_eop_vector_ftl), intent(in) :: this

! The reference element to use as selecting pattern
  type(t_eop), intent(in) :: reference

! The interface for the comparison operator
  procedure(binary_predicate), optional :: bpred

! The output vector contatining the selectec elements
  type(t_eop_vector_ftl) :: res

! Counter
  integer :: i

! Local variables
  logical :: flag


! Loop on the vector elements
  do i = 1, this%count

!   Decide whether element matches the criterion
    if( present(bpred) ) then
      flag = bpred( this%data(i)%element, reference )
    else
      flag = ( this%data(i)%element == reference )
    end if
    if( flag ) then
      call res%push_back( this%data(i)%element )
    end if

  end do

end function vector_select


! Create an array (allocatabe) of elements from a vector
function vector_array( this ) result(res)

! The input vector
  class(t_eop_vector_ftl), intent(in) :: this

! The returned array of elements (unallocated if memory failure)
  type(t_eop), allocatable, dimension(:) :: res

! Memory allocation status
  integer :: status

! Local counter
  integer :: i

! Allocate memory for returned array
  allocate( res( this%count ), mold=this%data(1)%element, stat=status )
  if( status == 0 ) then

!   Copy vector elements
    do i = 1, this%count
      res(i) = this%data(i)%element
    end do

  end if

end function vector_array


! Implement the assignment between two elements (contained in the container node)
! Centralises the implementation allowing the handling of polymorphism (store parent classes pointing derived clasess)
! at the time thta allows the invocation of assignment operators in the cases when the element implements it
pure subroutine element_assign_pointer( left, right )

! Element to be allocated and assigned (pointer interface)
  type(t_eop), pointer, intent(inout) :: left

! Source element
  type(t_eop), intent(in) :: right

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
  type(t_eop), allocatable, intent(inout) :: left

! Source element
  type(t_eop), intent(in) :: right

! Allocate first. Use mold to allow polymorphic object storage through parent class
  allocate( left, mold=right )

! Assign explicitly to allow invoking the assignment operator if implemented in the element
  left = right

end subroutine element_assign_allocatable











! Return iterator to next node in the vector
function vector_iterator_next( this ) result(res)

! the vector iterator
  class(t_eop_vector_ftl_iterator), intent(in) :: this

! Pointer to next node in the vector
  type(t_eop_vector_ftl_iterator) :: res

! Assign index
  if( this%idx >= this%parent%count ) then
    res%idx = 0
  else
    res%idx = this%idx + 1
  end if

! Assign container vector pointer
  res%parent => this%parent

end function vector_iterator_next


! Return iterator to previous node in the vector
function vector_iterator_previous( this ) result(res)

! the vector iterator
  class(t_eop_vector_ftl_iterator), intent(in) :: this

! The previous node in the vector
  type(t_eop_vector_ftl_iterator) :: res


! Assign index
  if( this%idx <= 1 ) then
    res%idx = 0
  else
    res%idx = this%idx - 1
  end if

! Assign container vector pointer
  res%parent => this%parent

end function vector_iterator_previous


! Return the vector index in an iterator
pure function vector_iterator_index( iterator ) result(res)

! The vector iterator
  class(t_eop_vector_ftl_iterator), intent(in) :: iterator

! The vector index
  integer :: res

! Return the vector index
  res = iterator%idx

end function vector_iterator_index


! Return the distance (number of elements) between two nodes (by iterator)
function vector_iterator_distance( origin, target ) result(res)

! The first node
  class(t_eop_vector_ftl_iterator), intent(in) :: origin

! The final node
  class(t_eop_vector_ftl_iterator), intent(in) :: target

! The number of elements between the iterators
  integer :: res

! Check end node asscoiation
! Return -1 if target lees that origin
  if( origin%idx > target%idx ) then
    res = -1
  else
    res = target%idx - origin%idx
  end if

end function vector_iterator_distance


! Return the association status of an iterator
pure function vector_iterator_associated( this ) result(res)

! The vector iterator
  class(t_eop_vector_ftl_iterator), intent(in) :: this

! The association status
  logical :: res

! Return the association status
  res = ( this%idx > 0 )

end function vector_iterator_associated


! Nullify an iterator
pure subroutine vector_iterator_nullify( this )

! The vector iterator
  class(t_eop_vector_ftl_iterator), intent(inout) :: this

! Nullify pointer
  this%idx = 0

end subroutine vector_iterator_nullify


! Get an elememnt in the node pointed by the iterator
pure function vector_iterator_get_element( this ) result(res)

! The iterator
  class(t_eop_vector_ftl_iterator), intent(in) :: this

! The returned pointer to element
  type(t_eop), allocatable :: res


! Check that index is within bounds
  if( this%idx >= 1 .and. this%idx <= this%parent%count ) then

!   Return the data element pointed by the iterator
    call element_assign_allocatable( res, this%parent%data(this%idx)%element )

  end if

end function vector_iterator_get_element


! Get a pointer to the data elememnt in the node pointed by the iterator
function vector_iterator_get_element_ptr( this ) result(res)

! The iterator
  class(t_eop_vector_ftl_iterator), intent(in) :: this

! The returned element
  type(t_eop), pointer :: res


! Check that index is within bounds
  if( this%idx >= 1 .and. this%idx <= this%parent%count ) then

!   Return the data element pointed by the iterator
    res => this%parent%data(this%idx)%element

  end if

end function vector_iterator_get_element_ptr



! Set the element in the node pointed by the iterator
pure subroutine vector_iterator_set_element( this, val )

! The iterator
  class(t_eop_vector_ftl_iterator), intent(inout) :: this

! The element to be assigned
  type(t_eop), intent(in) :: val

! Check that index is within bounds; else do nothing
  if( this%idx >= 1 .and. this%idx <= this%parent%count ) then

!   Set the element
    call element_assign_pointer( this%parent%data(this%idx)%element, val )

  end if

end subroutine vector_iterator_set_element


! Create an interator from an existing iterator (assignment operator)
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
subroutine vector_iterator_assign( left, right )

! The output iterator
  class(t_eop_vector_ftl_iterator), intent(out) :: left

! The input iterator
  class(t_eop_vector_ftl_iterator), intent(in) :: right

! Associate iterator
  left%idx = right%idx

! Associate parent
  left%parent => right%parent

end subroutine vector_iterator_assign


! Compare two iterators (equality)
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
pure function vector_iterator_equal( left, right ) result(res)

! The first iterator
  class(t_eop_vector_ftl_iterator), intent(in) :: left

! The second iterator
  class(t_eop_vector_ftl_iterator), intent(in) :: right

! The comparison result
  logical :: res

! Compute result by pointer association check
  res = associated( left%parent, right%parent )
  if( res ) res = ( left%idx == right%idx )


end function vector_iterator_equal


! Compare two iterator (inequality)
! This interface is also designed to allow inheritance of the list type
! and then to extend also the list_iterator type such that the derived list type
! can invoke this method with the derivied list_iterator type
pure function vector_iterator_not_equal( left, right ) result(res)

! The first iterator
  class(t_eop_vector_ftl_iterator), intent(in) :: left

! The second iterator
  class(t_eop_vector_ftl_iterator), intent(in) :: right

! The comparison result
  logical :: res

! Compute result by pointer association check
  res = .not. ( left == right )

end function vector_iterator_not_equal


! Request a change in capacity
! Requests that the vector capacity be at least enough to contain n elements.
! If n is greater than the current vector capacity, the function causes the container 
! to reallocate its storage increasing its capacity to n (or greater).
! In all other cases, the function call does not cause a reallocation and the vector capacity 
! is not affected.
! This function has no effect on the vector size and cannot alter its elements.
pure subroutine vector_reserve( this, capacity )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! The user reqested capacity
  integer, intent(in) :: capacity

! Local intermediate storage
  type(t_eop_vector_ftl) :: local

! Local variables
  integer :: i, m

! Compute the size to allocate
! The default behaviour never removes used storage
  m = maxval( [ capacity, this%count ] )

! Check array status
  if( .not. allocated(this%data) ) then

!   Initialise capacity to requested size
    allocate( this%data(m) )
    this%count = 0
  
  else 
  
!   Check if reallocation is necessary
    if( m > size(this%data) ) then

!     Preserve pointer to current elements in local buffer
      allocate( local%data( m ) )
      do i = 1, this%count
        local%data(i)%element => this%data(i)%element
      end do
      local%count = this%count

!     Reallocate vector to new capacity
      deallocate( this%data )
      allocate( this%data(m) )

!     Restore pointers to vector vector elements
      do i = 1, local%count
        this%data(i)%element => local%data(i)%element
      end do
      this%count = local%count

!     Deallocate local buffer
      deallocate( local%data )

    end if

  end if

end subroutine vector_reserve


! Internal array memory handling (realloc)
pure subroutine vector_realloc( this )

! The vector
  class(t_eop_vector_ftl), intent(inout) :: this

! Local intermediate storage
  type(t_eop_vector_ftl) :: local

! Local variables
  integer :: i
  integer :: newsize

! Check array status
  if( .not. allocated(this%data) ) then

!   Simply reserve the necessary memory
    call this%reserve( vector_base_capacity )

! Check if the array capacitz has been exhausted
  else if( this%count == size(this%data) ) then

!   Request array reallocation
    call this%reserve( int( this%count * vector_base_capacity_increase ) )
  
  end if

end subroutine vector_realloc

end module m_eop_vector_ftl

! 2022-02-05T22:12:26
