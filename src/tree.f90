module xxmodulebase___tree_ftl

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Tree container template
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
!------------------------------------------------------------------------------

!- Start of use statements ----------------------------------------------------

  use m_object
  use xxuse__

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private

  public xxtypebase___tree_ftl, xxconstructor___tree_ftl
  public xxtypebase___tree_ftl_iterator

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Tree node type
  type t_tree_node
    private

!     Pointer to parent (null at root)
      type(t_tree_node), pointer :: parent => null()

!     Pointer to the previous sibling
      type(t_tree_node), pointer :: previous_sibling => null()

!     Pointer to the next sibling
      type(t_tree_node), pointer :: next_sibling => null()

!     Total number of children
      integer :: children = 0

!     Pointer to the first child
      type(t_tree_node), pointer :: first_child => null()

!     Pointer to the last child
      type(t_tree_node), pointer :: last_child => null()

!     The element type
      class(xxtypebase__), pointer :: element => null()

    contains

!     Generic interfaces for data insertion/deletion at boundaries
      procedure :: push_front_child => tree_push_front_child_node
      procedure :: push_back_child => tree_push_back_child_node
      procedure :: pop_front_child => tree_pop_front_child_node
      procedure :: pop_back_child => tree_pop_back_child_node

!     Generic interfaces for navigation
      procedure :: next_tree_node_up => tree_next_tree_node_up
      procedure :: empty_node => tree_empty_node

!     Generic interface for tree/node clean-up
      procedure :: clear => tree_clear_node

!     Generic interfaces for seach and sort
      procedure :: swap => tree_swap_node

  end type t_tree_node


! Tree management type (tree root)
  type, extends(t_object) :: xxtypebase___tree_ftl
    private

!     The root node
      type(t_tree_node) :: root

    contains

!     Assign procedure
      procedure :: assign => tree_assign_from_tree

!     Generic interfaces for data insertion/deletion at boundaries
      generic :: push_front_child =>  tree_push_front_child_tree, &
                                      tree_push_front_child_iterator
      procedure, private :: tree_push_front_child_tree
      procedure, nopass, private :: tree_push_front_child_iterator
      generic :: push_back_child => tree_push_back_child_tree, &
                                    tree_push_back_child_iterator
      procedure, private :: tree_push_back_child_tree
      procedure, nopass, private :: tree_push_back_child_iterator
      generic :: pop_front_child => tree_pop_front_child_tree, &
                                    tree_pop_front_child_iterator
      procedure, private :: tree_pop_front_child_tree
      procedure, nopass, private :: tree_pop_front_child_iterator
      generic :: pop_back_child => tree_pop_back_child_tree, &
                                   tree_pop_back_child_iterator
      procedure, private :: tree_pop_back_child_tree
      procedure, nopass, private :: tree_pop_back_child_iterator

!     Generic interfaces for data insertion/deletion
      procedure, nopass :: push_front_sibling => tree_push_front_sibling
      procedure, nopass :: push_back_sibling => tree_push_back_sibling
      procedure, nopass :: pop_front_sibling => tree_pop_front_sibling
      procedure, nopass :: pop_back_sibling => tree_pop_back_sibling
      procedure, nopass :: insert => tree_insert_sibling
      procedure, nopass :: erase => tree_erase

!     Generic interfaces for tree status query
      procedure :: empty => tree_empty_tree

!     Generic interface for tree/node clean-up
      generic :: clear => tree_clear_tree, &
                          tree_clear_iterator
      procedure :: tree_clear_tree
      procedure, nopass :: tree_clear_iterator

!     Generic interfaces for navigation
      generic :: begin => tree_begin_tree, &
                          tree_begin_iterator
      procedure, private :: tree_begin_tree
      procedure, private :: tree_begin_iterator
      generic :: end => tree_end_tree, &
                        tree_end_iterator
      procedure, private :: tree_end_tree
      procedure, private :: tree_end_iterator
      procedure :: begin_sibling => tree_begin_sibling
      procedure :: end_sibling => tree_end_sibling

!     Generic interfaces for data element access
      procedure :: front => tree_front
      procedure :: back => tree_back

!     Assignment operator
      generic :: assignment(=) => tree_assign_from_tree
      procedure :: tree_assign_from_tree

!     Destructor
      final :: tree_

  end type xxtypebase___tree_ftl


! Tree iterator type
  type, extends(t_object) :: xxtypebase___tree_ftl_iterator
    private

!     Pointer to the referenced node
      type(t_tree_node), pointer :: node => null()

!     Pointer to the container tree
      type(xxtypebase___tree_ftl), pointer :: tree => null()

    contains

!     Assign procedure
      procedure :: assign => tree_iterator_assign_from_tree_iterator

!     Generic interfaces for data insertion/deletion at generic positions
      procedure :: swap => tree_swap_iterator

!     Generic interfaces for list status query
      procedure :: empty => tree_empty_iterator
      procedure :: has_children => tree_has_children
      procedure :: children => tree_children
      procedure :: has_siblings => tree_has_siblings
      procedure :: siblings => tree_Siblings
      procedure :: parent => tree_Parent
      procedure :: depth => tree_Depth
      procedure :: sibling_position => tree_sibling_position

!     Generic interfaces for navigation
      procedure :: next => tree_next_tree_node
      procedure :: previous => tree_previous_node
      procedure :: associated => tree_Associated
      procedure :: nullify => tree_nullify
      procedure :: next_sibling => tree_next_sibling
      procedure :: previous_sibling => tree_previous_sibling

!     Generic interfaces for data element access
      procedure :: has_data => tree_has_data
      procedure :: get_element => tree_iterator_get_element
      procedure :: get_element_ptr => tree_iterator_get_element_ptr
      procedure :: set_element => tree_element_to_iterator

!     Access pointer to iterator container instance
      procedure :: container => tree_container_ptr

!     Assignment operator
      generic :: assignment(=) => tree_iterator_assign_from_tree_iterator
      procedure, private :: tree_iterator_assign_from_tree_iterator

  end type xxtypebase___tree_ftl_iterator


! Constructor interface
  interface xxconstructor___tree_ftl
    module procedure tree_default
    module procedure tree_copy
  end interface xxconstructor___tree_ftl

!- End of module variable declarations ----------------------------------------

contains

! Default constructor
function tree_default() result(res)

! The result tree
  type(xxtypebase___tree_ftl) :: res


end function tree_default


! Copy constructor
function tree_copy( other ) result(res)

! The input tree
  type(xxtypebase___tree_ftl), intent(in) :: other

! The result tree
  type(xxtypebase___tree_ftl) :: res

! Copy the tree
  call res%assign(other)

end function tree_copy


! Destructor
subroutine tree_( this )

! The list
  type(xxtypebase___tree_ftl), intent(inout) :: this

! Clear the tree
  call this%clear()

end subroutine tree_


! Add child node at the beginning of the tree root children list; include element if present
subroutine tree_push_front_child_tree( this, val )

! The tree
  class(xxtypebase___tree_ftl), intent(inout) :: this

! The element
  class(xxtypebase__), optional, intent(in) :: val

! Invoke the node insertion for the root iterator
  call this%root%push_front_child( val )

end subroutine tree_push_front_child_tree


! Add child node to the beginning of the children list; include element if present
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
subroutine tree_push_front_child_iterator( iterator, val )

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: iterator

! The element
  class(xxtypebase__), optional, intent(in) :: val

! Invoke the node insertion for the node pointed by the iterator
  call iterator%node%push_front_child( val )

end subroutine tree_push_front_child_iterator


! Add child node to the beginning of the children list; include element if present
subroutine tree_push_front_child_node( this, val )

! The node
  class(t_tree_node),  target, intent(inout) :: this

! The element
  class(xxtypebase__), optional, intent(in) :: val

! Chek if node already contains children
  if( associated(this%last_child) ) then

!   Allocate child node
    allocate( this%first_child%previous_sibling )

!   Allocate pointers in new node
    this%first_child%previous_sibling%next_sibling => this%first_child
    this%first_child%previous_sibling%parent => this

!   Reassign pointer to first element
    this%first_child => this%first_child%previous_sibling

  else

!   Allocate child node
    allocate( this%first_child )

!   Assign pointers
    this%last_child => this%first_child
    this%first_child%parent => this

  end if

! Copy the element into its node position
  if( present(val) ) then
    call element_assign_pointer( this%first_child%element, val )
  end if

! Increase counter
  this%children = this%children + 1

end subroutine tree_push_front_child_node


! Add child node at the end of the tree root children list; include element if present
subroutine tree_push_back_child_tree( this, val )

! The tree
  class(xxtypebase___tree_ftl), intent(inout) :: this

! The element
  class(xxtypebase__), optional, intent(in) :: val

! Invoke the node insertion for the root iterator
  call this%root%push_back_child( val )

end subroutine tree_push_back_child_tree


! Add child node to the end of the children list; include element if present
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
subroutine tree_push_back_child_iterator( iterator, val )

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: iterator

! The element
  class(xxtypebase__), optional, intent(in) :: val

! Invoke the node insertion for the node pointed by the iterator
  call iterator%node%push_back_child( val )

end subroutine tree_push_back_child_iterator


! Add child node to the end of the children list; include element if present
subroutine tree_push_back_child_node( this, val )

! The node
  class(t_tree_node), target, intent(inout) :: this

! The element
  class(xxtypebase__), optional, intent(in) :: val

! Chek if node already contains children
  if( associated(this%last_child) ) then

!   Allocate child node
    allocate( this%last_child%next_sibling )

!   Allocate pointers in new node
    this%last_child%next_sibling%previous_sibling => this%last_child
    this%last_child%next_sibling%parent => this

!   Reassign pointer to last element
    this%last_child => this%last_child%next_sibling

  else

!   Allocate child node
    allocate( this%last_child )

!   Assign pointers
    this%first_child => this%last_child
    this%last_child%parent => this

  end if

! Copy the element into its node position
  if( present(val) ) then
    call element_assign_pointer( this%last_child%element, val )
  end if

! Increase counter
  this%children = this%children + 1

end subroutine tree_push_back_child_node


! Remove child node at the beginning of the tree root children list
subroutine tree_pop_front_child_tree( this )

! The tree
  class(xxtypebase___tree_ftl), intent(inout) :: this

! Invoke the removal for the root iterator
  call this%root%pop_front_child()

end subroutine tree_pop_front_child_tree


! Remove child node at the beginning of the children list
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
subroutine tree_pop_front_child_iterator( iterator )

! The tree iterator
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: iterator

! Invoke the removal for the root iterator
  call iterator%node%pop_front_child()

end subroutine tree_pop_front_child_iterator


! Remove child node to the beginning of the children list
subroutine tree_pop_front_child_node( this )

! The node
  class(t_tree_node), intent(inout) :: this

! Check if the node is empty
  if( associated(this%first_child) ) then

!   Check if there is more than one child node
    if( associated(this%first_child%next_sibling) ) then

!     Reassign first child pointer
      this%first_child => this%first_child%next_sibling

!     Remove the first child node
      call this%first_child%previous_sibling%clear()
      this%first_child%previous_sibling => null()

    else

!     Only one child node; remove it
      call this%first_child%clear()

    end if

!   Decrease counter
    this%children = this%children - 1

  end if

end subroutine tree_pop_front_child_node


! Remove child node at the beginning of the tree root children list
subroutine tree_pop_back_child_tree( this )

! The tree
  class(xxtypebase___tree_ftl), intent(inout) :: this

! Invoke the removal for the root iterator
  call this%root%pop_back_child()

end subroutine tree_pop_back_child_tree


! Remove child node at the beginning of the children list
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
subroutine tree_pop_back_child_iterator( iterator )

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: iterator

! Invoke the removal for the root iterator
  call iterator%node%pop_back_child()

end subroutine tree_pop_back_child_iterator


! Remove child node to the end of the children list
subroutine tree_pop_back_child_node( this )

! The node
  class(t_tree_node), intent(inout) :: this

! Check if the node is empty
  if( associated(this%last_child) ) then

!   Destroy data element in the last child node
    if( associated(this%last_child%element) ) then
      deallocate( this%last_child%element )
    end if

!   Check if there is more than one child node
    if( associated(this%last_child%previous_sibling) ) then

!     Reassign last child pointer
      this%last_child => this%last_child%previous_sibling

!     Remove the last child node
      deallocate( this%last_child%next_sibling )

    else

!     Only one child node; remove it
      deallocate( this%last_child )
      this%first_child => null()

    end if

!   Decrease counter
    this%children = this%children - 1

  end if

end subroutine tree_pop_back_child_node


! Add child node to the beginning of the sibling list
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
subroutine tree_push_front_sibling( iterator, val )

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: iterator

! The element
  class(xxtypebase__), optional, intent(in) :: val

! Invoke insertion through the parent node
  call iterator%node%parent%push_front_child( val )

end subroutine tree_push_front_sibling


! Add child node to the end of the sibling list
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
subroutine tree_push_back_sibling( iterator, val )

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: iterator

! The element
  class(xxtypebase__), optional, intent(in) :: val

! Invoke insertion through the node parent
  call iterator%node%parent%push_back_child( val )

end subroutine tree_push_back_sibling


! Remove child node from the beginning of the sibling list
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
subroutine tree_pop_front_sibling( iterator )

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: iterator

! Invoke deletion through the parent node
  call iterator%node%parent%pop_front_child()

end subroutine tree_pop_front_sibling


! Remove child node from the end of the sibling list
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
subroutine tree_pop_back_sibling( iterator )

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: iterator

! Invoke insertion through the node parent
  call iterator%node%parent%pop_back_child()

end subroutine tree_pop_back_sibling


! Insert node before node pointed by the iterator
! Iterator remains associated to input iterator
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
subroutine tree_insert_sibling( iterator, val )

! Iterator to element used as reference for insertion
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: iterator

! The element
  class(xxtypebase__), intent(in) :: val

! Local iterator pointer
  type(t_tree_node), pointer :: node

! Allocate new node
  allocate( node )

! Assign forwar pointers
  if( associated(iterator%node%previous_sibling) ) then
    iterator%node%previous_sibling%next_sibling => node
  else
    iterator%node%parent%first_child => node
  end if
  node%next_sibling => iterator%node

! Assign backward pointers
  node%previous_sibling => iterator%node%previous_sibling
  iterator%node%previous_sibling => node

! Assign parent pointer
  node%parent => iterator%node%parent

! Copy the element into its list position
  call element_assign_pointer( node%element, val )

! Increase counter
  iterator%node%parent%children = iterator%node%parent%children + 1

end subroutine tree_insert_sibling


! Remove node pointed by the iterator
! Input pointer returns not associated
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
 subroutine tree_erase( iterator )

! Iterator to element to remove
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: iterator

! Local node pointer
  type(t_tree_node), pointer :: node

! Check the node
  if( associated(iterator%node) ) then

!   Initialise node pointer
    node => iterator%node

!   Assign forward pointers
    if( associated(node%previous_sibling) ) then
      node%previous_sibling%next_sibling => node%next_sibling
    else
      node%parent%first_child => node%next_sibling
    end if

!   Assign backward pointers
    if( associated(node%next_sibling) ) then
      node%next_sibling%previous_sibling => node%previous_sibling
    else
      node%parent%last_child => node%previous_sibling
    end if

!   Decrease counter
    node%parent%children = node%parent%children - 1

!   Deallocate list element
    call node%clear()
    deallocate( node )

  end if

end subroutine tree_erase


! Return iterator to first node in the tree
function tree_begin_tree( this ) result(res)

! The tree
  class(xxtypebase___tree_ftl), target, intent(in) :: this

! Pointer to the root element
  type(xxtypebase___tree_ftl_iterator) :: res

! Assign the iterator pointers
  res%node => this%root
  res%tree => this

end function tree_begin_tree


! Return pointer to last node in the tree
function tree_end_tree( this ) result(res)

! The tree
  class(xxtypebase___tree_ftl), target, intent(in) :: this

! Pointer to the root element
  type(xxtypebase___tree_ftl_iterator) :: res

! Local iterator
  type(t_tree_node), pointer :: it

! Loop in the levels looking for last level in the last list of children
  it => this%root
  do while( associated(it%last_child) )
    it => it%last_child
  end do

! Assign the iterator pointers
  res%node => it
  res%tree => this

end function tree_end_tree


! Return pointer to first child node in the input node
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
function tree_begin_iterator( this, iterator ) result(res)

! The tree
  class(xxtypebase___tree_ftl), target, intent(in) :: this

! The pointer to the node
  class(xxtypebase___tree_ftl_iterator), intent(in) :: iterator

! Pointer to the first child in the node
  type(xxtypebase___tree_ftl_iterator) :: res

! Assign the iterator pointers
  res%node => iterator%node%first_child
  res%tree => this

end function tree_begin_iterator


! Return pointer to last child node in the input node
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
function tree_end_iterator( this, iterator ) result(res)

! The tree
  class(xxtypebase___tree_ftl), target, intent(in) :: this

! The pointer to the node
  class(xxtypebase___tree_ftl_iterator), target, intent(in) :: iterator

! Pointer to the first child in the node
  type(xxtypebase___tree_ftl_iterator) :: res

! Assign the iterator pointers
  res%node => iterator%node%last_child
  res%tree => this

end function tree_end_iterator


! Return pointer to next node in the tree
function tree_next_tree_node( this ) result(res)

! The tree iterator
  class(xxtypebase___tree_ftl_iterator), intent(in) :: this

! The output iterator
  type(xxtypebase___tree_ftl_iterator) :: res

! Local node pointer
  type(t_tree_node), pointer :: node

! Initialise node pointer
  node => this%node

! Select the navigation path
  if( associated(node%first_child) ) then

!   The first to node to navigate is the first child
    res%node => node%first_child

  else

!   Check if there is a next sibling
    if( associated( node%next_sibling ) ) then

!     Next item is sibling
      res%node => node%next_sibling

    else

!     This level is exhausted; move up
      res%node => node%next_tree_node_up()

    end if

  end if

! Complte the return iterator
  res%tree => this%tree

end function tree_next_tree_node


! Navigate the tree forward from an exhausted level upwards in the hierarchy
! When a level is exhausted, the next node is
!   - either a sibling of an already navigated node
!   - or the root and then the navigation is finished
function tree_next_tree_node_up( this ) result(res)

! The tree iterator
  class(t_tree_node), target :: this

! The output iterator
  type(t_tree_node), pointer :: res

! Local iterator
  type(t_tree_node), pointer :: it

! Look for the next node
  it => this
  do

!   Check if already at root (null parent)
    if( associated(it%parent) ) then

!     Check if parent has sibling at next
      if( associated(it%parent%next_sibling) ) then

!       This is the next node
        res => it%parent%next_sibling
        exit

      else

!       Continue moving upwards
        it => it%parent

      end if

    else

!     At root level; finish navigation
      res => null()
      exit

    end if

  end do

end function tree_next_tree_node_up


! Return pointer to previous node in the tree
function tree_previous_node( this ) result(res)

! The tree iterator
  class(xxtypebase___tree_ftl_iterator), intent(in) :: this

! The output iterator
  type(xxtypebase___tree_ftl_iterator) :: res

! Local node pointer
  type(t_tree_node), pointer :: node

! Initialise node pointer
  node => this%node

! Select the navigation path
  if( associated( node%previous_sibling ) ) then

!   There is a sibling at prev; try to navigate its children first
    if( associated(node%previous_sibling%last_child) ) then

!     The first to node to navigate is the last child in the
!     deepest level hanging from the sibling at prev
      res%node => node%previous_sibling%last_child
      do while( associated(res%node%last_child) )
        res%node => res%node%last_child
      end do

    else

!     There are no children in sibling at prev; navigate sibling itself
      res%node => node%previous_sibling

    end if

  else

!   This level is exhausted; move up
    res%node => node%parent

  end if

! Complte the return iterator
  res%tree => this%tree

end function tree_previous_node


! Return the association status of an iterator
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
pure function tree_associated( this, other ) result(res)

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(in) :: this

! The node iterator (optional association check)
  class(xxtypebase___tree_ftl_iterator), optional, intent(in) :: other

! The association status
  logical :: res

! Return the association status
  if( present(other) ) then
    res = associated( this%node, other%node )
  else
    res = associated( this%node )
  end if

end function tree_associated


! Nullify an iterator
subroutine tree_nullify( this )

! The tree iterator
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: this

! Nullify pointer
  this%node => null()

end subroutine tree_nullify


! Return pointer to first sibling node to the input node
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
function tree_begin_sibling( this, iterator ) result(res)

! The tree
  class(xxtypebase___tree_ftl), target, intent(in) :: this

! The pointer to the node
  class(xxtypebase___tree_ftl_iterator), intent(in) :: iterator

! Pointer to the first child in the node
  type(xxtypebase___tree_ftl_iterator) :: res

! Local variables
  type(xxtypebase___tree_ftl_iterator) :: parit

! Get the parent
  parit = iterator%parent()
  if( parit%associated() ) then

!   Assign the iterator pointers
    res%node => iterator%node%parent%first_child
    res%tree => this

  else

!   Parent is null; this occurs at root element only
!   This is just a protection; in general code is not reachable
    res%node => null()
    res%tree => null()

  end if

end function tree_begin_sibling


! Return pointer to last sibling node to the input node
! This interface is also designed to allow inheritance of the tree type
! and then to extend also the tree_iterator type such that the derived tree type
! can invoke this method with the derivied tree_iterator type
function tree_end_sibling( this, iterator ) result(res)

! The tree
  class(xxtypebase___tree_ftl), target, intent(in) :: this

! The pointer to the node
  class(xxtypebase___tree_ftl_iterator), intent(in) :: iterator

! Pointer to the first child in the node
  type(xxtypebase___tree_ftl_iterator) :: res

! Local variables
  type(xxtypebase___tree_ftl_iterator) :: parit

! Get the parent
  parit = iterator%parent()
  if( parit%associated() ) then

!   Assign the iterator pointers
    res%node => iterator%node%parent%last_child
    res%tree => this

  else

!   Parent is null; this occurs at root element only
!   This is just a protection; in general code is not reachable
    res%node => null()
    res%tree => null()
  end if

end function tree_end_sibling


! Return pointer to next sibling node
function tree_next_sibling( this ) result(res)

! The tree iterator
  class(xxtypebase___tree_ftl_iterator), target, intent(in) :: this

! The output iterator
  type(xxtypebase___tree_ftl_iterator) :: res

! Assign iterator pointers
  res%node => this%node%next_sibling
  res%tree => this%tree

end function tree_next_sibling


! Return pointer to previous sibling node
function tree_previous_sibling( this ) result(res)

! The tree iterator
  class(xxtypebase___tree_ftl_iterator), target, intent(in) :: this

! The output iterator
  type(xxtypebase___tree_ftl_iterator) :: res

! Assign iterator pointers
  res%node => this%node%previous_sibling
  res%tree => this%tree

end function tree_previous_sibling


! Return whether a node has data
pure function tree_has_data( this ) result(res)

! The iterator
  class(xxtypebase___tree_ftl_iterator), intent(in) :: this

! The data contents flag (true if node contains data element)
  logical :: res

! Return the status
  res = associated(this%node%element)

end function tree_has_data


! Get a copy of the data elememnt pointed by the iterator
function tree_iterator_get_element( this ) result(res)

! The iterator
  class(xxtypebase___tree_ftl_iterator), intent(in) :: this

! The returned element
  class(xxtypebase__), allocatable :: res

! Assign result
  call element_assign_allocatable( res, this%node%element )

end function tree_iterator_get_element


! Get a pointer to the data elememnt pointed by the iterator
function tree_iterator_get_element_ptr( this ) result(res)

! The iterator
  class(xxtypebase___tree_ftl_iterator), intent(in) :: this

! The returned element
  class(xxtypebase__), pointer :: res

! Assign result
  res => this%node%element

end function tree_iterator_get_element_ptr


! Set the value pointed by the iterator
subroutine tree_element_to_iterator( this, val )

! The iterator
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: this

! The element to be assigned
  class(xxtypebase__), intent(in) :: val

! Check element allocation status
  call element_assign_pointer( this%node%element, val )

end subroutine tree_element_to_iterator


! First element in the tree
function tree_front( this ) result(res)

! The tree
  class(xxtypebase___tree_ftl), intent(in) :: this

! Pointer to first element in The tree
  class(xxtypebase__), allocatable :: res

! Assign the return value
  call element_assign_allocatable( res, this%root%element )

end function tree_front


! Last element in The tree
function tree_back( this ) result(res)

! The tree
  class(xxtypebase___tree_ftl), intent(in) :: this

! Pointer to first element in the tree
  class(xxtypebase__), allocatable :: res

! Local iterator
  type(xxtypebase___tree_ftl_iterator) :: iterator

! Assign the return value
  iterator = this%end()
  call element_assign_allocatable( res, iterator%node%element )

end function tree_back


! Return empty status of tree
pure function tree_empty_tree( this ) result(res)

! The tree
  class(xxtypebase___tree_ftl), intent(in) :: this

! The tree empty status
  logical :: res

! Assing the return value
  res = this%root%empty_node()

end function tree_empty_tree


! Return empty status of node
pure function tree_empty_iterator( this ) result(res)

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(in) :: this

! The node empty status
  logical :: res

! Assing the return value
  res = this%node%empty_node()

end function tree_empty_iterator


! Return empty status of node
pure function tree_empty_node( this ) result(res)

! The node iterator
  class(t_tree_node), intent(in) :: this

! The node empty status
  logical :: res

! Assing the return value
  res = ( this%children == 0 .and. .not. associated(this%element) )

end function tree_empty_node


! Return whether node has children
pure function tree_has_children( this ) result(res)

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(in) :: this

! The children presence status
  logical :: res

! Assing the return value
  res = ( this%node%children /= 0 )

end function tree_has_children


! Return whether node has siblings
pure function tree_has_siblings( this ) result(res)

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(in) :: this

! The sibling presence status
  logical :: res

! Check parent association
  if( associated(this%node%parent) ) then

!   Assign the return value (self does not count as sibling)
    res = ( this%node%parent%children > 1 )

  else

!   Parent node; no siblings
    res = .false.

  end if

end function tree_has_siblings


! Return the number of children in a node
pure function tree_children( this ) result(res)

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(in) :: this

! The number of children
  integer :: res

! Assing the return value
  res = this%node%children

end function tree_children


! Return the number of siblings of a node
pure function tree_siblings( this ) result(res)

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(in) :: this

! The number of siblings (excluding self)
  integer :: res

! Assign the return value
  res = this%node%parent%children - 1

end function tree_siblings


! Return iterator to the parent
function tree_parent( this ) result(res)

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(in) :: this

! The parent node iterator
  type(xxtypebase___tree_ftl_iterator) :: res

! Return iterator to parent
  if( associated(this%node) ) then
    res%node => this%node%parent
  end if
  res%tree => this%tree

end function tree_parent


! Return the depth in a tree (from iterator); root is depth 0
function tree_depth( this ) result(res)

! The node iterator
  class(xxtypebase___tree_ftl_iterator), target, intent(in) :: this

! The iterator depth
  integer :: res

! Local node pointer
  type(t_tree_node), pointer :: node

! Initialise navigation pointer
  node => this%node

! Compute the depth
  res = 0
  do while( associated(node%parent) )
    res = res + 1
    node => node%parent
  end do

end function tree_depth


! Return the sibling position in a tree (from iterator); root is sibling position 0
function tree_sibling_position( this ) result(res)

! The node iterator
  class(xxtypebase___tree_ftl_iterator), target, intent(in) :: this

! The iterator sibling position
  integer :: res

! Local node pointer
  type(t_tree_node), pointer :: node

! Initialise navigation pointer
  node => this%node

! Compute the sibling position
  res = 1
  do while( associated(node%previous_sibling) )
    res = res + 1
    node => node%previous_sibling
  end do

end function tree_sibling_position


! Clear the whole tree
subroutine tree_clear_tree( this )

! The tree
  class(xxtypebase___tree_ftl), intent(inout) :: this

! Recursively delete the tree nodes starting at root
  call this%root%clear()

! Reset the number of children
  this%root%children = 0

end subroutine tree_clear_tree


! Clear the node pointed by the iterator
subroutine tree_clear_iterator( iterator )

! The node iterator
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: iterator

! Local variables
  type(t_tree_node), pointer :: node, parent

! Keep the nodes before reassigning pointers
  node => iterator%node
  parent => node%parent

! Check if node is the first node in the parent
  if( associated(parent%first_child,node) ) then

!   Set the next node to be the first child in the parent
    parent%first_child => node%next_sibling

!   Assign pointers
    if( associated(node%next_sibling) ) then

!     Not the last node in the siblings
      node%next_sibling%previous_sibling => null()

    else

!     Last node in the siblings (only one node in the siblings before removal)
      parent%last_child => null()

    end if

! Check if node is the last child in the parent
  else if( associated(parent%last_child,node) ) then

!   Set the previous node to be last node in the parent
    parent%last_child => node%previous_sibling

!   Assign pointers
    if( associated(node%previous_sibling) ) then

!     Not the first node in the siblings
      node%previous_sibling%next_sibling => null()

    else

!     First node in the siblings (only one node in the siblings before removal)
!     This line cannot be reached beacuse executions follows the branch
!     of the first node in the parent
!      parent%first_child => null()

    end if

! Generic intermediate nodes (at least three nodes present)
  else

!   Assign pointers
    node%previous_sibling%next_sibling => node%next_sibling
    node%next_sibling%previous_sibling => node%previous_sibling

  end if

! Remove one chlid from parent
  node%parent%children = node%parent%children - 1

! Recursively clear all nodes below the one pointed by the iterator
  call node%clear()
  deallocate( node )

end subroutine tree_clear_iterator


! Recursively clear the node and all descendants
recursive subroutine tree_clear_node( this )

! The top node
  class(t_tree_node), intent(inout) :: this

! Local node pointer
  type(t_tree_node), pointer :: it

! Remove the element stored in the node
  if( associated(this%element) ) then
    deallocate( this%element )
  end if

! Check for children
  if( associated(this%last_child ) ) then

!   Recursively remove all children
    do

!     The node to delete is always the last one
      it => this%last_child
      if( associated(it) ) then

!       Reassign the last child pointer before deleting
        this%last_child => this%last_child%previous_sibling

!       Delete the last child node
        call it%clear()
        deallocate( it )

      else
        exit
      end if

    end do

!   Nullify the child iterators
    this%first_child => null()
    this%last_child => null()

  end if

end subroutine tree_clear_node


! Swap two elements in a tree
pure subroutine tree_swap_iterator( this, other )

! Iterator to first element
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: this

! Iterator to second element
  class(xxtypebase___tree_ftl_iterator), intent(inout) :: other

! Swap the nodes pointed by the iterators
  call this%node%swap( other%node )

end subroutine tree_swap_iterator


! Swap two elements in a tree
pure subroutine tree_swap_node( this, other )

! Iterator to first element
  class(t_tree_node), target, intent(inout) :: this

! Iterator to second element
  type(t_tree_node), target, intent(inout) :: other

! Intermediate iterators
  type(t_tree_node) :: node

! Keep the first node pointers in the temporary storage
  node = this

! Assign potinters in first node
  if( associated(this%next_sibling,other) ) then

!   Other is the next node to this in the siblings
    this%previous_sibling => other
    this%next_sibling => other%next_sibling

  else if( associated(this%previous_sibling,other) ) then

!   Other is the previous node to this in the siblings
    this%previous_sibling => other%previous_sibling
    this%next_sibling => other
  else

!   Nodes in this and other are not adjacent nodes
    this%previous_sibling => other%previous_sibling
    this%next_sibling => other%next_sibling

  end if

! Assign pointers in second node
  if( associated(other%next_sibling,this) ) then

!   This is the next node to other in the siblings
    other%previous_sibling => this
    other%next_sibling => node%next_sibling

  else if( associated(other%previous_sibling,this) ) then

!   This is the previous node to other in the siblings
    other%previous_sibling => node%previous_sibling
    other%next_sibling => this
  else

!   Nodes in this and other are not adjacent nodes
    other%previous_sibling => node%previous_sibling
    other%next_sibling => node%next_sibling

  end if

! Exchange children pointers
  this%first_child => other%first_child
  other%first_child => node%first_child
  this%last_child => other%last_child
  other%last_child => node%last_child

! Exchange parents
  this%parent => other%parent
  other%parent => node%parent

! Check this at start of the list
  if( associated(this%previous_sibling) ) then

!   This has not become the first node in its parent
    this%previous_sibling%next_sibling => this

  else

!   This has become the first node in its parent
    this%parent%first_child => this

!   Move the child count from other to this
    this%parent%children = this%parent%children + 1
    other%parent%children = other%parent%children - 1

  end if

! Check this at the end of the list
  if( associated(this%next_sibling) ) then

!   This has not become the last node in its parent
    this%next_sibling%previous_sibling => this

  else

!   This has become the last node in its parent
    this%parent%last_child => this

!   Move the child count from other to this
    this%parent%children = this%parent%children + 1
    other%parent%children = other%parent%children - 1

  end if

! Check other at start of the list
  if( associated(other%previous_sibling) ) then

!   Other has not become the first node in its parent
    other%previous_sibling%next_sibling => other

  else

!   Other has become the first node in its parent
    other%parent%first_child => other

!   Move the child count from this to other
    other%parent%children = other%parent%children + 1
    this%parent%children = this%parent%children - 1

  end if

! Check other at the end of the list
  if( associated(other%next_sibling) ) then

!   Other has not become the last node in its parent
    other%next_sibling%previous_sibling => other

  else

!   Other has become the last node in its parent
    other%parent%last_child => other

!   Move the child count from this to other
    other%parent%children = other%parent%children + 1
    this%parent%children = this%parent%children - 1

  end if

end subroutine tree_swap_node


! Create a tree from an exisiting tree (assignment operator)
subroutine tree_assign_from_tree( this, other )

! The output tree
  class(xxtypebase___tree_ftl), intent(out) :: this

! The input tree
  type(xxtypebase___tree_ftl), intent(in) :: other

! Local variables
  type(xxtypebase___tree_ftl_iterator) :: it, oit

! Navigate recursively the children in he input tree
  it = this%begin()
  oit = other%begin()
  call tree_add_children_from_tree( it, oit )

! Set the number of root children
  this%root%children = other%root%children

end subroutine tree_assign_from_tree


! Copy recursively the nodes from the input to the output
recursive subroutine tree_add_children_from_tree( itthis, itother )

! The output tree iterator
  type(xxtypebase___tree_ftl_iterator), intent(inout) :: itthis

! The input tree iterator
  type(xxtypebase___tree_ftl_iterator), intent(inout) :: itother

! Local variables
  type(xxtypebase___tree_ftl_iterator) :: itt, ito, ito0

! Check if the node has children
  if( itother%has_children() ) then

!   Set output iterator to point to current node
    itt = itthis

!   Keep the iterator to the first node in this level
    ito0 = itother%tree%begin(itother)

!   Loop on the children in the input tree
    ito = ito0
    do while( ito%associated() )

!     Insert the node in its right level
      if( ito%associated(ito0) ) then

!       First node to be inserted in the following level
        call itthis%tree%push_back_child( itt, ito%get_element() )

!       Position the iterator in first=last element in next level
        itt = itthis%tree%end(itt)

      else

!       Extra nodes inserted in this level
        call itthis%tree%push_back_sibling( itt, ito%get_element() )

!       Position the iterator in the last element of this level
        itt = itthis%tree%end_sibling(itt)

      end if

!     Process recursively this node
      call tree_add_children_from_tree( itt, ito )

!     Iterate
      ito = ito%next_sibling()

    end do

  end if

  end subroutine tree_add_children_from_tree


! Access to iterator container
function tree_container_ptr( this ) result(res)

! The iterator
  class(xxtypebase___tree_ftl_iterator), intent(in) :: this

! The pointer to the tree
  type(xxtypebase___tree_ftl), pointer :: res

! Set the result
  res => this%tree

end function tree_container_ptr


! Create a tree from an exisiting tree (assignment operator)
subroutine tree_iterator_assign_from_tree_iterator( this, other )

! The output tree iterator
  class(xxtypebase___tree_ftl_iterator), intent(out) :: this

! The input tree iterator
  type(xxtypebase___tree_ftl_iterator), intent(in) :: other

! Assign the pointers
  this%node => other%node
  this%tree => other%tree

end subroutine tree_iterator_assign_from_tree_iterator


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

end module xxmodulebase___tree_ftl
