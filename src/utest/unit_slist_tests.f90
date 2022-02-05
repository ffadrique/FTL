module unit_slist_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for xxbase___slist_ftl
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

  use m_util_convert
  use m_messages
  use m_xfunit

  use xxuse__
  use xxmodulebase___slist_ftl

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_slist_suite_before
  public unit_slist_suite_after

  public unit_slist_test_001
  public unit_slist_before_001
  public unit_slist_after_001

  public unit_slist_test_002
  public unit_slist_before_002
  public unit_slist_after_002

  public unit_slist_test_003
  public unit_slist_before_003
  public unit_slist_after_003

  public unit_slist_test_004
  public unit_slist_before_004
  public unit_slist_after_004

  public unit_slist_test_005
  public unit_slist_before_005
  public unit_slist_after_005

  public unit_slist_test_006
  public unit_slist_before_006
  public unit_slist_after_006

  public unit_slist_test_007
  public unit_slist_before_007
  public unit_slist_after_007

  public unit_slist_test_008
  public unit_slist_before_008
  public unit_slist_after_008

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'fxx'
  character(len=*), parameter :: module = 'slist'

  character(len=130), parameter :: sccs_info = &
  '$Id: $'

!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite

! The error handling structure
  type(t_messages), save :: msg

! Reference array for list generation
  integer, parameter, dimension(15) :: reference = &
     [ 1, 2, 3, 4, 5, 6 , 7, 8, 9, 10, 11, 12, 13, 14, 15 ]
  integer, parameter, dimension(10) :: constref = &
     [ 5, 5, 5, 5, 5, 5, 5, 5, 5, 5 ]

! List structures
  type(xxtypebase___slist_ftl), save :: slista0
  type(xxtypebase___slist_ftl), save :: slistb0

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_slist_before_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

! Initialise the list
  call initialise_slists()

end subroutine unit_slist_before_001

! ############################################################################

subroutine unit_slist_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Element
  type(xxtypebase__) :: element

! List structures
  type(xxtypebase___slist_ftl) :: slista, slistb

! List iterator
  type(xxtypebase___slist_ftl_iterator) :: it1, it2

! Array of elements
  class(xxtypebase__), allocatable, dimension(:) :: elements

! Counters
  integer :: i, j

! Construct a default list
  slista = xxconstructor___slist_ftl()
  call ut%assert_equal( 'Constructor list size', slista%size(), 0 )

! Construct a list from a fixed element
  element = cxxtypebase__( 5, 5.0_8, "5" )
  slista = xxconstructor___slist_ftl( 10, element )
  call ut%assert_equal( 'Consturctor list_fill size', slista%size(), 10 )
  allocate(elements(slista%size()))
  elements = slista%array()
  call ut%assert_equal( 'Constructor list_fill', elements%get_i(), constref )
  deallocate(elements)

! Construct lsit from array
  allocate(elements(size(reference)))
  elements = cxxtypebase__( reference, dble(reference), character(reference) )
  slista = xxconstructor___slist_ftl( elements )
  call ut%assert_equal( 'Consturctor list_array size', slista%size(), 15 )
  elements = slista%array()
  call ut%assert_equal( 'Constructor list_array', elements%get_i(), reference )
  deallocate(elements)

! Construct list from range of elements
  it1 = slista%begin()
  it1 = it1%next()
  it2 = it1
  do i = 1, 5
    it2 = it2%next()
  end do
  slistb = xxconstructor___slist_ftl( it1, it2 )
  call ut%assert_equal( 'Consturctor list_range size', slistb%size(), 6 )
  allocate(elements(slistb%size()))
  elements = slistb%array()
  call ut%assert_equal( 'Constructor list_range', elements%get_i(), reference(2:7) )
  deallocate(elements)

! Copy constructor
  slista = xxconstructor___slist_ftl( slistb )
  call ut%assert_equal( 'Consturctor list_copy size', slista%size(), 6 )
  allocate(elements(slista%size()))
  elements = slista%array()
  call ut%assert_equal( 'Constructor list_copy', elements%get_i(), reference(2:7) )
  deallocate(elements)

! List assignment
  slista = slista0
  call ut%assert_equal( 'Assignment size', slista%size(), slista0%size() )
  allocate(elements(slista%size()))
  elements = slista%array()
  call ut%assert_equal( 'Assignment', elements%get_i(), reference )
  deallocate(elements)

end subroutine unit_slist_test_001

! ############################################################################

subroutine unit_slist_after_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Finalise list
  call slista0%clear()
  call slistb0%clear()

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_slist_after_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_slist_before_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

! Initialise the list
  call initialise_slists()

end subroutine unit_slist_before_002

! ############################################################################

subroutine unit_slist_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Element
  type(xxtypebase__) :: a_dummy_element

! List structures
  type(xxtypebase___slist_ftl) :: slista

! Size
  slista = slista0
  call ut%assert_equal( 'Size', slista%size(), size(reference) )

! Maximum size (dummy test)
  call ut%assert_equal( 'Max size', slista%max_Size(), 1073741823 )

! Empty
  call ut%assert_false( 'Not empty list', slista%empty() )
  call slista%clear()
  call ut%assert_true( 'Empty list', slista%empty() )

end subroutine unit_slist_test_002

! ############################################################################

subroutine unit_slist_after_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Finalise list
  call slista0%clear()
  call slistb0%clear()

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_slist_after_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_slist_before_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

! Initialise the list
  call initialise_slists()

end subroutine unit_slist_before_003

! ############################################################################

subroutine unit_slist_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Element
  type(xxtypebase__) :: element
  type(xxtypebase__), pointer :: pelement

! Array of elements
  class(xxtypebase__), allocatable, dimension(:) :: elements

! List structures
  type(xxtypebase___slist_ftl) :: slista, slistb

! List iterator
  type(xxtypebase___slist_ftl_iterator) :: it1, it2

! Front
  element = slista0%front()
  call ut%assert_equal( 'Front', element%get_i(), reference(1) )

! Back
  element = slista0%back()
  call ut%assert_equal( 'Back', element%get_i(), reference(size(reference)) )

! At
  element = slista0%at(7)
  call ut%assert_equal( 'At', element%get_i(), reference(7) )

! At (out of range)
  pelement => slista0%at(70)
  call ut%assert_false( 'At (out)', associated(pelement) )

! Select
  slistb = slista0%select( cxxtypebase__(6,6.0_8,"6") )
  call ut%assert_equal( 'Select size', slistb%size(), 1 )
  element = slistb%front()
  call ut%assert_equal( 'Select', element%get_i(), reference(6) )

! Select with predicate
  slistb = slista0%select( cxxtypebase__(12,12.0_8,"12"), binary_pred )
  call ut%assert_equal( 'Select predicate size', slistb%size(), 3 )
  allocate( elements(slistb%size()) )
  elements = slistb%array()
  call ut%assert_equal( 'Select predicate', elements%get_i(), reference(13:) )
  deallocate(elements)

! Binary search
  it1 = slista0%binary_search( cxxtypebase__(6,6.0_8,"6") )
  element = it1%get_element()
  call ut%assert_equal( 'Binary search', element%get_i(), reference(6) )

! Binary search with comparison functions
  it1 = slista0%binary_search( cxxtypebase__(7,7.0_8,"7"), local_less, local_greater )
  element = it1%get_element()
  call ut%assert_equal( 'Binary search', element%get_i(), reference(7) )

! Binary search not found
  it1 = slista0%binary_search( cxxtypebase__(66,66.0_8,"66") )
  call ut%assert_false( 'Binary search not found', it1%associated() )

end subroutine unit_slist_test_003

! ############################################################################

subroutine unit_slist_after_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Finalise list
  call slista0%clear()
  call slistb0%clear()

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_slist_after_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_slist_before_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

! Initialise the list
  call initialise_slists()

end subroutine unit_slist_before_004

! ############################################################################

subroutine unit_slist_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Element
  type(xxtypebase__) :: element

! Array of elements
  class(xxtypebase__), allocatable, dimension(:) :: elements

! List structures
  type(xxtypebase___slist_ftl) :: slista, slistb

! List iterators
  type(xxtypebase___slist_ftl_iterator) :: it1, it2

! Begin
  it1 = slista0%begin()
  element = it1%get_element()
  call ut%assert_equal( 'Begin', element%get_i(), reference(1) )

! Begin not associated
  it1 = slista%begin()
  call ut%assert_false( 'Begin not associated', it1%associated() )

! End
  it1 = slista0%end()
  element = it1%get_element()
  call ut%assert_equal( 'End', element%get_i(), reference(size(reference)) )

! End not associated
  it1 = slista%end()
  call ut%assert_false( 'End not associated', it1%associated() )

! Navigate forward
  it1 = slista0%begin()
  it1 = it1%next()
  it1 = it1%next()
  element = it1%get_element()
  call ut%assert_equal( 'Iteration 2', element%get_i(), reference(3) )

! Navigate backwards
  it1 = slista0%end()
  it1 = it1%previous()
  it1 = it1%previous()
  element = it1%get_element()
  call ut%assert_equal( 'Iteration -2', element%get_i(), reference(size(reference)-2) )

! Navigate combined
  it1 = slista0%begin()
  it1 = it1%next()
  it1 = it1%next()
  it1 = it1%next()
  it1 = it1%next()
  it1 = it1%previous()
  it1 = it1%previous()
  element = it1%get_element()
  call ut%assert_equal( 'Iteration 4-2', element%get_i(), reference(1+4-2) )

end subroutine unit_slist_test_004

! ############################################################################

subroutine unit_slist_after_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Finalise list
  call slista0%clear()
  call slistb0%clear()

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_slist_after_004

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_slist_before_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

! Initialise the list
  call initialise_slists()

end subroutine unit_slist_before_005

! ############################################################################

subroutine unit_slist_test_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Element
  type(xxtypebase__) :: element

! Array of elements
  class(xxtypebase__), allocatable, dimension(:) :: elements

! List structures
  type(xxtypebase___slist_ftl) :: slista, slistb

! List iterators
  type(xxtypebase___slist_ftl_iterator) :: it1, it2

! Local counters
  integer :: i, j

! Push back
  do i = 1, 5
    j = reference(i)
    element = cxxtypebase__( j, dble(j), character(j) )
    call slista%push_back(element)
  end do
  call ut%assert_equal( 'Push back size', slista%size(), 5 )
  allocate(elements(slista%size()))
  elements = slista%array()
  call ut%assert_equal( 'Push back', elements%get_i(), reference(:slista%size()) )
  deallocate(elements)

! Pop  back
  do i = 1, 3
    call slista%pop_back()
  end do
  call ut%assert_equal( 'Pop back size', slista%size(), 2 )
  allocate(elements(slista%size()))
  elements = slista%array()
  call ut%assert_equal( 'Pop back', elements%get_i(), reference(:slista%size()) )
  deallocate(elements)

! Push front
  do i = 1, 5
    j = reference(i)
    element = cxxtypebase__( j, dble(j), character(j) )
    call slistb%push_front(element)
  end do
  call ut%assert_equal( 'Push front size', slistb%size(), 5 )
  allocate(elements(slistb%size()))
  elements = slistb%array()
  call ut%assert_equal( 'Push front', elements%get_i(), reference(slistb%size():1:-1) )
  deallocate(elements)

! Pop  front
  do i = 1, 3
    call slistb%pop_front()
  end do
  call ut%assert_equal( 'Pop front size', slistb%size(), 2 )
  allocate(elements(slistb%size()))
  elements = slistb%array()
  call ut%assert_equal( 'Pop front', elements%get_i(), reference(slistb%size():1:-1) )
  deallocate(elements)

end subroutine unit_slist_test_005

! ############################################################################

subroutine unit_slist_after_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
    call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Finalise list
  call slista0%clear()
  call slistb0%clear()

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_slist_after_005

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_slist_before_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

! Initialise the list
  call initialise_slists()

end subroutine unit_slist_before_006

! ############################################################################

subroutine unit_slist_test_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Element
  type(xxtypebase__) :: element

! Array of elements
  class(xxtypebase__), allocatable, dimension(:) :: elements

! List structures
  type(xxtypebase___slist_ftl) :: slista, slistb

! List iterators
  type(xxtypebase___slist_ftl_iterator) :: it1, it2

! Local counters
  integer :: i, j

! Insert single element
! Insert 3 elements of value 33 by its iterators before the four the element in the main list
! Then checks
!   The total number of elemens in the list
!   The pointer to the inserted element
!   The navigation from the inserted element to the element before the insertion
!   The navigation to the inserted element from the element prior to isnertion
!   The navigation to the first element after the inserted element
!   The navigation from the first element after the inserted element to the inserted element
  slista = slista0
  it1 = slista%begin()
  it1 = it1%next()
  it1 = it1%next()
  it1 = it1%next()
  it2 = slista%insert( it1, cxxtypebase__(0,dble(0),character(0)) )
  call ut%assert_equal( 'Insert size', slista%size(), size(reference) + 1 )
  element = it2%get_element()
  call ut%assert_equal( 'Insert element', element%get_i(), 0 )
  it1 = it2%previous()
  element = it1%get_element()
  call ut%assert_equal( 'Insert element previous', element%get_i(), reference(3) )
  it1 = it1%next()
  element = it1%get_element()
  call ut%assert_equal( 'Insert element from previous', element%get_i(), 0 )
  it1 = it2%next()
  element = it1%get_element()
  call ut%assert_equal( 'Insert element next', element%get_i(), reference(4) )
  it1 = it1%previous()
  element = it1%get_element()
  call ut%assert_equal( 'Insert element from next', element%get_i(), 0 )

! Erase single element
! Then check the navigation to/from the elements around the deleted element
  it1 = it2%next()
  call slista%erase(it2)
  call ut%assert_equal( 'Erase size', slista%size(), size(reference) )
  it1 = it1%previous()
  element = it1%get_element()
  call ut%assert_equal( 'Erase element previous', element%get_i(), reference(3) )
  it1 = it1%next()
  element = it1%get_element()
  call ut%assert_equal( 'Erase element next', element%get_i(), reference(4) )

! Insert single element at beginning of list
! Insert 3 elements of value 66 by its iterators before the four the element in the main list
! Then checks
!   The total number of elemens in the list
!   The pointer to the inserted element
!   The navigation from the inserted element to the element before the insertion
!   The navigation to the inserted element from the element prior to isnertion
!   The navigation to the first element after the inserted element
!   The navigation from the first element after the inserted element to the inserted element
  slista = slista0
  it1 = slista%begin()
  it2 = slista%insert( it1, cxxtypebase__(66,dble(66),character(66)) )
  call ut%assert_equal( 'Insert at beginning size', slista%size(), size(reference) + 1 )
  element = it2%get_element()
  call ut%assert_equal( 'Insert at beginning element', element%get_i(), 66 )
  it1 = it2%previous()
  call ut%assert_false( 'Insert at beginning element previous', it1%associated() )
  it1 = it2%next()
  element = it1%get_element()
  call ut%assert_equal( 'Insert at beginning element next', element%get_i(), reference(1) )
  it1 = it1%previous()
  element = it1%get_element()
  call ut%assert_equal( 'Insert at beginning element from next', element%get_i(), 66 )

! Erase single element at the beginning of the list
  it1 = slista%begin()
  call slista%erase(it1)
  call ut%assert_equal( 'Erase first element size', slista%size(), size(reference) )
  it1 = slista%begin()
  element = it1%get_element()
  call ut%assert_equal( 'Erase first element', element%get_i(), reference(1) )

! Insert constant value
! Insert 3 consant elements of value 33 before the four the element in the main list
! Then checks
!   The total number of elemens in the list
!   The pointer to the first element in the inserted section
!   The navigation from the first inserted element to the element before the insertion
!   The navigation to the first inserted element from the element prior to isnertion
!   The navigation to the first element after the inserted section
!   The navigation from the first element after the inserted section to the last inserted element
  slista = slista0
  it1 = slista%begin()
  it1 = it1%next()
  it1 = it1%next()
  it1 = it1%next()
  it2 = slista%insert( it1, 3, cxxtypebase__(33,dble(33),character(33)) )
  call ut%assert_equal( 'Insert constant size', slista%size(), size(reference)+3 )
  element = it2%get_element()
  call ut%assert_equal( 'Insert constant', element%get_i(), 33 )
  allocate(elements(slista%size()))
  elements = slista%array()
  call ut%assert_equal( 'Insert constant range', elements(4:6)%get_i(), [ 33, 33, 33 ] )
  it1 = it2%previous()
  element = it1%get_element()
  call ut%assert_equal( 'Insert constant previous', element%get_i(), reference(3) )
  it1 = it1%next()
  element = it1%get_element()
  call ut%assert_equal( 'Insert constant from previous', element%get_i(), 33 )
  it1 = it2%next()
  it1 = it1%next()
  it1 = it1%next()
  element = it1%get_element()
  call ut%assert_equal( 'Insert constant next', element%get_i(), reference(4) )
  it1 = it1%previous()
  element = it1%get_element()
  call ut%assert_equal( 'Insert constant from next', element%get_i(), 33 )
  deallocate(elements)

! Erase range
! Then check the navigation to/from the elements around the deleted section
  it1 = it2
  it2 = it2%next()
  it2 = it2%next()
  call slista%erase( it1, it2 )
  call ut%assert_equal( 'Erase range size', slista%size(), size(reference) )
  allocate(elements(slista%size()))
  elements = slista%array()
  call ut%assert_equal( 'Esrase range', elements(1:5)%get_i(), reference(1:5) )
  deallocate(elements)

! Erase range from beginning of list
! Then check the navigation to/from the elements around the deleted section
  slista = slista0
  it1 = slista%begin()
  it2 = it1%next()
  it2 = it2%next()
  it2 = it2%next()
  call slista%erase( it1, it2 )
  call ut%assert_equal( 'Erase range from beginning size', slista%size(), slista0%size() - 4 )
  allocate(elements(slista%size()))
  elements = slista%array()
  call ut%assert_equal( 'Esrase range from beginning', elements(1:5)%get_i(), reference(5:9) )
  deallocate(elements)

! Erase range from end of list
! Then check the navigation to/from the elements around the deleted section
  slista = slista0
  it1 = slista%end()
  it2 = it1%previous()
  it2 = it2%previous()
  it2 = it2%previous()
  call slista%erase( it2, it1 )
  call ut%assert_equal( 'Erase range from end size', slista%size(), slista0%size() - 4 )
  allocate(elements(slista%size()))
  elements = slista%array()
  i = slista%size() - 4
  j = slista0%size() - 8
  call ut%assert_equal( 'Esrase range from end', elements(i:)%get_i(), reference(j:) )
  deallocate(elements)

! Insert range
! Insert 3 elements of value 33 by its iterators before the four the element in the main list
! Then checks
!   The total number of elemens in the list
!   The pointer to the first element in the inserted section
!   The navigation from the first inserted element to the element before the insertion
!   The navigation to the first inserted element from the element prior to isnertion
!   The navigation to the first element after the inserted section
!   The navigation from the first element after the inserted section to the last inserted element
  slista = slista0
  slistb = xxconstructor___slist_ftl( 3, cxxtypebase__(33,dble(33),character(33)) )
  it1 = slista%begin()
  it1 = it1%next()
  it1 = it1%next()
  it1 = it1%next()
  it2 = slista%insert( it1, slistb%begin(), slistb%end() )
  call ut%assert_equal( 'Insert range size', slista%size(), size(reference) + 3 )
  element = it2%get_element()
  call ut%assert_equal( 'Insert range', element%get_i(), 33 )
  allocate(elements(slista%size()))
  elements = slista%array()
  call ut%assert_equal( 'Insert range range', elements(4:6)%get_i(), [ 33, 33, 33 ] )
  it1 = it2%previous()
  element = it1%get_element()
  call ut%assert_equal( 'Insert range previous', element%get_i(), reference(3) )
  it1 = it1%next()
  element = it1%get_element()
  call ut%assert_equal( 'Insert range from previous', element%get_i(), 33 )
  it1 = it2%next()
  it1 = it1%next()
  it1 = it1%next()
  element = it1%get_element()
  call ut%assert_equal( 'Insert range next', element%get_i(), reference(4) )
  it1 = it1%previous()
  element = it1%get_element()
  call ut%assert_equal( 'Insert range from next', element%get_i(), 33 )
  deallocate(elements)

! Insert range at beginning of list
! Insert 3 elements of value 55 by its iterators before the four the element in the main list
! Then checks
!   The total number of elemens in the list
!   The pointer to the first element in the inserted section
!   The navigation from the first inserted element to the element before the insertion
!   The navigation to the first inserted element from the element prior to isnertion
!   The navigation to the first element after the inserted section
!   The navigation from the first element after the inserted section to the last inserted element
  slista = slista0
  slistb = xxconstructor___slist_ftl( 3, cxxtypebase__(55,dble(55),character(55)) )
  it1 = slista%begin()
  it2 = slista%insert( it1, slistb%begin(), slistb%end() )
  call ut%assert_equal( 'Insert range at beginning size', slista%size(), size(reference) + 3 )
  element = it2%get_element()
  call ut%assert_equal( 'Insert range at beginning', element%get_i(), 55 )
  allocate(elements(slista%size()))
  elements = slista%array()
  call ut%assert_equal( 'Insert range at beginning range', elements(1:3)%get_i(), [ 55, 55, 55 ] )
  it1 = it2%previous()
  call ut%assert_false( 'Insert range at beginning previous', it1%associated() )
  it1 = it2%next()
  it1 = it1%next()
  it1 = it1%next()
  element = it1%get_element()
  call ut%assert_equal( 'Insert range at beginning next', element%get_i(), reference(1) )
  it1 = it1%previous()
  element = it1%get_element()
  call ut%assert_equal( 'Insert range at beginning from next', element%get_i(), 55 )
  deallocate(elements)

! Insert array of elements
! Insert 3 elements of value 44 by its iterators before the four the element in the main list
! Then checks
!   The total number of elemens in the list
!   The pointer to the first element in the inserted section
!   The navigation from the first inserted element to the element before the insertion
!   The navigation to the first inserted element from the element prior to isnertion
!   The navigation to the first element after the inserted section
!   The navigation from the first element after the inserted section to the last inserted element
  slista = slista0
  allocate( elements(3) )
  elements = cxxtypebase__(44,dble(44),character(44))
  it1 = slista%begin()
  it1 = it1%next()
  it1 = it1%next()
  it1 = it1%next()
  it2 = slista%insert( it1, elements )
  deallocate( elements )
  call ut%assert_equal( 'Insert array size', slista%size(), size(reference) + 3 )
  element = it2%get_element()
  call ut%assert_equal( 'Insert array', element%get_i(), 44 )
  allocate(elements(slista%size()))
  elements = slista%array()
  call ut%assert_equal( 'Insert array range', elements(4:6)%get_i(), [ 44, 44, 44 ] )
  it1 = it2%previous()
  element = it1%get_element()
  call ut%assert_equal( 'Insert array previous', element%get_i(), reference(3) )
  it1 = it1%next()
  element = it1%get_element()
  call ut%assert_equal( 'Insert array from previous', element%get_i(), 44 )
  it1 = it2%next()
  it1 = it1%next()
  it1 = it1%next()
  element = it1%get_element()
  call ut%assert_equal( 'Insert array next', element%get_i(), reference(4) )
  it1 = it1%previous()
  element = it1%get_element()
  call ut%assert_equal( 'Insert array from next', element%get_i(), 44 )
  deallocate(elements)

! Swap
  slista = xxconstructor___slist_ftl( 5, cxxtypebase__(5,dble(5),character(5) ))
  slistb = xxconstructor___slist_ftl( 3, cxxtypebase__(3,dble(3),character(3) ))
  call slista%swap(slistb)
  call ut%assert_equal( 'Swap A size', slista%size(), 3 )
  call ut%assert_equal( 'Swap B size', slistb%size(), 5 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Swap A', elements%get_i(), [ 3, 3, 3 ] )
  deallocate(elements)
  allocate( elements(slistb%size()) )
  elements = slistb%array()
  call ut%assert_equal( 'Swap B', elements%get_i(), [ 5, 5, 5, 5, 5 ] )
  deallocate(elements)

! Resize
  slista = slista0
  call slista%resize(5)
  call ut%assert_equal( 'Resize reduce size', slista%size(), 5 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Resize reduce', elements%get_i(), reference(:5) )
  deallocate(elements)
  call slista%resize( 7, cxxtypebase__( 9, dble(9), character(9)) )
  call ut%assert_equal( 'Resize increase value size', slista%size(), 7 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Resize increase value', elements%get_i(), [ reference(:5), 9, 9 ] )
  deallocate(elements)
  call slista%resize(3)
  call slista%resize(5)
  call ut%assert_equal( 'Resize increase null size', slista%size(), 5 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Resize increase value', elements%get_i(), [ reference(:3), 0, 0 ] )
  deallocate(elements)

! Clear
  call slista%clear()
  call ut%assert_equal( 'Clear size', slista%size(), 0 )
  it1 = slista%begin()
  call ut%assert_false( 'Clear begin', it1%associated() )

end subroutine unit_slist_test_006

! ############################################################################

subroutine unit_slist_after_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Finalise list
  call slista0%clear()
  call slistb0%clear()

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_slist_after_006

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_slist_before_007( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

! Initialise the lists
  call initialise_slists()

end subroutine unit_slist_before_007

! ############################################################################

subroutine unit_slist_test_007( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Element
  type(xxtypebase__) :: element

! Array of elements
  class(xxtypebase__), allocatable, dimension(:) :: elements

! List structures
  type(xxtypebase___slist_ftl) :: slista, slistb

! List iterators
  type(xxtypebase___slist_ftl_iterator) :: it1, it2, it3

! Local counters
  integer :: i, j

! Splice full
  slista = slista0
  call slista%resize(5)
  it1 = slista%begin()
  it1 = it1%next()
  it1 = it1%next()
  slistb = xxconstructor___slist_ftl( 3, cxxtypebase__(33,dble(33), character(33)) )
  call slista%splice( it1, slistb )
  call ut%assert_equal( 'Splice full A size', slista%size(), 8 )
  call ut%assert_equal( 'Splice full B size', slistb%size(), 0 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Splice full', elements%get_i(), [ reference(:2), 33, 33, 33, reference(3:5) ] )
  deallocate(elements)

! Splice single
  slista = slista0
  call slista%resize(5)
  it1 = slista%begin()
  it1 = it1%next()
  it1 = it1%next()
  slistb = slista0
  it2 = slistb%begin()
  it2 = it2%next()
  call slista%splice( it1, slistb, it2 )
  call ut%assert_equal( 'Splice single A size', slista%size(), 6 )
  call ut%assert_equal( 'Splice single B size', slistb%size(), size(reference)-1 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Splice single A', elements%get_i(), [ reference(:2), reference(2), reference(3:5) ] )
  deallocate(elements)
  allocate( elements(slistb%size()) )
  elements = slistb%array()
  call ut%assert_equal( 'Splice single B', elements%get_i(), [ reference(:1), reference(3:) ] )
  deallocate(elements)

! Splice range
  slista = slista0
  call slista%resize(5)
  it1 = slista%begin()
  it1 = it1%next()
  it1 = it1%next()
  slistb = slista0
  it2 = slistb%begin()
  it2 = it2%next()
  it3 = it2%next()
  it3 = it3%next()
  it3 = it3%next()
  call slista%splice( it1, slistb, it2, it3 )
  call ut%assert_equal( 'Splice range A size', slista%size(), 9 )
  call ut%assert_equal( 'Splice range B size', slistb%size(), size(reference)-4 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Splice range A', elements%get_i(), [ reference(:2), reference(2:5), reference(3:5) ] )
  deallocate(elements)
  allocate( elements(slistb%size()) )
  elements = slistb%array()
  call ut%assert_equal( 'Splice range B', elements%get_i(), [ reference(:1), reference(6:) ] )
  deallocate(elements)

! Splice range (last source node at source boundary)
  slista = slista0
  call slista%resize(5)
  it1 = slista%begin()
  it1 = it1%next()
  it1 = it1%next()
  slistb = slista0
  call slistb%resize(5)
  it2 = slistb%begin()
  it2 = it2%next()
  it2 = it2%next()
  it2 = it2%next()
  it3 = slistb%end()
  call slista%splice( it1, slistb, it2, it3 )
  call ut%assert_equal( 'Splice range end boundary A size', slista%size(), 7 )
  call ut%assert_equal( 'Splice range end boundary B size', slistb%size(), 3 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Splice range end boundary A', elements%get_i(), [ reference(:2), reference(4:5), reference(3:5) ] )
  deallocate(elements)
  allocate( elements(slistb%size()) )
  elements = slistb%array()
  call ut%assert_equal( 'Splice range end boundary B', elements%get_i(), reference(:3) )
  deallocate(elements)

! Splice range (first source node at source boundary)
  slista = slista0
  call slista%resize(5)
  it1 = slista%begin()
  it1 = it1%next()
  it1 = it1%next()
  slistb = slista0
  call slistb%resize(5)
  it2 = slistb%begin()
  it3 = slistb%end()
  it3 = it3%previous()
  it3 = it3%previous()
  call slista%splice( it1, slistb, it2, it3 )
  call ut%assert_equal( 'Splice range begin boundary A size', slista%size(), 8 )
  call ut%assert_equal( 'Splice range begin boundary B size', slistb%size(), 2 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Splice range begin boundary A', elements%get_i(), [ reference(:2), reference(1:3), reference(3:5) ] )
  deallocate(elements)
  allocate( elements(slistb%size()) )
  elements = slistb%array()
  call ut%assert_equal( 'Splice begin boundary range B', elements%get_i(), reference(4:5) )
  deallocate(elements)

! Splice range (at begining of target)
  slista = slista0
  call slista%resize(5)
  it1 = slista%begin()
  slistb = slista0
  call slistb%resize(5)
  it2 = slistb%begin()
  it2 = it2%next()
  it3 = it2%next()
  it3 = it3%next()
  call slista%splice( it1, slistb, it2, it3 )
  call ut%assert_equal( 'Splice range begining of target A size', slista%size(), 8 )
  call ut%assert_equal( 'Splice range begining of target B size', slistb%size(), 2 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Splice range begining of target A', elements%get_i(), [ reference(2:4), reference(1:5) ] )
  deallocate(elements)
  allocate( elements(slistb%size()) )
  elements = slistb%array()
  call ut%assert_equal( 'Splice begining of target range B', elements%get_i(), reference( [ 1, 5 ] ) )
  deallocate(elements)

! Splice range (at end of target; append)
  slista = slista0
  call slista%resize(5)
  it1 = slista%end()
  it1 = it1%next()
  call ut%assert_false( 'Splice range after end of target A position', it1%associated() )
  slistb = slista0
  call slistb%resize(5)
  it2 = slistb%begin()
  it2 = it2%next()
  it3 = it2%next()
  it3 = it3%next()
  call slista%splice( it1, slistb, it2, it3 )
  call ut%assert_equal( 'Splice range after end of target A size', slista%size(), 8 )
  call ut%assert_equal( 'Splice range after end of target B size', slistb%size(), 2 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Splice range after end of target A', elements%get_i(), [ reference(1:5), reference(2:4) ] )
  deallocate(elements)
  allocate( elements(slistb%size()) )
  elements = slistb%array()
  call ut%assert_equal( 'Splice after end of target range B', elements%get_i(), reference( [ 1, 5 ] ) )
  deallocate(elements)


! Remove
  slista = slista0
  call slista%resize(5)
  do i = 1, slista%size()
    call slista%push_back( slista%at(i) )
  end do
  element = cxxtypebase__( 3, dble(3), character(3) )
  call slista%remove(element)
  call ut%assert_equal( 'Remove size', slista%size(), 8 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Remove', elements%get_i(), [ reference(1:2), reference(4:5), reference(1:2), reference(4:5) ] )
  deallocate(elements)

! Remove if
  slista = slista0
  call slista%remove_if(pred)
  call ut%assert_equal( 'Remove if size', slista%size(), 5 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Remove if', elements%get_i(), reference(1:5) )
  deallocate(elements)

! Unique
  slista = slista0
  call slista%resize(5)
  do i = 1, slista%size()
    call slista%push_back( slista%at(i) )
  end do
  call slista%unique()
  call ut%assert_equal( 'Unique size', slista%size(), 5 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Unique', elements%get_i(), reference(1:5) )
  deallocate(elements)

! Unique with predicate
  slista = slista0
  call slista%resize(5)
  do i = 1, slista%size()
    call slista%push_back( slista%at(i) )
  end do
  call slista%unique( local_equal )
  call ut%assert_equal( 'Unique with predicate size', slista%size(), 5 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Unique with predicate', elements%get_i(), reference(1:5) )
  deallocate(elements)

! Merge default
  slista = xxconstructor___slist_ftl( cxxtypebase__( reference(1::2), dble(reference(1::2)), character(reference(1::2)) ) )
  call slista%resize(5)
  slistb = xxconstructor___slist_ftl( cxxtypebase__( reference(2::2), dble(reference(2::2)), character(reference(2::2)) ) )
  call slistb%resize(5)
  call slista%merge(slistb)
  call ut%assert_equal( 'Merge default size', slista%size(), 10 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Merge default', elements%get_i(), reference  )
  deallocate(elements)

! Merge default (rever order to cover all branches)
  slista = xxconstructor___slist_ftl( cxxtypebase__( reference(1::2), dble(reference(1::2)), character(reference(1::2)) ) )
  call slista%resize(5)
  slistb = xxconstructor___slist_ftl( cxxtypebase__( reference(2::2), dble(reference(2::2)), character(reference(2::2)) ) )
  call slistb%resize(5)
  call slistb%merge(slista)
  call ut%assert_equal( 'Merge default reverse size', slistb%size(), 10 )
  allocate( elements(slistb%size()) )
  elements = slistb%array()
  call ut%assert_equal( 'Merge default reverse', elements%get_i(), reference  )
  deallocate(elements)

! Merge compare
  slista = xxconstructor___slist_ftl( cxxtypebase__( reference(1::2), dble(reference(1::2)), character(reference(1::2)) ) )
  call slista%resize(5)
  slistb = xxconstructor___slist_ftl( cxxtypebase__( reference(2::2), dble(reference(2::2)), character(reference(2::2)) ) )
  call slistb%resize(5)
  call slista%merge( slistb, local_less )
  call ut%assert_equal( 'Merge compare size', slista%size(), 10 )
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Merge compare', elements%get_i(), reference  )
  deallocate(elements)

! Reverse
  slista = slista0
  call slista%reverse()
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Reverse', elements%get_i(), reference(size(reference):1:-1)  )
  deallocate(elements)

! Sort default
  call slista%sort()
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Sort default', elements%get_i(), reference  )
  deallocate(elements)

! Sort compare
  call slista%sort(local_greater)
  allocate( elements(slista%size()) )
  elements = slista%array()
  call ut%assert_equal( 'Sort compare', elements%get_i(), reference(size(reference):1:-1)  )
  deallocate(elements)

end subroutine unit_slist_test_007

! ############################################################################

subroutine unit_slist_after_007( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Finalise list
  call slista0%clear()
  call slistb0%clear()

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_slist_after_007

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_slist_before_008( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

! Initialise the lists
  call initialise_slists()

end subroutine unit_slist_before_008

! ############################################################################

subroutine unit_slist_test_008( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Element
  type(xxtypebase__) :: element, element2
  type(xxtypebase__), pointer :: pelement, pelement2

! Array of elements
  class(xxtypebase__), allocatable, dimension(:) :: elements

! List structures
  type(xxtypebase___slist_ftl) :: slista, slistb

! List iterators
  type(xxtypebase___slist_ftl_iterator) :: it1, it2, it3

! Local counters
  integer :: i, j

! Iterators distance
  slista = slista0
  it1 = slista%begin()
  it1 = it1%next()
  it2 = it1%next()
  it2 = it2%next()
  it2 = it2%next()
  it2 = it2%next()
  call ut%assert_equal( 'Iterator distance', it1%distance(it2), 4 )

! Iterators distance (out of range)
  call ut%assert_equal( 'Iterator distance (out of range)', it1%distance(it3), -1 )

! Element pointer from iterator
  pelement => it2%get_element_ptr()
  call ut%assert_equal( 'Element pointer from iterator', pelement%get_i(), reference(6) )

! Swap iterators
  element = it1%get_element()
  element2 = it2%get_element()
  call it1%swap(it2)
  pelement => it1%get_element_ptr()
  pelement2 => it2%get_element_ptr()
  call ut%assert_equal( 'Iterator swap (first element)', pelement%get_i(), element2%get_i() )
  call ut%assert_equal( 'Iterator swap (second element)', pelement2%get_i(), element%get_i() )

! Set element from iterator
  element = it1%get_element()
  call it2%set_element( element )
  pelement => it2%get_element_ptr()
  call ut%assert_equal( 'Set element from iterator', pelement%get_i(), element%get_i() )

! Un-equality of iterators
  call ut%assert_true( 'Iterators not equal', it1 /= it3 )

! Nullify iterator
  call it2%nullify()
  call ut%assert_false( 'Iterator nullify', it2%associated() )

! Pop front till the last element is exhausted
  slista = slista0
  do while( slista%size() > 0 )
    call slista%pop_front()
  end do
  call ut%assert_equal( 'Pop front all elements', slista%size(), 0 )

! Pop back till the last element is exhausted
  slista = slista0
  do while( slista%size() > 0 )
    call slista%pop_back()
  end do
  call ut%assert_equal( 'Pop back all elements', slista%size(), 0 )

! Erase till the last element is exhausted
  slista = slista0
  do while( slista%size() > 0 )
    it1 = slista%begin()
    call slista%erase( it1 )
  end do
  call ut%assert_equal( 'Erase all elements', slista%size(), 0 )

end subroutine unit_slist_test_008

! ############################################################################

subroutine unit_slist_after_008( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Finalise list
  call slista0%clear()
  call slistb0%clear()

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_slist_after_008

! ############################################################################
! # Suite before procedure ###################################################
! ############################################################################

subroutine unit_slist_suite_before( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite


end subroutine unit_slist_suite_before

! ############################################################################
! # Suite after procedure ####################################################
! ############################################################################

subroutine unit_slist_suite_after( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite


end subroutine unit_slist_suite_after


! ############################################################################
! # Initialise the lists #####################################################
! ############################################################################
subroutine initialise_slists()

! Element
  type(xxtypebase__) :: a_dummy_element

! List iterator
  type(xxtypebase___slist_ftl_iterator) :: iterator

! Initialise the list
  slista0 = xxconstructor___slist_ftl( cxxtypebase__( reference, dble(reference), character(reference) ) )
  slistb0 = xxconstructor___slist_ftl( cxxtypebase__( -reference, -dble(reference), character(-reference) ) )

end subroutine initialise_slists

! Local equal function for sorting and binary search
pure function local_equal( this, right ) result(res)

  class(xxtypebase__), intent(in) :: this

  class(xxtypebase__), intent(in) :: right

  logical :: res

  res = ( this%get_i() == right%get_i() )

end function local_equal


! Local less function for sorting and binary search
pure function local_less( this, right ) result(res)

  class(xxtypebase__), intent(in) :: this

  class(xxtypebase__), intent(in) :: right

  logical :: res

  res = ( this%get_i() < right%get_i() )

end function local_less


! Local greater function for sorting and binary search
pure function local_greater( this, right ) result(res)

  class(xxtypebase__), intent(in) :: this

  class(xxtypebase__), intent(in) :: right

  logical :: res

  res = ( this%get_i() > right%get_i() )

end function local_greater


! Unary predicate function
pure function pred( this ) result(res)

  class(xxtypebase__), intent(in) :: this

  logical :: res

  res = ( this%get_i() > 5 )

end function pred


! Binary predicate function
pure function binary_pred( this, reference ) result(res)

  class(xxtypebase__), intent(in) :: this

  class(xxtypebase__), intent(in) :: reference

  logical :: res

  res = ( this%get_i() > reference%get_i() )

end function binary_pred


end module unit_slist_tests


