module unit_tree_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for xxbase___tree_ftl
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
  use m_string
  use m_messages
  use m_xfunit

  use xxuse__
  use xxmodulebase___vector_ftl
  use xxmodulebase___tree_ftl
  use xxmodulebase___vector_ftl
  use xxmodulebase___tree_ftl

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_tree_suite_before
  public unit_tree_suite_after

  public unit_tree_test_001
  public unit_tree_before_001
  public unit_tree_after_001

  public unit_tree_test_002
  public unit_tree_before_002
  public unit_tree_after_002

  public unit_tree_test_003
  public unit_tree_before_003
  public unit_tree_after_003

  public unit_tree_test_004
  public unit_tree_before_004
  public unit_tree_after_004

  public unit_tree_test_005
  public unit_tree_before_005
  public unit_tree_after_005

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'fxx'
  character(len=*), parameter :: module = 'tree'

  character(len=130), parameter :: sccs_info = &
  '$Id: $'

!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite

! The error handling structure
  type(t_messages), save :: msg

! Test tree structures to use as reference
  type(xxtypebase___tree_ftl), save :: treea, treeb
  type(xxtypebase___vector_ftl), save :: nodes

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_tree_before_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()


end subroutine unit_tree_before_001

! ############################################################################

subroutine unit_tree_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  type(xxtypebase___tree_ftl_iterator), save :: ita, itb
  type(xxtypebase__) :: element, velement
  integer :: nodexount, inode

! Forward-navigate the whole tree
! Store the nodes in this first navigation
  ita = treea%begin()
  inode = 1
  call nodes%push_back(element)   ! Null element
  itb = ita%parent()
  call ut%assert_false( 'Root node', itb%associated() )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 01', element%get_c(), "1.1" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 02', element%get_c(), "1.1.1" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 03', element%get_c(), "1.1.1.1" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 04', element%get_c(), "1.1.1.2" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 05', element%get_c(), "1.1.1.3" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 06', element%get_c(), "1.1.1.4" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 07', element%get_c(), "1.1.2" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 08', element%get_c(), "1.1.2.1" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 09', element%get_c(), "1.1.2.2" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 10', element%get_c(), "1.1.2.3" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 11', element%get_c(), "1.1.2.4" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 12', element%get_c(), "1.1.3" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 13', element%get_c(), "1.1.3.1" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 14', element%get_c(), "1.1.3.2" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 15', element%get_c(), "1.1.3.3" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 16', element%get_c(), "1.1.3.4" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 17', element%get_c(), "1.2" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 18', element%get_c(), "1.2.1" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 19', element%get_c(), "1.2.2" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 20', element%get_c(), "1.2.3" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 21', element%get_c(), "1.3" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 22', element%get_c(), "1.3.1" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 23', element%get_c(), "1.3.1.1" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 24', element%get_c(), "1.3.1.2" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 25', element%get_c(), "1.3.2" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 26', element%get_c(), "1.3.2.1" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 27', element%get_c(), "1.3.2.2" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 28', element%get_c(), "1.3.3" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 29', element%get_c(), "1.3.3.1" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 30', element%get_c(), "1.3.3.2" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 31', element%get_c(), "1.4" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 32', element%get_c(), "1.4.1" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 33', element%get_c(), "1.4.2" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 34', element%get_c(), "1.4.3" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 35', element%get_c(), "1.5" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 36', element%get_c(), "1.5.1" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 37', element%get_c(), "1.5.2" )
  ita = ita%next()
  inode = inode + 1
  element = ita%get_element()
  call nodes%push_back(element)
  call ut%assert_equal( 'Node 38', element%get_c(), "1.5.3" )
  ita = ita%next()
  call ut%assert_false( 'Past last node', ita%associated() )

! Backward-navigate the whole tree
  ita = treea%end()
  do inode = nodes%size(), 1, -1
    if( ita%has_data() ) then
      element = ita%get_element()
      velement = nodes%at(inode)
      call ut%assert_equal( 'Node '//trim(character(inode)), element%get_c(), velement%get_c() )
    end if
    ita = ita%previous()
  end do

end subroutine unit_tree_test_001

! ############################################################################

subroutine unit_tree_after_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_tree_after_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_tree_before_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()


end subroutine unit_tree_before_002

! ############################################################################

subroutine unit_tree_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local tree
  type(xxtypebase___tree_ftl), save :: ltree

! Local variables
  type(xxtypebase___tree_ftl_iterator), save :: ita, itb
  type(xxtypebase__) :: element
  class(xxtypebase__), allocatable, dimension(:) :: elements, velements
  integer :: inode

! Initialise locals
  allocate( elements(nodes%size()), velements(nodes%size()) )
  velements = nodes%array()

! Default constructor
  ltree = xxconstructor___tree_ftl()
  ita = ltree%begin()
  call ut%assert_true( 'Default constructor (root)', ita%associated() )
  ita = ita%next()
  call ut%assert_false( 'Default constructor (first element)', ita%associated() )

! Copy constructor
  ltree = xxconstructor___tree_ftl( treea )
  ita = ltree%begin()
  ita = ita%next()
  inode = 1
  do while( ita%associated() )
    inode = inode + 1
    elements(inode) = ita%get_element()
    ita = ita%next()
  end do
  call ut%assert_equal( 'Copy constructor', elements%get_c(), velements%get_c() )

! Finalise the tree
  call ltree%clear()
  ita = ltree%begin()
  call ut%assert_true( 'After clear (root)', ita%associated() )
  ita = ita%next()
  call ut%assert_false( 'After clear (first element)', ita%associated() )

! Assignment
  call ltree%assign(treea)
  ita = ltree%begin()
  ita = ita%next()
  inode = 1
  do while( ita%associated() )
    inode = inode + 1
    elements(inode) = ita%get_element()
    ita = ita%next()
  end do
  call ut%assert_equal( 'Assignment', elements%get_c(), velements%get_c() )

! Finalise the tree
  call ltree%clear()

! Assignment operator
  ltree = treea
  ita = ltree%begin()
  ita = ita%next()
  inode = 1
  do while( ita%associated() )
    inode = inode + 1
    elements(inode) = ita%get_element()
    ita = ita%next()
  end do
  call ut%assert_equal( 'Assignment operator', elements%get_c(), velements%get_c() )

! Finalise the tree
  call ltree%clear()

end subroutine unit_tree_test_002

! ############################################################################

subroutine unit_tree_after_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_tree_after_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_tree_before_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_tree_before_003

! ############################################################################

subroutine unit_tree_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local tree
  type(xxtypebase___tree_ftl), save :: ltree

! Local variables
  type(xxtypebase___tree_ftl_iterator), save :: ita, itb
  type(xxtypebase__) :: element, element7
  type(xxtypebase__), allocatable, dimension(:) :: elements, velements
  integer :: inode, children

! Initialise local tree
  ltree = treea

! Initialise local control variables
  ita = treea%begin()
  children = ita%children()
  element7 = cxxtypebase__( 7, dble(7), character(7) )

! Add child at the end on the root children list
  call ltree%push_back_child( element7 )
  itb = ltree%begin()
  call ut%assert_equal( 'Push back child root (size)', itb%children(), children+1 )
  itb = ltree%begin()
  itb = itb%next()
  itb = ltree%end_sibling(itb)
  element = itb%get_element()
  call ut%assert_equal( 'Push back child root (inserted)', element%get_c(), '7' )
  ita = itb%previous()
  element = ita%get_element()
  call ut%assert_equal( 'Push back child root (previous)', element%get_c(), '1.5.3' )
  ita = itb%previous_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Push back child root (previous sibling)', element%get_c(), '1.5' )


! Remove child at the end on the root children list
  call ltree%pop_back_child()
  itb = ltree%begin()
  call ut%assert_equal( 'Pop back child root (size)', itb%children(), children )
  itb = ltree%begin()
  itb = itb%next()
  itb = ltree%end_sibling(itb)
  element = itb%get_element()
  call ut%assert_equal( 'Pop back child root (last)', element%get_c(), '1.5' )
  ita = itb%previous()
  element = ita%get_element()
  call ut%assert_equal( 'Pop back child root (previous)', element%get_c(), '1.4.3' )
  ita = itb%previous_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Pop back child root (previous sibling)', element%get_c(), '1.4' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Pop back child root (next)', element%get_c(), '1.5.1' )

! Add child at the beginning on the root children list
  call ltree%push_front_child( element7 )
  itb = ltree%begin()
  call ut%assert_equal( 'Push front child root (size)', itb%children(), children + 1 )
  itb = ltree%begin()
  itb = itb%next()
  itb = ltree%begin_sibling(itb)
  element = itb%get_element()
  call ut%assert_equal( 'Push front child root (inserted)', element%get_c(), '7' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Push front child root (next)', element%get_c(), '1.1' )
  ita = itb%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Push front child root (next sibling)', element%get_c(), '1.1' )

! Limitng case (try root siblings)
  itb = ltree%begin()
  itb = itb%parent()
  itb = ltree%begin_sibling(itb)
  call ut%assert_false( 'Root begin sibling is null', itb%associated() )
  itb = ltree%begin()
  itb = itb%parent()
  itb = ltree%end_sibling(itb)
  call ut%assert_false( 'Root end sibling is null', itb%associated() )

! Remove child at the beginning on the root children list
  call ltree%pop_front_child()
  itb = ltree%begin()
  call ut%assert_equal( 'Pop front child root (size)', itb%children(), children )
  itb = ltree%begin()
  itb = itb%next()
  itb = ltree%begin_sibling(itb)
  element = itb%get_element()
  call ut%assert_equal( 'Pop front child root (first)', element%get_c(), '1.1' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Pop front child root (next)', element%get_c(), '1.1.1' )
  ita = itb%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Pop front child root (next sibling)', element%get_c(), '1.2' )

! Re-initialise local cotrol variables
  ita = treea%begin()
  ita = ita%next()
  children = ita%children()

! Add child at the end of the children list
  ita = ltree%begin()
  ita = ita%next()
  call ltree%push_back_child( ita, element7 )
  call ut%assert_equal( 'Push back child (size)', ita%children(), children + 1 )
  itb = ita%next()
  itb = ltree%end_sibling(itb)
  element = itb%get_element()
  call ut%assert_equal( 'Push back child (inserted)', element%get_c(), '7' )
  ita = itb%previous()
  element = ita%get_element()
  call ut%assert_equal( 'Push back child (previous)', element%get_c(), '1.1.3.4' )
  ita = itb%previous_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Push back child (previous sibling)', element%get_c(), '1.1.3' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Push back child (next)', element%get_c(), '1.2' )

! Remove child from the end of children list
  ita = ltree%begin()
  ita = ita%next()
  call ltree%pop_back_child( ita )
  call ut%assert_equal( 'Pop back child (size)', ita%children(), children )
  itb = ltree%end(ita)
  element = itb%get_element()
  call ut%assert_equal( 'Pop back child (last)', element%get_c(), '1.1.3' )
  ita = itb%previous()
  element = ita%get_element()
  call ut%assert_equal( 'Pop back child (previous)', element%get_c(), '1.1.2.4' )
  ita = itb%previous_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Pop back child (previous sibling)', element%get_c(), '1.1.2' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Pop back child (next)', element%get_c(), '1.1.3.1' )

! Add child at the beginning of the children list
  ita = ltree%begin()
  ita = ita%next()
  call ltree%push_front_child( ita, element7 )
  call ut%assert_equal( 'Push front child (size)', ita%children(), children + 1 )
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  element = itb%get_element()
  call ut%assert_equal( 'Push front child (inserted)', element%get_c(), '7' )
  ita = itb%previous()
  element = ita%get_element()
  call ut%assert_equal( 'Push front child (previous)', element%get_c(), '1.1' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Push front child (next)', element%get_c(), '1.1.1' )
  ita = itb%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Push front child (next sibling)', element%get_c(), '1.1.1' )

! Remove child from the beginning of children list
  ita = ltree%begin()
  ita = ita%next()
  call ltree%pop_front_child( ita )
  call ut%assert_equal( 'Pop front child (size)', ita%children(), children )
  itb = ltree%begin(ita)
  element = itb%get_element()
  call ut%assert_equal( 'Pop front child (first)', element%get_c(), '1.1.1' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Pop front child (next)', element%get_c(), '1.1.1.1' )
  ita = itb%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Pop front child (next sibling)', element%get_c(), '1.1.2' )
  ita = itb%previous()
  element = ita%get_element()
  call ut%assert_equal( 'Pop front child (previous)', element%get_c(), '1.1' )

! Add/remove node into an empty node (push/pop)
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  ita = ita%next()
  call ltree%push_back_child( ita, element7 )
  ita = ita%next()
  element = ita%get_element()
  call ut%assert_equal( 'Push back new child', element%get_C(), '7' )
  itb = ita%previous()
  element = itb%get_element()
  call ut%assert_equal( 'Push back new child (previous)', element%get_c(), '1.1.1.1' )
  itb = ita%next()
  element = itb%get_element()
  call ut%assert_equal( 'Push back new child (next)', element%get_c(), '1.1.1.2' )
  ita = ita%parent()
  itb = ita
  call ltree%pop_back_child(ita)
  ita = itb%next()
  call ut%assert_equal( 'Pop back new child (next)', element%get_c(), '1.1.1.2' )
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  ita = ita%next()
  call ltree%push_front_child( ita, element7 )
  ita = ita%next()
  element = ita%get_element()
  call ut%assert_equal( 'Push front new child', element%get_C(), '7' )
  itb = ita%previous()
  element = itb%get_element()
  call ut%assert_equal( 'Push front new child (previous)', element%get_c(), '1.1.1.1' )
  itb = ita%next()
  element = itb%get_element()
  call ut%assert_equal( 'Push front new child (next)', element%get_c(), '1.1.1.2' )
  ita = ita%parent()
  itb = ita
  call ltree%pop_front_child(itb)
  ita = itb%next()
  call ut%assert_equal( 'Pop front new child (next)', element%get_c(), '1.1.1.2' )

! Add sibling at the end of the sibling list
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  itb = ita%parent()
  call ltree%push_back_sibling( ita, element7 )
  call ut%assert_equal( 'Push back sibling (size)', itb%children(), children + 1 )
  itb = ltree%end_sibling(ita)
  element = itb%get_element()
  call ut%assert_equal( 'Push back sibling (inserted)', element%get_c(), '7' )
  ita = itb%previous()
  element = ita%get_element()
  call ut%assert_equal( 'Push back child (previous)', element%get_c(), '1.1.3.4' )
  ita = itb%previous_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Push back child (previous sibling)', element%get_c(), '1.1.3' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Push back child (next)', element%get_c(), '1.2' )

! Remove sibling from the end of sibling list
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  itb = ita%parent()
  call ltree%pop_back_sibling( ita )
  call ut%assert_equal( 'Pop back sibling (size)', itb%children(), children )
  itb = ltree%end(itb)
  element = itb%get_element()
  call ut%assert_equal( 'Pop back sibling (last)', element%get_c(), '1.1.3' )
  ita = itb%previous()
  element = ita%get_element()
  call ut%assert_equal( 'Pop back sibling (previous)', element%get_c(), '1.1.2.4' )
  ita = itb%previous_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Pop back sibling (previous sibling)', element%get_c(), '1.1.2' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Pop back sibling (next)', element%get_c(), '1.1.3.1' )

! Add sibling at the beginning of the sibling list
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  itb = ita%parent()
  call ltree%push_front_sibling( ita, element7 )
  call ut%assert_equal( 'Push front sibling (size)', itb%children(), children + 1 )
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  element = itb%get_element()
  call ut%assert_equal( 'Push front sibling (inserted)', element%get_c(), '7' )
  ita = itb%previous()
  element = ita%get_element()
  call ut%assert_equal( 'Push front sibling (previous)', element%get_c(), '1.1' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Push front sibling (next)', element%get_c(), '1.1.1' )
  ita = itb%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Push front sibling (next sibling)', element%get_c(), '1.1.1' )

! Remove sibling from the beginning of sibling list
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  itb = ita%parent()
  call ltree%pop_front_sibling( ita )
  call ut%assert_equal( 'Pop front sibling (size)', itb%children(), children )
  itb = ltree%begin(itb)
  element = itb%get_element()
  call ut%assert_equal( 'Pop front sibling (first)', element%get_c(), '1.1.1' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Pop front sibling (next)', element%get_c(), '1.1.1.1' )
  ita = itb%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Pop front sibling (next sibling)', element%get_c(), '1.1.2' )
  ita = itb%previous()
  element = ita%get_element()
  call ut%assert_equal( 'Pop front sibling (previous)', element%get_c(), '1.1' )

! Insert sibling at the beginning of the sibling list
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  itb = ita%parent()
  call ltree%insert( ita, element7 )
  call ut%assert_equal( 'Insert sibling beginning (size)', itb%children(), children+1 )
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  element = itb%get_element()
  call ut%assert_equal( 'Insert sibling beginning (inserted)', element%get_c(), '7' )
  ita = itb%previous()
  element = ita%get_element()
  call ut%assert_equal( 'Insert sibling beginning (previous)', element%get_c(), '1.1' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Insert sibling beginning (next)', element%get_c(), '1.1.1' )
  ita = itb%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Insert sibling beginning (next sibling)', element%get_c(), '1.1.1' )

! Remove sibling from the beginning of sibling list
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  itb = ita%parent()
  call ltree%erase( ita )
  call ut%assert_equal( 'Erase sibling beginning (size)', itb%children(), children )
  itb = ltree%begin(itb)
  element = itb%get_element()
  call ut%assert_equal( 'Erase sibling beginning (first)', element%get_c(), '1.1.1' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Erase sibling beginning (next)', element%get_c(), '1.1.1.1' )
  ita = itb%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Erase sibling beginning (next sibling)', element%get_c(), '1.1.2' )
  ita = itb%previous()
  element = ita%get_element()
  call ut%assert_equal( 'Erase sibling beginning (previous)', element%get_c(), '1.1' )

! Insert sibling in the middle of the sibling list
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  ita = ita%next_sibling()
  itb = ita%parent()
  call ltree%insert( ita, element7 )
  call ut%assert_equal( 'Insert sibling middle (size)', itb%children(), children+1 )
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  ita = ita%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Insert sibling middle (inserted)', element%get_c(), '7' )
  itb = ita
  ita = itb%previous()
  element = ita%get_element()
  call ut%assert_equal( 'Insert sibling beginning (previous)', element%get_c(), '1.1.1.4' )
  ita = itb%previous_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Insert sibling beginning (previous sibling)', element%get_c(), '1.1.1' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Insert sibling beginning (next)', element%get_c(), '1.1.2' )
  ita = itb%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Insert sibling beginning (next sibling)', element%get_c(), '1.1.2' )

! Remove sibling from the middle of the sibling list
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  ita = ita%next_sibling()
  itb = ita%parent()
  call ltree%erase( ita )
  call ut%assert_equal( 'Erase sibling middle (size)', itb%children(), children )
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Erase sibling middle (next)', element%get_c(), '1.1.1.1' )
  ita = itb%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Erase sibling middle (next sibling)', element%get_c(), '1.1.2' )
  itb = ita%previous()
  element = itb%get_element()
  call ut%assert_equal( 'Erase sibling middle (previous)', element%get_c(), '1.1.1.4' )
  itb = ita%previous_sibling()
  element = itb%get_element()
  call ut%assert_equal( 'Erase sibling middle (previous sibling)', element%get_c(), '1.1.1' )

! Remove sibling from the end of the sibling list
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  ita = ltree%end_sibling(ita)
  itb = ita%parent()
  call ltree%erase( ita )
  call ut%assert_equal( 'Erase sibling end (size)', itb%children(), children - 1 )
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  itb = ltree%end_sibling(itb)
  element = itb%get_element()
  call ut%assert_equal( 'Erase sibling end (last)', element%get_c(), '1.1.2' )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Erase sibling end (next)', element%get_c(), '1.1.2.1' )
  ita = itb%previous()
  element = itb%get_element()
  call ut%assert_equal( 'Erase sibling end (previous)', element%get_c(), '1.1.2' )
  ita = itb%previous_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Erase sibling end (previous sibling)', element%get_c(), '1.1.1' )

! Remove a complete branch of the tree
  ltree = treea
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  itb = ita
  ita = ita%next_sibling()
  call ltree%clear(ita)
  call ut%assert_equal( 'Remove branch (siblings)', itb%siblings(), 1 )
  element = itb%get_element()
  call ut%assert_equal( 'Remove branch previous', element%get_c(), '1.1.1' )
  ita = itb%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Remove branch next', element%get_c(), '1.1.3' )

! Remove branch that is first child
  ltree = treea
  ita = ltree%begin()
  ita = ita%next()
  itb = ita
  ita = ita%next()
  call ltree%clear(ita)
  ita = itb%next()
  call ut%assert_equal( 'Remove branch at first child (siblings)', ita%siblings(), 1 )
  element = itb%get_element()
  call ut%assert_equal( 'Remove branch at first child previous', element%get_c(), '1.1' )
  element = ita%get_element()
  call ut%assert_equal( 'Remove branch at first child next', element%get_c(), '1.1.2' )

! Remove branch that is last child
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  itb = ita
  ita = ita%next_sibling()
  call ltree%clear(ita)
  call ut%assert_equal( 'Remove branch at last child (siblings)', itb%siblings(), 0 )
  itb = itb%parent()
  ita = ltree%begin(itb)
  element = ita%get_element()
  call ut%assert_equal( 'Remove branch at last child (first)', element%get_c(), '1.1.2' )
  ita = ltree%end(itb)
  element = ita%get_element()
  call ut%assert_equal( 'Remove branch at last child (last)', element%get_c(), '1.1.2' )

! Remove branch that is unique child
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next()
  itb = ita%parent()
  call ltree%clear(ita)
  call ut%assert_equal( 'Remove branch as unique child (siblings)', itb%children(), 0 )
  ita = itb%previous()
  call ut%assert_true( 'Remove branch at unique child (previous)', ita%associated(ltree%begin()) )
  ita = itb%next()
  element = ita%get_element()
  call ut%assert_equal( 'Remove branch at unique child (next)', element%get_c(), '1.2' )

! Finalise the tree
  call ltree%clear()

end subroutine unit_tree_test_003

! ############################################################################

subroutine unit_tree_after_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_tree_after_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_tree_before_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()


end subroutine unit_tree_before_004

! ############################################################################

subroutine unit_tree_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local tree
  type(xxtypebase___tree_ftl) :: ltree

! Local tree pointers
  type(xxtypebase___tree_ftl), pointer :: ptree

! Local variables
  type(xxtypebase___tree_ftl_iterator), save :: ita, itb
  type(xxtypebase__) :: element
  type(xxtypebase__), pointer :: pelement
  integer :: children

! Initialise local tree
  ltree = treea

! Initialise local control variables
  ita = treea%begin()
  children = ita%children()
  itb = ltree%begin()
  element = cxxtypebase__( 7, dble(7), character(7) )

! Parent node
  ita = treea%begin()
  ita = ita%next()
  ita = ita%next()
  ita = ita%next()
  itb = ita%parent()
  element = itb%get_element()
  call ut%assert_equal( 'Parent 1.1.1.1', element%get_c(), '1.1.1' )
  ita = ita%next()
  itb = ita%parent()
  element = itb%get_element()
  call ut%assert_equal( 'Parent 1.1.1.2', element%get_c(), '1.1.1' )
  itb = itb%parent()
  element = itb%get_element()
  call ut%assert_equal( 'Parent 1.1.1', element%get_c(), '1.1' )

! Has data
  ita = ltree%begin()
  call ut%assert_false( 'Has no data', ita%has_data() )
  ita = ita%next()
  call ut%assert_true( 'Has data', ita%has_data() )

! Has children
  ita = ltree%begin()
  call ut%assert_true( 'Has children', ita%has_children() )
  ita = ita%next()
  ita = ita%next()
  ita = ita%next()
  call ut%assert_false( 'Has no children', ita%has_children() )

! Has siblings
  ita = ltree%begin()
  call ut%assert_false( 'Has no siblings', ita%has_siblings() )
  ita = ita%next()
  call ut%assert_true( 'Has siblings', ita%has_siblings() )

! Depth
  ita = ltree%begin()
  call ut%assert_equal( 'Root depth', ita%depth(), 0 )
  ita = ita%next()
  ita = ita%next()
  call ut%assert_equal( 'Depth', ita%depth(), 2 )

! Sibling position
  ita = ltree%begin()
  call ut%assert_equal( 'Root sibling position', ita%sibling_position(), 1 )
  ita = ita%next()
  call ut%assert_equal( 'Initial sibling position', ita%sibling_position(), 1 )
  ita = ita%next_sibling()
  ita = ita%next_sibling()
  call ut%assert_equal( 'Initial sibling position', ita%sibling_position(), 3 )

! Empty tree
  call ut%assert_true( 'Empty tree', treeb%empty() )
  call ut%assert_false( 'Not-empty tree', treea%empty() )

! Empty iterator
  ita = treeb%begin()
  call ut%assert_true( 'Empty iterator', ita%empty() )
  ita = ltree%begin()
  call ut%assert_false( 'Not-empty iterator', ita%empty() )

! Front
  ita = ltree%begin()
  call ita%set_element( cxxtypebase__( 0, 0.0_8, '0' ) )
  element = ltree%front()
  call ut%assert_equal( 'Front', element%get_c(), '0' )

! Back element
  element = ltree%back()
  call ut%assert_equal( 'Back', element%get_c(), '1.5.3' )

! Element to iterator
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next_sibling()
  call ita%set_element( cxxtypebase__( 8, 8.0_8, '8' ) )
  element = ita%get_element()
  call ut%assert_equal( 'Set/get element', element%get_c(), '8' )

! Element pointer
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next_sibling()
  pelement => ita%get_element_ptr()
  call ut%assert_equal( 'Get element pointer', pelement%get_c(), '8' )

! ITterator container
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next_sibling()
  ptree => ita%container()
  ita = ptree%begin()
  ita = ita%next()
  ita = ita%next_sibling()
  pelement => ita%get_element_ptr()
  call ut%assert_equal( 'Element from containter pointer', pelement%get_c(), '8' )

end subroutine unit_tree_test_004

! ############################################################################

subroutine unit_tree_after_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_tree_after_004

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_tree_before_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_tree_before_005

! ############################################################################

subroutine unit_tree_test_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  type(xxtypebase___tree_ftl) :: ltree
  type(xxtypebase___tree_ftl_iterator) :: ita, itb
  type(xxtypebase__) :: element

! Iterator association
  ita = treea%begin()
  call ut%assert_true( 'Iterator associated', ita%associated() )
  call ita%nullify()
  call ut%assert_false( 'Iterator not associated', ita%associated() )
  ita = treea%begin()
  itb = treea%begin()
  call ut%assert_true( 'Iterator associated to another iterator', ita%associated(itb) )

! Swap element by iterators (general sibling position)
  ltree = treea
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next_sibling()
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  itb = itb%next_sibling()
  call ita%swap(itb)
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Swapped general from 1.2', element%get_c(), '1.1.2' )
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  itb = itb%next_sibling()
  element = itb%get_element()
  call ut%assert_equal( 'Swapped general from 1.1.2', element%get_c(), '1.2' )

! Swap element by iterators (first sibling position)
  ltree = treea
  ita = ltree%begin()
  ita = ita%next()
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  itb = itb%next_sibling()
  call ita%swap(itb)
  ita = ltree%begin()
  ita = ita%next()
  element = ita%get_element()
  call ut%assert_equal( 'Swapped first from 1.1', element%get_c(), '1.1.2' )
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  itb = itb%next_sibling()
  element = itb%get_element()
  call ut%assert_equal( 'Swapped first from 1.1.2', element%get_c(), '1.1' )
  ltree = treea
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next_sibling()
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  call ita%swap(itb)
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Swapped first from 1.2', element%get_c(), '1.1.1' )
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  element = itb%get_element()
  call ut%assert_equal( 'Swapped first from 1.1.1', element%get_c(), '1.2' )

! Swap element by iterators (last sibling position)
  ltree = treea
  ita = ltree%begin()
  ita = ita%next()
  ita = ltree%end_sibling(ita)
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  itb = itb%next_sibling()
  call ita%swap(itb)
  ita = ltree%begin()
  ita = ita%next()
  ita = ltree%end_sibling(ita)
  element = ita%get_element()
  call ut%assert_equal( 'Swapped last from 1.5', element%get_c(), '1.1.2' )
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  itb = itb%next_sibling()
  element = itb%get_element()
  call ut%assert_equal( 'Swapped last from 1.1.2', element%get_c(), '1.5' )
  ltree = treea
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next_sibling()
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  itb = ltree%end_sibling(itb)
  call ita%swap(itb)
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Swapped iterator from 1.2', element%get_c(), '1.1.3' )
  itb = ltree%begin()
  itb = itb%next()
  itb = itb%next()
  itb = ltree%end_sibling(itb)
  element = itb%get_element()
  call ut%assert_equal( 'Swapped iterator from 1.1.3', element%get_c(), '1.2' )

! Swap element by iterators (adjacent sibling position)
  ltree = treea
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next_sibling()
  itb = ita%next_sibling()
  call ita%swap(itb)
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Swapped adjacent from 1.2', element%get_c(), '1.3' )
  itb = itb%next_sibling()
  element = itb%get_element()
  call ut%assert_equal( 'Swapped adjacent from 1.3', element%get_c(), '1.2' )
  ltree = treea
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next_sibling()
  ita = ita%next_sibling()
  ita = ita%next_sibling()
  itb = ita%previous_sibling()
  call ita%swap(itb)
  ita = ltree%begin()
  ita = ita%next()
  ita = ita%next_sibling()
  ita = ita%next_sibling()
  ita = ita%next_sibling()
  element = ita%get_element()
  call ut%assert_equal( 'Swapped adjacent from 1.4', element%get_c(), '1.3' )
  itb = ita%previous_sibling()
  element = itb%get_element()
  call ut%assert_equal( 'Swapped adjacent from 1.3', element%get_c(), '1.4' )

end subroutine unit_tree_test_005

! ############################################################################

subroutine unit_tree_after_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
    call ut%error( 1, 'Errors during unit test execution', msg )
end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_tree_after_005

! ############################################################################
! # Suite before procedure ###################################################
! ############################################################################

subroutine unit_tree_suite_before( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite

! Tree iterator
  type(xxtypebase___tree_ftl_iterator) :: iterator

! Tree element
  type(xxtypebase__) :: element

! Local counter
  integer :: i, j, k

!  root ___ 1.1___ 1.1.1___1.1.1.1
!       |      |        |__1.1.1.2
!       |      |        |__1.1.1.3
!       |      |        |__1.1.1.4
!       |      |
!       |      |__ 1.1.2___1.1.2.1
!       |      |        |__1.1.2.2
!       |      |        |__1.1.2.3
!       |      |        |__1.1.2.4
!       |      |
!       |      |__ 1.1.3___1.1.3.1
!       |               |__1.1.3.2
!       |               |__1.1.3.3
!       |               |__1.1.3.4
!       |
!       |__ 1.2___ 1.2.1
!       |      |__ 1.2.2
!       |      |__ 1.2.3
!       |
!       |__ 1.3___ 1.3.1___1.3.1.1
!       |      |        |__1.3.1.2
!       |      |
!       |      |__ 1.3.2___1.3.2.1
!       |      |        |__1.3.2.2
!       |      |
!       |      |__ 1.3.3___1.3.3.1
!       |               |__1.3.3.2
!       |
!       |__ 1.4___ 1.4.1
!       |      |__ 1.4.2
!       |      |__ 1.4.3
!       |
!       |__ 1.5___ 1.5.1
!              |__ 1.5.2
!              |__ 1.5.3
!

! Add five nodes to the root (no data element)
  do i = 1, 5
    k = i
    element = cxxtypebase__( k, dble(k), "1."//trim(character(i)) )
    call treea%push_back_child( element )
  end do

! Add three children to each element at first level
  iterator = treea%begin()
  iterator = iterator%next()
  do i = 1, iterator%siblings() + 1
    do j = 1, 3
      k = 10 * i + j
      element = cxxtypebase__( k, dble(k), "1."//trim(character(i))//"."//trim(character(j)) )
      call treea%push_back_child( iterator, element )
    end do
    iterator = iterator%next_sibling()
  end do

! Add four children to each element at second level in the first subtree
  iterator = treea%begin()
  iterator = iterator%next()
  iterator = iterator%next()
  do i = 1, iterator%siblings() + 1
    do j = 1, 4
      k = 100 * i + j
      element = cxxtypebase__( k, dble(k), "1.1."//trim(character(i))//"."//trim(character(j)) )
      call treea%push_back_child( iterator, element )
    end do
    iterator = iterator%next_sibling()
  end do

! Add two children to each element at second level in the third subtree
  iterator = treea%begin()
  iterator = iterator%next()
  iterator = iterator%next_sibling()
  iterator = iterator%next_sibling()
  iterator = iterator%next()
  do i = 1, iterator%siblings() + 1
    do j = 1, 2
      k = 100 * i + j
      element = cxxtypebase__( k, dble(k), "1.3."//trim(character(i))//"."//trim(character(j)) )
      call treea%push_back_child( iterator, element )
    end do
    iterator = iterator%next_sibling()
  end do

end subroutine unit_tree_suite_before

! ############################################################################
! # Suite after procedure ####################################################
! ############################################################################

subroutine unit_tree_suite_after( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite


end subroutine unit_tree_suite_after

end module unit_tree_tests

