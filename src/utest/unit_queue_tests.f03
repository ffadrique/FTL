module unit_queue_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for xxtypebase___queue_ftl
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
  use m_xfunit
  use m_messages

  use xxuse__
  use xxmodulebase___queue_ftl

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_queue_suite_before
  public unit_queue_suite_after

  public unit_queue_before_001
  public unit_queue_test_001
  public unit_queue_after_001

  public unit_queue_before_002
  public unit_queue_test_002
  public unit_queue_after_002

  public unit_queue_before_003
  public unit_queue_test_003
  public unit_queue_after_003

  public manager, suite

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'fxx'
  character(len=*), parameter :: module = 'xxtypebase___queue_ftl'

  character(len=130), parameter :: sccs_info = &
  '\$Id: \$'

!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite

! The error handling structure
  type(t_messages), save :: msg

! List of object used as reference for the queue
  type(xxtypebase__), dimension(10), save :: elements

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_queue_before_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_queue_before_001

! ############################################################################

subroutine unit_queue_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(xxtypebase___queue_ftl) :: o, oo

! Local variables
  type(xxtypebase__), allocatable, dimension(:) :: oarray, ooarray

! Reset error handling structures
  call msg%reset_error()

! Default constructor
  o = xxconstructor___queue_ftl()
  call ut%assert_equal( 'Default constructor (size)', o%size(), 0 )
  call ut%assert_true( 'Default constructor (empty)', o%empty() )

! Constructor from array
  o = xxconstructor___queue_ftl(elements)
  call ut%assert_equal( 'Array constructor (size)', o%size(), size(elements) )
  allocate( oarray, source=o%array() )
  call ut%assert_equal( 'Array constructor', oarray, elements, ecompare, eserialize )

! Copy constructor
  oo = xxconstructor___queue_ftl(o)
  call ut%assert_equal( 'Copy constructor (size)', oo%size(), o%size() )
  allocate( ooarray, source=oo%array() )
  call ut%assert_equal( 'Copy constructor', ooarray, oarray, ecompare, eserialize )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_queue_test_001

! ############################################################################

subroutine unit_queue_after_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_queue_after_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_queue_before_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_queue_before_002

! ############################################################################

subroutine unit_queue_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(xxtypebase___queue_ftl) :: o, oo

! Local variables
  type(xxtypebase__), allocatable, dimension(:) :: oarray
  integer :: i

! Reset error handling structures
  call msg%reset_error()

! Initialise queue
  o = xxconstructor___queue_ftl()

! Add one element
  call o%push(elements(1))
  call ut%assert_equal( 'Push one element (size)', o%size(), 1 )
  call ut%assert_equal( 'Push one element (front)', o%front(), elements(1), ecompare, eserialize )
  call ut%assert_equal( 'Push one element (back)', o%back(), elements(1), ecompare, eserialize )

! Remove the one element
  call o%pop()
  call ut%assert_equal( 'Pop the one element (size)', o%size(), 0 )

! Add few elements
  do i = 1, 5
    call o%push( elements(i) )
  end do
  call ut%assert_equal( 'Push few elements (size)', o%size(), 5 )
  call ut%assert_equal( 'Push few elements (front)', o%front(), elements(1), ecompare, eserialize )
  call ut%assert_equal( 'Push few elements (back)', o%back(), elements(5), ecompare, eserialize )
  allocate( oarray, source=o%array() )
  call ut%assert_equal( 'Push few elements', oarray, elements(1:5), ecompare, eserialize )

! Remove two elements
  call o%pop()
  call o%pop()
  call ut%assert_equal( 'Pop two elements (size)', o%size(), 3 )
  call ut%assert_equal( 'Pop two elements (front)', o%front(), elements(3), ecompare, eserialize )
  call ut%assert_equal( 'Pop two elements (back)', o%back(), elements(5), ecompare, eserialize )
  deallocate( oarray )
  allocate( oarray, source=o%array() )
  call ut%assert_equal( 'Pop two elements', oarray, elements(3:5), ecompare, eserialize )

! Clear queue
  call o%clear()
  call ut%assert_equal( 'Clear queue (size)', o%size(), 0 )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_queue_test_002

! ############################################################################

subroutine unit_queue_after_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_queue_after_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_queue_before_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_queue_before_003

! ############################################################################

subroutine unit_queue_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(xxtypebase___queue_ftl) :: o, oo

! Local variables
  type(xxtypebase__), allocatable, dimension(:) :: oarray, ooarray

! Reset error handling structures
  call msg%reset_error()

! Initialise from array
  o = xxconstructor___queue_ftl(elements)

! Queue assignment
  oo = o
  call ut%assert_equal( 'Assignment (size)', oo%size(), o%size() )
  allocate( oarray, source=o%array() )
  allocate( ooarray, source=oo%array() )
  call ut%assert_equal( 'Assignment', ooarray, oarray, ecompare, eserialize )

! Queue assignment from array
  oo = elements(3:8)
  call ut%assert_equal( 'Assignment from array (size)', oo%size(), size(elements(3:8)) )
  deallocate( ooarray )
  allocate( ooarray, source=oo%array() )
  call ut%assert_equal( 'Assignment from array', ooarray, elements(3:8), ecompare, eserialize )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_queue_test_003

! ############################################################################

subroutine unit_queue_after_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_queue_after_003

! ############################################################################
! # Suite before procedure ###################################################
! ############################################################################

subroutine unit_queue_suite_before( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite

! Local variables
  integer :: i

! Create the reference array of elements for the queue
  do i = 1, size(elements)
      elements(i) = cxxtypebase__( i, real(i,8), character(i,fmt='(i4.4)'))
  end do

end subroutine unit_queue_suite_before

! ############################################################################
! # Suite after procedure ####################################################
! ############################################################################

subroutine unit_queue_suite_after( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite


end subroutine unit_queue_suite_after

! ############################################################################
! # Auxiliary functions ######################################################
! ############################################################################

function ecompare( actual, expected ) result(res)

  class(*), intent(in) :: actual
  class(*), intent(in) :: expected
  logical :: res

  select type( a => actual )
    type is(xxtypebase__)
      select type( e => expected )
        type is(xxtypebase__)
          res = ( a == e )
      end select
  end select

end function ecompare


function eserialize( element ) result(res)

  class(*), intent(in) :: element
  character(len=:), allocatable :: res

  select type( e => element )
    type is(xxtypebase__)
      res = trim(e%get_c())
  end select

end function eserialize

end module unit_queue_tests
