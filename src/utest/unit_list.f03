program unit_list

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests driver for xxbase___list_ftl
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

  use m_xfunit

  use unit_list_tests

!---End of use statements-------------------------------------------------------

  implicit none

  character(len=*), parameter :: package = 'fxx'
  character(len=*), parameter :: module = 'xxbase___list_ftl'
  
  character(len=130), parameter :: sccs_info = &
  '$Id: $'

!---Declaration of local variables----------------------------------------------


!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

! Local variables
  character(len=256) :: xfunit_root_dir
  logical :: junit_strict
  type(t_xfunit_suite), allocatable :: suite
  type(t_xfunit_unit), allocatable :: ut

! Initialise unit test infrastructure
  allocate( manager )
  allocate( suite )
  allocate( ut )

! Initialise report generation flag
  junit_strict = .false.

! Initialise the unit test manager
  call get_environment_variable( 'XFUNIT_ROOT_DIR', xfunit_root_dir )
  manager = xfunit_manager_eclipse( module, xfunit_root_dir, junit_strict )

! Initialise test suite
  suite = xfunit_suite( package=package, &
                        source='xxbase___list_ftl.f03', &
                        before=unit_list_suite_before, &
                        after=unit_list_suite_after, &
                        annotation='Double linked list' )

! Create test
  ut = xfunit_unit( name='unit_list_test_001', &
                    classname="list", &
                    executer=unit_list_test_001, &
                    before=unit_list_before_001, &
                    after=unit_list_after_001, &
                    annotation='Constructors' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_list_test_002', &
                    classname="list", &
                    executer=unit_list_test_002, &
                    before=unit_list_before_002, &
                    after=unit_list_after_002, &
                    annotation='Capacity' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_list_test_003', &
                    classname="list", &
                    executer=unit_list_test_003, &
                    before=unit_list_before_003, &
                    after=unit_list_after_003, &
                    annotation='Element access' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_list_test_004', &
                    classname="list", &
                    executer=unit_list_test_004, &
                    before=unit_list_before_004, &
                    after=unit_list_after_004, &
                    annotation='Iterators' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_list_test_005', &
                    classname="list", &
                    executer=unit_list_test_005, &
                    before=unit_list_before_005, &
                    after=unit_list_after_005, &
                    annotation='Push/Pop' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_list_test_006', &
                    classname="list", &
                    executer=unit_list_test_006, &
                    before=unit_list_before_006, &
                    after=unit_list_after_006, &
                    annotation='Complex modfiers' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_list_test_007', &
                    classname="list", &
                    executer=unit_list_test_007, &
                    before=unit_list_before_007, &
                    after=unit_list_after_007, &
                    annotation='Operations' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_list_test_008', &
                    classname="list", &
                    executer=unit_list_test_008, &
                    before=unit_list_before_008, &
                    after=unit_list_after_008, &
                    annotation='Complete test coverage' )
  call suite%add_unit_test( ut )

! Execute tests
  call manager%execute( suite )
  if( manager%is_error() ) then
    call manager%dump_error( 0 )
  end if

! Generate output
  call manager%write_xml( suite )

! Terminate unit test infrastructure
  deallocate( manager )
  deallocate( suite )
  deallocate( ut )

end program unit_list

