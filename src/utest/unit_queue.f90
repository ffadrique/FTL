program unit_xxbase___queue_ftl

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests driver for xxbase___queue_ftl
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

  use unit_queue_tests

!---End of use statements-------------------------------------------------------

  implicit none

  character(len=*), parameter :: package = 'fxx'
  character(len=*), parameter :: module = 'xxbase___queue_ftl'

  character(len=130), parameter :: sccs_info = &
  '\$Id: \$'

!---Declaration of local variables----------------------------------------------


!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

! Local variables
  character(len=256) :: xfunit_root_dir
  logical :: junit_strict
  type(t_xfunit_unit), allocatable :: ut

! Initialise report generation flag
  junit_strict = .false.

! Initialise the unit test manager
  call get_environment_variable( 'XFUNIT_ROOT_DIR', xfunit_root_dir )
  allocate( manager, source=xfunit_manager_eclipse( module, xfunit_root_dir, junit_strict ) )

! Initialise test suite
  allocate( suite )
  allocate( ut )
  suite = xfunit_suite( package=package, &
                        source='xxbase___queue_ftl.f90', &
                        before=unit_queue_suite_before, &
                        after=unit_queue_suite_after, &
                        annotation='Queue container' )

! Create test
  ut = xfunit_unit( name='unit_queue_test_001', &
                    classname='queue', &
                    executer=unit_queue_test_001, &
                    before=unit_queue_before_001, &
                    after=unit_queue_after_001, &
                    annotation='Constructor' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_queue_test_002', &
                    classname='queue', &
                    executer=unit_queue_test_002, &
                    before=unit_queue_before_002, &
                    after=unit_queue_after_002, &
                    annotation='Queue handling' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_queue_test_003', &
                    classname='queue', &
                    executer=unit_queue_test_003, &
                    before=unit_queue_before_003, &
                    after=unit_queue_after_003, &
                    annotation='Queue assignment' )
  call suite%add_unit_test( ut )

! Execute tests
  call manager%execute( suite )
  if( manager%is_error() ) then
    call manager%dump_error( 0 )
  end if

! Generate output
  call manager%write_xml( suite )

! Terminate unit testing
  deallocate( manager )
  deallocate( suite )
  deallocate( ut )

end program unit_xxbase___queue_ftl
