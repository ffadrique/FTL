module xxuse__

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Reference : http://www.cplusplus.com/reference/list
! Synopsis  : Conveniene module to allow compilation and unit testing of the
!             Fortran Template Library containers.
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

  implicit none

  private

  public xxtypebase__, cxxtypebase__

  type xxtypebase__

    private

      integer :: i = 0

      real(kind=8) :: r = 0.0_8

      character(len=32) :: c = ' '

    contains

      generic :: assignment(=) => assign
      procedure :: assign

      generic :: operator(==) => equal
      procedure :: equal

      generic :: operator(<) => less
      procedure :: less

      generic :: operator(>) => greater
      procedure :: greater

      procedure :: get_i

      procedure :: get_c

  end type xxtypebase__

  interface cxxtypebase__
    module procedure xxtypebase_default
  end interface cxxtypebase__

contains

elemental function xxtypebase_default( i, r, c ) result(res)

  integer, intent(in) :: i

  real(kind=8), intent(in) :: r

  character(len=*), intent(in) :: c

  type(xxtypebase__) :: res

  res%i = i
  res%r = r
  res%c = c

end function xxtypebase_default

function equal( left, right ) result(res)

  class(xxtypebase__), intent(in) :: left

  class(xxtypebase__), intent(in) :: right

  logical :: res

  res = ( left%i == right%i )

end function equal


pure subroutine assign( left, right )

  class(xxtypebase__), intent(inout) :: left

  class(xxtypebase__), intent(in) :: right

  left%i = right%i
  left%r = right%r
  left%c = right%c

end subroutine assign


function less( left, right ) result(res)

  class(xxtypebase__), intent(in) :: left

  class(xxtypebase__), intent(in) :: right

  logical :: res

  res = ( left%i < right%i )

end function less


function greater( left, right ) result(res)

  class(xxtypebase__), intent(in) :: left

  class(xxtypebase__), intent(in) :: right

  logical :: res

  res = ( left%i > right%i )

end function greater


elemental function get_i( this ) result(res)

  class(xxtypebase__), intent(in) :: this

  integer :: res

  res = this%i

end function get_i


elemental function get_c( this ) result(res)

  class(xxtypebase__), intent(in) :: this

  character(len=len(this%c)) :: res

  res = this%c

end function get_c


end module xxuse__