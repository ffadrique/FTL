module m_object

! -----------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Supports all classes in the framework class hierarchy and provides
!             low-level services to derived classes.
!             This is the ultimate base class of all classes in the Framework;
!             it is the root of the type hierarchy.
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
! -----------------------------------------------------------------------------

! Dependencies ----------------------------------------------------------------

  implicit none

! Public/Private declarations -------------------------------------------------

  private
  public t_object, object

! Module declarations ---------------------------------------------------------

! Supports all classes in the framework class hierarchy and provides
! low-level services to derived classes.
! This is the ultimate base class of all classes in the Framework;
! it is the root of the type hierarchy.
  type t_object
    private

    contains

!     Determines whether the specified object is equal to the current object
      procedure :: equals => object_equals

!     Default hash function
      procedure :: get_hash_code => object_get_hash_code

!     Default has algorithm
      procedure, private, nopass :: hash => object_hash_adler32

  end type t_object

! Constructor interface
  interface object
    module procedure object_default
  end interface object

! Implementation --------------------------------------------------------------

contains

! Default constructor
elemental function object_default() result(res)

! Returned object
  type(t_object) :: res

! Set return object
  res = t_object()

end function object_default


! Determines whether the specified object is equal to the current object
elemental function object_equals( this, other ) result(res)

! Calling object
  class(t_object), intent(in) :: this

! Other object
  class(t_object), intent(in) :: other

! Comparison result
  logical :: res

! Compute comparison
  res = ( this%get_hash_code() == other%get_hash_code() )

end function object_equals


! Default hash function
elemental function object_get_hash_code( this ) result(res)

! Calling object
  class(t_object), intent(in) :: this

! String representation of the object
  integer :: res

! Local variables
  integer :: osize
  integer(kind=1), dimension(:), allocatable :: object

! Get the size of the input object
  osize = 8 * storage_size(this)

! Allocate object
  allocate( object(osize), source=0_1 )

! Get the object data as an array
  object = transfer( this, mold=object )

! Compute the hash
  res = this%hash(object)

end function object_get_hash_code


! Hash algorithm
! Reference: https://stackoverflow.com/questions/14409466/simple-hash-functions
!            https://en.wikipedia.org/wiki/Adler-32
pure function object_hash_adler32( object ) result(res)

! Array of bytes representing the object
  integer(kind=1), dimension(:), intent(in) :: object

! Hash value
  integer :: res

! Local variables
  integer :: i
  integer :: s1, s2

! Initialise algorithm
  s1 = 1
  s2 = 0

! Loop on the bytes
  do i = 1, size(object)
    s1 = mod( s1 + object(i), 65521 )
    s2 = mod( s2 + s1, 65521 )
  end do

! Compute the resilt
  res = ior( ishft( s2, 16 ), s1 )

end function object_hash_adler32

end module m_object
