module m_eop_list

! Use statements
  use m_eop_vector_ftl
  use m_leap_list_ftl


! Container object declarations
  type(t_eop_vector_ftl) :: eop_object
  type(t_leap_list_ftl) :: leap_object

! Contained object declarations wit the intent attribute
  class(t_eop_vector_ftl), pointer, intent(in) :: eop_object
  class(t_leap_list_ftl), allocatable, dimension(:), intent(out) :: leap_object


! Iterator decalrations
  type(t_eop_vector_ftl_iterator) :: eop_object
  type(t_leap_list_ftl_iterator) :: leap_object


! Inheritance of a container
  type, extends(eop_vector_ftl) :: t_eop_list
  type, extends(leap_list_ftl), abstract :: t_leap_list


! Type selection (class or type)
  select type( x )
    type is(t_eop_vector_ftl)

    class is(t_leap_list_ftl)

  end select


! Reference to the parent in a type derived by inheritance
  x = object%t_eop_vector_ftl%get_param()
  y = object%t_leap_list_ftl%get_param()


! Constructor
  x = eop_vector_ftl()
  y = leap_list_ftl( a, b )


end module m_eop_list

! 2022-02-01T00:46:47