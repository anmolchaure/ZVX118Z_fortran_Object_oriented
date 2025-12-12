module vector2d_mod
  implicit none
  integer, parameter :: rkind = kind(1.0d0)

  type :: vector2d
    real(kind=rkind) :: x = 0.0_rkind
    real(kind=rkind) :: y = 0.0_rkind
  contains
    procedure :: norm => vector2d_norm
    procedure :: summ => vector2d_sum
  end type vector2d

contains

  function vector2d_norm(self) result(val)
    class(vector2d), intent(in) :: self
    real(kind=rkind) :: val
    val = sqrt(self%x*self%x + self%y*self%y)
  end function vector2d_norm

  function vector2d_sum(self, other) result(res)
    class(vector2d), intent(in) :: self
    class(vector2d), intent(in) :: other
    type(vector2d) :: res
    res%x = self%x + other%x
    res%y = self%y + other%y
  end function vector2d_sum

end module vector2d_mod
