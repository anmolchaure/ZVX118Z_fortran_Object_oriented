module shapes2d
  use types
  implicit none


  ! Abstract base type
  type, abstract :: shape
  contains
    procedure(area_ifc), deferred :: area
    procedure(perimeter_ifc), deferred :: perimeter
  end type shape




  abstract interface
      function area_ifc(self) result(val)
        import shape, rkind
        class(shape), intent(in) :: self
        real(kind=rkind):: val
      end function area_ifc

      function perimeter_ifc(self) result(val)
        import shape, rkind
        class(shape), intent(in) :: self
        real(kind=rkind) :: val
      end function perimeter_ifc
    end interface


  ! === right angled triangle ===
  type, extends(shape) :: right_angled_triangle
    real(kind=rkind):: a,b ! legs
  contains
    procedure :: area => right_angled_triangle_area
    procedure :: perimeter => right_angled_triangle_perimeter
  end type right_angled_triangle

   ! ===== for rectangle ====

  type, extends(shape) :: rectangle
    real(kind=rkind):: width, height
  contains
    procedure :: area => rectangle_area
    procedure :: perimeter => rectangle_perimeter
  end type rectangle



  ! == triangle defined by three 2D points ===


  type, extends(shape) :: triangle_xy
    real(kind=rkind):: x1, y1
    real(kind=rkind):: x2, y2
    real(kind=rkind):: x3, y3
  contains
    procedure :: area      => triangle_xy_area
    procedure :: perimeter => triangle_xy_perimeter
  end type triangle_xy

  ! quadrangle by cordinate (split into 2 triangle)

  type, extends(shape) :: quadrangle_xy
    real(kind=rkind):: x1, y1
    real(kind=rkind):: x2, y2
    real(kind=rkind):: x3, y3
    real(kind=rkind):: x4, y4
  contains
    procedure :: area      => quadrangle_xy_area
    procedure :: perimeter => quadrangle_xy_perimeter
  end type quadrangle_xy




contains
  ! computing area = (1/2) * a * b
  function right_angled_triangle_area(self) result(area)
    class(right_angled_triangle), intent(in) :: self
    real(kind=rkind):: area
    area = 0.5 * self%a * self%b
  end function right_angled_triangle_area


  ! perimeter = a + b + sqrt(a^2 + b^2)
  function right_angled_triangle_perimeter(self) result(peri)
    class(right_angled_triangle), intent(in) :: self
    real(kind=rkind):: peri
    peri = self%a + self%b + sqrt(self%a **2 + self%b **2)
  end function right_angled_triangle_perimeter



! == rectangle ==
  ! for area = width * height

  function rectangle_area(self) result(rec_area)
    class(rectangle), intent(in) :: self
    real(kind=rkind):: rec_area
    rec_area = self%width * self%height
  end function rectangle_area


  ! for perimeter = 2 (width + height)
  function rectangle_perimeter(self) result(rec_peri)
    class(rectangle), intent(in) :: self
    real(kind=rkind):: rec_peri
    rec_peri = 2.0 * (self%width + self%height)
  end function rectangle_perimeter

! cordinate triangle area
  function triangle_xy_area(self) result(t_area)
    class(triangle_xy), intent(in) :: self
    real(kind=rkind):: t_area

    t_area = 0.5 * abs(  self%x1*(self%y2 - self%y3)  &
                        + self%x2*(self%y3 - self%y1)  &
                        + self%x3*(self%y1 - self%y2) )
  end function triangle_xy_area

! cordinate triangle perimeter
  function triangle_xy_perimeter(self) result(t_peri)
    class(triangle_xy), intent(in) :: self
    real(kind=rkind):: t_peri
    real(kind=rkind):: d12, d23, d31

    d12 = sqrt( (self%x2 - self%x1)**2 + (self%y2 - self%y1)**2 )
    d23 = sqrt( (self%x3 - self%x2)**2 + (self%y3 - self%y2)**2 )
    d31 = sqrt( (self%x1 - self%x3)**2 + (self%y1 - self%y3)**2 )

    t_peri = d12 + d23 + d31
  end function triangle_xy_perimeter



  ! quadrangle area = area(1,2,3) + area(1,3,4)
 ! quadrangle area = area(1,2,3) + area(1,3,4)  (inline, split into two triangles)
  function quadrangle_xy_area(self) result(area_quad)
      class(quadrangle_xy), intent(in) :: self
      real(kind=rkind):: area_quad
      real(kind=rkind):: a1, a2

      a1 = 0.5 * abs( self%x1*(self%y2 - self%y3)  &
                          + self%x2*(self%y3 - self%y1)  &
                          + self%x3*(self%y1 - self%y2) )

      a2 = 0.5 * abs( self%x1*(self%y3 - self%y4)  &
                          + self%x3*(self%y4 - self%y1)  &
                          + self%x4*(self%y1 - self%y3) )

      area_quad = a1 + a2
  end function quadrangle_xy_area



  ! quadrangle perimeter = sum of edges (1-2, 2-3, 3-4, 4-1)
  function quadrangle_xy_perimeter(self) result(peri_quad)
    class(quadrangle_xy), intent(in) :: self
    real(kind=rkind):: peri_quad
    real(kind=rkind):: d12, d23, d34, d41
    d12 = sqrt( (self%x2 - self%x1)**2 + (self%y2 - self%y1)**2 )
    d23 = sqrt( (self%x3 - self%x2)**2 + (self%y3 - self%y2)**2 )
    d34 = sqrt( (self%x4 - self%x3)**2 + (self%y4 - self%y3)**2 )
    d41 = sqrt( (self%x1 - self%x4)**2 + (self%y1 - self%y4)**2 )
    peri_quad = d12 + d23 + d34 + d41
  end function quadrangle_xy_perimeter






end module shapes2d
