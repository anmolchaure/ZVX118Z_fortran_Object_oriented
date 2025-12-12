module solids3d
  use types
  implicit none
  private
  public :: solid, tetrahedron_xyz


   ! Abstract base type for 3D solids
  type, abstract :: solid
  contains
    procedure(volume_ifc), deferred :: volume
  end type solid



  ! Abstract interface for volume()
  abstract interface
    function volume_ifc(self) result(V)
      import :: solid, rkind
      class(solid), intent(in) :: self
      real(kind=rkind) :: V
    end function volume_ifc
  end interface




  ! Concrete tetrahedron storing four 3D points
  type, extends(solid) :: tetrahedron_xyz
    real(kind=rkind) :: x1, y1, z1
    real(kind=rkind) :: x2, y2, z2
    real(kind=rkind) :: x3, y3, z3
    real(kind=rkind) :: x4, y4, z4
  contains
    procedure :: volume => tetrahedron_xyz_volume
  end type tetrahedron_xyz


contains

  ! Compute determinant of 3x3 matrix whose columns are (a1,a2,a3), (b1,b2,b3), (c1,c2,c3)
  pure function det3(a1,a2,a3, b1,b2,b3, c1,c2,c3) result(d)
    real(kind=rkind), intent(in) :: a1,a2,a3, b1,b2,b3, c1,c2,c3
    real(kind=rkind) :: d
    d = a1*(b2*c3 - b3*c2) - a2*(b1*c3 - b3*c1) + a3*(b1*c2 - b2*c1)
  end function det3


  ! volume = abs(det([x2-x1, x3-x1, x4-x1; ...])) / 6
  function tetrahedron_xyz_volume(self) result(volume_tetra)
    class(tetrahedron_xyz), intent(in) :: self
    real(kind=rkind) :: volume_tetra
    real(kind=rkind) :: a1,a2,a3, b1,b2,b3, c1,c2,c3
    real(kind=rkind) :: d

    ! build column vectors relative to point 1
    a1 = self%x2 - self%x1; a2 = self%y2 - self%y1; a3 = self%z2 - self%z1
    b1 = self%x3 - self%x1; b2 = self%y3 - self%y1; b3 = self%z3 - self%z1
    c1 = self%x4 - self%x1; c2 = self%y4 - self%y1; c3 = self%z4 - self%z1

    d = det3(a1,a2,a3, b1,b2,b3, c1,c2,c3)
    volume_tetra = abs(d) / 6.0_rkind
  end function tetrahedron_xyz_volume

end module solids3d
