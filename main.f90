program main
  use types
  use vector2d_mod
  use shapes2d
  use solids3d
  implicit none

    real :: expected_V, Vcomp
    type(vector2d) :: v1, v2, v3
    class(shape), allocatable :: s_rt, s_rec, s_tri, s_quad
    type(right_angled_triangle) :: rt
    type(rectangle) :: rec
    type(triangle_xy) :: tri
    type(quadrangle_xy) :: quad
    type(tetrahedron_xyz) :: T


    ! == testing vector2d ==
    v1%x = 1.0; v1%y = 2.0
    v2%x = 3.0; v2%y = 4.0
    v3 = v1%summ(v2)
    print *, "v1+v2:", v3%x, v3%y



    ! == testing shapes2d :: area and perimeter ==
    rt%a = 3.0; rt%b = 4.0
    rec%width = 5.0; rec%height = 2.0
    tri%x1 = 0.0; tri%y1 = 0.0
    tri%x2 = 4.0; tri%y2 = 0.0
    tri%x3 = 0.0; tri%y3 = 3.0
    quad%x1 = 0.0; quad%y1 = 0.0
    quad%x2 = 5.0; quad%y2 = 0.0
    quad%x3 = 5.0; quad%y3 = 2.0
    quad%x4 = 0.0; quad%y4 = 2.0

    allocate(s_rt, source=rt); allocate(s_rec, source=rec)
    allocate(s_tri, source=tri); allocate(s_quad, source=quad)

    print *, "area of right angled triangle:", s_rt%area()
    print *, "area of rectangle:", s_rec%area()
    print *, "area of tiangle:", s_tri%area()
    print *, "area of quadrangle :", s_quad%area()

    T%x1 = 0.0; T%y1 = 0.0; T%z1 = 0.0
    T%x2 = 4.0; T%y2 = 0.0; T%z2 = 0.0
    T%x3 = 0.0; T%y3 = 3.0; T%z3 = 0.0
    T%x4 = 0.0; T%y4 = 0.0; T%z4 = 2.0


    ! == testing shapes3d :: volume

    expected_V = (1.0/3.0) * (0.5*4.0*3.0) * 2.0
    Vcomp = T%volume()
    print *, "computed value of tetrahedron =", Vcomp, " expected =", expected_V

end program main
