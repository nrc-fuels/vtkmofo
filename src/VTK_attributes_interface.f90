MODULE vtk_attributes
    USE Precision
    USE Misc, ONLY : def_len
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/13/2017
    !!
    !! This module contains the dataset attributes for vtk format
    !!
    !! The following dataset attributes are available:
    !! 1) scalars
    !! 2) vectors
    !! 3) normals
    !! 4) texture coordinates (1D, 2D & 3D)
    !! 5) 3x3 tensors
    !! 6) field data
    !!
    PRIVATE
    PUBLIC :: attribute, attributes, scalar, vector, normal, texture, tensor, field, field_data_array

    CHARACTER(LEN=*), PARAMETER :: default = 'default'     !! Default table name

    TYPE, ABSTRACT :: attribute
        !! Abstract DT of attribute information
        CHARACTER(LEN=:), ALLOCATABLE :: dataname
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
    CONTAINS
        PROCEDURE(abs_read_formatted),   DEFERRED, PRIVATE :: read_formatted
        PROCEDURE(abs_read_unformatted), DEFERRED, PRIVATE :: read_unformatted
        GENERIC, PUBLIC :: READ(FORMATTED)   => read_formatted
        GENERIC, PUBLIC :: READ(UNFORMATTED) => read_unformatted
        PROCEDURE(abs_write_formatted),   DEFERRED, PRIVATE :: write_formatted
        PROCEDURE(abs_write_unformatted), DEFERRED, PRIVATE :: write_unformatted
        GENERIC, PUBLIC :: WRITE(FORMATTED)   => write_formatted
        GENERIC, PUBLIC :: WRITE(UNFORMATTED) => write_unformatted
        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: init => initialize  !! Initialize the attribute
        PROCEDURE, PRIVATE :: check_for_diffs
        GENERIC :: OPERATOR(.diff.) => check_for_diffs
    END TYPE attribute

    TYPE, EXTENDS(attribute) :: scalar
        !! Scalar attribute DT
        INTEGER(i4k) :: numcomp = 0
        CHARACTER(LEN=:), ALLOCATABLE :: tablename
        INTEGER(i4k), DIMENSION(:), ALLOCATABLE :: ints
        REAL(r8k),    DIMENSION(:), ALLOCATABLE :: reals
    CONTAINS
        PROCEDURE :: read_formatted  => scalar_read_formatted
        PROCEDURE :: read_unformatted  => scalar_read_unformatted
        PROCEDURE :: write_formatted => scalar_write_formatted
        PROCEDURE :: write_unformatted => scalar_write_unformatted
        PROCEDURE :: setup => scalar_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_scalar
    END TYPE scalar

    TYPE, EXTENDS(attribute) :: vector
        !! Vector attribute DT
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: vectors
    CONTAINS
        PROCEDURE :: read_formatted  => vector_read_formatted
        PROCEDURE :: read_unformatted => vector_read_unformatted
        PROCEDURE :: write_formatted => vector_write_formatted
        PROCEDURE :: write_unformatted => vector_write_unformatted
        PROCEDURE :: setup => vector_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_vector
    END TYPE vector

    TYPE, EXTENDS(attribute) :: normal
        !! Normal attribute DT
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: normals
    CONTAINS
        PROCEDURE :: read_formatted  => normal_read_formatted
        PROCEDURE :: read_unformatted => normal_read_unformatted
        PROCEDURE :: write_formatted => normal_write_formatted
        PROCEDURE :: write_unformatted => normal_write_unformatted
        PROCEDURE :: setup => normal_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_normal
    END TYPE normal

    TYPE, EXTENDS(attribute) :: texture
        !! Texture attribute DT
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: textures
    CONTAINS
        PROCEDURE :: read_formatted  => texture_read_formatted
        PROCEDURE :: read_unformatted => texture_read_unformatted
        PROCEDURE :: write_formatted => texture_write_formatted
        PROCEDURE :: write_unformatted => texture_write_unformatted
        PROCEDURE :: setup => texture_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_texture
    END TYPE texture

    TYPE :: tensor_array
        !! Tensor data DT
        REAL(r8k), DIMENSION(3,3) :: val = 0.0_r8k
    END TYPE tensor_array

    TYPE, EXTENDS(attribute) :: tensor
        !! Tensor attribute DT
        TYPE(tensor_array), DIMENSION(:), ALLOCATABLE :: tensors
    CONTAINS
        PROCEDURE :: read_formatted  => tensor_read_formatted
        PROCEDURE :: read_unformatted => tensor_read_unformatted
        PROCEDURE :: write_formatted => tensor_write_formatted
        PROCEDURE :: write_unformatted => tensor_write_unformatted
        PROCEDURE :: setup => tensor_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_tensor
    END TYPE tensor

    TYPE :: field_data_array
        !! Field data DT
        CHARACTER(LEN=:), ALLOCATABLE :: name
        INTEGER(i4k) :: numComponents = 0
        INTEGER(i4k) :: numTuples     = 0
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: data
    END TYPE field_data_array

    TYPE, EXTENDS(attribute) :: field
        !! Field attribute DT
        TYPE(field_data_array), DIMENSION(:), ALLOCATABLE :: array
    CONTAINS
        PROCEDURE :: read_formatted  => field_read_formatted
        PROCEDURE :: read_unformatted => field_read_unformatted
        PROCEDURE :: write_formatted => field_write_formatted
        PROCEDURE :: write_unformatted => field_write_unformatted
        PROCEDURE :: setup => field_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_field
    END TYPE field

    TYPE :: attributes
        INTEGER(i4k) :: n = 0                       !! # of points or cells in the dataset
        CLASS(attribute), ALLOCATABLE :: attribute
    END TYPE attributes

    INTERFACE

        MODULE SUBROUTINE abs_read_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !! author: Ian Porter
        !! date: 2/4/2019
        !!
        !! Abstract read
        !!
        CLASS(attribute), INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE abs_read_formatted

        MODULE SUBROUTINE abs_read_unformatted (me, unit, iostat, iomsg)
        !! author: Ian Porter
        !! date: 2/11/2019
        !!
        !! Abstract unformatted read
        !!
        CLASS(attribute), INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE abs_read_unformatted

        MODULE SUBROUTINE abs_write_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !! author: Ian Porter
        !! date: 2/4/2019
        !!
        !! Abstract write
        !!
        CLASS(attribute), INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE abs_write_formatted

        MODULE SUBROUTINE abs_write_unformatted (me, unit, iostat, iomsg)
        !! author: Ian Porter
        !! date: 2/11/2019
        !!
        !! Abstract unformatted write
        !!
        CLASS(attribute), INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE abs_write_unformatted

        MODULE SUBROUTINE initialize (me, dataname, datatype, numcomp, tablename, ints1d, ints2d, ints3d, &
          &                           values1d, values2d, values3d, field_arrays)
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Abstract for performing the set-up of an attribute
        !!
        CLASS(attribute), INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
        INTEGER(i4k), DIMENSION(:),     INTENT(IN), OPTIONAL :: ints1d
        INTEGER(i4k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: ints2d
        INTEGER(i4k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: ints3d
        REAL(r8k),    DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k),    DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k),    DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

        END SUBROUTINE initialize

        MODULE FUNCTION check_for_diffs (me, you) RESULT (diffs)
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Function checks for differences in an attribute
        !!
        CLASS(attribute), INTENT(IN) :: me, you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs
!********
! Scalars
!********
        MODULE SUBROUTINE scalar_read_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !! author: Ian Porter
        !! date: 2/4/2019
        !!
        !! Subroutine performs the read for a scalar attribute
        !!
        CLASS(scalar),    INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE scalar_read_formatted

        MODULE SUBROUTINE scalar_read_unformatted (me, unit, iostat, iomsg)
        !! author: Ian Porter
        !! date: 2/4/2019
        !!
        !! Subroutine performs the unformatted read for a scalar attribute
        !!
        CLASS(scalar),    INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE scalar_read_unformatted

        MODULE SUBROUTINE scalar_write_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !! author: Ian Porter
        !! date: 2/4/2019
        !!
        !! Subroutine performs the write for a scalar attribute
        !!
        CLASS(scalar),    INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE scalar_write_formatted

        MODULE SUBROUTINE scalar_write_unformatted (me, unit, iostat, iomsg)
        !! author: Ian Porter
        !! date: 2/4/2019
        !!
        !! Subroutine performs the unformatted write for a scalar attribute
        !!
        CLASS(scalar),    INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE scalar_write_unformatted

        MODULE SUBROUTINE scalar_setup (me, dataname, datatype, numcomp, tablename, ints1d, values1d)
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the set-up for a scalar attribute
        !!
        CLASS(scalar),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN)  :: numcomp
        CHARACTER(LEN=*), INTENT(IN)  :: datatype
        CHARACTER(LEN=*), INTENT(IN)  :: tablename
        INTEGER(i4k), DIMENSION(:), INTENT(IN), OPTIONAL :: ints1d
        REAL(r8k),    DIMENSION(:), INTENT(IN), OPTIONAL :: values1d

        END SUBROUTINE scalar_setup

        MODULE FUNCTION check_for_diffs_scalar (me, you) RESULT (diffs)
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Function checks for differences in a scalar attribute
        !!
        CLASS(scalar),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs_scalar
!********
! Vectors
!********
        MODULE SUBROUTINE vector_read_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the formatted read for a vector attribute
        !!
        CLASS(vector),    INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE vector_read_formatted

        MODULE SUBROUTINE vector_read_unformatted (me, unit, iostat, iomsg)
        !! author: Ian Porter
        !! date: 3/25/2019
        !!
        !! Subroutine performs the unformatted read for a vector attribute
        !!
        CLASS(vector),    INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE vector_read_unformatted

        MODULE SUBROUTINE vector_write_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the formatted write for a vector attribute
        !!
        CLASS(vector),    INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE vector_write_formatted

        MODULE SUBROUTINE vector_write_unformatted (me, unit, iostat, iomsg)
        !! author: Ian Porter
        !! date: 3/25/2019
        !!
        !! Subroutine performs the unformatted write for a vector attribute
        !!
        CLASS(vector),    INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE vector_write_unformatted

        MODULE SUBROUTINE vector_setup (me, dataname, datatype, values2d)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the set-up for a vector attribute
        !!
        CLASS(vector),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype
        REAL(r8k), DIMENSION(:,:), INTENT(IN)  :: values2d

        END SUBROUTINE vector_setup

        MODULE FUNCTION check_for_diffs_vector (me, you) RESULT (diffs)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a vector attribute
        !!
        CLASS(vector),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs_vector
!********
! Normals
!********
        MODULE SUBROUTINE normal_read_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the formatted read for a normal attribute
        !!
        CLASS(normal),    INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE normal_read_formatted

        MODULE SUBROUTINE normal_read_unformatted (me, unit, iostat, iomsg)
        !! author: Ian Porter
        !! date: 3/25/2019
        !!
        !! Subroutine performs the unformatted read for a normal attribute
        !!
        CLASS(normal),    INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE normal_read_unformatted

        MODULE SUBROUTINE normal_write_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the formatted write for a normal attribute
        !!
        CLASS(normal),    INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE normal_write_formatted

        MODULE SUBROUTINE normal_write_unformatted (me, unit, iostat, iomsg)
        !! author: Ian Porter
        !! date: 3/25/2019
        !!
        !! Subroutine performs the unformatted write for a normal attribute
        !!
        CLASS(normal),    INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE normal_write_unformatted

        MODULE SUBROUTINE normal_setup (me, dataname, datatype, values2d)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the set-up for a normal attribute
        !!
        CLASS(normal),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype
        REAL(r8k), DIMENSION(:,:), INTENT(IN)  :: values2d

        END SUBROUTINE normal_setup

        MODULE FUNCTION check_for_diffs_normal (me, you) RESULT (diffs)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a normal attribute
        !!
        CLASS(normal),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs_normal
!*********
! Textures
!*********
        MODULE SUBROUTINE texture_read_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the formatted read for a texture attribute
        !!
        CLASS(texture),   INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE texture_read_formatted

        MODULE SUBROUTINE texture_read_unformatted (me, unit, iostat, iomsg)
        !! author: Ian Porter
        !! date: 3/25/2019
        !!
        !! Subroutine performs the unformatted read for a texture attribute
        !!
        CLASS(texture),   INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE texture_read_unformatted

        MODULE SUBROUTINE texture_write_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the formatted write for a texture attribute
        !!
        CLASS(texture),   INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE texture_write_formatted

        MODULE SUBROUTINE texture_write_unformatted (me, unit, iostat, iomsg)
        !! author: Ian Porter
        !! date: 3/25/2019
        !!
        !! Subroutine performs the unformatted write for a texture attribute
        !!
        CLASS(texture),   INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE texture_write_unformatted

        MODULE SUBROUTINE texture_setup (me, dataname, datatype, values2d)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the set-up for a texture attribute
        !!
        CLASS(texture),   INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype
        REAL(r8k), DIMENSION(:,:), INTENT(IN)  :: values2d

        END SUBROUTINE texture_setup

        MODULE FUNCTION check_for_diffs_texture (me, you) RESULT (diffs)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a texture attribute
        !!
        CLASS(texture),   INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs_texture
!********
! Tensors
!********
        MODULE SUBROUTINE tensor_read_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the formatted read for a tensor attribute
        !!
        CLASS(tensor),    INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE tensor_read_formatted

        MODULE SUBROUTINE tensor_read_unformatted (me, unit, iostat, iomsg)
        !! author: Ian Porter
        !! date: 3/25/2019
        !!
        !! Subroutine performs the unformatted read for a tensor attribute
        !!
        CLASS(tensor),    INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE tensor_read_unformatted

        MODULE SUBROUTINE tensor_write_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the formatted write for a tensor attribute
        !!
        CLASS(tensor),    INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE tensor_write_formatted

        MODULE SUBROUTINE tensor_write_unformatted (me, unit, iostat, iomsg)
        !! author: Ian Porter
        !! date: 3/25/2019
        !!
        !! Subroutine performs the unformatted write for a tensor attribute
        !!
        CLASS(tensor),    INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE tensor_write_unformatted

        MODULE SUBROUTINE tensor_setup (me, dataname, datatype, values3d)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the set-up for a tensor attribute
        !!
        CLASS(tensor),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL  :: datatype
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN) :: values3d

        END SUBROUTINE tensor_setup

        MODULE FUNCTION check_for_diffs_tensor (me, you) RESULT (diffs)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a tensor attribute
        !!
        CLASS(tensor),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs_tensor
!********
! Fields
!********
        MODULE SUBROUTINE field_read_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the formatted read for a field attribute
        !!
        CLASS(field),     INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE field_read_formatted

        MODULE SUBROUTINE field_read_unformatted (me, unit, iostat, iomsg)
        !! author: Ian Porter
        !! date: 3/25/2019
        !!
        !! Subroutine performs the unformatted read for a field attribute
        !!
        CLASS(field),     INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE field_read_unformatted

        MODULE SUBROUTINE field_write_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the formatted write for a field attribute
        !!
        CLASS(field),     INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE field_write_formatted

        MODULE SUBROUTINE field_write_unformatted (me, unit, iostat, iomsg)
        !! author: Ian Porter
        !! date: 3/25/2019
        !!
        !! Subroutine performs the unformatted write for a field attribute
        !!
        CLASS(field),     INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg

        END SUBROUTINE field_write_unformatted

        MODULE SUBROUTINE field_setup (me, dataname, datatype, field_arrays)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the set-up for a field attribute
        !!
        CLASS(field),     INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype
        TYPE(field_data_array), DIMENSION(:), INTENT(IN) :: field_arrays

        END SUBROUTINE field_setup

        MODULE FUNCTION check_for_diffs_field (me, you) RESULT (diffs)
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a field attribute
        !!
        CLASS(field),     INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs_field

    END INTERFACE

END MODULE vtk_attributes
