submodule (vtk_element) vtk_element_procedures
    implicit none
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! this is the basic file for a serial vtk file
    !!

contains

    module procedure vtk_element_setup
        use file_utility, only : is_little_endian
        implicit none
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! this writes the header for a vtk file
        !!
        !! example:
        !! <VTKFile type=”ImageData” version=”0.1” byte_order=”LittleEndian”>
        character(len=*), parameter   :: name = 'VTKFile'
        character(len=:), allocatable :: string
        character(len=:), allocatable :: type_string
        character(len=*), parameter   :: version_string = ' version="' // def_version // '"'
        character(len=:), allocatable :: byte_order_string
        character(len=:), allocatable :: compression_string

        if (allocated(me%type)) then
            allocate(type_string,source=' type="' // me%type // '"')
        else
            error stop "error. can't create vtk file without a known type. terminated in vtk_element_setup"
        end if
        if (is_little_endian()) then
            allocate(byte_order_string,source=' byte_order="LittleEndian"')
        else
            allocate(byte_order_string,source=' byte_order="BigEndian"')
        end if
        if (allocated(me%compression)) then
            allocate(compression_string,source=' compression="' // me%compression // '"')
        else
            allocate(compression_string,source='')
        end if

        allocate(string, source=type_string // version_string // byte_order_string // compression_string)

        call me%setup(name=name,string=string,offset=4)

    end procedure vtk_element_setup

    module procedure initialize
        implicit none
        !! author: Ian Porter
        !! date: 05/07/2019
        !!
        !! this is an external interface to allow the user or other routines to set the internal variables
        !!

        if (present(type))           allocate(me%type,source=type)
        if (present(compression))    allocate(me%compression,source=compression)
        if (present(file_extension)) allocate(me%file_extension,source=file_extension)

        call me%vtk_element_setup()

    end procedure initialize

    module procedure finalize
        implicit none
        !! author: Ian Porter
        !! date: 05/07/2019
        !!
        !!
        !!
        if (allocated(me%vtk_element)) then
            call me%vtk_element%finalize()
            call me%add(me%vtk_element)
        end if

    end procedure finalize

    module procedure gcc_bug_workaround_deallocate_vtk_element_single
        implicit none
        !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings

        if (allocated(foo%type))           deallocate(foo%type)
        if (allocated(foo%version))        deallocate(foo%version)
        if (allocated(foo%compression))    deallocate(foo%compression)
        if (allocated(foo%file_extension)) deallocate(foo%file_extension)
        if (allocated(foo%filename))       deallocate(foo%filename)
        !call foo%piece%deallocate_piece_dt()
        if (allocated(foo%vtk_element)) then
            call foo%vtk_element%me_deallocate()
            call foo%vtk_element%deallocate()
        end if
        call foo%deallocate()

    end procedure gcc_bug_workaround_deallocate_vtk_element_single

end submodule vtk_element_procedures
