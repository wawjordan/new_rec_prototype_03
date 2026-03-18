module set_precision
  use iso_fortran_env, only : real64, int32, int64
  implicit none
  private
  public :: dp, i4, i8
  integer, parameter :: dp  = real64
  integer, parameter :: i4  = int32
  integer, parameter :: i8  = int64
end module set_precision

module set_constants
  use set_precision, only : dp
  implicit none
  private
  public :: zero, one, two, three, four, ten
  public :: half, third, fourth
  public :: pi, large, near_zero
  public :: max_text_line_length
  real(dp), parameter :: zero      = 0.0_dp
  real(dp), parameter :: one       = 1.0_dp
  real(dp), parameter :: two       = 2.0_dp
  real(dp), parameter :: three     = 3.0_dp
  real(dp), parameter :: four      = 4.0_dp
  real(dp), parameter :: ten       = 10.0_dp
  real(dp), parameter :: third     = one / three
  real(dp), parameter :: fourth    = 0.25_dp
  real(dp), parameter :: half      = 0.50_dp
  real(dp), parameter :: large  = huge(one)
  real(dp), parameter :: pi     = acos(-one)
  real(dp), parameter :: near_zero = epsilon(one)
  integer,  parameter :: max_text_line_length = 1024
end module set_constants

module project_inputs
  use set_constants, only : zero, one, max_text_line_length
  use set_precision, only : dp
  implicit none
  private
  public :: allocate_inputs, deallocate_inputs
  public :: job_name, verbose_level
  public :: n_dim, rec_degree, n_rec_vars, n_nodes, n_ghost, n_skip
  public :: column_scaling, local_scaling, use_tci
  public :: use_cweno, epsilon_cweno, r_cweno, lambda_0_cweno, use_cwenoz
  public :: geom_space_r, grid_perturb
  public :: out_quad_order, out_derivatives
  ! public :: space_coefs, time_coefs
  public :: space_scale, space_origin
  public :: time_scale, time_origin
  public :: rand_coefs, rand_seed, test_function

  character(max_text_line_length) :: job_name = 'test'
  integer :: verbose_level = 0
  integer :: n_dim         = 1
  integer :: rec_degree    = 2
  integer :: n_rec_vars    = 1
  integer, dimension(3) :: n_nodes       = [9,1,1]
  integer, dimension(3) :: n_ghost       = [0,0,0]
  integer, dimension(3) :: n_skip        = [1,1,1]
  logical  :: column_scaling   = .true.
  logical  :: local_scaling    = .true.
  logical  :: use_tci          = .true.
  logical  :: use_cweno        = .false.
  logical  :: use_cwenoz       = .false.
  real(dp) :: epsilon_cweno    = 1.0e-14_dp
  real(dp) :: lambda_0_cweno   = 1.0e5_dp
  integer  :: r_cweno          = 4
  real(dp) :: geom_space_r = 1.1_dp
  real(dp) :: grid_perturb = zero
  integer  :: out_quad_order=1
  logical  :: out_derivatives=.false.

  logical :: rand_coefs    = .false.
  integer :: rand_seed     = 1
  integer :: test_function=1
  ! real(dp), dimension(:,:), allocatable :: space_coefs, time_coefs
  real(dp), dimension(:,:), allocatable :: space_scale, space_origin
  real(dp), dimension(:),   allocatable :: time_scale, time_origin

contains
  subroutine allocate_inputs()
    allocate(space_scale(n_dim,n_rec_vars))
    allocate(space_origin(n_dim,n_rec_vars))
    allocate(time_scale(n_rec_vars))
    allocate(time_origin(n_rec_vars))
  end subroutine allocate_inputs

  subroutine deallocate_inputs()
    if ( allocated(space_scale)  ) deallocate(space_scale )
    if ( allocated(space_origin) ) deallocate(space_origin)
    if ( allocated(time_scale)   ) deallocate(time_scale  )
    if ( allocated(time_origin)  ) deallocate(time_origin )
  end subroutine deallocate_inputs

end module project_inputs

module string_stuff
  implicit none
  private
  public :: generate_newline_string
  public :: progress_line, iteration_line
  public :: write_integer_tuple
contains
  subroutine write_integer_tuple(integer_list,out_string,plus,delim)
    integer, dimension(:),  intent(in)  :: integer_list
    character(*),           intent(out) :: out_string
    logical,      optional, intent(in)  :: plus
    character(*), optional, intent(in)  :: delim
    integer :: j, sz
    character(1) :: p
    p = ' '
    if ( present(plus) ) then
      if ( plus ) p = '+'
    end if
    sz = size(integer_list)
    out_string=''
    if (sz<1) return
    if ( integer_list(1)>0 ) then
      write(out_string,'(A,I0)') trim(out_string)//p,integer_list(1)
    elseif ( integer_list(1)==0 ) then
      write(out_string,'(A,I0)') trim(out_string)//' ',integer_list(1)
    else
      write(out_string,'(A,I0)') trim(out_string),integer_list(1)
    end if
    if ( present(delim) ) then
      do j = 2,sz
        if ( integer_list(j)>0 ) then
          write(out_string,'(A,I0)') trim(out_string)//delim//p,integer_list(j)
        elseif ( integer_list(j)==0 ) then
          write(out_string,'(A,I0)') trim(out_string)//delim//' ',integer_list(j)
        else
          write(out_string,'(A,I0)') trim(out_string)//delim,integer_list(j)
        end if
      end do
    else
      do j = 2,sz
        if ( integer_list(j)>0 ) then
          write(out_string,'(A,I0)') trim(out_string)//', '//p,integer_list(j)
        elseif ( integer_list(j)==0 ) then
          write(out_string,'(A,I0)') trim(out_string)//',  ',integer_list(j)
        else
          write(out_string,'(A,I0)') trim(out_string)//', ',integer_list(j)
        end if
      end do
    end if
  end subroutine write_integer_tuple
  
  subroutine generate_newline_string(strings,out_fmt)
    character(*), dimension(:), intent(in)  :: strings
    character(*)              , intent(out) :: out_fmt
    integer :: j, sz
    sz = size(strings)
    out_fmt = '('//trim(strings(1))
    do j=2,sz
        out_fmt=trim(out_fmt)//',/,'//trim(strings(j))
    end do
    out_fmt=trim(out_fmt)//')'
  end subroutine generate_newline_string

  subroutine update_current_line(string)
    use iso_fortran_env, only : std_out => output_unit
    character(*), intent(in) :: string
    write(std_out,'(A)',advance='no') string
    flush(std_out)
  end subroutine update_current_line

  subroutine progress_line(string,n,n_total)
    use set_precision, only : dp
    character(*), intent(in) :: string
    integer,      intent(in) :: n, n_total
    character(*), parameter :: fmt = '(A,I0,A,I0,A,F5.1,A)'
    character(*), parameter :: carriage_return = achar(13)
    character(len=100) :: out_string
    write(out_string,fmt) string, n,'/',n_total, ' (', real(n,dp)/real(n_total,dp)*100.0_dp, '%)'
    call update_current_line(carriage_return//trim(out_string))
  end subroutine progress_line

  subroutine iteration_line(string,n,residual)
    use set_precision, only : dp
    character(*), intent(in) :: string
    integer,      intent(in) :: n
    real(dp), dimension(:), intent(in) :: residual
    character(*), parameter :: fmt1 = '("(A,I0,",I0,"("" "",ES18.12))")'
    character(*), parameter :: carriage_return = achar(13)
    character(len=100) :: fmt, out_string
    write(fmt,fmt1) size(residual)
    write(out_string,fmt) string, n, residual
    call update_current_line(carriage_return//trim(out_string))
  end subroutine iteration_line
end module string_stuff

module message
  use ISO_FORTRAN_ENV, only : error_unit
  implicit none
  private
  public :: error_message, warning_message
  public :: WARN_ALWAYS, WARN_SOMETIMES, WARN_RARELY
  integer, parameter :: WARN_ALWAYS    = 0
  integer, parameter :: WARN_SOMETIMES = 1
  integer, parameter :: WARN_RARELY    = 2
contains
  function error_message( routine_name, message ) result( err )
    character(*), intent(in) :: routine_name
    character(*), intent(in) :: message
    logical                  :: err
    err = .true.
    write(error_unit,*)
    write(error_unit,*) ' ERROR: In ' // trim(routine_name)
    write(error_unit,*) '   ', trim(message)
    write(error_unit,*) ' Stopping ...'
    call abort
    stop
  end function error_message

  function warning_message( warn_level, routine_name, message ) result( warn )
    use project_inputs,  only : verbose_level
    integer,      intent(in) :: warn_level
    character(*), intent(in) :: routine_name
    character(*), intent(in) :: message
    logical                  :: warn
    warn = .true. ! Setup
    if ( warn_level <= verbose_level ) then ! Print Warning Message
      write(error_unit,*)
      write(error_unit,*) ' WARNING: In ' // trim(routine_name)
      write(error_unit,*) '   ', trim(message)
    end if
  end function warning_message

end module message

module file_routines
  implicit none
  private
  public :: open_existing_file
  public :: merge_files
contains
  function open_existing_file( filename, append ) result(fid)
    use project_inputs, only : verbose_level
    use message,        only : warning_message, error_message, WARN_RARELY
    character(*),      intent(in) :: filename
    logical,           intent(in) :: append
    logical :: exists, err
    integer :: fid
    character(*), parameter :: routine_name = 'open_existing_file'

    inquire( file=trim(filename), exist=exists )
    if ( .not. exists ) then
      err = error_message( routine_name, ' Attempting to open '//trim(filename)//'... Failed. File does not exist. Stopping.' )
    else
      if( append ) then
        open( newunit=fid, file=trim(filename), status='old', position='append' )
      else
        open( newunit=fid, file=trim(filename), status='old' )
      end if
      err = warning_message( WARN_RARELY, routine_name, ' Attempting to open '//trim(filename)//'... Success!' )
    end if
  end function open_existing_file

  subroutine merge_files( pattern, file_name )
    character(*),               intent(in) :: pattern
    character(*),               intent(in) :: file_name
    integer :: N_files, i, fid, fid_tmp
    
    ! concatenate files
    call system('cat '//trim(pattern)//' >> '//trim(file_name))

    ! archive in case you fucked something up
    call system('tar -cf '//trim(file_name)//'.backup.tar '//trim(pattern))

    ! remove tmp files
    call system('rm '//trim(pattern))
  end subroutine merge_files
end module file_routines

module namelist_helper
  use set_constants,  only : MAX_TEXT_LINE_LENGTH
  implicit none
  private
  public :: nml_warnings, check_for_input_error
contains
  subroutine nml_warnings( nml_unit, err, name, quiet, err_tot, required )
    integer,           intent(in)    :: nml_unit
    integer,           intent(in)    :: err
    character(*),      intent(in)    :: name
    logical,           intent(in)    :: quiet
    integer,           intent(inout) :: err_tot
    logical, optional, intent(in)    :: required
    logical :: flag
    character(MAX_TEXT_LINE_LENGTH) :: line
    flag = .false.
    if (present(required)) flag = required

    if ( err < 0 ) then
      if (flag) then
          write(*,'(A)') "NML "//trim(name)//" not found"
          write(*,'(A)') "This is a required namelist!"
          err_tot = err_tot+1
      else
        if ( .not. quiet ) then
          write(*,'(A)') "NML "//trim(name)//" not found, using defaults."
        end if
      end if
      
    else if ( err > 0 ) then
      write(*,'(A)') "WARNING: Error in NML "//trim(name)//" inputs!"
      backspace(nml_unit)
      backspace(nml_unit)
      read(nml_unit,fmt='(A)') line
      write(*,'(A)') "    Invalid line in namelist: "//trim(line)
      write(*,*)
      err_tot = err_tot + 1
    end if
  end subroutine nml_warnings

  subroutine check_for_input_error( var, var_default, condition, option_error, &
                                     namelist_name, message )
    use set_precision, only : dp
    real(dp),     intent(inout) :: var
    real(dp),     intent(in)    :: var_default
    logical,      intent(in)    :: condition
    integer,      intent(inout) :: option_error
    character(*), intent(in)    :: namelist_name
    character(*), intent(in)    :: message
    if ( condition ) then
      write(*,*) ""
      write(*,'(A)') "Error in NML "//trim(namelist_name)//":"
      write(*,'(A)') "  "//trim(message)
      option_error = option_error+1
      ! set to default to avoid tripping other unrelated checks
      var = var_default
    end if
  end subroutine check_for_input_error
end module namelist_helper

module nml_project
  use namelist_helper, only : nml_warnings
  use project_inputs, only : job_name, verbose_level, n_dim, rec_degree,       &
                             n_rec_vars, n_nodes, n_ghost, n_skip,             &
                             column_scaling, local_scaling, use_tci,           &
                             use_cweno, epsilon_cweno, r_cweno, lambda_0_cweno,&
                             use_cwenoz, grid_perturb, geom_space_r,           &
                             out_quad_order, out_derivatives
  implicit none
  private
  public :: read_nml_project
  public :: write_nml_project
  namelist /project/ job_name, verbose_level, n_dim, n_rec_vars, rec_degree,   &
                     n_nodes, n_ghost, n_skip, column_scaling, local_scaling,  &
                     use_tci,                                                  &
                     use_cweno, epsilon_cweno, r_cweno, lambda_0_cweno,        &
                     use_cwenoz, grid_perturb, geom_space_r,                   &
                     out_quad_order, out_derivatives
contains
  subroutine read_nml_project( nml_unit, quiet, err_tot, option_error )
    use project_inputs, only : allocate_inputs
    use set_precision,  only : dp
    use set_constants,  only : zero, one
    integer, intent(in)    :: nml_unit
    logical, intent(in)    :: quiet
    integer, intent(inout) :: err_tot
    integer, intent(inout) :: option_error
    integer :: ierr
    ! PROJECT Namelist
    job_name = 'test'
    verbose_level = 0
    n_dim         = 1
    rec_degree    = 2
    n_rec_vars    = 1
    n_nodes       = [9,1,1]
    n_ghost       = [0,0,0]
    n_skip        = [1,1,1]
    column_scaling   = .true.
    local_scaling    = .true.
    use_tci          = .true.
    use_cweno        = .false.
    use_cwenoz       = .false.
    epsilon_cweno    = 1.0e-14_dp
    r_cweno          = 4
    lambda_0_cweno   = 1.0e5_dp
    grid_perturb  = zero
    geom_space_r  = one
    out_quad_order=1
    out_derivatives=.false.
    rewind( nml_unit )
    read( nml_unit, nml = project, iostat = ierr )
    call nml_warnings( nml_unit, ierr, 'PROJECT', quiet, err_tot )
    call check_nml_project( option_error )
    call allocate_inputs()
  end subroutine read_nml_project

  subroutine write_nml_project( nml_unit )
    integer, intent(in) :: nml_unit
    integer :: ierr
    write( nml_unit, nml = project, iostat = ierr )
  end subroutine write_nml_project
  subroutine check_nml_project( option_error )
    integer, intent(inout) :: option_error
    continue
  end subroutine check_nml_project
end module nml_project

module nml_exact
  use namelist_helper, only : nml_warnings
  use project_inputs, only : n_dim, n_rec_vars
  use project_inputs, only : rand_coefs, rand_seed, space_scale, space_origin, &
                             time_scale, time_origin, test_function
  implicit none
  private
  public :: read_nml_exact
  public :: write_nml_exact
  namelist /exact/ rand_coefs, rand_seed, space_scale, space_origin,           &
                   time_scale, time_origin, test_function
contains
  subroutine read_nml_exact( nml_unit, quiet, err_tot, option_error )
    use project_inputs, only : allocate_inputs
    use set_constants,  only : zero, one
    integer, intent(in)    :: nml_unit
    logical, intent(in)    :: quiet
    integer, intent(inout) :: err_tot
    integer, intent(inout) :: option_error
    integer :: ierr
    ! EXACT Namelist
    rand_coefs    = .false.
    rand_seed     = 1
    space_scale   = one
    space_origin  = zero
    time_scale    = one
    time_origin   = zero
    rewind( nml_unit )
    read( nml_unit, nml = exact, iostat = ierr )
    call nml_warnings( nml_unit, ierr, 'EXACT', quiet, err_tot )
    call check_nml_exact( option_error )
  end subroutine read_nml_exact
  subroutine write_nml_exact( nml_unit )
    integer, intent(in) :: nml_unit
    integer :: ierr
    write( nml_unit, nml = exact, iostat = ierr )
  end subroutine write_nml_exact
  subroutine check_nml_exact( option_error )
    integer, intent(inout) :: option_error
    continue
  end subroutine check_nml_exact
end module nml_exact

module namelist
  use namelist_helper, only : nml_warnings
  implicit none
  private
  public :: read_nml, write_nml
contains
  subroutine read_nml( )
    use set_precision,        only : dp
    use project_inputs,       only : verbose_level
    use message,              only : WARN_SOMETIMES
    use file_routines,        only : open_existing_file
    use nml_project,          only : read_nml_project
    use nml_exact,            only : read_nml_exact
    logical :: quiet
    integer :: nml_unit, err_tot, option_error

    quiet = WARN_SOMETIMES > verbose_level
    err_tot = 0
    option_error = 0
    nml_unit = open_existing_file( 'project.nml', append=.false. )
    if ( .not. quiet ) then
      write(*,*) 'Reading project.nml...'
    end if
    quiet = .false.
    call read_nml_project( nml_unit, quiet, err_tot, option_error )
    quiet = WARN_SOMETIMES > verbose_level
    call read_nml_exact( nml_unit, quiet, err_tot, option_error )
    close(nml_unit)

    if ( err_tot /= 0 .or. option_error /= 0 ) then
      write(*,*)
      write(*,*) "STOPPING: Errors in project.nml!"
      write(*,*) ""
      write(*,'(a,i10)') 'Total # of Errors found: ',(err_tot + option_error)
      write(*,*) ""
      write(*,*) "Review the above WARNINGS and fix before continuing!"
      write(*,*) "See project.nml.updated for an up-to-date version."
      write(*,*) ""
      call write_nml('updated')
      stop
    end if
  end subroutine read_nml

  subroutine write_nml(str)
    use nml_project,          only : write_nml_project
    character(*), intent(in), optional :: str
    integer       :: nml_unit, err_tot
    character(16) :: today_str
    character(12), dimension(3) :: now
    call date_and_time( now(1), now(2), now(3) )
    today_str = now(1)(1:4)//'-'//now(1)(5:6)//'-'//now(1)(7:8)//'_'//         &
                now(2)(1:2)//'.'//now(2)(3:4)
    err_tot = 0
    if (present(str)) then
      open(newunit=nml_unit, file='project.nml.'//trim(str), status='unknown')
    else
      open(newunit=nml_unit, file='project.nml.'//trim(today_str),              &
        status='unknown')
    end if
    call write_nml_project(          nml_unit )
    close(nml_unit)
  end subroutine write_nml
end module namelist

module timer_derived_type

  use set_precision, only : dp
  use set_constants, only : zero
  implicit none
  private
  public :: basic_timer_t

  type :: basic_timer_t
    private
    real(dp)         :: time_start   = zero
    real(dp), public :: time_elapsed = zero
  contains
    private
    procedure, public, pass :: tic => timer_tick
    procedure, public, pass :: toc => timer_tock
  end type basic_timer_t

contains

  function get_time()
    integer(kind=8) :: ticks, ticks_per_sec, max_ticks
    real(dp) :: get_time
    call system_clock( count      = ticks,                                     &
                      count_rate = ticks_per_sec,                              &
                      count_max  = max_ticks )
    if ( ticks_per_sec == 0 ) then
      get_time = zero
    else
      get_time = real(ticks,dp) / real(ticks_per_sec,dp)
    end if
  end function get_time

  subroutine timer_tick( this )
    class(basic_timer_t), intent(inout) :: this
    this%time_elapsed = zero
    this%time_start   = get_time()
  end subroutine timer_tick

  function timer_tock( this )
    class(basic_timer_t), intent(in) :: this
    real(dp)                         :: timer_tock
    timer_tock = get_time() - this%time_start
  end function timer_tock

end module timer_derived_type

module quick_sort
  implicit none
  private
  public :: sort
contains
  pure subroutine sort(array,sorted,idx)
    integer, dimension(:),           intent(in) :: array
    integer, dimension(size(array)), optional, intent(out) :: sorted
    integer, dimension(:), optional, intent(inout) :: idx
    integer, dimension(size(array)) :: idx_, sorted_
    integer :: m
    m = size(array)
    sorted_ = array
    if ( present(idx) ) then
      call qsort_1D( m, sorted_, idx )
    else
      call qsort_1D( m, sorted_, idx_ )
    end if
    if ( present(sorted) ) sorted = sorted_
  end subroutine sort

  pure recursive subroutine qsort_1D( m, A, indx )
    integer,               intent(in)    :: m
    integer, dimension(m), intent(inout) :: A
    integer, dimension(m), intent(inout) :: indx
    integer :: iq
    if ( m > 1 ) then
      call partition_1D( m, A, indx, iq )
      call qsort_1D( iq-1  , A(1:iq-1), indx(1:iq-1) )
      call qsort_1D( m-iq+1, A(iq:m)  , indx(iq:m)   )
    end if
  end subroutine qsort_1D

  pure subroutine partition_1D( m, A, indx, marker )
    integer,               intent(in)    :: m
    integer, dimension(m), intent(inout) :: A
    integer, dimension(m), intent(inout) :: indx
    integer,               intent(out)   :: marker
    integer :: i, j
    integer :: temp_indx
    integer :: x, temp_A
    x = A(1)
    i = 0
    j = m+1
    do
      do; j = j-1; if ( A(j) <= x ) exit; end do
      do; i = i+1; if ( A(i) >= x ) exit; end do
      if ( i < j ) then
        temp_A    = A(i);   temp_indx = indx(i)
        A(i)      = A(j);   indx(i)   = indx(j)
        A(j)      = temp_A; indx(j)   = temp_indx
      elseif ( i == j ) then
        marker = i+1; return
      else
        marker = i; return
      end if
    end do
  end subroutine partition_1D
end module quick_sort

module index_conversion
  implicit none
  private
  public :: in_bound
  public :: global2local, global2local_bnd, global2local_ghost
  public :: local2global, local2global_bnd, local2global_ghost
  public :: global2local_face, local2global_face
  public :: cell_face_nbors
  public :: get_face_idx_from_id
  public :: get_reshape_indices
  public :: range_intersect, bound_intersect
  public :: node_to_cell_idx, get_neighbor_idx
  public :: shift_val_to_start
  public :: get_exterior_mask
  public :: remove_duplicates_unsorted

  interface cell_face_nbors
    module procedure cell_face_nbors_lin
    module procedure cell_face_nbors_sub
  end interface cell_face_nbors
  
contains

  pure function in_bound( dim, idx, bnd_min, bnd_max )
    integer,                 intent(in) :: dim
    integer, dimension(dim), intent(in) :: idx, bnd_min, bnd_max
    logical                             :: in_bound
    in_bound =     all(idx>=bnd_min).and.all(idx<=bnd_max)                       &
              .or. all(idx<=bnd_min).and.all(idx>=bnd_max)
  end function in_bound

  pure function global2local(iG,nSub) result(iSub)
    integer,               intent(in) :: iG
    integer, dimension(:), intent(in) :: nSub
    integer, dimension(size(nSub)) :: iSub
    integer :: i, nDims, p, iGtmp, iTmp
    nDims = size(nSub)
    if (nDims==1) then
      iSub(1) = iG
      return
    end if
    p = product(nSub)
    iGtmp = iG
    do i = nDims,1,-1
      p = p/nSub(i)
      iTmp = mod(iGtmp-1,p) + 1
      iSub(i) = (iGtmp-iTmp)/p + 1
      iGtmp = iTmp
    end do
  end function global2local

  pure function local2global(iSub,nSub) result(iG)
    integer, dimension(:), intent(in) :: iSub, nSub
    integer :: iG
    integer :: nDims, p, i
    nDims = size(iSub)
    p = 1
    iG = 1
    do i = 1,nDims
        iG = iG + ( iSub(i) - 1 )*p
        p = p*nSub(i)
    end do
  end function local2global

  pure function global2local_ghost(iG,nSub,nGhost) result(iSub)
    integer,               intent(in) :: iG
    integer, dimension(:), intent(in) :: nSub, nGhost
    integer, dimension(size(nSub)) :: iSub, nSub2
    nSub2 = nSub + 2*nGhost
    iSub = global2local(iG,nSub2)
    iSub = iSub - nGhost
  end function global2local_ghost

  pure function local2global_ghost(iSub,nSub,nGhost) result(iG)
    integer, dimension(:), intent(in) :: iSub, nSub, nGhost
    integer, dimension(size(nSub)) :: iSub2, nSub2
    integer :: iG
    iSub2 = iSub + nGhost
    nSub2 = nSub + 2*nGhost
    iG = local2global(iSub2,nSub2)
  end function local2global_ghost

  pure function global2local_bnd(iG,lo,hi) result(iSub)
    integer,               intent(in) :: iG
    integer, dimension(:), intent(in) :: lo, hi
    integer, dimension(size(lo)) :: iSub, nSub
    nSub = hi - lo + 1
    iSub = global2local(iG,nSub)
    iSub = iSub + lo - 1
  end function global2local_bnd

  pure function local2global_bnd(iSub,lo,hi) result(iG)
    integer, dimension(:), intent(in) :: iSub, lo, hi
    integer, dimension(size(iSub)) :: idx, nSub
    integer :: iG
    idx  = iSub - lo + 1
    nSub = hi - lo + 1
    iG   = local2global(idx,nSub)
  end function local2global_bnd

  pure function get_face_intervals(n_dim,n_cells) result(intervals)
    integer,                   intent(in)  :: n_dim
    integer, dimension(n_dim), intent(in)  :: n_cells
    integer,  dimension(n_dim)             :: intervals
    integer, dimension(n_dim) :: tmp
    integer :: d
    tmp = n_cells
    tmp(1) = tmp(1) + 1
    intervals(1) = product(tmp)
    do d = 2,n_dim
      tmp = n_cells
      tmp(d) = tmp(d) + 1
      intervals(d) = intervals(d-1) + product(tmp)
    end do
  end function get_face_intervals

  pure subroutine local2global_face(n_dim,n_cells,dir,local_idx,lin_face_idx)
    integer,                   intent(in)  :: n_dim
    integer, dimension(n_dim), intent(in)  :: n_cells
    integer,                   intent(in)  :: dir
    integer, dimension(n_dim), intent(in)  :: local_idx
    integer,                   intent(out) :: lin_face_idx
    integer, dimension(n_dim) :: idx_extents
    integer, dimension(n_dim) :: nsub
    idx_extents = get_face_intervals(n_dim,n_cells)
    nsub = n_cells
    nsub(dir) = nsub(dir) + 1
    lin_face_idx = ( idx_extents(dir) - idx_extents(1) ) + local2global(local_idx,nsub)
  end subroutine local2global_face

  pure subroutine global2local_face(n_dim,n_cells,lin_face_idx,dir,local_idx)
    integer,                   intent(in)  :: n_dim
    integer, dimension(n_dim), intent(in)  :: n_cells
    integer,                   intent(in)  :: lin_face_idx
    integer,                   intent(out) :: dir
    integer, dimension(n_dim), intent(out) :: local_idx
    integer, dimension(n_dim) :: idx_extents
    integer, dimension(n_dim) :: nsub
    integer                   :: shifted_lin_idx
    integer, dimension(1) :: loc
    idx_extents = get_face_intervals(n_dim,n_cells)
    loc = findloc((lin_face_idx <= idx_extents),.true.)
    dir = loc(1)
    shifted_lin_idx = lin_face_idx - ( idx_extents(dir) - idx_extents(1) )
    nsub = n_cells
    nsub(dir) = nsub(dir) + 1
    local_idx = global2local( shifted_lin_idx,nsub)
  end subroutine global2local_face

  pure subroutine cell_face_nbors_sub( dim, idx, bnd_min, bnd_max, nbor_cell_idx, nbor_face_id, n_int )
    integer,                       intent(in) :: dim
    integer, dimension(dim),       intent(in) :: idx, bnd_min, bnd_max
    integer, dimension(dim,2*dim), intent(out) :: nbor_cell_idx
    integer, dimension(2*dim),     intent(out) :: nbor_face_id
    integer,                       intent(out) :: n_int
    integer, dimension(dim,2*dim) :: nbor_cell_idx_tmp
    integer, dimension(2*dim) :: nbor_face_id_tmp
    integer, dimension(dim) :: idx_tmp
    integer :: s, j, n_ext, cnt
    cnt   = 0
    n_int = 0
    n_ext = 0
    do j = 1,dim
      do s = -1,1,2
        cnt = cnt + 1
        idx_tmp = idx
        idx_tmp(j) = idx_tmp(j) + s
        if ( in_bound(dim,idx_tmp,bnd_min,bnd_max) ) then
            n_int = n_int + 1
            nbor_cell_idx(:,n_int) = idx_tmp
            nbor_face_id(n_int) = cnt
        else
          n_ext = n_ext + 1
          nbor_cell_idx_tmp(:,n_ext) = idx_tmp
          nbor_face_id_tmp(n_ext) = cnt
        end if
      end do
    end do
    do j = 1,n_ext
      nbor_cell_idx(:,n_int+j) = nbor_cell_idx_tmp(:,j)
      nbor_face_id(n_int+j) = nbor_face_id_tmp(j)
    end do
  end subroutine cell_face_nbors_sub

  pure subroutine cell_face_nbors_lin( dim, lin_idx, bnd_min, bnd_max, &
                                       nbor_cell_idx, nbor_face_id, n_int )
    integer,                       intent(in) :: dim, lin_idx
    integer, dimension(dim),       intent(in) :: bnd_min, bnd_max
    integer, dimension(2*dim), intent(out) :: nbor_cell_idx
    integer, dimension(2*dim), intent(out) :: nbor_face_id
    integer,                       intent(out) :: n_int
    integer, dimension(dim,2*dim) :: nbor_idx
    integer, dimension(dim) :: idx
    integer :: j
    idx = global2local_bnd(lin_idx,bnd_min,bnd_max)
    call cell_face_nbors_sub( dim, idx, bnd_min, bnd_max, nbor_idx, nbor_face_id, n_int )
    do j = 1,2*dim
      nbor_cell_idx(j) = local2global_bnd(nbor_idx(:,j),bnd_min,bnd_max)
    end do
  end subroutine cell_face_nbors_lin

  pure elemental subroutine get_face_info_from_id(face_id,dir,offset)
    integer, intent(in)  :: face_id
    integer, intent(out) :: dir, offset
    dir    = (face_id-1)/2 + 1
    offset = mod(face_id+1,2)
  end subroutine get_face_info_from_id

  pure subroutine get_face_idx_from_id(idx,face_id,dir,face_idx)
    integer, dimension(:),         intent(in) :: idx
    integer,                       intent(in)  :: face_id
    integer,                       intent(out) :: dir
    integer, dimension(size(idx)), intent(out) :: face_idx

    integer, dimension(size(idx)) :: face_offset
    integer :: offset
    call get_face_info_from_id(face_id,dir,offset)
    face_offset = 0
    face_offset(dir) = offset
    face_idx = idx + face_offset
  end subroutine get_face_idx_from_id

  pure subroutine get_neighbor_idx( dim, bnd1_min, bnd1_max, bnd2_min, bnd2_max, idx, out_idx )
    integer,                 intent(in)  :: dim
    integer, dimension(dim), intent(in)  :: bnd1_min, bnd1_max, &
                                            bnd2_min, bnd2_max
    integer, dimension(dim), intent(in)  :: idx
    integer, dimension(dim), intent(out) :: out_idx
    integer, dimension(dim) :: delta1, delta2, s1, s2, s, offset
    delta1 = bnd1_max - bnd1_min
    delta2 = bnd2_max - bnd2_min
    s1 = sign(1,delta1)
    s2 = sign(1,delta2)
    s  = s1*s2
    offset = idx - bnd1_min - s1
    out_idx = bnd2_min + s*offset + s2
  end subroutine get_neighbor_idx

  pure subroutine get_reshape_indices( sz_in, loc, sz_out, sz_cnt, idx_start, idx_end )
    integer, dimension(:),           intent(in)  :: sz_in
    integer, dimension(size(sz_in)), intent(in)  :: loc
    integer, dimension(size(sz_in)), intent(out) :: sz_out
    integer,                         intent(out) :: sz_cnt
    integer, dimension(size(sz_in)), intent(out) :: idx_start
    integer, dimension(size(sz_in)), intent(out) :: idx_end

    logical, dimension(size(sz_in)) :: lo, hi, varies

    lo     = (loc==0)
    hi     = (loc==1)
    varies = (loc==2)
    sz_out    = 1
    sz_cnt    = count(varies)
    sz_out(1:sz_cnt) = pack(sz_in,varies)
    idx_start = 1
    idx_end   = 1

    where ( lo .or. varies ) idx_start = 1
    where ( hi             ) idx_start = sz_in
    where ( lo             ) idx_end   = 1
    where ( hi .or. varies ) idx_end   = sz_in

  end subroutine get_reshape_indices

  pure elemental function range_intersect( startA, endA, startB, endB )
    integer, intent(in) :: startA, endA, startB, endB
    logical             :: range_intersect
    range_intersect = ( startA <= endB ).and.( endA >= startB )
  end function range_intersect

  pure function bound_intersect( dim, bnd1_min, bnd1_max, bnd2_min, bnd2_max )
    integer,                 intent(in) :: dim
    integer, dimension(dim), intent(in) :: bnd1_min, bnd1_max, &
                                           bnd2_min, bnd2_max
    logical                             :: bound_intersect
    bound_intersect = all( range_intersect( bnd1_min, bnd1_max,                &
                                            bnd2_min, bnd2_max ) )
  end function bound_intersect

  pure elemental subroutine node_to_cell_idx(bnd_min,bnd_max)
    integer, intent(inout) :: bnd_min, bnd_max
    if ( bnd_max > bnd_min ) then
      bnd_max = bnd_max - 1
    elseif (bnd_min > bnd_max ) then
      bnd_min = bnd_min - 1
    end if
  end subroutine node_to_cell_idx

  pure subroutine shift_val_to_start(list,idx)
    integer, dimension(:), intent(inout) :: list
    integer,               intent(in)    :: idx
    integer, dimension(size(list)) :: tmp
    integer :: i
    tmp = list
    list(1) = list(idx)
    do i = 1,idx-1
      list(i+1) = tmp(i)
    end do
  end subroutine shift_val_to_start

  pure subroutine get_exterior_mask(lo,hi,lo2,hi2,mask)
    integer, dimension(:), intent(in) :: lo, hi, lo2, hi2
    logical, dimension(product(hi-lo+1)), intent(out) :: mask
    integer, dimension(size(hi-lo+1)) :: itmp
    integer :: iG, ni_Cells
    integer :: j

    ! n_Cells  = product(hi-lo+1)
    ni_Cells = product(hi2-lo2+1)
    ! ne_Cells = n_Cells - ni_Cells

    mask = .true.
    do j = 1,ni_Cells
      ! get the interior index
      itmp = global2local_bnd(j,lo2,hi2)
      ! figure out global index in the larger array
      iG   = local2global_bnd(itmp,lo,hi)
      mask(iG) = .false.
    end do
  end subroutine get_exterior_mask

  pure subroutine remove_duplicates_unsorted(input,output,n_unique)
    integer, dimension(:),           intent(in)  :: input
    integer, dimension(size(input)), intent(out) :: output
    integer,                         intent(out) :: n_unique
    integer :: i, n_max
    n_unique = 1
    output    = 0
    output(1) = input(1)
    n_max = size(input)
    do i = 2,n_max
      if (any(output==input(i))) cycle
      n_unique = n_unique + 1
      output(n_unique) = input(i)
    end do
end subroutine remove_duplicates_unsorted

end module index_conversion

module stencil_indexing

  implicit none

  private

  public :: get_linear_face_idx
  public :: cell_offsets

  public :: get_offsets
  public :: cube_mask2dir
  public :: mask_dim_split
  public :: mask_in_bounds
  
  public :: on_3d_boundary
  public :: is_ghost_cell
  public :: idx_to_offset
  public :: sort_stencil_idx
  public :: get_interior_mask
  public :: determine_interior_stencil_count

  public :: linear_map_offsets_check
  public :: inviscid_stencil_indices_3D
  public :: viscous_offsets, muscl_offsets, center_offsets
  public :: sector_offsets, identify_sector_stencils, identify_sector_stencils_alt

  public :: get_bounding_box

  public :: get_all_interior_vertex_nbors

  integer, parameter, dimension(7) :: map_idx = [5,3,1,0,2,4,6]
  integer, parameter, dimension(3,6) :: cell_offsets = reshape(                &
                              [-1,0,0,1,0,0,0,-1,0,0,1,0,0,0,-1,0,0,1],[3,6])
contains

  pure subroutine get_all_interior_vertex_nbors(n_dim,lin_idx,blk_size,n_nbor,nbor_list,status)
    use index_conversion, only : range_intersect, in_bound, global2local, local2global
    integer,                         intent(in)  :: n_dim, lin_idx
    integer, dimension(n_dim),       intent(in)  :: blk_size
    integer,                         intent(out) :: n_nbor
    integer, dimension(3**n_dim-1),  intent(out) :: nbor_list
    integer,                         intent(out) :: status
    integer, dimension(3**n_dim-1) :: linear_offset_list
    integer, dimension(n_dim) :: idx, lo, hi, idx_tmp
    integer :: i, N_max, N_total_cells

    N_total_cells = product(blk_size)

    ! check if lin_idx is in bounds
    if ( .not.( range_intersect(1,N_total_cells,lin_idx,lin_idx) ) ) then
      n_nbor = 0
      nbor_list = 0
      status    = -1
      return
    end if

    idx = global2local(lin_idx,blk_size)
    hi = max(blk_size-2,1)
    lo = min(hi,2)
    N_max = 3**n_dim-1
    linear_offset_list = get_nearest_nbor_offset_list(n_dim)
    
    if ( in_bound(n_dim,idx,lo,hi) ) then
      n_nbor = N_max
      status    = 0 ! cell is on the interior
      do i = 1,N_max
        nbor_list(i) = local2global(get_nearest_nbor_idx(n_dim,linear_offset_list(i),idx),blk_size)
      end do
    else
      status    = 1 ! cell is on the boundary
      n_nbor = 0
      hi = blk_size
      lo = 1
      do i = 1,N_max
        idx_tmp = get_nearest_nbor_idx(n_dim,linear_offset_list(i),idx)
        ! if neighbor is in bounds
        if ( in_bound(n_dim,idx_tmp,lo,hi) ) then
          n_nbor = n_nbor + 1
          nbor_list(n_nbor) = local2global(idx_tmp,blk_size)
        end if
      end do
    end if
  end subroutine get_all_interior_vertex_nbors

  pure function get_nearest_nbor_offset_list(n_dim) result(linear_offset_list)
    integer,            intent(in) :: n_dim
    integer, dimension(3**n_dim-1) :: linear_offset_list
    integer :: i, mid, N
    N = 3**n_dim
    mid = N/2 + 1
    linear_offset_list = [(i,i=1,mid-1),(i,i=mid+1,N)]
  end function get_nearest_nbor_offset_list

  pure function get_nearest_nbor_idx(n_dim,linear_offset,idx) result(nbor)
    use index_conversion, only : global2local
    integer,                   intent(in)  :: n_dim, linear_offset
    integer, dimension(n_dim), intent(in)  :: idx
    integer, dimension(n_dim)              :: nbor
    integer, dimension(n_dim) :: nsub
    nsub = 3
    nbor = idx + global2local(linear_offset,nsub)-2
  end function get_nearest_nbor_idx

  pure function get_linear_face_idx( idx )
    integer, dimension(3),   intent(in) :: idx
    integer                             :: get_linear_face_idx
    get_linear_face_idx = map_idx( idx(1) + 2*idx(2) + 3*idx(3) + 4 )
  end function get_linear_face_idx

  pure subroutine get_offsets(mask,offsets,n)
    logical, dimension(6),   intent(in)  :: mask
    integer, dimension(3,6), intent(out) :: offsets
    integer,                 intent(out) :: n
    integer :: i
    offsets = 0
    n = count(.not.mask)
    do i = 1,3
      offsets(i,:) = pack( cell_offsets(i,:), .not.mask,offsets(i,:) )
    end do
  end subroutine get_offsets

  pure function cube_mask2dir(mask) result(dir)
    logical, dimension(6), intent(in) :: mask
    integer, dimension(3)             :: dir
    integer, dimension(6), parameter :: ds = [-1,1,-2,2,-3,3]
    integer, dimension(6) :: dds
    integer :: n, i, d, s

    dds = 0
    dir = 0
    n = count(mask)
    if (n == 0) return
    
    dds(1:n) = pack( ds, mask )
    do i = 1,n
      d = abs( dds(i) )
      if (d/=0) then
        s      = sign( 1, dds(i) )
        dir(d) = dir(d) + s
      end if
    end do
  end function cube_mask2dir

  pure subroutine mask_dim_split( mask, idx, origin )
    logical, dimension(6), intent(inout) :: mask
    integer, dimension(3), intent(in)    :: idx, origin
    mask(1) = mask(1).or.( idx(1) > origin(1) )
    mask(2) = mask(2).or.( idx(1) < origin(1) )
    mask(3) = mask(3).or.( idx(2) > origin(2) )
    mask(4) = mask(4).or.( idx(2) < origin(2) )
    mask(5) = mask(5).or.( idx(3) > origin(3) )
    mask(6) = mask(6).or.( idx(3) < origin(3) )
  end subroutine mask_dim_split

  pure subroutine mask_in_bounds( mask, idx, lo, hi )
    logical, dimension(6), intent(inout) :: mask
    integer, dimension(3), intent(in)    :: idx, lo, hi

    integer, dimension(3,2) :: bnds

    bnds(:,1) = lo
    bnds(:,2) = hi

    mask = mask .or. on_3d_boundary( idx, bnds )
  end subroutine mask_in_bounds

  pure function on_3d_boundary( idx, bnds )
    use index_conversion, only : in_bound
    integer, dimension(3),   intent(in) :: idx
    integer, dimension(3,2), intent(in) :: bnds
    logical, dimension(6)               :: on_3d_boundary

    integer, dimension(3,2) :: bnds_tmp
    integer :: d, s

     do d = 1,3
      do s = 1,2
        bnds_tmp = bnds
        bnds_tmp(d,:) = bnds(d,s)
        on_3d_boundary(2*(d-1)+s) = in_bound( 3, idx, bnds_tmp(:,1), bnds_tmp(:,2) )
      end do
    end do
  end function on_3d_boundary

  pure function is_ghost_cell(idx,lo,hi,n_ghost_cells) result(mask)
    use index_conversion, only : in_bound
    integer, dimension(3), intent(in) :: idx, lo, hi, n_ghost_cells
    logical, dimension(6)             :: mask
    integer, dimension(2), parameter :: dir = [-1,1]

    integer, dimension(3,2) :: bnds, bnds2, bnds_tmp
    integer :: d, s

    bnds(:,1)  = lo
    bnds(:,2)  = hi
    bnds2(:,1) = lo - n_ghost_cells
    bnds2(:,2) = hi + n_ghost_cells
    mask = .false.
    do d = 1,3
      do s = 1,2
        bnds_tmp = bnds2
        bnds_tmp(d,:) = bnds(d,s) + dir(s)
        bnds_tmp(d,s) = bnds(d,s) + dir(s) * n_ghost_cells(d)
        mask(2*(d-1)+s) = in_bound( 3, idx, bnds_tmp(:,1), bnds_tmp(:,2) )
      end do
    end do
  end function is_ghost_cell

  pure subroutine get_interior_mask( offset_list, idx, lo_bnd, hi_bnd, interior_mask )
    use index_conversion, only : in_bound
    integer, dimension(:,:),            intent(in)  :: offset_list
    integer, dimension(3),              intent(in)  :: idx, lo_bnd, hi_bnd
    logical, dimension(:), allocatable, intent(out) :: interior_mask
    integer :: i, N_cells
    
    N_cells = size(offset_list,2)

    allocate( interior_mask(N_cells) )
    interior_mask = .false.
    do i = 1,N_cells
      interior_mask(i) = in_bound( 3, idx + offset_list(:,i), lo_bnd, hi_bnd )
    end do
  end subroutine get_interior_mask

  pure subroutine idx_to_offset( idx_list, center_idx, offset_list )
    integer, dimension(:,:),              intent(in)  :: idx_list
    integer, dimension(size(idx_list,1)), intent(in)  :: center_idx
    integer, dimension(size(idx_list,1),size(idx_list,2)), intent(out)  :: offset_list
    integer :: i

    do i = 1,size(idx_list,2)
      offset_list(:,i) = idx_list(:,i) - center_idx
    end do
  end subroutine idx_to_offset

  pure subroutine get_bounding_box( idx_list, idx, bnd_min, bnd_max )
    integer, dimension(:,:),              intent(in)  :: idx_list
    integer, dimension(size(idx_list,1)), intent(in)  :: idx
    integer, dimension(size(idx_list,1)), intent(out) :: bnd_min, bnd_max

    bnd_min = minval(idx_list,2) + idx
    bnd_max = maxval(idx_list,2) + idx

  end subroutine get_bounding_box

  pure subroutine determine_interior_stencil_count( idx_list, lo_bnd_in, hi_bnd_in,           &
                                               lo_bnd_out, hi_bnd_out,                   &
                                               offset, n_ghost, n_gc_sten )
    integer, dimension(:,:),         intent(in)  :: idx_list
    integer, dimension(3),           intent(in)  :: lo_bnd_in, hi_bnd_in
    integer, dimension(3), optional, intent(in)  :: offset, n_ghost
    integer,               optional, intent(in)  :: n_gc_sten
    integer, dimension(3),           intent(out) :: lo_bnd_out
    integer, dimension(3),           intent(out) :: hi_bnd_out
    integer, dimension(3,size(idx_list,2)) :: offset_list
    integer, dimension(3) :: idx,lo_bnd_stencil, hi_bnd_stencil, ng
    integer :: Ncells
    
    Ncells = size(idx_list,2)
    
    if ( present(offset) ) then
      call idx_to_offset( idx_list, offset, offset_list )
    else
      offset_list = idx_list
    end if

    ng  = 0
    if ( present(n_ghost) )   ng = n_ghost
    if ( present(n_gc_sten) ) ng = max(ng-n_gc_sten,0)

    idx = 0
    call get_bounding_box( offset_list, idx, lo_bnd_stencil, hi_bnd_stencil )

    lo_bnd_out = lo_bnd_in - lo_bnd_stencil - ng
    hi_bnd_out = hi_bnd_in - hi_bnd_stencil + ng

    if ( any(lo_bnd_out > hi_bnd_out) ) then
      lo_bnd_out = 1
      hi_bnd_out = 0
    end if
  end subroutine determine_interior_stencil_count

  pure subroutine sort_stencil_idx( n_stencil, stencil )
    use quick_sort, only : sort
    use index_conversion, only : local2global_bnd
    integer,                         intent(in)    :: n_stencil
    integer, dimension(:,:),         intent(inout) :: stencil
    integer, dimension(4,n_stencil) :: stencil_tmp
    integer, dimension(4) :: min_bnd, max_bnd, range
    integer, dimension(n_stencil)   :: ind1, ind2
    integer :: i
    min_bnd = minval(stencil,dim=2)
    max_bnd = maxval(stencil,dim=2)
    range   = max_bnd - min_bnd + 1
    do i = 1,n_stencil
      ind1(i) = local2global_bnd(stencil(:,i),min_bnd,max_bnd)
      ind2(i) = i
    end do
    call sort(ind1,idx=ind2)
    stencil_tmp = stencil(:,1:n_stencil)
    do i = 1,n_stencil
      stencil(:,i) = stencil_tmp(:,ind2(i))
    end do
  end subroutine sort_stencil_idx

  pure subroutine inviscid_stencil_indices_3D(len,idx)
    integer, intent(in) :: len
    integer, dimension(3,(2*len)*3+1), intent(out) :: idx
    integer, dimension(3) :: idx_tmp
    integer :: dim, off, cnt
    idx = 0
    cnt = 1
    do dim = 1,3
      do off = -len,-1
        cnt = cnt + 1
        idx_tmp = 0
        idx_tmp(dim) = idx_tmp(dim) + off
        idx(:,cnt) = idx_tmp
      end do
      do off = 1,len
        cnt = cnt + 1
        idx_tmp = 0
        idx_tmp(dim) = idx_tmp(dim) + off
        idx(:,cnt) = idx_tmp
      end do
    end do
  end subroutine inviscid_stencil_indices_3D

  pure subroutine sector_offsets(direction,n_dim,offsets)
    use index_conversion, only : shift_val_to_start
    integer,                             intent(in)  :: direction, n_dim
    integer, dimension(n_dim,2*n_dim-1), intent(out) :: offsets
    integer, dimension(n_dim) :: d
    integer :: s, dir
    integer :: i, j, k, cnt
    offsets = 0
    dir = abs(direction)
    s   = sign(1,direction)
    do i = 1,n_dim
      d(i) = i
    end do
    call shift_val_to_start(d,dir)
    offsets(dir,:) = s
    cnt = 1
    do i = 2,n_dim
      do j = -1,1,2
        cnt = cnt + 1
        offsets(d(i),cnt) = j
      end do
    end do
  end subroutine sector_offsets

  pure subroutine identify_sector_stencils(n_dim,n_stencil,stencil_idxs,n_sec,n_sec_idx,sec_idx)
    integer, intent(in) :: n_dim, n_stencil
    integer, dimension(n_dim+1,n_stencil), intent(in) :: stencil_idxs
    integer, intent(out) :: n_sec
    integer, dimension(2*n_dim), intent(out) :: n_sec_idx
    integer, dimension(2*n_dim-1,2*n_dim), intent(out) :: sec_idx
    integer :: d, s, i, j, k
    integer, dimension(n_dim,2*n_dim-1) :: sec_off
    integer, dimension(n_dim+1) :: tmp_idx

    n_sec     = 0
    n_sec_idx = 0
    sec_idx   = 0

    ! for each face
    do d = 1,n_dim
      do s = -1,1,2
        call sector_offsets(d*s,n_dim,sec_off)
        ! for each cell in the sector stencil
        do k = 1,2*n_dim-1
          ! check against all cells in stencil
          do j = 2,n_stencil
            tmp_idx = stencil_idxs(:,1)
            tmp_idx(2:) = tmp_idx(2:) + sec_off(:,k)

            if (all(tmp_idx==stencil_idxs(:,j))) then

              ! increment the counter for this sector stencil
              n_sec_idx(n_sec+1) = n_sec_idx(n_sec+1) + 1
              ! save the location in the stencil
              sec_idx( n_sec_idx(n_sec+1), n_sec+1 ) = j
            end if
          end do
        end do
        ! if there was any intersection, then increment the counter over the sector stencils
        if ( n_sec_idx(n_sec+1) > 0 ) then
          n_sec = n_sec+1
        end if
      end do
    end do
  end subroutine identify_sector_stencils

  pure subroutine identify_sector_stencils_alt(n_dim,n_stencil,stencil_idxs,n_list,list)
    integer, intent(in) :: n_dim, n_stencil
    integer, dimension(n_dim+1,n_stencil), intent(in) :: stencil_idxs
    integer,                          intent(out) :: n_list
    integer, dimension(4*n_dim**2+1), intent(out) :: list
    integer :: d, s, i, j, k
    integer, dimension(n_dim,2*n_dim-1) :: sec_off
    integer, dimension(n_dim+1) :: tmp_idx
    integer :: n_sec, offset
    integer, dimension(2*n_dim)           :: n_sec_idx
    integer, dimension(2*n_dim-1,2*n_dim) :: sec_idx

    ! integer :: n_sec2
    ! integer, dimension(2*n_dim)           :: n_sec_idx2
    ! integer, dimension(2*n_dim-1,2*n_dim) :: sec_idx2
    ! integer, dimension(4*n_dim**2+1)      :: lis

    n_sec     = 0
    n_sec_idx = 0
    sec_idx   = 0
    ! for each face
    do d = 1,n_dim
      do s = -1,1,2
        call sector_offsets(d*s,n_dim,sec_off)
        ! for each cell in the sector stencil
        do k = 1,2*n_dim-1
          ! check against all cells in stencil
          do j = 2,n_stencil
            tmp_idx = stencil_idxs(:,1)
            tmp_idx(2:) = tmp_idx(2:) + sec_off(:,k)

            if (all(tmp_idx==stencil_idxs(:,j))) then

              ! increment the counter for this sector stencil
              n_sec_idx(n_sec+1) = n_sec_idx(n_sec+1) + 1
              ! save the location in the stencil
              sec_idx( n_sec_idx(n_sec+1), n_sec+1 ) = j - 1 ! offset, since we aren't counting the central cell
            end if
          end do
        end do
        ! if there was any intersection, then increment the counter over the sector stencils
        if ( n_sec_idx(n_sec+1) > 0 ) then
          n_sec = n_sec+1
        end if
      end do
    end do

    list      = 0
    n_list    = 0
    
    list(1)         = n_sec
    list(2:n_sec+1) = n_sec_idx(1:n_sec)
    n_list = n_sec+1
    do k = 1,n_sec
      do j = 1,n_sec_idx(k)
        n_list = n_list + 1
        list(n_list) = sec_idx(j,k)
      end do
    end do

    ! lis = list

    ! ! check that you can recover the information
    ! n_sec2     = 0
    ! n_sec_idx2 = 0
    ! sec_idx2   = 0

    ! n_sec2 = lis(1)
    ! do k = 1,lis(1)
    !   n_sec_idx2(k) = lis(1+k)
    !   offset = 1+lis(1)+sum(lis(2:k))
    !   do j = 1,lis(1+k)
    !     sec_idx2(j,k) = lis(offset+j)
    !   end do
    ! end do

    ! if ( n_sec2 /= n_sec ) then
    !   tmp_idx = 0
    ! end if

    ! do k = 1,n_sec
    !   if ( n_sec_idx2(k) /= n_sec_idx(k) ) then
    !     tmp_idx = 0
    !   end if
    !   do j = 1,n_sec_idx(k)
    !     if ( sec_idx2(j,k) /= sec_idx(j,k) ) then
    !       tmp_idx = 0
    !     end if
    !   end do
    ! end do
        

    ! tmp_idx = 0

  end subroutine identify_sector_stencils_alt

  pure subroutine viscous_offsets(direction,ndim,offsets)
    use index_conversion, only : shift_val_to_start
    integer,                         intent(in)  :: direction, ndim
    integer, dimension(:,:), allocatable, intent(out) :: offsets
    integer, dimension(ndim) :: d
    integer :: s, dir
    integer :: i, j, k, cnt
    allocate( offsets(ndim,2+2**ndim) )
    offsets = 0
    dir = abs(direction)
    s   = sign(1,direction)
    do i = 1,ndim
      d(i) = i
    end do
    call shift_val_to_start(d,dir)
    cnt = 0
    do i = 1,2
      cnt = cnt + 1
      offsets(dir,cnt) = s*mod(cnt+1,2)
    end do
    do i = 2,ndim
      do j = -1,1,2
        do k = 1,2
          cnt = cnt + 1
          offsets(dir,cnt) = s*mod(cnt+1,2)
          offsets(d(i),cnt) = j
        end do
      end do
    end do
  end subroutine viscous_offsets

  pure subroutine muscl_offsets(direction,ndim,offsets)
    integer,                 intent(in)  :: direction, ndim
    integer, dimension(:,:), allocatable, intent(out) :: offsets
    integer :: i, dir, s

    allocate( offsets(ndim,4) )
    offsets = 0
    dir = abs(direction)
    s   = sign(1,direction)

    do i = 1,4
      offsets(dir,i) = i-2+(s-1)/2
    end do
  end subroutine muscl_offsets

  pure subroutine center_offsets(direction,ndim,offsets)
    integer,                 intent(in)  :: direction, ndim
    integer, dimension(:,:), allocatable, intent(out) :: offsets
    integer :: s, dir
    integer, dimension(ndim) :: start
    integer :: i, j, cnt

    allocate( offsets(ndim,1+2*ndim) )
    offsets = 0
    start = 0
    dir = abs(direction)
    s   = sign(1,direction)
    if (dir /= 0) then
      start(dir) = s
    end if

    cnt = 1
    offsets(:,cnt) = start
    do i = 1,ndim
      do j = -1,1,2
          cnt = cnt + 1
          offsets(:,cnt) = start
          offsets(i,cnt) = offsets(i,cnt) + j
      end do
    end do
  end subroutine center_offsets

  pure subroutine linear_map_offsets_check(offsets,max_bnd,min_bnd,idx,n_valid)
    use index_conversion, only : in_bound
    integer, dimension(:,:), intent(in) :: offsets
    integer, dimension(3),   intent(in) :: max_bnd, min_bnd
    integer, dimension(size(offsets,2)), intent(out) :: idx
    integer,                             intent(out) :: n_valid
    integer, dimension(3) :: range
    logical, dimension(size(offsets,2)) :: mask

    integer :: i, Ncells

    Ncells = size(offsets,2)

    range   = max_bnd - min_bnd + 1

    !! out of bounds check
    do i = 1,Ncells
      mask(i) = in_bound(3,offsets(:,i),min_bnd,max_bnd)
    end do
    n_valid = count(mask)
    do i = 1,Ncells
      idx(i) = ( offsets(1,i) - min_bnd(1) )                                   &
             + ( offsets(2,i) - min_bnd(2) ) * range(1)                        &
             + ( offsets(3,i) - min_bnd(3) ) * range(1) * range(2)
    end do

    idx(1:n_valid) = pack(idx,mask)

  end subroutine linear_map_offsets_check

end module stencil_indexing

module stencil_cell_derived_type

  use set_precision, only : dp

  implicit none

  private
  public :: stencil_cell_t, block_info
  public :: check_neighbors

  type basic_bc_info
    integer :: block_id = -1
    integer :: face_id  = -1
    integer, dimension(3) :: bnd_min = -1
    integer, dimension(3) :: bnd_max = -1
  end type basic_bc_info

  type connected_bc_info
    type(basic_bc_info) :: self, nbor
  end type connected_bc_info

  type block_info
    integer               :: block_id = -1
    integer, dimension(3) :: Ncells   = -1
    type(connected_bc_info), allocatable, dimension(:) :: connected_bc_list
  contains
    ! procedure, public, pass :: create  => create_block_info
    procedure, public, pass :: destroy => destroy_block_info
  end type block_info

  type stencil_cell_t
    type(block_info)        :: info
    integer, dimension(4)   :: idx      = 0
    integer, dimension(4,6) :: nbor_idx = 0
    logical, dimension(6)   :: free     = .false.
    logical, dimension(6)   :: on_boundary = .false.
    integer                 :: degree   = 0
  contains
    procedure, public, pass :: create  => create_stencil_cell
    procedure, public, pass :: destroy => destroy_stencil_cell
  end type stencil_cell_t

  interface block_info
    module procedure block_info_constructor
  end interface block_info

contains

  pure function block_info_constructor( block_id, Ncells, connected_bc_list ) result(this)
    integer, intent(in) :: block_id
    integer, dimension(3), intent(in) :: Ncells
    type(connected_bc_info), dimension(:), optional, intent(in) :: connected_bc_list
    type(block_info) :: this
    call this%destroy()
    this%block_id = block_id
    this%Ncells   = Ncells

    if ( present( connected_bc_list ) ) then
      allocate( this%connected_bc_list(size(connected_bc_list)) )
      this%connected_bc_list = connected_bc_list
    else
      allocate( this%connected_bc_list(0) )
    end if
    
  end function block_info_constructor

  ! subroutine create_block_info( this, gblock )
  !   use grid_derived_type, only : grid_block
  !   class(block_info), intent(inout) :: this
  !   type(grid_block),  intent(in)    :: gblock
  !   integer :: n, cnt, n_connected

  !   this%block_id = gblock%block_id
  !   this%Ncells   = gblock%Ncells

  !   n_connected = 0
  !   do n = 1,gblock%nbounds
  !     if ( gblock%boundaries(n)%bc%is_connected ) n_connected = n_connected+1
  !   end do
  !   allocate( this%connected_bc_list(n_connected) )

  !   cnt = 0
  !   do n = 1,gblock%nbounds
  !     if ( gblock%boundaries(n)%bc%is_connected ) then
  !       cnt = cnt + 1
  !       this%connected_bc_list(cnt)%self%block_id = gblock%boundaries(n)%bc%block_id
  !       this%connected_bc_list(cnt)%self%face_id  = gblock%boundaries(n)%bc%face_label
  !       ! this%connected_bc_list(cnt)%self%bnd_min  = gblock%boundaries(n)%bc%idx_min
  !       ! this%connected_bc_list(cnt)%self%bnd_max  = gblock%boundaries(n)%bc%idx_max
  !       this%connected_bc_list(cnt)%self%bnd_min  = gblock%boundaries(n)%bc%node_idx_min
  !       this%connected_bc_list(cnt)%self%bnd_max  = gblock%boundaries(n)%bc%node_idx_max

  !       call node_to_cell_idx( this%connected_bc_list(cnt)%self%bnd_min,       &
  !                              this%connected_bc_list(cnt)%self%bnd_max )

  !       this%connected_bc_list(cnt)%nbor%block_id = gblock%boundaries(n)%bc%nbor%block_id
  !       this%connected_bc_list(cnt)%nbor%face_id  = gblock%boundaries(n)%bc%nbor%face_label
  !       ! this%connected_bc_list(cnt)%nbor%bnd_min  = gblock%boundaries(n)%bc%nbor%idx_min
  !       ! this%connected_bc_list(cnt)%nbor%bnd_max  = gblock%boundaries(n)%bc%nbor%idx_max
  !       this%connected_bc_list(cnt)%nbor%bnd_min  = gblock%boundaries(n)%bc%nbor%node_idx_min
  !       this%connected_bc_list(cnt)%nbor%bnd_max  = gblock%boundaries(n)%bc%nbor%node_idx_max

  !       call node_to_cell_idx( this%connected_bc_list(cnt)%nbor%bnd_min,       &
  !                              this%connected_bc_list(cnt)%nbor%bnd_max )
  !     end if
  !   end do

  ! end subroutine create_block_info

  pure elemental subroutine destroy_block_info( this )
    class(block_info), intent(inout) :: this
    if ( allocated(this%connected_bc_list) ) deallocate(this%connected_bc_list)
  end subroutine destroy_block_info

  pure elemental subroutine destroy_stencil_cell( this )
    class(stencil_cell_t), intent(inout) :: this
    call this%info%destroy()
  end subroutine destroy_stencil_cell

  pure subroutine create_stencil_cell( this, block_id, idx, block_info_list )
    use index_conversion, only : in_bound, get_neighbor_idx
    use stencil_indexing, only : cell_offsets, on_3d_boundary
    class(stencil_cell_t),          intent(inout) :: this
    integer,                        intent(in)    :: block_id
    integer,          dimension(3), intent(in)    :: idx
    type(block_info), dimension(:), intent(in)    :: block_info_list

    logical, dimension(6)   :: mask
    integer, dimension(3) :: bnd_min, bnd_max
    integer, dimension(3,2) :: bnds

    integer :: i, j, k

    call this%destroy()

    this%idx(1)   = block_id
    this%idx(2:4) = idx
    ! get block info
    do i = 1,size(block_info_list)
      if ( block_info_list(i)%block_id == this%idx(1) ) then
        this%info = block_info_list(i)
        exit
      end if
    end do
    ! check if the current cell is on any of the block boundaries
    bnd_min = [1,1,1]
    bnd_max = this%info%Ncells
    bnds(:,1) = bnd_min
    bnds(:,2) = bnd_max
    mask = on_3d_boundary( this%idx(2:4), bnds )
    do i = 1,6
      if ( mask(i) ) then
        ! assume out-of-bounds for now
        this%free(i) = .false.
        ! find corresponding connected boundary if it exists
        do j = 1,size( this%info%connected_bc_list )
          ! try this BC ...
          if ( this%info%connected_bc_list(j)%self%face_id == i ) then
    associate( bnd_min1 => this%info%connected_bc_list(j)%self%bnd_min,        &
               bnd_max1 => this%info%connected_bc_list(j)%self%bnd_max )
            ! ... if the bounds match
            if ( in_bound( 3, this%idx(2:4), bnd_min1, bnd_max1 ) ) then
              ! get the neighbor
      associate( bnd_min2 => this%info%connected_bc_list(j)%nbor%bnd_min,      &
                 bnd_max2 => this%info%connected_bc_list(j)%nbor%bnd_max )
              this%nbor_idx(1,i) = this%info%connected_bc_list(j)%nbor%block_id
              call get_neighbor_idx( 3, bnd_min1, bnd_max1,                    &
                                        bnd_min2, bnd_max2,                    &
                                        this%idx(2:4), this%nbor_idx(2:4,i) )
              ! check if the neighbor is in bounds
              do k = 1,size( block_info_list )
                ! find the corresponding neighboring block dimensions
                if (                  block_info_list(k)%block_id ==           &
                     this%info%connected_bc_list(j)%nbor%block_id    ) then
                  this%free(i) = in_bound( 3, this%nbor_idx(2:4,i), bnd_min,   &
                                              block_info_list(k)%Ncells )
                  exit
                end if
              end do
      end associate
            end if
    end associate  
          end if
        end do
      else
        ! same block, simple offset
        this%nbor_idx(:,i) = this%idx
        this%nbor_idx(2:4,i) = this%nbor_idx(2:4,i) + cell_offsets(:,i)
        this%free(i) = in_bound( 3, this%nbor_idx(2:4,i), bnd_min, bnd_max )
      end if
    end do
    this%on_boundary = .not.this%free

  end subroutine create_stencil_cell

  pure subroutine check_neighbors( stencil, stencil_idx, n_cells )
    type(stencil_cell_t), dimension(:), intent(inout) :: stencil
    integer,                            intent(in)    :: stencil_idx, n_cells
    integer, dimension(4) :: idx_tmp
    integer :: i, j, k

    do j = 1,n_cells
      if ( j==stencil_idx ) cycle
      do i = 1,6
        if ( stencil(stencil_idx)%free(i) ) then
          idx_tmp = stencil(stencil_idx)%nbor_idx(:,i)
          if ( all( stencil(j)%idx == idx_tmp ) ) then
            ! update the mask on the current cell
            stencil(stencil_idx)%free(i) = .false.
            stencil(stencil_idx)%degree = min( stencil(stencil_idx)%degree,    &
                                               stencil(j)%degree + 1 )
            ! and on the jth cell
            do k = 1,6
              if ( all( stencil(j)%nbor_idx(:,k) ==                            &
                        stencil(stencil_idx)%idx ) ) then
                stencil(j)%free(k) = .false.
                exit
              end if
            end do
          end if
        end if
      end do
    end do
  end subroutine check_neighbors

end module stencil_cell_derived_type

module stencil_growing_routines

  use set_precision, only : dp
  use set_constants, only : zero, large

  implicit none

  private

  public :: grow_stencil_basic, get_max_degree

  character(*), parameter :: FMT = '("iter: ",I4," start: (",3(I3),"), '//     &
                                   'shift: (",3(I3),"), end: (",3(I3),")")'
  character(*), parameter :: msg_FMT = '("Warning: Could not fill stencil'//   &
                                       ' to requested size, n =",I4)'

  contains

  pure subroutine grow_stencil_basic( block_id, idx, N_cells, sz_in, sz_out, nbor_block, nbor_idx, nbor_degree, on_boundary, n_sec, sec_idx )
    use stencil_cell_derived_type, only : block_info
    use index_conversion,          only : local2global_bnd
    use stencil_indexing,          only : identify_sector_stencils, identify_sector_stencils_alt
    integer,                              intent(in)  :: block_id
    integer, dimension(3),                intent(in)  :: idx, N_cells
    integer,                              intent(in)  :: sz_in
    integer,                              intent(out) :: sz_out
    integer, dimension(6*sz_in),          intent(out) :: nbor_block, nbor_idx, nbor_degree
    logical, dimension(6,6*sz_in), optional, intent(out) :: on_boundary
    integer,                       optional, intent(out) :: n_sec
    integer, dimension(37),        optional, intent(out) :: sec_idx
    type(block_info), dimension(1) :: bi
    integer, dimension(4,6*sz_in) :: idx_list
    integer :: i

    integer                 :: n_sec_
    integer, dimension(37)  :: sec_idx_

    bi(1) = block_info(block_id,N_cells)
    call grow_stencil_new_connected_block( block_id, idx, bi, sz_in, sz_out, idx_list, nbor_degree, on_boundary=on_boundary )
    call bi%destroy()

    n_sec_     = 0
    sec_idx_   = 0
    if ( present(n_sec).or.present(sec_idx)) then
      ! call identify_sector_stencils(3,sz_out,idx_list(:,1:sz_out),n_sec_,n_sec_idx_,sec_idx_)
      call identify_sector_stencils_alt(3,sz_out,idx_list(:,1:sz_out),n_sec_,sec_idx_)
      if ( present(n_sec)     ) n_sec     = n_sec_
      if ( present(sec_idx)   ) sec_idx   = sec_idx_
    end if

    nbor_block = 0
    nbor_idx   = 0
    do i = 1,sz_out
      nbor_block(i) = idx_list(1,i)
      nbor_idx(i)   = local2global_bnd( idx_list(2:4,i), [1,1,1], N_cells )
    end do

  end subroutine grow_stencil_basic

  pure function get_max_degree( block_id, idx, N_cells, sz_in ) result(max_degree)
    use stencil_cell_derived_type, only : block_info, stencil_cell_t
    integer,                              intent(in)  :: block_id
    integer, dimension(3),                intent(in)  :: idx, N_cells
    integer,                              intent(in)  :: sz_in
    integer                                           :: max_degree
    type(stencil_cell_t), dimension(6*sz_in) :: stencil
    integer :: sz_out
    call build_stencil_connected_block( block_id, idx, sz_in, [block_info(block_id,N_cells)], &
                                        sz_out, stencil, balanced=.true. )
    max_degree = maxval(stencil%degree)
  end function get_max_degree

  pure subroutine grow_stencil_new_connected_block( block_id, idx, block_info_list, sz_in, sz_out, idx_list, degree, on_boundary )
    use stencil_indexing, only : sort_stencil_idx
    use stencil_cell_derived_type, only : block_info, stencil_cell_t
    integer,                              intent(in)  :: block_id
    integer, dimension(3),                intent(in)  :: idx
    type(block_info), dimension(:),       intent(in)  :: block_info_list
    integer,                              intent(in)  :: sz_in
    integer,                              intent(out) :: sz_out
    integer, dimension(4,6*sz_in),        intent(out) :: idx_list
    integer, dimension(6*sz_in),          intent(out) :: degree
    logical, dimension(6,6*sz_in), optional, intent(out) :: on_boundary
    type(stencil_cell_t), dimension(6*sz_in) :: stencil
    integer :: i
    logical :: balanced
    balanced = .true.
    
    call build_stencil_connected_block( block_id, idx, sz_in, block_info_list, &
                                        sz_out, stencil, balanced=.true. )
    idx_list = 0
    degree   = 0
    do i = 1,sz_out
      idx_list(:,i)    = stencil(i)%idx
      degree(i)        = stencil(i)%degree
    end do
    if (present(on_boundary)) then
      on_boundary = .false.
      do i = 1,sz_out
        on_boundary(:,i) = stencil(i)%on_boundary
      end do
    end if

  end subroutine grow_stencil_new_connected_block

  pure subroutine build_stencil_connected_block( block_id, idx, n_stencil,          &
                                            block_info_list, n_out, stencil, balanced )
    use message,         only : warning_message
    use project_inputs,  only : verbose_level
    use stencil_indexing, only : cell_offsets, get_linear_face_idx
    use stencil_cell_derived_type, only : stencil_cell_t, block_info, check_neighbors

    integer,                                      intent(in)  :: block_id
    integer,              dimension(3),           intent(in)  :: idx
    integer,                                      intent(in)  :: n_stencil
    type(block_info),     dimension(:),           intent(in)  :: block_info_list
    integer,                                      intent(out) :: n_out
    type(stencil_cell_t), dimension(6*n_stencil), intent(out) :: stencil
    logical,              optional,               intent(in)  :: balanced
    integer :: n, i, cnt, in_it, out_it, min_degree, current_min_degree
    integer :: stencil_idx, face_idx, out_it_max, in_it_max
    integer, dimension(4) :: idx1
    character(*), parameter :: name = 'build_stencil_connected_block'
    character(200) :: msg
    logical :: ierr, flag

    if ( balanced ) then
      out_it_max  = 6*n_stencil
      in_it_max   = 6*n_stencil
    else
      out_it_max  = n_stencil
      in_it_max   = 1
    end if

    ! parent cell
    n         = 1
    idx1(1)   = block_id
    idx1(2:4) = idx
    call stencil(n)%create( block_id, idx, block_info_list )

    flag = .false.
    out_it = 0
    do while ( ( n < n_stencil ).and.( out_it < out_it_max ) )
      out_it = out_it + 1
      ! update masks and degree
      do i = 1,n
        call check_neighbors( stencil, i, n )
      end do
      ! iterate through cells and calculate minimum degree
      call get_distance( stencil, n, stencil_idx, face_idx, min_degree )

      cnt = 0
      in_it = 0
      current_min_degree = min_degree
      do while ( ( min_degree == current_min_degree ).and.(in_it < in_it_max) )

        in_it = in_it + 1

        if (stencil_idx == -1) then
          write(msg,msg_FMT) n
          n_out = n
          ! ierr = warning_message(verbose_level,name,msg)
          exit
        end if
        if ( .not.any(stencil(stencil_idx)%free) ) then
          exit
        end if

        idx1 = stencil(stencil_idx)%nbor_idx(:,face_idx)

        ! increment the counter
        cnt = cnt + 1

        ! add the new cell
        call stencil(n+cnt)%create( idx1(1), idx1(2:4), block_info_list )

        ! set the degree
        stencil(n+cnt)%degree = min_degree + 1

        ! check for any edge cases
        do i = 1,n+cnt
          call check_neighbors( stencil, i, n+cnt )
        end do
        ! iterate through cells and calculate new degrees
        call get_distance( stencil, n, stencil_idx, face_idx, min_degree )
      end do
      ! increment the counter
      n = n + cnt
      if ( flag ) exit
    end do
    n_out = n
  end subroutine build_stencil_connected_block

  pure subroutine get_distance( stencil, n_cells, stencil_idx, face_idx, min_degree )
    use stencil_cell_derived_type, only : stencil_cell_t
    use stencil_indexing, only : mask_in_bounds, get_offsets, get_linear_face_idx
    type(stencil_cell_t), dimension(:), intent(inout) :: stencil
    integer,                            intent(in)    :: n_cells
    integer,                            intent(out)   :: stencil_idx, face_idx, min_degree
    
    integer,  dimension(6*n_cells) :: degree
    integer,  dimension(6*n_cells) :: stencil_indices
    integer,  dimension(6*n_cells) :: mask_indices
    logical,  dimension(6*n_cells) :: min_mask
    integer,  dimension(3)   :: idx
    integer,  dimension(1)   :: min_idx, degree_tmp
    integer :: i, j, n, cnt, block_id

    stencil_idx = -1
    face_idx    = -1
    min_degree  = 6*n_cells
    degree(:)          = 1
    stencil_indices(:) = 1

    cnt = 0
    ! iterate through cells and calculate distances
    do j = 1,n_cells
      if ( .not. any( stencil(j)%free ) ) cycle
      ! for each neighbor
      do i = 1,6
        if ( stencil(j)%free(i) ) then
          cnt = cnt + 1
          degree(cnt) = stencil(j)%degree
          min_degree  = minval( degree(1:cnt) )
          block_id    = stencil(j)%nbor_idx(1,i)
          idx         = stencil(j)%nbor_idx(2:4,i)
          stencil_indices(cnt) = j
          mask_indices(cnt)    = i
        end if
      end do
    end do

    if (cnt > 0) then
      ! find the 1st minimum index
      min_idx = minloc( degree(1:cnt) )
        
      ! Now grab any other indices that match this distance
      degree_tmp = degree( min_idx(1) )
      min_mask = ( degree == degree_tmp(1) )
      
      n = count(min_mask)
      stencil_indices(1:n) = pack(stencil_indices,min_mask)
      mask_indices(1:n)    = pack(mask_indices,   min_mask)

      face_idx    = mask_indices( min_idx(1) )
      stencil_idx = stencil_indices( min_idx(1) )
      min_degree  = degree( min_idx(1) )
    end if

  end subroutine get_distance

end module stencil_growing_routines

module combinatorics
  implicit none
  private
  public :: nchoosek
  public :: get_exponents
contains

  pure function nchoosek( n, k ) result( c )
    integer, intent(in) :: n, k
    integer             :: c
    integer :: i
    c = 0
    if (k>n) return

    c = 1
    do i = 1, min(n-k,k)
      c = c * ( n - (i-1) )
      c = c / i
    end do
  end function nchoosek

  pure subroutine get_exponents(n_dim,degree,n_terms,exponents,idx,diff_idx)
    use index_conversion, only : global2local
    integer, intent(in) :: n_dim, degree, n_terms
    integer, dimension(n_dim,n_terms), intent(out) :: exponents
    integer, dimension(0:degree),      intent(out) :: idx
    integer, dimension(n_dim,n_terms), optional, intent(out) :: diff_idx
    integer :: curr_total_degree, i, j, cnt, N_full_terms
    integer, dimension(n_dim) :: tmp_exp, nsub
    cnt = 0
    do curr_total_degree = 0,degree
      ! idx(curr_total_degree+1) = cnt + 1
      nSub = curr_total_degree + 1
      N_full_terms = (curr_total_degree+1) ** n_dim
      do j = 0,N_full_terms
        tmp_exp = global2local(j+1,nsub)-1
        if ( sum(tmp_exp) == curr_total_degree ) then
          cnt = cnt + 1
          exponents(:,cnt) = tmp_exp
        end if
      end do
      idx(curr_total_degree) = cnt
      tmp_exp = 0
    end do

    ! determine corresponding gradient terms of a given term
    if (present(diff_idx)) then
      diff_idx = -1 ! last terms (idx(degree)+1:idx(degree+1)) are not defined
      if ( degree==0) return
      do j = 1,idx(degree-1)
        tmp_exp = exponents(:,j)
        curr_total_degree = sum(tmp_exp)
        cnt = 0
        do i = idx(curr_total_degree)+1,idx(curr_total_degree+1)
          if ( sum( abs( exponents(:,i) - tmp_exp ) )==1 ) then
            cnt = cnt + 1
            diff_idx(cnt,j) = i
          end if
        end do
      end do
    end if
  end subroutine get_exponents

end module combinatorics

module math
  use set_precision, only : dp
  implicit none
  private
  public :: cross_product, det_3x3, vector_norm
  public :: LUdecomp, LUsolve, mat_inv
  public :: LegendrePolynomialAndDerivative, LegendreGaussNodesAndWeights
  public :: maximal_extents
  public :: rand_int_in_range
  public :: compute_pseudo_inverse
  public :: careful_divide

  interface LUsolve
    module procedure LUsolve_single_rhs
    module procedure LUsolve_multiple_rhs
  end interface
contains

  pure elemental function careful_divide(num,den) result(q)
    use set_constants, only : near_zero
    real(dp), intent(in) :: num, den
    real(dp)             :: q
    q = num / merge( sign(near_zero,den), den, abs(den)<near_zero )
  end function careful_divide


  impure elemental function rand_int_in_range(lo,hi) result(num)
    integer, intent(in) :: lo, hi
    integer             :: num
    real(dp) :: harvest
    call random_number(harvest)
    num = nint( harvest*real(hi-lo,dp) + real(lo,dp) )
  end function rand_int_in_range

  pure function cross_product( vec1, vec2 )
    real(dp), dimension(3), intent(in) :: vec1, vec2
    real(dp), dimension(3)             :: cross_product
    cross_product(1) =  ( vec1(2)*vec2(3) - vec1(3)*vec2(2) )
    cross_product(2) = -( vec1(1)*vec2(3) - vec1(3)*vec2(1) )
    cross_product(3) =  ( vec1(1)*vec2(2) - vec1(2)*vec2(1) )
  end function cross_product

  pure function det_3x3( mat )
    real(dp), dimension(3,3), intent(in) :: mat
    real(dp)                             :: det_3x3
    continue
    det_3x3 = mat(1,1)*(mat(2,2)*mat(3,3)-mat(2,3)*mat(3,2)) &
            - mat(1,2)*(mat(2,1)*mat(3,3)-mat(2,3)*mat(3,1)) &
            + mat(1,3)*(mat(2,1)*mat(3,2)-mat(2,2)*mat(3,1))
  end function det_3x3

  pure function vector_norm( vector )
    use set_precision, only : dp
    use set_constants, only : zero
    real(dp), dimension(:), intent(in) :: vector
    real(dp)                           :: vector_norm
    integer :: i
    vector_norm = zero
    do i = 1, size(vector)
      vector_norm = vector_norm + vector(i)**2
    end do
    vector_norm = sqrt( vector_norm )
  end function vector_norm

  pure subroutine LUdecomp( LU, P, A, m )
    use set_precision, only : dp
    use set_constants, only : zero, one
    real(dp), dimension(m,m), intent(out) :: LU,P
    real(dp), dimension(m,m), intent(in)  :: A
    integer,                  intent(in)  :: m
    real(dp), dimension(m) :: ctemp1, LUtemp
    integer  :: col, row, maxi, ipr
    real(dp) :: factor
    LU = A
    P = zero
    do col = 1,m
      P(col,col) = one
    end do
    do col = 1,m-1
      maxi=maxloc(abs(LU(col:m,col)),1) !row pivot
      ipr=maxi+col-1
      if (ipr.ne.col) then
        ctemp1 = LU(ipr,:)
        LU(ipr,:) = LU(col,:)
        LU(col,:) = ctemp1
        ctemp1 = P(ipr,:)
        P(ipr,:) = P(col,:)
        P(col,:) = ctemp1
      end if
      if ( abs(LU(col,col)) > zero ) then
        do row = col+1,m
          factor = LU(row,col)/LU(col,col)
          LUtemp(col+1:m) = LU(row,col+1:m) - factor*LU(col,col+1:m)
          LU(row,col+1:m) = LUtemp(col+1:m)
          LU(row,col) = factor
        end do
      end if
    end do
  end subroutine LUdecomp

  pure subroutine LUsolve_single_rhs( x, LU, P, bin, m )
    use set_precision, only : dp
    real(dp), dimension(m),   intent(out) :: x
    real(dp), dimension(m,m), intent(in)  :: LU, P
    real(dp), dimension(m),   intent(in)  :: bin
    integer,                  intent(in)  :: m
    integer :: i, row
    real(dp), dimension(m) :: b, d
    b = matmul(P,bin) ! Permute b matrix
    d(1) = b(1) ! Forward substitution
    do row = 2,m
      d(row) = b(row) - sum( LU(row,1:row-1)*d(1:row-1) )
    end do
    x(m) = d(m)/LU(m,m) ! Backward substitution
    do i = 1,m-1
      row = m-i
      x(row) = ( d(row) - sum( LU(row,row+1:m)*x(row+1:m) ) ) / LU(row,row)
    end do
  end subroutine LUsolve_single_rhs

  pure subroutine LUsolve_multiple_rhs(x,LU,P,bin,m,n_rhs)
    real(dp), dimension(m,n_rhs), intent(out) :: x
    real(dp), dimension(m,m),     intent(in)  :: LU, P
    real(dp), dimension(m,n_rhs), intent(in)  :: bin
    integer,                      intent(in)  :: m, n_rhs
    integer :: n
    do n = 1, n_rhs
      call LUsolve_single_rhs(x(:,n),LU,P,bin(:,n),m)
    end do
  end subroutine LUsolve_multiple_rhs

  subroutine mat_inv( mat, inv, n )
    use set_precision, only : dp
    use set_constants, only : zero, one
    integer,                  intent(in)  :: n
    real(dp), dimension(n,n), intent(in)  :: mat
    real(dp), dimension(n,n), intent(out) :: inv
    integer                  :: i
    real(dp), dimension(n)   :: b
    real(dp), dimension(n,n) :: lu, p
    call ludecomp( lu, p, mat, n )
    inv = zero
    do i = 1,n
      b = zero
      b(i) = one
      call lusolve( inv(:,i), lu, p, b, n )
    end do
  end subroutine mat_inv

  elemental subroutine LegendrePolynomialAndDerivative(N,x,LN,dLN)
    use set_constants, only : zero, one, two
    integer, intent(in) :: N
    real(dp), intent(in) :: x
    real(dp), intent(out) :: LN, dLN
    real(dp) :: LNm2, LNm1, dLNm2, dLNm1
    integer :: j
    if (N == 0) then
      LN = one
      dLN = zero
    elseif (N == 1) then
      LN = x
      dLN = one
    else
      LNm2 = one
      LNm1 = x
      dLNm2 = zero
      dLNm1 = one
      do j = 2,N
        LN = real(2*j-1,dp)/real(j,dp) * x * LNm1 &
          - real(j-1,dp)/real(j,dp) * LNm2
        dLN = dLNm2 + real(2*j-1,dp) * LNm1
        LNm2 = LNm1
        LNm1 = LN
        dLNm2 = dLNm1
        dLNm1 = dLN
      end do
    end if
  end subroutine LegendrePolynomialAndDerivative

  pure subroutine LegendreGaussNodesAndWeights(N,x,w)
    use set_constants, only : zero, one, two, four, third, pi
    integer,                  intent(in)  :: N
    real(dp), dimension(N+1), intent(out) :: x, w
    integer :: j, k
    real(dp) :: eps4, delta, LNp1, dLNp1
    integer, parameter :: quad_n_iter = 10
    eps4 = four*epsilon(one)
    x = zero
    w = zero

    if (N == 0) then
      x(1) = zero
      w(1) = two
    elseif (N == 1) then
      x(1) = -sqrt(third)
      w(1) = one
      x(2) = -x(1)
      w(2) = w(1)
    else
      do j = 0,(N+1)/2 - 1
        x(j+1) = -cos( ( real(2*j+1,dp)/real(2*N+2,dp) )*pi )
        do k = 1,quad_n_iter
          call LegendrePolynomialAndDerivative(N+1,x(j+1),LNp1,dLNp1)
          delta = -LNp1/dLNp1
          x(j+1) = x(j+1) + delta
          if ( abs(delta) <= eps4*abs(x(j+1)) ) then
            exit
          end if
        end do
        call LegendrePolynomialAndDerivative(N+1,x(j+1),LNp1,dLNp1)
        x(N+1-j) = -x(j+1)
        w(j+1) = two/( (one-x(j+1)**2)*dLNp1**2)
        w(N+1-j) = w(j+1)
      end do
      if (mod(N,2) == 0) then
        call LegendrePolynomialAndDerivative(N+1,zero,LNp1,dLNp1)
        x(N/2+1) = zero
        w(N/2+1) = two/dLNp1**2
      end if
    end if
  end subroutine LegendreGaussNodesAndWeights

  pure function maximal_extents(dim,n_points,points) result(d)
    use set_constants, only : half
    integer, intent(in) :: dim, n_points
    real(dp), dimension(dim,n_points), intent(in) :: points
    real(dp), dimension(dim) :: d
    d = half*(maxval(points,dim=2) - minval(points,dim=2))
  end function maximal_extents

!=========================== compute_pseudo_inverse ==========================80
!>
!! Description: Computes the pseudo-inverse of the reconstruction LHS.
!!
!! Inputs:      LHS_m: Extent of reconstruction LHS
!!              LHS_n: Extent of reconstruction LHS
!!              LHS:   Reconstruction LHS
!!
!! Outputs:     LHS_plus: Pseudo-inverse of reconstruction LHS
!<
!=============================================================================80
  subroutine compute_pseudo_inverse( LHS_m, LHS_n, LHS, LHS_plus, cond )

    use set_precision, only : dp
    use set_constants, only : zero, one, ten

    external dgesvd

    integer,                          intent(in)  :: LHS_m, LHS_n
    ! real(dp), dimension(LHS_m,LHS_n), intent(in)  :: LHS
    real(dp), dimension(:,:),         intent(in)  :: LHS
    real(dp), dimension(LHS_n,LHS_m), intent(out) :: LHS_plus
    real(dp), optional,               intent(out) :: cond

    integer :: LDA, LDU, LDVT
    integer :: max_LHS_extents, min_LHS_extents
    integer :: LWORK
    integer :: INFO
    integer :: i, j
    integer :: min_s_loc

    real(dp) :: machine_precision
    real(dp) :: abs_tolerance
    real(dp) :: rel_tolerance

    real(dp), dimension(LHS_m,LHS_n)      :: LHS_cpy
    real(dp), dimension(:),   allocatable :: S
    real(dp), dimension(:,:), allocatable :: U
    real(dp), dimension(:,:), allocatable :: VT
    real(dp), dimension(:),   allocatable :: WORK
    real(dp), dimension(:),   allocatable :: Sinv
    real(dp), dimension(:),   allocatable :: V_Sinv

    continue

    ! Setup
    LHS_plus = zero

    LHS_cpy = LHS(1:LHS_m,1:LHS_n)

    ! Define SVD Parameters
    LDA   = LHS_m
    LDU   = LHS_m
    LDVT  = LHS_n

    max_LHS_extents = max(LHS_m,LHS_n)
    min_LHS_extents = min(LHS_m,LHS_n)

    LWORK = max( 3*min_LHS_extents + max_LHS_extents, 5*min_LHS_extents )

    ! Allocate Storage for the Singular Value Decomposition
    allocate( S(min_LHS_extents) )
    allocate( U(LHS_m,LHS_m) )
    allocate( VT(LHS_n,LHS_n) )
    allocate( WORK(LWORK) )
    allocate( Sinv( min_LHS_extents ) )
    allocate( V_Sinv( LHS_m ) )

    ! Compute the Singular Value Decomposition of the Reconstruction LHS
    ! Note: dgesvd = LAPACK routine
    ! Note: dgesdd makes assumptions about floating point arithmetic.
    call dgesvd( 'A', 'A', LHS_m, LHS_n, LHS_cpy, LDA, S, U, LDU, VT, LDVT,        &
                 WORK, LWORK, INFO )
    !call sgesvd( 'A', 'A', LHS_m, LHS_n, LHS, LDA, S, U, LDU, VT, LDVT,        &
    !             WORK, LWORK, INFO )


    ! Condition Number
    if ( present(cond) ) then
      min_s_loc = max(min_LHS_extents,count(S>zero))
      cond = S(1)/S(min_s_loc)
    end if
    
    ! Determine Truncation Tolerance
    machine_precision = (ten)**(-precision(one))
    abs_tolerance     = sqrt(machine_precision)
    rel_tolerance     = S(1)*abs_tolerance

    ! Compute Inverse of Singular Values
    Sinv = zero

    do i = 1, min_LHS_extents
      if ( S(i) <= rel_tolerance ) then
        ! Truncate Singular Value
        Sinv(i) = zero
      else
        Sinv(i) = one/S(i)
      end if
    end do

    ! Compute Pseudo-Inverse
    do i = 1, LHS_n
      V_Sinv = zero
      do j = 1, LHS_n
        V_Sinv(j) = VT(j,i)*Sinv(j)
      end do

      do j = 1, LHS_m
        LHS_plus(i,j) = dot_product( V_Sinv, U(j,:) )
      end do
    end do

    ! Deallocate Storage
    deallocate( S      )
    deallocate( U      )
    deallocate( VT     )
    deallocate( WORK   )
    deallocate( Sinv   )
    deallocate( V_Sinv )

  end subroutine compute_pseudo_inverse

end module math

module vector_derived_type
  use set_precision, only : dp
  use set_constants, only : zero
  implicit none
  private
  public :: face_vec
  public :: face_vec_ptr_3D
  type face_vec
    integer :: n
    real(dp), allocatable, dimension(:,:) :: v
  contains
    private
    procedure, public, pass :: create  => allocate_face_vec
    procedure, public, pass :: destroy => deallocate_face_vec
  end type face_vec

  type face_vec_ptr_3D
    type(face_vec), dimension(:,:,:), pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_face_vec_ptr_3D
  end type face_vec_ptr_3D

contains

  subroutine allocate_face_vec( this, n )
    class(face_vec), intent(inout) :: this
    integer,       intent(in)      :: n
    continue
    this%n = n
    allocate( this%v(3,n) )
    this%v = zero
  end subroutine allocate_face_vec

  pure elemental subroutine deallocate_face_vec( this )
    class(face_vec), intent(inout) :: this
    continue
    this%n = 0
    if( allocated( this%v  ) ) deallocate( this%v )
  end subroutine deallocate_face_vec

  pure elemental subroutine destroy_face_vec_ptr_3D( this )
    class(face_vec_ptr_3D), intent(inout) :: this
    this%p => null()
  end subroutine destroy_face_vec_ptr_3D

end module vector_derived_type

module pointers
  use set_precision, only : dp
  implicit none
  private
  public :: array_ptr_3D_real, array_ptr_4D_real

  type array_ptr_3D_real
    real(dp), dimension(:,:,:),     pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_real_3D
  end type array_ptr_3D_real

  type array_ptr_4D_real
    real(dp), dimension(:,:,:,:),   pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_real_4D
  end type array_ptr_4D_real

contains

  pure elemental subroutine destroy_real_3D( this )
    class(array_ptr_3D_real), intent(inout) :: this
    this%p => null()
  end subroutine destroy_real_3D

  pure elemental subroutine destroy_real_4D( this )
    class(array_ptr_4D_real), intent(inout) :: this
    this%p => null()
  end subroutine destroy_real_4D
end module pointers

module linspace_helper
  use set_precision, only : dp
  implicit none
  private
  public :: linspace, meshgrid2, meshgrid3
  public :: map_1D_fun, cartesian_mesh, perturb_mesh

  interface
    pure function map_1D_fun(x_in) result(x_out)
      use set_precision, only : dp
      real(dp), dimension(:), intent(in) :: x_in
      real(dp), dimension(size(x_in)) :: x_out
    end function map_1D_fun
  end interface
contains

  pure function cartesian_mesh(nx,ny,nz,end_pts,x_fun,y_fun,z_fun) result(xyz)
    use set_constants, only : zero, one
    integer, intent(in) :: nx, ny, nz
    real(dp), dimension(3,2), optional, intent(in) :: end_pts
    procedure(map_1D_fun), optional :: x_fun, y_fun, z_fun
    real(dp), dimension(3,nx,ny,nz) :: xyz
    real(dp), dimension(nx) :: x_vec1, x_vec2
    real(dp), dimension(ny) :: y_vec1, y_vec2
    real(dp), dimension(nz) :: z_vec1, z_vec2

    if ( present(end_pts) ) then
      x_vec1 = linspace(nx,end_pts(1,1),end_pts(1,2))
      y_vec1 = linspace(ny,end_pts(2,1),end_pts(2,2))
      z_vec1 = linspace(nz,end_pts(3,1),end_pts(3,2))
    else
      x_vec1 = linspace(nx,zero,one)
      y_vec1 = linspace(ny,zero,one)
      z_vec1 = linspace(nz,zero,one)
    end if

    if ( present(x_fun) ) then
      x_vec2 = x_vec1
      x_vec1 = x_fun(x_vec2)
    end if

    if ( present(y_fun) ) then
      y_vec2 = y_vec1
      y_vec1 = y_fun(y_vec2)
    end if

    if ( present(z_fun) ) then
      z_vec2 = z_vec1
      z_vec1 = z_fun(z_vec2)
    end if

    xyz = cartesian_mesh_coords(x_vec1,y_vec1,z_vec1)
  end function cartesian_mesh

  pure function cartesian_mesh_coords(x_vec,y_vec,z_vec) result(xyz)
    real(dp), dimension(:), intent(in) :: x_vec, y_vec, z_vec
    real(dp), dimension(3,size(x_vec),size(y_vec),size(z_vec)) :: xyz
    call meshgrid3( x_vec, y_vec, z_vec, xyz(1,:,:,:),xyz(2,:,:,:),xyz(3,:,:,:) )
  end function cartesian_mesh_coords

  subroutine perturb_mesh(xyz,delta,include_boundaries)
    use index_conversion, only : in_bound
    use set_constants, only : zero, one, two, large
    real(dp), dimension(:,:,:,:), intent(inout) :: xyz
    real(dp),                     intent(in)    :: delta
    logical, optional,            intent(in)    :: include_boundaries
    integer, dimension(4) :: sz_tmp
    integer, dimension(3) :: n_nodes, o, min_bnd, max_bnd
    real(dp), dimension(3) :: mult
    real(dp), allocatable, dimension(:,:,:,:) :: xyz_tmp
    real(dp) :: diff, h0
    integer :: i, j, k, d, s, n_dim
    sz_tmp = shape(xyz)
    n_dim   = sz_tmp(1)
    n_nodes = sz_tmp(2:4)

    mult = zero
    where ( n_nodes > 2 ) mult = delta

    allocate( xyz_tmp(n_dim,0:n_nodes(1)+1,0:n_nodes(2)+1,0:n_nodes(3)+1) )
    xyz_tmp = large
    xyz_tmp(:,1:n_nodes(1),1:n_nodes(2),1:n_nodes(3)) = xyz

    max_bnd = max(n_nodes-1,1)
    min_bnd = min([2,2,2],max_bnd)

    if (present(include_boundaries) ) then
      if ( include_boundaries ) then
        min_bnd = [1,1,1]
        max_bnd = n_nodes
      end if
    end if

    do k = 1,n_nodes(3)
      do j = 1,n_nodes(2)
        do i = 1,n_nodes(1)
          if ( in_bound(3,[i,j,k],min_bnd,max_bnd) ) then
            do d = 1,n_dim
              o = 0
              h0 = large
              do s = -1,1,2
                o(d) = s
                h0 = min( h0, abs( xyz_tmp(d,i,j,k) - xyz_tmp(d,i+o(1),j+o(2),k+o(3)) ) )
              end do
              call random_number(diff); diff = two*diff - one
              xyz(d,i,j,k) = xyz(d,i,j,k) + mult(d)*diff*h0
            end do
          end if
        end do
      end do
    end do

    deallocate( xyz_tmp )

  end subroutine perturb_mesh

  pure function linspace(N,x1,x2) result(array)
    integer,  intent(in)   :: N
    real(dp), intent(in)   :: x1, x2
    real(dp), dimension(N) :: array
    real(dp) :: range_den
    integer :: i
    if (N==0) return
    if (N==1) then
      array(1) = x1
      return
    end if
    range_den = (x2-x1)/real(N-1,dp)
    do i = 1,N
      array(i) = x1 + range_den*real(i-1,dp)
    end do
  end function linspace

  pure subroutine meshgrid2(x1,x2,x1_array,x2_array)
    real(dp), dimension(:),   intent(in)  :: x1, x2
    real(dp), dimension(:,:), intent(out) :: x1_array, x2_array
    integer :: N1, N2
    N1 = size(x1)
    N2 = size(x2)
    x1_array = spread(x1,2,N2)
    x2_array = spread(x2,1,N1)
  end subroutine meshgrid2

  pure subroutine meshgrid3(x1,x2,x3,x1_array,x2_array,x3_array)
    real(dp), dimension(:),     intent(in)  :: x1, x2, x3
    real(dp), dimension(:,:,:), intent(out) :: x1_array, x2_array, x3_array
    real(dp), dimension(size(x1),size(x2)) :: x1_tmp
    real(dp), dimension(size(x2),size(x3)) :: x2_tmp
    real(dp), dimension(size(x2),size(x3),size(x1)) :: x2_tmp2
    real(dp), dimension(size(x3),size(x1)) :: x3_tmp
    real(dp), dimension(size(x3),size(x1),size(x2)) :: x3_tmp2
    integer, parameter, dimension(3) :: o2 = [2,3,1], o3 = [3,1,2]
    integer :: N1, N2, N3
    N1 = size(x1)
    N2 = size(x2)
    N3 = size(x3)

    x1_tmp   = spread(x1,2,N2)
    x2_tmp   = spread(x2,2,N3)
    x3_tmp   = spread(x3,2,N1)
    x1_array = spread(x1_tmp,3,N3)
    x2_tmp2  = spread(x2_tmp,3,N1)
    x3_tmp2  = spread(x3_tmp,3,N2)
    x2_array = reshape(x2_tmp2,shape(x2_array),order=o2)
    x3_array = reshape(x3_tmp2,shape(x3_array),order=o3)
  end subroutine meshgrid3

end module linspace_helper

module tecplot_output
  use set_precision, only : dp
  use set_constants, only : zero
  use message,       only : error_message
  implicit none
  private
  public :: write_tecplot_ordered_zone_header
  public :: write_tecplot_ordered_zone_block_packed
  character(*), dimension(6), parameter :: data_types=['DOUBLE  ','SINGLE  ',  &
                                                       'LONGINT ','SHORTINT',  &
                                                       'BYTE    ','BIT     ' ]
  character(*), dimension(6), parameter :: formats = ['(ES23.15)','(ES16.7) ', &
                                                      '(I11)    ','(I6)     ', &
                                                      '(I4)     ','(I1)     ']

  interface write_tecplot_size_fmt
    ! module procedure :: write_tecplot_size_fmt_fe
    module procedure :: write_tecplot_size_fmt_ordered
  end interface write_tecplot_size_fmt
contains

! subroutine write_tecplot_zone_header( fid, n_dim, n_nodes, n_cells,            &
!                                       n_node_vars, n_cell_vars, zone_name,     &
!                                       data_packing, data_format, var_share,    &
!                                       strand_id, solution_time )
!     integer,                                intent(in) :: fid, n_dim, n_vars
!     integer,      dimension(:),             intent(in) :: n_nodes
!     character(*), dimension(:),             intent(in) :: var_names
!     integer,                      optional, intent(in) :: n_cells
!     integer,                      optional, intent(in) :: n_cell_vars
!     character(*),                 optional, intent(in) :: zone_name
!     integer,                      optional, intent(in) :: data_packing
!     integer, dimension(:),        optional, intent(in) :: data_format
!     integer, dimension(:),        optional, intent(in) :: var_share
!     integer,                      optional, intent(in) :: strand_id
!     real(dp),                     optional, intent(in) :: solution_time
!     integer,                            intent(in) :: fid, n_dim
!     integer,                            intent(in) :: n_nodes, n_cells
!     integer,                            intent(in) :: n_node_var, n_cell_var
!     character(*),                       intent(in) :: zone_name
    
!     integer, dimension(:),    optional, intent(in) :: data_format
!     integer,                  optional, intent(in) :: strand_id
!     real(dp),                 optional, intent(in) :: solution_time
!     if ( present(n_cells) ) then
!       call write_tecplot_fe_brick_zone_header( fid, n_dim, n_nodes, n_cells,  &
!                                                  n_node_vars, n_cell_vars,        &
!                                                  zone_name, var_name,           &
!                                                  data_format,                   &
!                                                  strand_id, solution_time )
! end subroutine write_tecplot_zone_header
! subroutine write_tecplot_ordered_zone_header( fid, n_dim, n_nodes,           &
!                                                 n_node_vars, n_cell_vars,      &
!                                                 zone_name,                     &
!                                                 data_packing,                  &
!                                                 data_format,                   &
!                                                 var_share,                     &
!                                                 strand_id,                     &
!                                                 solution_time)!,               &
! subroutine write_tecplot_fe_brick_zone_header( fid, n_dim, n_nodes, n_cells,  &
!                                                  n_node_vars, n_cell_vars,        &
!                                                  zone_name, var_name,           &
!                                                  data_format,                   &
!                                                  strand_id, solution_time )

  ! subroutine write_tecplot_size_fmt_fe( n_dim, n_nodes, n_cells, fmt )
  !   integer,      intent(in)  :: n_dim, n_nodes, n_cells
  !   character(*), intent(out) :: fmt
  !   logical :: err
  !   character(*), parameter :: routine_name = 'write_tecplot_size_fmt_fe'
  !   character(*), parameter :: fmt_2D = "ZONETYPE=FEQUADRILATERAL, "
  !   character(*), parameter :: fmt_3D = "ZONETYPE=FEBRICK, "
  !   character(*), parameter :: fmt_ne = "(A,', NODES=',I0,', ELEMENTS=',I0,',')"
  !   select case(n_dim)
  !   case(2)
  !     write(fmt,fmt_ne) fmt_2D, n_nodes, n_cells 
  !   case(3)
  !     write(fmt,fmt_ne) fmt_3D, n_nodes, n_cells
  !   case default
  !     err = error_message(routine_name,"Only n_dim=2 or 3 supported")
  !   end select
  ! end subroutine write_tecplot_size_fmt_fe

  subroutine write_tecplot_size_fmt_ordered( n_dim, n_nodes, fmt )
    integer,               intent(in)  :: n_dim
    integer, dimension(:), intent(in)  :: n_nodes
    character(*),          intent(out) :: fmt
    character(*), dimension(3), parameter :: IJK = ['I','J','K']
    character(*), parameter :: zone_type_fmt = 'ZONETYPE=ORDERED'
    character(*), parameter :: sz_fmt1 = "(""('"",(A,""=',I0,', ""),"
    character(*), parameter :: sz_fmt2 = "(A,""=',I0,', ""),""')"")"
    if (n_dim == 1) then
      write( fmt, '("I=",I0,",")') n_nodes(1)
    else
      write( fmt, '(A,I0,A)') sz_fmt1, n_dim-1, sz_fmt2
      write( fmt,trim(fmt)) IJK(1:n_dim)
      write( fmt,trim(fmt)) n_nodes(1:n_dim)
    end if
    fmt = zone_type_fmt//", "//trim(fmt)
  end subroutine write_tecplot_size_fmt_ordered

  subroutine write_tecplot_var_fmt( var_name, fmt )
    character(*), dimension(:),  intent(in)  :: var_name
    character(*),                intent(out) :: fmt
    integer               :: n_vars, i
    n_vars = size(var_name)
    write(fmt,'((A),I0,(A))') "('VARIABLES = ',", n_vars-1,                  &
                  "('""',(A),'""',', '),'""',(A),'""')"
    write(fmt,trim(fmt)) (trim(var_name(i)),i=1,n_vars)
  end subroutine write_tecplot_var_fmt

  subroutine write_tecplot_loc_fmt( n_node_vars, n_cell_vars, fmt )
    integer,      intent(in)  :: n_node_vars, n_cell_vars
    character(*), intent(out) :: fmt
    integer                   :: loc_ind, i
    integer, dimension(4)     :: loc_range
    character(100)            :: loc_cell, loc_nodal
    loc_ind   = 1
    loc_range = 1
    if ( n_node_vars > 0 ) then
      write(loc_nodal,'(A)') "('[',I0,'-',I0,']=NODAL'"
      if (n_cell_vars>0) then; loc_nodal = trim(loc_nodal)//",',')"
      else;                    loc_nodal = trim(loc_nodal)//")"
      end if
      loc_range(2) = n_node_vars
      loc_ind = 2
    else
      write(loc_nodal,'(A)') ''
    end if

    if ( n_cell_vars > 0 ) then
      write(loc_cell,'(A)') "'[',I0,'-',I0,']=CELLCENTERED')"
      if (n_node_vars>0) then; loc_cell = ",("//trim(loc_cell)
      else;                    loc_cell = "("//trim(loc_cell)
      end if
      loc_range(3) = n_node_vars + 1
      loc_range(4) = n_node_vars + n_cell_vars
      loc_ind = 4
    else
      write(loc_cell,'(A)') ''
    end if
    write(fmt,'(A)') "('VARLOCATION=(',"//trim(loc_nodal)//                  &
                                          trim(loc_cell)//"')')"
    write(fmt,trim(fmt)) (loc_range(i),i=1,loc_ind)
  end subroutine write_tecplot_loc_fmt


  subroutine write_tecplot_pack_fmt(n_cell_vars,data_packing,fmt)
    integer, intent(in) :: n_cell_vars
    character(*), intent(in) :: data_packing
    character(*), intent(out) :: fmt
    character(*), parameter :: routine_name = 'write_tecplot_pack_fmt'
    logical :: err
    select case(data_packing)
      case('point')
        if (n_cell_vars > 0) then
          err = error_message(routine_name, 'data_packing must be "block" '//  &
                                            'for cell-centered data' )
        end if
        write( fmt, '(A)' ) 'DATAPACKING=POINT'
      case('block')
        write( fmt, '(A)' ) 'DATAPACKING=BLOCK'
      case default
        err = error_message(routine_name, 'unrecognized data_packing! must '// &
                                          'be "point" or "block")' )
    end select
  end subroutine write_tecplot_pack_fmt

  subroutine write_tecplot_data_type_fmt( n_vars, fmt, data_format )
    integer,                              intent(in)  :: n_vars
    character(*),                         intent(out) :: fmt
    integer, dimension(n_vars), optional, intent(in)  :: data_format
    character(*), parameter :: routine_name = 'write_tecplot_data_type_fmt'
    integer :: i
    logical :: err
    write(fmt,'((A),I0,(A))') "('DT=(',(A),", n_vars-1,"(',',(A))')')"
    if ( present(data_format) ) then
      if ( any(data_format<1).or.any(data_format>6) ) then
        err = error_message(routine_name,'data_format must be in range [1,6]')
      end if
      write(fmt,trim(fmt)) ( trim( data_types( data_format(i) ) ),i=1,n_vars)
    else
      write(fmt,trim(fmt)) (trim(data_types(1)),i=1,n_vars)
    end if
  end subroutine write_tecplot_data_type_fmt

  subroutine write_tecplot_var_share_fmt( var_info, fmt )
  ! assume donor zone is first integer in var_info
    integer, dimension(:),       intent(in)  :: var_info
    character(*),                intent(out) :: fmt
    character(*), parameter :: routine_name = 'write_tecplot_var_share_fmt'
    integer :: n_vars, zone_num, i
    logical :: err
    n_vars = size(var_info) - 1
    if (n_vars < 1) then
      err = error_message(routine_name,'No variables were specified to be shared')
    end if
    zone_num = var_info(1)
    write(fmt,'((A),I0,(A),I0,(A))') "('VARSHARELIST=([',I0,", n_vars-1,       &
                                                 "(',',I0),']=", zone_num, ")')"
    write(fmt,trim(fmt)) ( var_info(i+1),i=1,n_vars)
  end subroutine write_tecplot_var_share_fmt

  !======================== write_tecplot_file_header ==========================80
  !>
  !! Generic routine for writing header info in Tecplot ASCII file
  !<
  !=============================================================================80
  ! subroutine write_tecplot_file_header( fid, var_names, title, filetype )

  !   integer,                            intent(in) :: fid
  !   character(*), dimension(:),         intent(in) :: var_names
  !   character(*), optional,             intent(in) :: title
  !   integer,      optional,             intent(in) :: filetype ! 0, 1, or 2

  !   logical               :: err
  !   character(1024)       :: var_fmt, title_fmt

  !   character(*), parameter :: routine_name = 'write_tecplot_file_header'

  !   call write_tecplot_var_fmt(var_names,var_fmt)

  !   write( fid, * )
  !   if ( present(title) ) then
  !     write(title_fmt,"('TITLE=','""',(A),'""')") trim(title)
  !     write( fid, '(A)' ) trim( title_fmt )
  !   end if
  !   if ( present(filetype) ) then
  !     select case(filetype)
  !     case(0)
  !       write( fid, '(A)' ) 'FILETYPE=FULL'
  !     case(1)
  !       write( fid, '(A)' ) 'FILETYPE=GRID'
  !     case(2)
  !       write( fid, '(A)' ) 'FILETYPE=SOLUTION'
  !     case default
  !       err = error_message(routine_name, 'unrecognized filetype! must be '//  &
  !                               '0 ("FULL"), 1 ("GRID"), or 2 ("SOLUTION")' )
  !     end select
  !   end if
  !   write( fid, '(A)' ) trim( var_fmt )

  ! end subroutine write_tecplot_file_header

!===================== write_tecplot_ordered_zone_header =====================80
!>
!! Generic routine for writing ordered zone header info in Tecplot ASCII file
!<
!=============================================================================80
  subroutine write_tecplot_ordered_zone_header( fid, n_dim, n_nodes,           &
                                                n_node_vars,                   &
                                                n_cell_vars,                   &
                                                zone_name,                     &
                                                var_names,                     &
                                                var_share,                     &
                                                data_packing,                  &
                                                data_format,                   &
                                                strand_id,                     &
                                                solution_time )!,              &
                                                ! aux_data )
    use set_constants, only : max_text_line_length
    integer,                                intent(in) :: fid, n_dim
    integer, dimension(:),                  intent(in) :: n_nodes
    integer,                                intent(in) :: n_node_vars
    integer,                                intent(in) :: n_cell_vars
    character(*),                 optional, intent(in) :: zone_name
    character(*), dimension(:),   optional, intent(in) :: var_names
    integer,      dimension(:),   optional, intent(in) :: var_share
    character(*),                 optional, intent(in) :: data_packing
    integer,      dimension(:),   optional, intent(in) :: data_format
    integer,                      optional, intent(in) :: strand_id
    real(dp),                     optional, intent(in) :: solution_time
    ! character(*), dimension(:,:), optional, intent(in) :: aux_data
    integer :: n_vars
    logical :: err
    character(max_text_line_length) :: pack_fmt, loc_fmt, type_fmt
    character(max_text_line_length) :: var_fmt, var_share_fmt
    character(max_text_line_length) :: zone_fmt, size_fmt
    character(*), parameter :: routine_name = 'write_tecplot_ordered_zone_header'
    
    if ( n_dim < 1 .or. n_dim > 3 ) then
      err = error_message( routine_name, 'Tecplot ordered file-format '//      &
                                         'supports only 1-3 dim.' )
    end if

    if ( present(zone_name) ) then
      write(zone_fmt,"('ZONE T=','""',(A),'""')") trim(zone_name)
    else
      write(zone_fmt,"(A)") 'ZONE'
    end if

    call write_tecplot_size_fmt_ordered(n_dim,n_nodes,size_fmt)

    if ( present(data_packing) ) then
      call write_tecplot_pack_fmt(n_cell_vars,data_packing,pack_fmt)
    end if

    if ( present(var_names) ) then
      call write_tecplot_var_fmt(var_names,var_fmt)
    end if

    if ( present(var_share) )  then
      call write_tecplot_var_share_fmt( var_share, var_share_fmt )
    end if

    call write_tecplot_loc_fmt( n_node_vars, n_cell_vars, loc_fmt )

    n_vars = n_node_vars + n_cell_vars
    if ( present(data_format) )  then
      if (size(data_format)/=n_vars) then
        err = error_message(routine_name,"Size of optional argument "//        &
                                        "'data_format' does not match "//      &
                                        "number of variables.")
      end if
      call write_tecplot_data_type_fmt( n_vars, type_fmt,                      &
                                        data_format=data_format )
    else
      call write_tecplot_data_type_fmt( n_vars, type_fmt )
    end if

    write( fid, *     )
    if ( present(var_names) ) write( fid, '(A)' ) trim( var_fmt )
    write( fid, '(A)' ) trim( zone_fmt )
    if ( present(var_share) ) write( fid, '(A)' ) trim( var_share_fmt )
    write( fid, '(A)' ) trim( size_fmt )
    if ( present(data_packing) ) write(fid,'(A)' ) trim( pack_fmt )
    write( fid, '(A)' ) trim( loc_fmt   )
    write( fid, '(A)' ) trim( type_fmt  )

    if ( present(strand_id) )  then
      write( fid, '((A),I0)' ) 'STRANDID=',strand_id
    end if

    if ( present(solution_time) )  then
      write( fid, '((A),ES23.15)' ) 'SOLUTIONTIME=',solution_time
    end if

  end subroutine write_tecplot_ordered_zone_header

  ! subroutine write_tecplot_fe_brick_zone_header( fid, n_dim, n_nodes,          &
  !                                                n_cells,                      &
  !                                                n_node_vars,                  &
  !                                                n_cell_vars,                  &
  !                                                zone_name,                    &
  !                                                var_names,                    &
  !                                                data_format,                  &
  !                                                strand_id,                    &
  !                                                solution_time )
  !   use set_constants, only : max_text_line_length
  !   integer,                              intent(in) :: fid, n_dim
  !   integer,                              intent(in) :: n_nodes, n_cells
  !   integer,                              intent(in) :: n_node_vars, n_cell_vars
  !   character(*),               optional, intent(in) :: zone_name
  !   character(*), dimension(:), optional, intent(in) :: var_names
  !   integer,      dimension(:), optional, intent(in) :: data_format
  !   integer,                    optional, intent(in) :: strand_id
  !   real(dp),                   optional, intent(in) :: solution_time
  !   integer               :: n_vars
  !   logical               :: err
  !   character(max_text_line_length) :: var_fmt, loc_fmt, type_fmt
  !   character(max_text_line_length) :: zone_fmt, size_fmt
  !   character(*), parameter :: routine_name = 'write_tecplot_fe_brick_zone_header'

  !   if ( present(zone_name) ) then
  !     write(zone_fmt,"('ZONE T=','""',(A),'""')") trim(zone_name)
  !   else
  !     write(zone_fmt,"(A)") 'ZONE'
  !   end if

  !   if ( present(var_names) ) then
  !     call write_tecplot_var_fmt(var_names,var_fmt)
  !   end if
  !   call write_tecplot_size_fmt_fe(n_dim,n_nodes,n_cells,size_fmt)
  !   call write_tecplot_loc_fmt( n_node_vars, n_cell_vars, loc_fmt )

  !   n_vars = n_node_vars + n_cell_vars
  !   if ( present(data_format) ) then
  !     if (size(data_format)/=n_vars) then
  !       err = error_message(routine_name,"Size of optional argument "//        &
  !                                        "'data_format' does not match "//     &
  !                                        "number of variables.")
  !     end if
  !     call write_tecplot_data_type_fmt( n_vars, type_fmt,                      &
  !                                       data_format=data_format )
  !   else
  !     call write_tecplot_data_type_fmt( n_vars, type_fmt )
  !   end if

  !   write( fid, *     )
  !   write( fid, '(A)' ) trim( zone_fmt )
  !   write( fid, '(A)' ) trim( size_fmt )
  !   if ( present(var_names) ) write( fid, '(A)' ) trim( var_fmt )
  !   write( fid, '(A)' ) trim( loc_fmt   )
  !   write( fid, '(A)' ) trim( type_fmt  )

  !   if ( present(strand_id) )  then
  !     write( fid, '((A),I0)' ) 'STRANDID=',strand_id
  !   end if

  !   if ( present(solution_time) )  then
  !     write( fid, '((A),ES23.15)' ) 'SOLUTIONTIME=',solution_time
  !   end if

  ! end subroutine write_tecplot_fe_brick_zone_header

  subroutine write_tecplot_ordered_zone_block_packed( fid, ijk_size,       &
                                                      n_node_vars, n_cell_vars,&
                                                      NODE_DATA, CELL_DATA,    &
                                                      data_format )
    integer,                  intent(in) :: fid
    integer, dimension(:),    intent(in) :: ijk_size
    integer,                  intent(in) :: n_node_vars, n_cell_vars
    real(dp), dimension(:,:), intent(in) :: NODE_DATA
    real(dp), dimension(:,:), intent(in) :: CELL_DATA
    integer,  dimension(:), optional, intent(in) :: data_format
    integer :: n_nodes, n_cells, cnt
    character(*), parameter :: routine_name = 'write_tecplot_ordered_zone_block_packed'
    n_nodes = product(ijk_size)
    n_cells = product(max(ijk_size-1,1))
    if ( present(data_format) ) then
      cnt = 1
      call formatted_write(fid,n_nodes,n_node_vars,cnt,NODE_DATA,.false.,data_format=data_format)
      cnt = cnt + n_node_vars
      call formatted_write(fid,n_cells,n_cell_vars,cnt,CELL_DATA,.false.,data_format=data_format)
    else
      call formatted_write(fid,n_nodes,n_node_vars,1,NODE_DATA,.false.)
      call formatted_write(fid,n_cells,n_cell_vars,1,CELL_DATA,.false.)
    end if
  end subroutine write_tecplot_ordered_zone_block_packed

  ! subroutine write_tecplot_ordered_zone_point_packed( fid, n_nodes,       &
  !                                                     n_vars, NODE_DATA,       &
  !                                                     data_format )
  !   integer,                  intent(in) :: fid, n_nodes, n_vars
  !   real(dp), dimension(:,:), intent(in) :: NODE_DATA
  !   integer,  dimension(:), optional, intent(in) :: data_format
  !   character(*), parameter :: routine_name = 'write_tecplot_ordered_zone_point_packed'
  !   if ( present(data_format) ) then
  !     call formatted_write(fid,n_nodes,n_vars,1,NODE_DATA,.true.,data_format=data_format)
  !   else
  !     call formatted_write(fid,n_nodes,n_vars,1,NODE_DATA,.true.)
  !   end if
  ! end subroutine write_tecplot_ordered_zone_point_packed

  ! subroutine write_tecplot_fe_brick_zone( fid, n_nodes, n_cells, n_node_vars,   &
  !                                         n_cell_vars, NODE_DATA, CELL_DATA,    &
  !                                         conn_idx, data_format )
  !   integer,                  intent(in) :: fid
  !   integer,                  intent(in) :: n_nodes,    n_cells
  !   integer,                  intent(in) :: n_node_vars, n_cell_vars
  !   real(dp), dimension(:,:), intent(in) :: NODE_DATA
  !   real(dp), dimension(:,:), intent(in) :: CELL_DATA
  !   integer,  dimension(:,:), intent(in) :: conn_idx
  !   integer,  dimension(:), optional, intent(in) :: data_format
  !   integer :: v, t, n_brick, cnt
  !   character(len=100) :: conn_fmt
  !   n_brick = size(conn_idx,1)
  !   if ( present(data_format) ) then
  !     cnt = 1
  !     call formatted_write(fid,n_nodes,n_node_vars,cnt,NODE_DATA,.false.,data_format=data_format)
  !     cnt = cnt + n_node_vars
  !     call formatted_write(fid,n_cells,n_cell_vars,cnt,CELL_DATA,.false.,data_format=data_format)
  !   else
  !     call formatted_write(fid,n_nodes,n_node_vars,1,NODE_DATA,.false.)
  !     call formatted_write(fid,n_cells,n_cell_vars,1,CELL_DATA,.false.)
  !   end if

  !   write(conn_fmt,'((A),I0,(A))') "((I0),", n_brick-1,"(' ',(I0)))"
  !   do t = 1,merge(n_cells,n_nodes,n_cell_vars>0)
  !     write(fid,trim(conn_fmt)) ( conn_idx(v,t), v=1,n_brick )
  !   end do

  ! end subroutine write_tecplot_fe_brick_zone

  subroutine formatted_write(fid,n_data,n_vars,fmt_idx,DATA,point_zone,data_format)
    use set_constants, only : max_text_line_length
    integer,                          intent(in) :: fid,n_data, n_vars, fmt_idx
    real(dp), dimension(:,:),         intent(in) :: DATA
    logical,                          intent(in) :: point_zone
    integer,  dimension(:), optional, intent(in) :: data_format
    
    integer :: v, i, cnt
    character(max_text_line_length) :: tmp_fmt

    if ( present(data_format) ) then
      if ( point_zone ) then
        do i = 1,n_data
          cnt = fmt_idx - 1
          do v = 1,n_vars
            cnt = cnt + 1
            write(tmp_fmt,'(A,A)') trim(formats(data_format(cnt))), " "
            select case(data_format(cnt))
            case(1,2)
              write(fid,trim(tmp_fmt),advance='no') DATA(v,i)
            case default
              write(fid,trim(tmp_fmt),advance='no') int(DATA(v,i))
            end select
          end do
          write(fid,*)
        end do
      else
        cnt = fmt_idx - 1
        do v = 1,n_vars
          cnt = cnt + 1
          select case(data_format(cnt))
          case(1,2)
            write(fid,trim(formats(data_format(cnt)))) ( DATA(v,i), i=1,n_data )
          case default
            write(fid,trim(formats(data_format(cnt)))) ( int(DATA(v,i)), i=1,n_data )
          end select
        end do
      end if
    else
      if (point_zone) then
        tmp_fmt = trim(formats(1))
        do v = 2,n_vars
          tmp_fmt = trim(tmp_fmt)//' '//trim(formats(1))
        end do
        do i = 1,n_data
          write(fid,trim(tmp_fmt)) ( DATA(v,i), v=1,n_vars )
        end do
      else
        write(fid,trim(formats(1))) ( ( DATA(v,i), i=1,n_data ), v=1,n_vars )
      end if
    end if
  end subroutine formatted_write
end module tecplot_output

module interpolant_derived_type
  use set_precision, only : dp
  use set_constants, only : zero, one, two, half
  implicit none
  private
  public :: interpolant_t

  type :: interpolant_t
    integer :: Nmax
    real(dp), dimension(:,:),     allocatable :: xb, wb
    real(dp), dimension(:,:,:,:), allocatable :: Dmat
  contains
    private
    procedure, public, nopass :: constructor
    procedure, public, pass   :: destroy       => destroy_interpolant
    procedure,         pass   :: lagbary, lagbary_wderiv, lagbary_wderiv2
    procedure,         pass   :: lagbary_2D, lagbary_2D_wgrad, lagbary_2D_whess
    procedure,         pass   :: lagbary_3D, lagbary_3D_wgrad, lagbary_3D_whess
    procedure, public, pass   :: calc_grid_metrics, calc_grid_metrics_alt
    procedure, public, pass   :: normal_vectors
    ! procedure, public, pass   :: map_point_3D
    procedure, public, pass   :: map_point_3D_curve, map_point_3D_surface, map_point_3D_volume
  end type interpolant_t

  interface interpolant_t
    procedure constructor
  end interface interpolant_t

contains

  pure elemental subroutine destroy_interpolant(this)
    class(interpolant_t), intent(inout) :: this
    if ( allocated(this%Dmat) ) deallocate(this%Dmat)
    if ( allocated(this%xb) )   deallocate(this%xb)
    if ( allocated(this%wb) )   deallocate(this%wb)
    this%Nmax = 0
  end subroutine destroy_interpolant

  pure elemental function constructor(N) result(this)
    use linspace_helper, only : linspace
    use set_constants,   only : zero, one
    integer, optional, intent(in) :: N
    type(interpolant_t)           :: this
    integer :: j
    call this%destroy()
    if ( present(N) ) this%Nmax = max(N,2)
    allocate( this%Dmat(this%Nmax,this%Nmax,this%Nmax,2) )
    allocate(   this%xb(this%Nmax,this%Nmax), this%wb(this%Nmax,this%Nmax) )
    this%Dmat = zero; this%xb = zero; this%wb = zero
    this%wb(1,1) = one
    do j = 2,this%Nmax
      this%xb(1:j,j) = linspace(j,-one,one)
      this%wb(1:j,j) = barycentric_weights( this%xb(1:j,j) )
      this%Dmat(1:j,1:j,j,:) = mth_order_polynomial_derivative_matrix( this%xb(1:j,j), this%wb(1:j,j), 2 )
    end do
  end function constructor

  pure elemental logical function almost_equal(a,b)
    real(dp), intent(in) :: a, b
    logical :: test1, test2, test3
    test1 = ( (a==zero) .or. (b==zero) )
    test2 = ( abs(a-b) <= two*epsilon(one) )
    test3 = ( ( abs(a-b) <= epsilon(abs(a)) ) .and. &
              ( abs(a-b) <= epsilon(abs(b)) ) )
    almost_equal = ( ( test1 .and. test2 ) .or. ( (.not. test1) .and. test3 ) )
  end function almost_equal

  pure function barycentric_weights(x) result(w)
    real(dp), dimension(:), intent(in) :: x
    real(dp), dimension(size(x))       :: w
    integer :: j, k, N
    N = size(x)
    w = one
    do j = 2,N
      do k = 1,j-1
        w(k) = w(k) * ( x(k) - x(j) )
        w(j) = w(j) * ( x(j) - x(k) )
      end do
    end do
    w = one/w
  end function barycentric_weights

  pure function polynomial_derivative_matrix(x,w) result(D)
    real(dp), dimension(:), intent(in) :: x, w
    real(dp), dimension(size(x),size(x)) :: D
    integer :: i, j, N
    D = zero
    N = size(x)
    do i = 1,N
      do j = 1,N
        if (j/=i) then
          D(i,j) = w(j)/w(i) * one / ( x(i) - x(j) )
          D(i,i) = D(i,i) - D(i,j)
        end if
      end do
    end do
  end function polynomial_derivative_matrix

  pure function mth_order_polynomial_derivative_matrix(x,w,M) result(D)
    real(dp), dimension(:), intent(in) :: x, w
    integer,                intent(in) :: M
    real(dp), dimension(size(x),size(x),M) :: D
    integer :: i, j, k, N
    D = zero
    N = size(x)
    D(:,:,1) = polynomial_derivative_matrix(x,w)
    do k = 2,M
      do i = 1,N
        D(i,i,k) = zero
        do j = 1,N
          if (j/=i) then
            D(i,j,k) = ( real(k,dp) / (x(i) - x(j)) )        &
                     * ( w(j)/w(i)*D(i,i,k-1) - D(i,j,k-1) )
            D(i,i,k) = D(i,i,k) - D(i,j,k)
          end if
        end do
      end do
    end do
  end function mth_order_polynomial_derivative_matrix

  pure subroutine lagbary(this,x,dir,fval,Npts,val)
    class(interpolant_t),    intent(in)  :: this
    real(dp),               intent(in)  :: x
    integer,                intent(in)  :: dir
    integer,  dimension(:), intent(in)  :: Npts
    real(dp), dimension(:), intent(in)  :: fval
    real(dp),               intent(out) :: val
    real(dp) :: A, F
    real(dp) :: x1, t1
    integer :: j, N
    A = zero; F = zero
    N = Npts(dir)
    do j = 1,N
      x1 = this%xb(j,N) - x
      if ( almost_equal(x1,zero) ) then
        val = fval(j)
        return
      end if
      t1 = this%wb(j,N)/x1
      A = A + t1 * fval(j)
      F = F + t1
    end do
    val = A/F
  end subroutine lagbary

  pure subroutine lagbary_wderiv(this,x,dir,fval,Npts,val,dval)
    class(interpolant_t),    intent(in)  :: this
    real(dp),               intent(in)  :: x
    integer,                intent(in)  :: dir
    integer,  dimension(:), intent(in)  :: Npts
    real(dp), dimension(:), intent(in)  :: fval
    real(dp),               intent(out) :: val, dval
    real(dp) :: A, B, C, F
    real(dp) :: x1, t1, t2, FF, AC
    integer :: j, N
    A = zero; B = zero; C = zero; F = zero
    N = Npts(dir)
    do j = 1,N
      x1 = this%xb(j,N) - x
      if ( almost_equal(x1,zero) ) then
        val = fval(j)
        dval = dot_product( this%Dmat(j,1:N,N,1), fval )
        return
      end if
      t1 = this%wb(j,N)/x1
      A = A + t1 * fval(j)
      F = F + t1
      t2 = t1/x1
      B = B + t2 * fval(j)
      C = C + t2
    end do
    val = A/F
    FF = F*F
    AC = A*C
    dval = (B * F - AC)/FF
  end subroutine lagbary_wderiv

  pure subroutine lagbary_wderiv2(this,x,dir,fval,Npts,val,dval,d2val)
    class(interpolant_t),    intent(in)  :: this
    real(dp),               intent(in)  :: x
    integer,                intent(in)  :: dir
    integer,  dimension(:), intent(in)  :: Npts
    real(dp), dimension(:), intent(in)  :: fval
    real(dp),               intent(out) :: val, dval, d2val
    real(dp) :: A, B, C, D, E, F
    real(dp) :: x1, t1, t2, t3, FF, AC
    integer :: j, N
    A = zero; B = zero; C = zero; D = zero; E = zero; F = zero
    N = Npts(dir)
    do j = 1,N
      x1 = this%xb(j,N) - x
      if ( almost_equal(x1,zero) ) then
        val   = fval(j)
        dval  = dot_product( this%Dmat(j,1:N,N,1), fval )
        d2val = dot_product( this%Dmat(j,1:N,N,2), fval )
        return
      end if
      t1 = this%wb(j,N)/x1
      A = A + t1 * fval(j)
      F = F + t1
      t2 = t1/x1
      B = B + t2 * fval(j)
      C = C + t2
      t3 = t2/x1
      D = D + t3 * fval(j)
      E = E + t3
    end do
    val = A/F
    FF = F*F
    AC = A*C
    dval = (B * F - AC)/FF
    d2val = ( two * D      ) / F          &
          - ( two * E * A  ) / FF         &
          - ( two * B * C  ) / FF         &
          + ( two * C * AC ) / ( FF * F )
  end subroutine lagbary_wderiv2

  pure subroutine lagbary_2D(this,x,fval,Npts,val)
    class(interpolant_t),      intent(in)  :: this
    real(dp), dimension(2),   intent(in)  :: x
    real(dp), dimension(:,:), intent(in)  :: fval
    integer,  dimension(2),   intent(in)  :: Npts
    real(dp),                 intent(out) :: val
    real(dp), dimension(size(fval,2)) :: tmp
    integer :: j
    do j = 1,Npts(2)
      call this%lagbary( x(1), 1, fval(:,j), Npts, tmp(j) )
    end do
    call this%lagbary( x(2), 2, tmp, Npts, val )
  end subroutine lagbary_2D

  pure subroutine lagbary_2D_wgrad(this,x,fval,Npts,val,grad)
    class(interpolant_t),      intent(in)  :: this
    real(dp), dimension(2),   intent(in)  :: x
    real(dp), dimension(:,:), intent(in)  :: fval
    integer,  dimension(2),   intent(in)  :: Npts
    real(dp),                 intent(out) :: val
    real(dp), dimension(2),   intent(out) :: grad
    real(dp), dimension(size(fval,2)) :: tmp, gtmp
    integer :: j
    do j = 1,Npts(2)
      call this%lagbary_wderiv( x(1), 1, fval(:,j), Npts, tmp(j), gtmp(j) )
    end do
    call this%lagbary_wderiv( x(2), 2,  tmp, Npts, val, grad(2) )
    call this%lagbary(        x(2), 2, gtmp, Npts,      grad(1) )
  end subroutine lagbary_2D_wgrad

  pure subroutine lagbary_2D_whess(this,x,fval,Npts,val,grad,hess)
    class(interpolant_t),      intent(in)  :: this
    real(dp), dimension(2),   intent(in)  :: x
    real(dp), dimension(:,:), intent(in)  :: fval
    integer,  dimension(2),   intent(in)  :: Npts
    real(dp),                 intent(out) :: val
    real(dp), dimension(2),   intent(out) :: grad
    real(dp), dimension(3),   intent(out) :: hess
    real(dp), dimension(size(fval,2)) :: tmp, gtmp, htmp
    integer :: j
    do j = 1,Npts(2)
      call this%lagbary_wderiv2( x(1), 1, fval(:,j), Npts, tmp(j), gtmp(j), htmp(j) )
    end do
    call this%lagbary_wderiv2( x(2), 2,  tmp, Npts, val, grad(2), hess(3) )
    call this%lagbary_wderiv(  x(2), 2, gtmp, Npts,      grad(1), hess(2) )
    call this%lagbary(         x(2), 2, htmp, Npts,               hess(1) )
  end subroutine lagbary_2D_whess

  pure subroutine lagbary_3D(this,x,fval,Npts,val)
    class(interpolant_t),        intent(in)  :: this
    real(dp), dimension(3),     intent(in)  :: x
    real(dp), dimension(:,:,:), intent(in)  :: fval
    integer,  dimension(3),     intent(in)  :: Npts
    real(dp),                   intent(out) :: val
    real(dp), dimension(size(fval,2),size(fval,3)) :: tmp
    real(dp), dimension(size(fval,3)) :: tmp2
    integer :: k, j
    do k = 1,Npts(3)
      do j = 1,Npts(2)
        call this%lagbary( x(1), 1, fval(:,j,k), Npts, tmp(j,k) )
      end do
    end do
    do k = 1,Npts(3)
      call this%lagbary( x(2), 2, tmp(:,k), Npts, tmp2(k) )
    end do
    call this%lagbary( x(3), 3, tmp2, Npts, val )
  end subroutine lagbary_3D

  pure subroutine lagbary_3D_wgrad(this,x,fval,Npts,val,grad)
    class(interpolant_t),        intent(in)  :: this
    real(dp), dimension(3),     intent(in)  :: x
    real(dp), dimension(:,:,:), intent(in)  :: fval
    integer,  dimension(3),     intent(in)  :: Npts
    real(dp),                   intent(out) :: val
    real(dp), dimension(3),     intent(out) :: grad
    real(dp), dimension(size(fval,2),size(fval,3)) :: tmp, gtmp0
    real(dp), dimension(size(fval,3)) :: tmp2, gtmp1, gtmp2
    integer :: k, j
    do k = 1,Npts(3)
      do j = 1,Npts(2)
        call this%lagbary_wderiv( x(1), 1, fval(:,j,k), Npts, tmp(j,k), gtmp0(j,k) )
      end do
    end do
    do k = 1,Npts(3)
      call this%lagbary_wderiv( x(2), 2,   tmp(:,k), Npts, tmp2(k), gtmp2(k) )
      call this%lagbary(        x(2), 2, gtmp0(:,k), Npts, gtmp1(k) )
    end do
    call this%lagbary_wderiv( x(3), 3,  tmp2, Npts, val, grad(3) )
    call this%lagbary(        x(3), 3, gtmp2, Npts,      grad(2) )
    call this%lagbary(        x(3), 3, gtmp1, Npts,      grad(1) )
  end subroutine lagbary_3D_wgrad

  pure subroutine lagbary_3D_whess(this,x,fval,Npts,val,grad,hess)
    class(interpolant_t),        intent(in)  :: this
    real(dp), dimension(3),     intent(in)  :: x
    real(dp), dimension(:,:,:), intent(in)  :: fval
    integer,  dimension(3),     intent(in)  :: Npts
    real(dp),                   intent(out) :: val
    real(dp), dimension(3),     intent(out) :: grad
    real(dp), dimension(6),     intent(out) :: hess
    real(dp), dimension(size(fval,2),size(fval,3)) :: tmp, gtmp, htmp
    real(dp), dimension(size(fval,3)) :: tmp1, gtmp1, gtmp2, htmp1, htmp2, htmp3
    integer :: k, j
    do k = 1,Npts(3)
      do j = 1,Npts(2)
        call this%lagbary_wderiv2( x(1), 1, fval(:,j,k), Npts, tmp(j,k), gtmp(j,k), htmp(j,k) )
      end do
    end do
    do k = 1,Npts(3)
      call this%lagbary_wderiv2( x(2), 2,  tmp(:,k), Npts, tmp1(k), gtmp2(k), htmp3(k) )
      call this%lagbary_wderiv(  x(2), 2, gtmp(:,k), Npts,          gtmp1(k), htmp2(k) )
      call this%lagbary(         x(2), 2, htmp(:,k), Npts,                    htmp1(k) )
    end do
    call this%lagbary_wderiv2( x(3), 3,  tmp1, Npts, val, grad(3), hess(6) )
    call this%lagbary_wderiv(  x(3), 3, gtmp2, Npts,      grad(2), hess(5) )
    call this%lagbary(         x(3), 3, htmp3, Npts,               hess(4) )
    call this%lagbary_wderiv(  x(3), 3, gtmp1, Npts,      grad(1), hess(3) )
    call this%lagbary(         x(3), 3, htmp2, Npts,               hess(2) )
    call this%lagbary(         x(3), 3, htmp1, Npts,               hess(1) )
  end subroutine lagbary_3D_whess

  pure function calc_grid_metrics(this,point,X1,X2,X3) result(Ja)
    use set_constants, only : zero
    class(interpolant_t),       intent(in) :: this
    real(dp), dimension(3),     intent(in) :: point
    real(dp), dimension(:,:,:), intent(in) :: X1, X2, X3
    real(dp), dimension(3,3) :: Ja
    ! real(dp), dimension(size(X1,1),size(X1,2),size(X1,3),3) :: X
    real(dp), dimension(size(X1,1),size(X1,2),size(X1,3))   :: tmp
    real(dp), dimension(3) :: dX_l, dX_m, dd1, dd2, dd3
    real(dp) :: junk
    integer, dimension(3), parameter :: ijk = [1,2,3]
    integer, dimension(3), parameter :: kij = cshift(ijk,1)
    integer, dimension(3), parameter :: jki = cshift(kij,1)
    integer, dimension(3) :: Npts
    Ja = zero
    Npts = shape(X1)
    call this%lagbary_3D_wgrad( point, X3, Npts, junk, dX_l )
    call this%lagbary_3D_wgrad( point, X2, Npts, junk, dX_m )
    tmp = X3*dX_m(1) - X2*dX_l(1)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd1 )
    tmp = X3*dX_m(2) - X2*dX_l(2)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd2 )
    tmp = X3*dX_m(3) - X2*dX_l(3)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd3 )
    Ja(1,1) = -half*( dd3(2) - dd2(3) );
    Ja(1,2) = -half*( dd1(3) - dd3(1) );
    Ja(1,3) = -half*( dd2(1) - dd1(2) );

    call this%lagbary_3D_wgrad( point, X1, Npts, junk, dX_l )
    call this%lagbary_3D_wgrad( point, X3, Npts, junk, dX_m )
    tmp = X1*dX_m(1) - X3*dX_l(1)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd1 )
    tmp = X1*dX_m(2) - X3*dX_l(2)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd2 )
    tmp = X1*dX_m(3) - X3*dX_l(3)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd3 )
    Ja(2,1) = -half*( dd3(2) - dd2(3) );
    Ja(2,2) = -half*( dd1(3) - dd3(1) );
    Ja(2,3) = -half*( dd2(1) - dd1(2) );

    call this%lagbary_3D_wgrad( point, X2, Npts, junk, dX_l )
    call this%lagbary_3D_wgrad( point, X1, Npts, junk, dX_m )
    tmp = X2*dX_m(1) - X1*dX_l(1)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd1 )
    tmp = X2*dX_m(2) - X1*dX_l(2)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd2 )
    tmp = X2*dX_m(3) - X1*dX_l(3)
    call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd3 )
    Ja(3,1) = -half*( dd3(2) - dd2(3) );
    Ja(3,2) = -half*( dd1(3) - dd3(1) );
    Ja(3,3) = -half*( dd2(1) - dd1(2) );
  end function calc_grid_metrics

  pure function calc_grid_metrics_alt(this,point,X1,X2,X3) result(Ja)
    use set_constants, only : zero
    class(interpolant_t),       intent(in) :: this
    real(dp), dimension(3),     intent(in) :: point
    real(dp), dimension(:,:,:), intent(in) :: X1, X2, X3
    real(dp), dimension(3,3) :: Ja
    real(dp), dimension(size(X1,1),size(X1,2),size(X1,3),3) :: X
    real(dp), dimension(size(X1,1),size(X1,2),size(X1,3))   :: tmp
    real(dp), dimension(3) :: dX_l, dX_m, dd1, dd2, dd3
    real(dp) :: junk
    integer, dimension(3), parameter :: ijk = [1,2,3]
    integer, dimension(3), parameter :: kij = cshift(ijk,1)
    integer, dimension(3), parameter :: jki = cshift(kij,1)
    integer, dimension(3) :: Npts
    integer :: i
    Ja = zero
    X(:,:,:,1) = X1
    X(:,:,:,2) = X2
    X(:,:,:,3) = X3
    Npts = shape(X1)
    do i = 1,3
      associate( l => kij(i), m => jki(i) )
        associate( X_l => X(:,:,:,l), X_m => X(:,:,:,m) )
          call this%lagbary_3D_wgrad( point, X_l, Npts, junk, dX_l )
          call this%lagbary_3D_wgrad( point, X_m, Npts, junk, dX_m )
          tmp = X_l*dX_m(1) - X_m*dX_l(1)
          call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd1 )
          tmp = X_l*dX_m(2) - X_m*dX_l(2)
          call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd2 )
          tmp = X_l*dX_m(3) - X_m*dX_l(3)
          call this%lagbary_3D_wgrad( point, tmp, Npts, junk, dd3 )
          Ja(i,1) = -half*( dd3(2) - dd2(3) );
          Ja(i,2) = -half*( dd1(3) - dd3(1) );
          Ja(i,3) = -half*( dd2(1) - dd1(2) );
        end associate
      end associate
    end do
  end function calc_grid_metrics_alt

  pure function normal_vectors(this,point,X1,X2,X3) result(Nvec)
    use math, only : cross_product
    class(interpolant_t),       intent(in) :: this
    real(dp), dimension(3),     intent(in) :: point
    real(dp), dimension(:,:,:), intent(in) :: X1, X2, X3
    real(dp), dimension(3,3) :: Nvec
    Nvec = this%calc_grid_metrics(point,X1,X2,X3)
    Nvec(:,1) = Nvec(:,1)/norm2(Nvec(:,1))
    Nvec(:,2) = Nvec(:,2)/norm2(Nvec(:,2))
    Nvec(:,3) = Nvec(:,3)/norm2(Nvec(:,3))
  end function normal_vectors

  pure subroutine map_point_3D_curve(this,point,X1,X2,X3,xyz,dS)
    class(interpolant_t),   intent(in)  :: this
    real(dp), dimension(1), intent(in)  :: point ! [t]
    real(dp), dimension(:), intent(in)  :: X1, X2, X3
    real(dp), dimension(3), intent(out) :: xyz
    real(dp),               intent(out) :: dS
    integer,  dimension(1) :: Npts
    real(dp), dimension(3) :: dval
    Npts = shape(X1)
    call this%lagbary_wderiv(point(1),1,X1,Npts,xyz(1),dval(1))
    call this%lagbary_wderiv(point(1),1,X2,Npts,xyz(2),dval(2))
    call this%lagbary_wderiv(point(1),1,X3,Npts,xyz(3),dval(3))
    dS = norm2(dval)
  end subroutine map_point_3D_curve

  pure subroutine map_point_3D_surface(this,point,X1,X2,X3,xyz,dS)
    use math, only : cross_product
    class(interpolant_t),     intent(in)  :: this
    real(dp), dimension(2),   intent(in)  :: point ! [u,v]
    real(dp), dimension(:,:), intent(in)  :: X1, X2, X3
    real(dp), dimension(3),   intent(out) :: xyz
    real(dp),                 intent(out) :: dS
    integer,  dimension(2) :: Npts
    real(dp), dimension(2) :: tmp
    real(dp), dimension(3) :: drdu, drdv
    Npts = shape(X1)
    call this%lagbary_2D_wgrad(point,X1,Npts,xyz(1),tmp)
    drdu(1) = tmp(1); drdv(1) = tmp(2)
    call this%lagbary_2D_wgrad(point,X2,Npts,xyz(2),tmp)
    drdu(2) = tmp(1); drdv(2) = tmp(2)
    call this%lagbary_2D_wgrad(point,X3,Npts,xyz(3),tmp)
    drdu(3) = tmp(1); drdv(3) = tmp(2)
    dS = norm2( cross_product(drdu,drdv) )
  end subroutine map_point_3D_surface

  pure subroutine map_point_3D_volume(this,point,X1,X2,X3,xyz,dS)
    use math, only : det_3x3
    class(interpolant_t),       intent(in)  :: this
    real(dp), dimension(3),     intent(in)  :: point ! [xi,eta,zeta]
    real(dp), dimension(:,:,:), intent(in)  :: X1, X2, X3
    real(dp), dimension(3),     intent(out) :: xyz
    real(dp),                   intent(out) :: dS
    integer, dimension(3) :: Npts
    real(dp), dimension(3,3) :: A
    Npts = shape(X1)
    call this%lagbary_3D_wgrad(point,X1,Npts,xyz(1),A(:,1))
    call this%lagbary_3D_wgrad(point,X2,Npts,xyz(2),A(:,2))
    call this%lagbary_3D_wgrad(point,X3,Npts,xyz(3),A(:,3))
    dS = det_3x3(A)
  end subroutine map_point_3D_volume


end module interpolant_derived_type

module quadrature_derived_type

  use set_precision,       only : dp
  use set_constants,       only : zero
  implicit none
  private
  public :: quad_t
  public :: quad_ptr, quad_ptr_3D
  public :: create_quad_ref_1D, create_quad_ref_2D, create_quad_ref_3D
  public :: map_quad_ref_to_physical
  public :: num_quad_pts
  type quad_t
    integer :: n_quad = 0
    real(dp), allocatable, dimension(:,:) :: quad_pts
    real(dp), allocatable, dimension(:)   :: quad_wts
  contains
    private
    procedure, public, pass :: create  => allocate_quad
    procedure, public, pass :: destroy => deallocate_quad
    generic,   public :: integrate => integrate_scalar, integrate_vector
    procedure :: integrate_scalar
    procedure :: integrate_vector
  end type quad_t

  type quad_ptr
    type(quad_t), pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_quad_ptr
  end type quad_ptr

  type quad_ptr_3D
    type(quad_t), dimension(:,:,:), pointer :: p => null()
  contains
    private
    procedure, public, pass :: destroy => destroy_quad_ptr_3D
  end type quad_ptr_3D

contains

  pure elemental subroutine destroy_quad_ptr_3D( this )
    class(quad_ptr_3D), intent(inout) :: this
    this%p => null()
  end subroutine destroy_quad_ptr_3D

  pure elemental subroutine destroy_quad_ptr( this )
    class(quad_ptr), intent(inout) :: this
    this%p => null()
  end subroutine destroy_quad_ptr

  pure elemental subroutine allocate_quad( this, n_quad )
    use set_constants, only : zero
    class(quad_t), intent(inout) :: this
    integer,       intent(in)    :: n_quad
    this%n_quad = n_quad
    allocate( this%quad_pts(3,n_quad) )
    this%quad_pts = zero
    allocate( this%quad_wts(n_quad) )
    this%quad_wts = zero
  end subroutine allocate_quad

  pure elemental subroutine deallocate_quad( this )
    class(quad_t), intent(inout) :: this
    this%n_quad = 0
    if( allocated( this%quad_wts  ) ) deallocate( this%quad_wts  )
    if( allocated( this%quad_pts  ) ) deallocate( this%quad_pts  )
  end subroutine deallocate_quad

  pure function integrate_scalar( this, f ) result( integral )
    use set_precision, only : dp
    class(quad_t),                    intent(in) :: this
    real(dp), dimension(this%n_quad), intent(in) :: f
    real(dp)                                     :: integral
    integral = dot_product(f,this%quad_wts)
  end function integrate_scalar

  pure function integrate_vector( this, neq, f ) result( integral )
    use set_precision, only : dp
    class(quad_t),                        intent(in) :: this
    integer,                              intent(in) :: neq
    real(dp), dimension(neq,this%n_quad), intent(in) :: f
    real(dp), dimension(neq)                         :: integral
    integer :: n
    do n = 1, neq
      integral(n) = dot_product(f(n,:),this%quad_wts)
    end do
  end function integrate_vector

  pure function num_quad_pts(quad_order,n_dim,include_ends) result(n_pts)
    integer, intent(in) :: quad_order, n_dim
    logical, intent(in) :: include_ends
    integer, dimension(n_dim) :: n_pts
    n_pts(1:n_dim) = gauss_1D_size(quad_order) + merge(2,0,include_ends)
  end function num_quad_pts

  pure function gauss_1D_size( polynomial_order ) result( N_quad )
    use set_constants, only : half
    integer, intent(in) :: polynomial_order
    integer             :: N_quad
    N_quad = ceiling( half*(polynomial_order + 1) )
  end function gauss_1D_size

  pure subroutine gauss_1D( n_quad, pts_1D, wts_1D )
    use math, only : LegendreGaussNodesAndWeights
    integer,                       intent(in)  :: n_quad
    real(dp), dimension( n_quad ), intent(out) :: pts_1D
    real(dp), dimension( n_quad ), intent(out) :: wts_1D
    call LegendreGaussNodesAndWeights(n_quad-1, pts_1D, wts_1D)
  end subroutine gauss_1D

  pure subroutine gauss_1D_w_ends(n_quad,pts_1D,wts_1D)
    use set_constants, only : one
    integer,                       intent(in)  :: n_quad
    real(dp), dimension(0:n_quad+1), intent(out) :: pts_1D
    real(dp), dimension(0:n_quad+1), intent(out) :: wts_1D
    pts_1D = zero; pts_1D(0) = -one; pts_1D(n_quad+1) = one
    wts_1D = zero
    call gauss_1D(n_quad,pts_1D(1:n_quad),wts_1D(1:n_quad))
  end subroutine gauss_1D_w_ends

  pure subroutine create_quad_ref_1D( quad_order, quad_ref, include_ends )
    integer,           intent(in)  :: quad_order
    type(quad_t),      intent(out) :: quad_ref
    logical, optional, intent(in)  :: include_ends
    logical :: include_ends_
    real(dp), dimension(:), allocatable :: pts_1D
    real(dp), dimension(:), allocatable :: wts_1D
    integer :: n_quad, n_gauss
    include_ends_ =.false.
    if ( present(include_ends) ) include_ends_ = include_ends
    n_gauss = gauss_1D_size( quad_order )
    if (include_ends_) then
      allocate( pts_1D(0:n_gauss+1) )
      allocate( wts_1D(0:n_gauss+1) )
      call gauss_1D_w_ends( n_gauss, pts_1D, wts_1D )
      n_quad = n_gauss + 2
    else
      allocate( pts_1D(n_gauss) )
      allocate( wts_1D(n_gauss) )
      call gauss_1D( n_gauss, pts_1D, wts_1D )
      n_quad = n_gauss
    end if
    call quad_ref%destroy()
    call quad_ref%create( n_quad )
    quad_ref%quad_wts      = pack(wts_1D,.true.)
    quad_ref%quad_pts(1,:) = pack(pts_1D,.true.)
    deallocate( pts_1D, wts_1D )
  end subroutine create_quad_ref_1D

  pure subroutine create_quad_ref_2D( quad_order, quad_ref, include_ends )
    use set_constants, only : zero
    integer,      intent(in)  :: quad_order
    type(quad_t), intent(out) :: quad_ref
    logical, optional, intent(in)  :: include_ends
    logical :: include_ends_
    integer :: n_quad, n_gauss
    integer :: i, j, cnt, o
    real(dp), dimension(:), allocatable :: pts_1D
    real(dp), dimension(:), allocatable :: wts_1D
    include_ends_ =.false.
    if ( present(include_ends) ) include_ends_ = include_ends
    n_gauss = gauss_1D_size( quad_order )
    if (include_ends_) then
      allocate( pts_1D(0:n_gauss+1) )
      allocate( wts_1D(0:n_gauss+1) )
      call gauss_1D_w_ends( n_gauss, pts_1D, wts_1D )
      n_quad = n_gauss + 2 
      o = 1 
    else
      allocate( pts_1D(n_gauss) )
      allocate( wts_1D(n_gauss) )
      call gauss_1D( n_gauss, pts_1D, wts_1D )
      n_quad = n_gauss
      o = 0
    end if
    call quad_ref%destroy()
    call quad_ref%create( n_quad**2 )
    cnt = 0
    do j = 1-o, n_gauss+o
      do i = 1-o, n_gauss+o
        cnt = cnt + 1
        quad_ref%quad_pts(:,cnt) = [ pts_1D(i), pts_1D(j), zero ]
        quad_ref%quad_wts(cnt) = wts_1D(i)*wts_1D(j)
      end do
    end do
    deallocate( pts_1D, wts_1D )
  end subroutine create_quad_ref_2D

  pure subroutine create_quad_ref_3D( quad_order, quad_ref, include_ends )
    integer,      intent(in)  :: quad_order
    type(quad_t), intent(out) :: quad_ref
    logical, optional, intent(in) :: include_ends
    logical :: include_ends_
    integer :: n_quad, n_gauss
    integer :: i, j, k, cnt, o
    real(dp), dimension(:), allocatable :: pts_1D
    real(dp), dimension(:), allocatable :: wts_1D
    include_ends_ =.false.
    if ( present(include_ends) ) include_ends_ = include_ends
    n_gauss = gauss_1D_size( quad_order )
    if (include_ends_) then
      allocate( pts_1D(0:n_gauss+1) )
      allocate( wts_1D(0:n_gauss+1) )
      call gauss_1D_w_ends( n_gauss, pts_1D, wts_1D )
      n_quad = n_gauss + 2 
      o = 1 
    else
      allocate( pts_1D(n_gauss) )
      allocate( wts_1D(n_gauss) )
      call gauss_1D( n_gauss, pts_1D, wts_1D )
      n_quad = n_gauss
      o = 0
    end if
    call quad_ref%destroy()
    call quad_ref%create( n_quad**3 )
    cnt = 0
    do k = 1-o, n_gauss+o
      do j = 1-o, n_gauss+o
        do i = 1-o, n_gauss+o
          cnt = cnt + 1
          quad_ref%quad_pts(:,cnt) = [ pts_1D(i), pts_1D(j), pts_1D(k) ]
          quad_ref%quad_wts(cnt) = wts_1D(i)*wts_1D(j)*wts_1D(k)
        end do
      end do
    end do
    deallocate( pts_1D, wts_1D )
  end subroutine create_quad_ref_3D

  pure subroutine map_quad_ref_to_physical_0D( X1, X2, X3, interpolant, quad_ref, quad_physical )
    use interpolant_derived_type, only : interpolant_t
    use set_constants,            only : one
    real(dp),               intent(in)  :: X1, X2, X3
    type(interpolant_t),    intent(in)  :: interpolant
    type(quad_t),           intent(in)  :: quad_ref
    type(quad_t),           intent(out) :: quad_physical
    real(dp) :: dS
    integer  :: n
    call quad_physical%create(quad_ref%n_quad)
    do n = 1,quad_ref%n_quad
      quad_physical%quad_pts(1,n) = X1
      quad_physical%quad_pts(2,n) = X2
      quad_physical%quad_pts(3,n) = X3
      quad_physical%quad_wts(n) = one
    end do
  end subroutine map_quad_ref_to_physical_0D

  pure subroutine map_quad_ref_to_physical_1D( X1, X2, X3, interpolant, quad_ref, quad_physical )
    use interpolant_derived_type, only : interpolant_t
    real(dp), dimension(:), intent(in)  :: X1, X2, X3
    type(interpolant_t),    intent(in)  :: interpolant
    type(quad_t),           intent(in)  :: quad_ref
    type(quad_t),           intent(out) :: quad_physical
    real(dp) :: dS
    integer  :: n
    call quad_physical%create(quad_ref%n_quad)
    do n = 1,quad_ref%n_quad
      call interpolant%map_point_3D_curve( [quad_ref%quad_pts(1,n)], X1, X2, X3, quad_physical%quad_pts(:,n), dS )
      quad_physical%quad_wts(n) = dS * quad_ref%quad_wts(n)
    end do
  end subroutine map_quad_ref_to_physical_1D

  pure subroutine map_quad_ref_to_physical_2D( X1, X2, X3, interpolant, quad_ref, quad_physical )
    use interpolant_derived_type, only : interpolant_t
    real(dp), dimension(:,:), intent(in)  :: X1, X2, X3
    type(interpolant_t),      intent(in)  :: interpolant
    type(quad_t),             intent(in)  :: quad_ref
    type(quad_t),             intent(out) :: quad_physical
    real(dp) :: dA
    integer  :: n
    call quad_physical%create(quad_ref%n_quad)
    do n = 1,quad_ref%n_quad
      call interpolant%map_point_3D_surface( quad_ref%quad_pts(1:2,n), X1, X2, X3, quad_physical%quad_pts(:,n), dA )
      quad_physical%quad_wts(n) = dA * quad_ref%quad_wts(n)
    end do
  end subroutine map_quad_ref_to_physical_2D

  pure subroutine map_quad_ref_to_physical_3D( X1, X2, X3, interpolant, quad_ref, quad_physical )
    use interpolant_derived_type, only : interpolant_t
    real(dp), dimension(:,:,:), intent(in)  :: X1, X2, X3
    type(interpolant_t),        intent(in)  :: interpolant
    type(quad_t),               intent(in)  :: quad_ref
    type(quad_t),               intent(out) :: quad_physical
    real(dp) :: dV
    integer  :: n
    call quad_physical%create(quad_ref%n_quad)
    do n = 1,quad_ref%n_quad
      call interpolant%map_point_3D_volume( quad_ref%quad_pts(1:3,n), X1, X2, X3, quad_physical%quad_pts(:,n), dV )
      quad_physical%quad_wts(n) = dV * quad_ref%quad_wts(n)
    end do
  end subroutine map_quad_ref_to_physical_3D



  pure subroutine map_quad_ref_to_physical( X1, X2, X3, loc, interpolant, quad_ref, quad_physical, status )
    use set_constants,                        only : zero
    use index_conversion,                     only : get_reshape_indices
    use interpolant_derived_type, only : interpolant_t
    real(dp), dimension(:,:,:), intent(in)  :: X1, X2, X3
    integer,  dimension(3),     intent(in)  :: loc
    type(interpolant_t),        intent(in)  :: interpolant
    type(quad_t),               intent(in)  :: quad_ref
    type(quad_t),               intent(out) :: quad_physical
    integer, optional,          intent(out) :: status
    real(dp), allocatable, dimension(:,:) :: X1_tmp, X2_tmp, X3_tmp
    integer, dimension(3) :: idx_start, idx_end, sz_in, sz_out
    integer  :: sz_cnt
    if (present(status)) status = 1
    sz_in = shape(X1)
    call get_reshape_indices(sz_in, loc, sz_out, sz_cnt, idx_start, idx_end )
    select case(sz_cnt)
    case(0)
      call map_quad_ref_to_physical_0D( X1(idx_end(1),idx_end(2),idx_end(3)),  &
                                        X2(idx_end(1),idx_end(2),idx_end(3)),  &
                                        X3(idx_end(1),idx_end(2),idx_end(3)),  &
                                        interpolant, quad_ref, quad_physical )

    case(1)
      allocate( X1_tmp(sz_out(1),1), X2_tmp(sz_out(1),1), X3_tmp(sz_out(1),1) )
      X1_tmp(:,1) = reshape( X1( idx_start(1):idx_end(1), &
                                 idx_start(2):idx_end(2), &
                                 idx_start(3):idx_end(3)), [sz_out(1)] )
      X2_tmp(:,1) = reshape( X2( idx_start(1):idx_end(1), &
                                 idx_start(2):idx_end(2), &
                                 idx_start(3):idx_end(3)), [sz_out(1)] )
      X3_tmp(:,1) = reshape( X3( idx_start(1):idx_end(1), &
                                 idx_start(2):idx_end(2), &
                                 idx_start(3):idx_end(3)), [sz_out(1)] )
      call map_quad_ref_to_physical_1D( X1_tmp(:,1), X2_tmp(:,1), X3_tmp(:,1), &
                                        interpolant, quad_ref, quad_physical )
    case(2)
      allocate( X1_tmp(sz_out(1),sz_out(2)) )
      allocate( X2_tmp(sz_out(1),sz_out(2)) )
      allocate( X3_tmp(sz_out(1),sz_out(2)) )
      X1_tmp = reshape( X1( idx_start(1):idx_end(1), &
                            idx_start(2):idx_end(2), &
                            idx_start(3):idx_end(3)), sz_out(1:2) )
      X2_tmp = reshape( X2( idx_start(1):idx_end(1), &
                            idx_start(2):idx_end(2), &
                            idx_start(3):idx_end(3)), sz_out(1:2) )
      X3_tmp = reshape( X3( idx_start(1):idx_end(1), &
                            idx_start(2):idx_end(2), &
                            idx_start(3):idx_end(3)), sz_out(1:2) )
      call map_quad_ref_to_physical_2D(X1_tmp,X2_tmp,X3_tmp,interpolant,quad_ref,quad_physical)
    case(3)
      call map_quad_ref_to_physical_3D(X1,X2,X3,interpolant,quad_ref,quad_physical)
    case default
      if (present(status)) status = -1
    end select

    if ( allocated(X1_tmp) ) deallocate( X1_tmp )
    if ( allocated(X2_tmp) ) deallocate( X2_tmp )
    if ( allocated(X3_tmp) ) deallocate( X3_tmp )

  end subroutine map_quad_ref_to_physical

end module quadrature_derived_type

module grid_derived_type
  use set_precision,           only : dp
  use quadrature_derived_type, only : quad_t, quad_ptr_3D
  use vector_derived_type,     only : face_vec, face_vec_ptr_3D
  use pointers,                only : array_ptr_3D_real, array_ptr_4D_real
  use interpolant_derived_type, only : interpolant_t
  implicit none
  private
  public :: derived_grid_vars
  public :: grid_block
  public :: grid_type
  public :: deallocate_grid
  public :: allocate_grid_block, deallocate_grid_block
  public :: allocate_derived_grid, deallocate_derived_grid

  public :: pack_cell_node_coords, get_cell_nodes

  type derived_grid_vars
    real(dp),       allocatable, dimension(:,:,:,:) :: cell_c
    real(dp),       allocatable, dimension(:,:,:)   :: volume
    real(dp),       allocatable, dimension(:,:,:)   :: xi_area
    real(dp),       allocatable, dimension(:,:,:)   :: eta_area
    real(dp),       allocatable, dimension(:,:,:)   :: zeta_area
    type(quad_t),   allocatable, dimension(:,:,:)   :: quad
    type(quad_t),   allocatable, dimension(:,:,:)   :: xi_face_quad
    type(quad_t),   allocatable, dimension(:,:,:)   :: eta_face_quad
    type(quad_t),   allocatable, dimension(:,:,:)   :: zeta_face_quad
    type(face_vec), allocatable, dimension(:,:,:)   :: xi_nv
    type(face_vec), allocatable, dimension(:,:,:)   :: eta_nv
    type(face_vec), allocatable, dimension(:,:,:)   :: zeta_nv
    type(quad_ptr_3D),           dimension(3)       :: face_quads
    type(face_vec_ptr_3D),       dimension(3)       :: normals
    integer, dimension(:), pointer :: n_cells, n_ghost
    integer,               pointer :: n_dim
    type(interpolant_t) :: interp
  contains
    private
    procedure, public, pass :: setup   =>   allocate_derived_grid
    procedure, public, pass :: destroy => deallocate_derived_grid
  end type derived_grid_vars

  type :: grid_block
    integer, dimension(3) :: n_nodes
    integer, dimension(3) :: n_cells
    integer, dimension(3) :: n_ghost
    integer  :: n_dim
    integer  :: total_cells
    real(dp) :: total_volume
    real(dp), allocatable, dimension(:,:,:,:) ::  node_coords
    type(derived_grid_vars) :: grid_vars
  contains
    private
    procedure, public, pass :: setup     => allocate_grid_block
    procedure, public, pass :: set_nodes => set_grid_block_nodes
    procedure, public, pass :: destroy   => deallocate_grid_block
  end type grid_block

  type grid_type
    integer  :: n_blocks
    integer  :: total_int_cells
    real(dp) :: total_int_volume
    type(grid_block), allocatable, dimension(:) :: gblock
  contains
    private
    procedure, public, pass   :: setup => init_grid_type
    procedure, public, pass :: destroy => deallocate_grid
  end type grid_type

contains

  pure function pack_cell_node_coords( idx, bnd_min, bnd_max, n_skip,          &
                                       coords_in ) result(coords_out)
    integer, dimension(3),                            intent(in)  :: idx, bnd_min, bnd_max, n_skip
    real(dp), dimension( 3, bnd_min(1):bnd_max(1), &
                            bnd_min(2):bnd_max(2), &
                            bnd_min(3):bnd_max(3) ), intent(in)  :: coords_in
    real(dp), dimension(3,product(n_skip+1))                     :: coords_out
    integer :: i,j,k,cnt
    cnt = 0
    do k = idx(3),idx(3)+n_skip(3)
      do j = idx(2),idx(2)+n_skip(2)
        do i = idx(1),idx(1)+n_skip(1)
          cnt = cnt + 1
          coords_out(:,cnt) = coords_in(:,i,j,k)
        end do
      end do
    end do
  end function pack_cell_node_coords

  ! subroutine coarsen_grid(n_skip,grid,grid_coarse)
  !   integer, dimension(3), intent(in)  :: n_skip
  !   type(grid_type),       intent(in)  :: grid
  !   type(grid_type),       intent(out) :: grid_coarse
  !   integer :: n, n_blocks, n_dim
  !   integer, dimension(3) :: n_nodes, n_ghost, lo1, hi1, lo2, hi2
  !   n_blocks = grid%n_blocks
  !   n_nodes  = grid
  !   call grid_coarse%setup( n_blocks )
  !   do n = 1,n_blocks
  !     n_nodes = grid%gblock(n)%n_nodes
  !     n_nodes = grid%gblock(n)%n_ghost

  !     call grid_coarse%gblock(n)%setup(n)
  ! end subroutine coarsen_grid

  ! pure subroutine coarsen_node_coords( lo1, hi1, lo2, hi2, n_skip, coords_in, coords_out )
  !   integer, dimension(3), intent(in) :: lo1, hi1, lo2, hi2, n_skip
  !   real(dp), dimension(3,lo1(1):hi1(1),lo1(2):hi1(2),lo1(3):hi1(3)), intent(in)  :: coords_in
  !   real(dp), dimension(3,lo2(1):hi2(1),lo2(2):hi2(2),lo2(3):hi2(3)), intent(out) :: coords_out
  !   coords_out = coords_in(:,lo1(1):hi1(1):n_skip(1),lo1(2):hi1(2):n_skip(2),lo1(3):hi1(3):n_skip(3))
  ! end subroutine coarsen_node_coords

  pure function get_cell_nodes(gblock,start_idx,n_skip) result(coords_out)
    type(grid_block),      intent(in) :: gblock
    integer, dimension(3), intent(in) :: start_idx
    integer, dimension(3), intent(in) :: n_skip
    real(dp), dimension(n_skip(1)+1,n_skip(2)+1,n_skip(3)+1,3) :: coords_out
    integer, dimension(4) :: bnd_tmp
    integer, dimension(3) :: bnd_min, bnd_max
    bnd_tmp = lbound(gblock%node_coords)
    bnd_min = bnd_tmp(2:4)
    bnd_tmp = ubound(gblock%node_coords)
    bnd_max = bnd_tmp(2:4)
    coords_out = cell_node_coords( start_idx, n_skip, bnd_min, bnd_max,        &
                                   gblock%node_coords )
  end function get_cell_nodes

  pure function cell_node_coords( idx, stride, bnd_min, bnd_max, coords_in )   &
                                                             result(coords_out)
    integer, dimension(3),                            intent(in)  :: idx
    integer, dimension(3),                            intent(in)  :: stride
    integer, dimension(3),                            intent(in)  :: bnd_min
    integer, dimension(3),                            intent(in)  :: bnd_max
    real(dp), dimension( 3, bnd_min(1):bnd_max(1), &
                            bnd_min(2):bnd_max(2), &
                            bnd_min(3):bnd_max(3) ), intent(in)  :: coords_in
    real(dp), dimension(stride(1)+1,stride(2)+1,stride(3)+1,3)   :: coords_out
    integer :: i,j,k,ii,jj,kk
    kk = 0
    do k = idx(3),idx(3)+stride(3)
      kk = kk + 1
      jj = 0
      do j = idx(2),idx(2)+stride(2)
        jj = jj + 1
        ii = 0
        do i = idx(1),idx(1)+stride(1)
          ii = ii + 1
          coords_out(ii,jj,kk,1) = coords_in(1,i,j,k)
          coords_out(ii,jj,kk,2) = coords_in(2,i,j,k)
          coords_out(ii,jj,kk,3) = coords_in(3,i,j,k)
        end do
      end do
    end do
  end function cell_node_coords

  pure subroutine init_grid_type( this, n_blocks )
    use set_constants, only : zero
    class(grid_type), intent(inout) :: this
    integer, intent(in) :: n_blocks

    this%n_blocks = n_blocks
    this%total_int_cells = 0
    this%total_int_volume = zero
    allocate( this%gblock(n_blocks) )
  end subroutine init_grid_type

  pure subroutine allocate_grid_block( this, n_dim, n_nodes, n_ghost )
    use set_constants, only : zero
    integer,               intent(in)  :: n_dim
    integer, dimension(3), intent(in)  :: n_nodes, n_ghost
    class(grid_block),     intent(inout) :: this
    integer, dimension(3) :: lo, hi
    this%n_dim   = n_dim
    this%n_nodes = 1; this%n_nodes(1:n_dim) = n_nodes(1:n_dim)
    this%n_ghost = n_ghost
    this%n_cells = 1; this%n_cells(1:n_dim) = n_nodes(1:n_dim) - 1
    lo = 1; lo(1:n_dim) = 1 - n_ghost(1:n_dim)
    hi = 1; hi(1:n_dim) = n_nodes(1:n_dim) + n_ghost(1:n_dim)
    allocate( this%node_coords( 3, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    this%node_coords = zero
    this%total_volume = zero
    this%total_cells  = product(this%n_cells)
  end subroutine allocate_grid_block

  pure subroutine set_grid_block_nodes( this, node_coords )
    use set_constants, only : zero
    class(grid_block),                         intent(inout) :: this
    real(dp), dimension( 3, this%n_nodes(1),                                   &
                            this%n_nodes(2),                                   &
                            this%n_nodes(3) ), intent(in) :: node_coords
    integer, dimension(3) :: lo, hi
    lo = 1
    hi = 1; hi(1:this%n_dim) = this%n_nodes(1:this%n_dim)
    this%node_coords( :, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) =             &
          node_coords(:, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) )
  end subroutine set_grid_block_nodes

  subroutine allocate_derived_grid( this, gblock )
    use set_constants,  only : zero
    use interpolant_derived_type, only : interpolant_t
    class(derived_grid_vars), target, intent(inout)   :: this
    class(grid_block),       target, intent(inout) :: gblock
    integer, dimension(3) :: lo, hi, n_skip
    this%n_cells => gblock%n_cells
    this%n_ghost => gblock%n_ghost
    this%n_dim   => gblock%n_dim
    lo = 1
    lo(1:this%n_dim) = 1 - this%n_ghost(1:this%n_dim)
    hi = 1
    hi(1:this%n_dim) = this%n_cells(1:this%n_dim) + this%n_ghost(1:this%n_dim)
    allocate( this%volume(    lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    allocate( this%cell_c( 3, lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    ! not including ghost cells
    ! allocate( this%quad(      lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    this%volume = zero
    this%cell_c = zero

    lo = 1
    hi = 1
    hi(1:this%n_dim) = this%n_cells(1:this%n_dim)
    allocate( this%quad(      lo(1):hi(1), lo(2):hi(2), lo(3):hi(3) ) )
    allocate( this%xi_area(   lo(1):hi(1)+1, lo(2):hi(2),   lo(3):hi(3)   ) )
    allocate( this%eta_area(  lo(1):hi(1),   lo(2):hi(2)+1, lo(3):hi(3)   ) )
    allocate( this%zeta_area( lo(1):hi(1),   lo(2):hi(2),   lo(3):hi(3)+1 ) )
    allocate( this%xi_face_quad(   lo(1):hi(1)+1, lo(2):hi(2),   lo(3):hi(3)   ) )
    allocate( this%eta_face_quad(  lo(1):hi(1),   lo(2):hi(2)+1, lo(3):hi(3)   ) )
    allocate( this%zeta_face_quad( lo(1):hi(1),   lo(2):hi(2),   lo(3):hi(3)+1 ) )
    allocate( this%xi_nv(   lo(1):hi(1)+1, lo(2):hi(2),   lo(3):hi(3)   ) )
    allocate( this%eta_nv(  lo(1):hi(1),   lo(2):hi(2)+1, lo(3):hi(3)   ) )
    allocate( this%zeta_nv( lo(1):hi(1),   lo(2):hi(2),   lo(3):hi(3)+1 ) )
    this%xi_area   = zero
    this%eta_area  = zero
    this%zeta_area = zero

    this%interp = interpolant_t(N=10)

    n_skip = 0; n_skip(1:this%n_dim) = 1
    ! call compute_quadrature_points( gblock, gblock, [1,1,1], 4 )
    call compute_quadrature_points( gblock, gblock, n_skip, 4 )

    this%face_quads(1)%p => this%xi_face_quad
    this%face_quads(2)%p => this%eta_face_quad
    this%face_quads(3)%p => this%zeta_face_quad
    this%normals(1)%p    => this%xi_nv
    this%normals(2)%p    => this%eta_nv
    this%normals(3)%p    => this%zeta_nv
  end subroutine allocate_derived_grid

  subroutine compute_quadrature_points( gblock1, gblock, n_skip, quad_order )
    use quadrature_derived_type, only : create_quad_ref_1D,                    &
                                        create_quad_ref_2D,                    &
                                        create_quad_ref_3D,                    &
                                        map_quad_ref_to_physical
    class(grid_block),     intent(in)    :: gblock1
    class(grid_block),     intent(inout) :: gblock
    integer, dimension(3), intent(in)    :: n_skip
    integer,               intent(in)    :: quad_order
    type(quad_t), dimension(0:3) :: ref_quads
    real(dp), dimension(n_skip(1)+1,n_skip(2)+1,n_skip(3)+1,3) :: coords_tmp
    integer :: i, j, k
    integer :: status
    integer, dimension(4) :: bnd_tmp
    integer, dimension(3) :: idx, bnd_min, bnd_max, sz, loc1, loc2

    sz = n_skip + 1
    call create_quad_ref_1D(          1, ref_quads(0) )
    call create_quad_ref_1D( quad_order, ref_quads(1) )
    call create_quad_ref_2D( quad_order, ref_quads(2) )
    call create_quad_ref_3D( quad_order, ref_quads(3) )

    bnd_tmp = lbound(gblock1%node_coords)
    bnd_min = bnd_tmp(2:4)
    bnd_tmp = ubound(gblock1%node_coords)
    bnd_max = bnd_tmp(2:4)
    ! first the volume quads
    loc1 = 2
    loc1(gblock%n_dim+1:) = 0
    do k = 1,gblock%n_cells(3)
      do j = 1,gblock%n_cells(2)
        do i = 1,gblock%n_cells(1)
          idx = [i,j,k]
          coords_tmp = cell_node_coords( idx, n_skip, bnd_min, bnd_max,        &
                                         gblock1%node_coords )
          associate( X1 => coords_tmp(:,:,:,1), &
                     X2 => coords_tmp(:,:,:,2), &
                     X3 => coords_tmp(:,:,:,3), &
                     gv => gblock%grid_vars )
            call map_quad_ref_to_physical( X1, X2, X3, loc1,                   &
                                           gv%interp,                          &
                                           ref_quads(gblock%n_dim),            &
                                           gv%quad(i,j,k), status=status )
            gv%volume(i,j,k) = sum( gv%quad(i,j,k)%quad_wts )
          end associate
        end do
      end do
    end do
    ! now the face quads

    ! xi-faces
    loc1 = 2
    ! if ( gblock%n_dim /= 1)
    loc1(gblock%n_dim+1:) = 0
    loc1(1) = 0

    loc2 = 2
    ! if ( gblock%n_dim /= 1)
    loc2(gblock%n_dim+1:) = 0
    loc2(1) = 1

    do k = 1,gblock%n_cells(3)
      do j = 1,gblock%n_cells(2)
        do i = 1,1
          idx = [i,j,k]
          coords_tmp = cell_node_coords( idx, n_skip, bnd_min, bnd_max,        &
                                         gblock1%node_coords )
          associate( X1 => coords_tmp(:,:,:,1), &
                     X2 => coords_tmp(:,:,:,2), &
                     X3 => coords_tmp(:,:,:,3), &
                     gv => gblock%grid_vars )
            call map_quad_ref_to_physical( X1, X2, X3, loc1,                   &
                                           gv%interp,                          &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%xi_face_quad(i,j,k),             &
                                           status=status )
            gv%xi_area(i,j,k) = sum( gv%xi_face_quad(i,j,k)%quad_wts )
          end associate
        end do
        do i = 1,gblock%n_cells(1)
          idx = [i,j,k]
          coords_tmp = cell_node_coords( idx, n_skip, bnd_min, bnd_max,        &
                                         gblock1%node_coords )
          associate( X1 => coords_tmp(:,:,:,1), &
                     X2 => coords_tmp(:,:,:,2), &
                     X3 => coords_tmp(:,:,:,3), &
                     gv => gblock%grid_vars )
            call map_quad_ref_to_physical( X1, X2, X3, loc2,                   &
                                           gv%interp,                          &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%xi_face_quad(i+1,j,k),           &
                                           status=status )
            gv%xi_area(i+1,j,k) = sum( gv%xi_face_quad(i+1,j,k)%quad_wts )
          end associate
        end do
      end do
    end do

    if ( gblock%n_dim == 1) return

    ! eta-faces
    loc1 = 2
    loc1(gblock%n_dim+1:) = 0
    loc1(2) = 0

    loc2 = 2
    loc2(gblock%n_dim+1:) = 0
    loc2(2) = 1

    do k = 1,gblock%n_cells(3)
      do j = 1,1
        do i = 1,gblock%n_cells(1)
          idx = [i,j,k]
          coords_tmp = cell_node_coords( idx, n_skip, bnd_min, bnd_max,        &
                                         gblock1%node_coords )
          associate( X1 => coords_tmp(:,:,:,1), &
                     X2 => coords_tmp(:,:,:,2), &
                     X3 => coords_tmp(:,:,:,3), &
                     gv => gblock%grid_vars )
            call map_quad_ref_to_physical( X1, X2, X3, loc1,                   &
                                           gv%interp,                          &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%eta_face_quad(i,j,k),            &
                                           status=status )
            gv%eta_area(i,j,k) = sum( gv%eta_face_quad(i,j,k)%quad_wts )
          end associate
        end do
      end do
      do j = 1,gblock%n_cells(2)
        do i = 1,gblock%n_cells(1)
          idx = [i,j,k]
          coords_tmp = cell_node_coords( idx, n_skip, bnd_min, bnd_max,        &
                                         gblock1%node_coords )
          associate( X1 => coords_tmp(:,:,:,1), &
                     X2 => coords_tmp(:,:,:,2), &
                     X3 => coords_tmp(:,:,:,3), &
                     gv => gblock%grid_vars )
            call map_quad_ref_to_physical( X1, X2, X3, loc2,                   &
                                           gv%interp,                          &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%eta_face_quad(i,j+1,k),          &
                                           status=status )
            gv%eta_area(i,j+1,k) = sum( gv%eta_face_quad(i,j+1,k)%quad_wts )
          end associate
        end do
      end do
    end do

    if ( gblock%n_dim == 2) return

    ! zeta-faces
    loc1 = 2
    loc1(3) = 0

    loc2 = 2
    loc2(3) = 1

    do k = 1,1
      do j = 1,gblock%n_cells(2)
        do i = 1,gblock%n_cells(1)
          idx = [i,j,k]
          coords_tmp = cell_node_coords( idx, n_skip, bnd_min, bnd_max,        &
                                         gblock1%node_coords )
          associate( X1 => coords_tmp(:,:,:,1), &
                     X2 => coords_tmp(:,:,:,2), &
                     X3 => coords_tmp(:,:,:,3), &
                     gv => gblock%grid_vars )
            call map_quad_ref_to_physical( X1, X2, X3, loc1,                   &
                                           gv%interp,                          &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%zeta_face_quad(i,j,k),           &
                                           status=status )
            gv%zeta_area(i,j,k) = sum( gv%zeta_face_quad(i,j,k)%quad_wts )
          end associate
        end do
      end do
    end do
    do k = 1,gblock%n_cells(3)
      do j = 1,gblock%n_cells(2)
        do i = 1,gblock%n_cells(1)
          idx = [i,j,k]
          coords_tmp = cell_node_coords( idx, n_skip, bnd_min, bnd_max,        &
                                         gblock1%node_coords )
          associate( X1 => coords_tmp(:,:,:,1), &
                     X2 => coords_tmp(:,:,:,2), &
                     X3 => coords_tmp(:,:,:,3), &
                     gv => gblock%grid_vars )
            call map_quad_ref_to_physical( X1, X2, X3, loc2,                   &
                                           gv%interp,                          &
                                           ref_quads(gblock%n_dim-1),          &
                                           gv%zeta_face_quad(i,j,k+1),         &
                                           status=status )
            gv%zeta_area(i,j,k+1) = sum( gv%zeta_face_quad(i,j,k+1)%quad_wts )
          end associate
        end do
      end do
    end do

    call ref_quads%destroy()

  end subroutine compute_quadrature_points

  pure elemental subroutine deallocate_grid(this)
    use set_constants, only : zero
    class(grid_type), intent(inout) :: this
    if ( allocated(this%gblock) ) then
      call this%gblock%destroy()
      deallocate( this%gblock)
    end if
    this%n_blocks = 0
    this%total_int_cells = 0
    this%total_int_volume = zero
  end subroutine deallocate_grid

  pure elemental subroutine deallocate_grid_block( this )
    use set_constants, only : zero
    class(grid_block), intent(inout) :: this
    call this%grid_vars%destroy()
    if (allocated( this%node_coords ) ) deallocate( this%node_coords )
    this%total_volume = zero
    this%total_cells  = 0
    this%n_dim        = 0
    this%n_nodes      = 0
    this%n_cells      = 0
  end subroutine deallocate_grid_block

  pure elemental subroutine deallocate_derived_grid( this )
    class(derived_grid_vars), intent(inout) :: this
    call this%interp%destroy()
    call this%face_quads(1)%destroy()
    call this%face_quads(2)%destroy()
    call this%face_quads(3)%destroy()
    call this%normals(1)%destroy()
    call this%normals(2)%destroy()
    call this%normals(3)%destroy()
    if ( allocated(this%cell_c)    ) deallocate( this%cell_c    )
    if ( allocated(this%volume)    ) deallocate( this%volume    )
    if ( allocated(this%xi_area)   ) deallocate( this%xi_area   )
    if ( allocated(this%eta_area)  ) deallocate( this%eta_area  )
    if ( allocated(this%zeta_area) ) deallocate( this%zeta_area )
    if ( allocated(this%quad) ) then
      call this%quad%destroy()
      deallocate( this%quad )
    end if
    if ( allocated(this%xi_face_quad) ) then
      call this%xi_face_quad%destroy()
      deallocate( this%xi_face_quad )
    end if
    if ( allocated(this%eta_face_quad) ) then
      call this%eta_face_quad%destroy()
      deallocate( this%eta_face_quad )
    end if
    if ( allocated(this%zeta_face_quad) ) then
      call this%zeta_face_quad%destroy()
      deallocate( this%zeta_face_quad )
    end if
    if ( allocated(this%xi_nv) ) then
      call this%xi_nv%destroy()
      deallocate(this%xi_nv)
    end if
    if ( allocated(this%eta_nv) ) then
      call this%eta_nv%destroy()
      deallocate(this%eta_nv)
    end if
    if ( allocated(this%zeta_nv) ) then
      call this%zeta_nv%destroy()
      deallocate(this%zeta_nv)
    end if
  end subroutine deallocate_derived_grid

end module grid_derived_type

module grid_local
  use grid_derived_type, only : grid_type

  implicit none
  private
  public :: grid

  type(grid_type) :: grid

end module grid_local

module monomial_basis_derived_type
  implicit none
  private
  public :: monomial_basis_t
  type :: monomial_basis_t
    private
    integer, public :: total_degree
    integer, public :: n_dim
    integer, public :: n_terms
    integer, public, allocatable, dimension(:)   :: idx
    integer, public, allocatable, dimension(:,:) :: exponents
    integer, public, allocatable, dimension(:,:) :: diff_idx
  contains
    private
    procedure, public, pass :: eval_m  => evaluate_monomial
    procedure, public, pass :: deval_m => evaluate_monomial_derivative
    procedure, public, pass :: destroy => destroy_monomial_basis_t
    procedure, public, pass :: check_gradient_indexing
  end type monomial_basis_t

  interface monomial_basis_t
    procedure constructor
  end interface monomial_basis_t

contains

  pure function constructor( total_degree, n_dim ) result(this)
    use combinatorics, only : nchoosek, get_exponents
    integer, intent(in) :: total_degree, n_dim
    type(monomial_basis_t) :: this

    call this%destroy()

    this%total_degree  = total_degree
    this%n_dim   = n_dim
    this%n_terms = nchoosek( n_dim + total_degree, total_degree )
    allocate( this%exponents( this%n_dim, this%n_terms ) )
    allocate( this%idx(0:this%total_degree) )
    allocate( this%diff_idx( this%n_dim, this%n_terms ) )
    call get_exponents( this%n_dim, this%total_degree, this%n_terms,           &
                        this%exponents, this%idx, diff_idx=this%diff_idx )
  end function constructor
  
  pure subroutine destroy_monomial_basis_t(this)
    class(monomial_basis_t), intent(inout) :: this
    if ( allocated(this%exponents) ) deallocate( this%exponents )
    if ( allocated(this%idx) )       deallocate( this%idx )
    if ( allocated(this%diff_idx) )  deallocate( this%diff_idx )
  end subroutine destroy_monomial_basis_t

  pure subroutine evaluate_monomial(this,term,x,val,coef)
    use set_precision, only : dp
    use set_constants, only : one
    class(monomial_basis_t), intent(in)  :: this
    integer,                 intent(in)  :: term
    real(dp), dimension(:),  intent(in)  :: x
    real(dp),                        intent(out) :: val
    integer,                         intent(out) :: coef
    integer :: d, i
    val  = one ! << x^[a] >>
    coef = 1   ! << [a]! >>
    do d = 1,this%n_dim
      do i = this%exponents(d,term),1,-1
        val  = val * x(d)
        coef = coef * i
      end do
    end do
  end subroutine evaluate_monomial

  pure subroutine evaluate_monomial_derivative( this, term, x, order,          &
                                                dval, dcoef, coef )
    use set_precision, only : dp
    use set_constants, only : zero, one
    class(monomial_basis_t),         intent(in)  :: this
    integer,                         intent(in)  :: term
    real(dp), dimension(:),          intent(in)  :: x
    integer,  dimension(:),          intent(in)  :: order
    real(dp),                        intent(out) :: dval
    integer,                         intent(out) :: dcoef, coef
    integer :: d, i
    
    dcoef = 1 ! D^[b](x^[a]) = ([a]!)/([a]-[b])! x^[a-b] => << ([a]!)/([a]-[b])! >>
    coef  = 1 ! << ([a]-[b])! >>
    dval  = zero ! << D^[b](x^[a]) >>
    if ( any( this%exponents(:,term)-order(1:this%n_dim) < 0 ) ) return

    dval  = one
    do d = 1,this%n_dim
      do i = this%exponents(d,term),this%exponents(d,term)-order(d)+1,-1
        dcoef = dcoef * i
      end do
      do i = this%exponents(d,term)-order(d),1,-1
        dval  = dval * x(d)
        coef = coef * i
      end do
    end do
  end subroutine evaluate_monomial_derivative

  subroutine check_gradient_indexing( this )
    use set_constants, only : max_text_line_length
    use string_stuff, only : write_integer_tuple
    class(monomial_basis_t), intent(in)  :: this
    integer :: d, term, a, b
    integer, dimension(this%n_dim) :: grad_idx
    character(max_text_line_length) :: tmp_string, out_string
    ! do d = this%total_degree-1,0,-1
    !   ! for each term with this total degree:
    !   do term = this%idx(d),this%idx(d+1)-1
    write(*,*) this%idx
    do d = this%total_degree-1,1,-1
      ! for each term with this total degree:
      ! do term = this%idx(d-1)+1,this%idx(d-1) + 1 + (this%idx(d)-this%idx(d-1)-1)
      do term = this%idx(d-1)+1,this%idx(d)
        grad_idx = this%diff_idx(:,term) ! indices to extract gradient information
        out_string=''
        call write_integer_tuple([d,term],tmp_string)
        write(out_string,'(A,A)') trim(out_string),trim(tmp_string)//' : '
        call write_integer_tuple(this%exponents(:,term),tmp_string)
        write(out_string,'(A,A)') trim(out_string),'['//trim(tmp_string)//'] : '
        call write_integer_tuple(grad_idx,tmp_string)
        write(out_string,'(A,A)') trim(out_string),'('//trim(tmp_string)//')'
        write(*,'(A)') trim(out_string)
      end do
    end do
    term = 1
    grad_idx = this%diff_idx(:,term) ! indices to extract gradient information
    out_string=''
    call write_integer_tuple([d,term],tmp_string)
    write(out_string,'(A,A)') trim(out_string),trim(tmp_string)//' : '
    call write_integer_tuple(this%exponents(:,term),tmp_string)
    write(out_string,'(A,A)') trim(out_string),'['//trim(tmp_string)//'] : '
    call write_integer_tuple(grad_idx,tmp_string)
    write(out_string,'(A,A)') trim(out_string),'('//trim(tmp_string)//')'
    write(*,'(A)') trim(out_string)

    ! constant term
  end subroutine check_gradient_indexing

end module monomial_basis_derived_type

module function_holder_type
  use set_precision, only : dp
  use set_constants, only : zero
  implicit none
  private
  public :: func_h_t

  type, abstract :: func_h_t
    integer :: n_eq
    integer :: n_dim
    logical :: derivatives_implemented
  contains
    procedure :: initialize_super
    procedure, pass :: test_eval
    procedure, pass :: dx_eval   => not_implemented_dx_eval
    procedure, pass :: dt_eval   => not_implemented_dt_eval
    procedure, pass :: dtx_eval  => not_implemented_dtx_eval
    procedure, pass :: grad_eval => not_implemented_grad_eval
    procedure, pass :: hess_eval => not_implemented_hess_eval
    procedure(eval_i),      public, deferred :: eval
    ! procedure(dx_eval_i),   public, deferred :: dx_eval
    ! procedure(dt_eval_i),   public, deferred :: dt_eval
    ! procedure(dtx_eval_i),  public, deferred :: dtx_eval
    ! procedure(grad_eval_i), public, deferred :: grad_eval
    ! procedure(hess_eval_i), public, deferred :: hess_eval
    procedure(destroy_i),   public, deferred :: destroy
  end type func_h_t

  abstract interface
    pure function eval_i( this, x, t ) result(q)
      use set_precision,  only : dp
      import func_h_t
      class(func_h_t),        intent(in) :: this
      real(dp), dimension(:), intent(in) :: x
      real(dp), optional,     intent(in) :: t
      real(dp), dimension(this%n_eq)     :: q
    end function eval_i

    ! pure function dx_eval_i( this, x, t, order ) result(dqdx)
    !   use set_precision,  only : dp
    !   import func_h_t
    !   class(func_h_t),                          intent(in) :: this
    !   real(dp), dimension(:),                   intent(in) :: x
    !   real(dp),                       optional, intent(in) :: t
    !   integer, dimension(this%n_dim), optional, intent(in) :: order
    !   real(dp), dimension(this%n_eq)                       :: dqdx
    ! end function dx_eval_i

    ! pure function dt_eval_i( this, x, t ) result(dqdt)
    !   use set_precision,  only : dp
    !   import func_h_t
    !   class(func_h_t),                          intent(in) :: this
    !   real(dp), dimension(:),                   intent(in) :: x
    !   real(dp),                       optional, intent(in) :: t
    !   real(dp), dimension(this%n_eq)                       :: dqdt
    ! end function dt_eval_i

    ! pure function dtx_eval_i( this, x, t, order, t_order ) result(dqdtdx)
    !   use set_precision,  only : dp
    !   import func_h_t
    !   class(func_h_t),                          intent(in) :: this
    !   real(dp), dimension(:),                   intent(in) :: x
    !   real(dp),                       optional, intent(in) :: t
    !   integer, dimension(this%n_dim), optional, intent(in) :: order
    !   integer,                        optional, intent(in) :: t_order
    !   real(dp), dimension(this%n_eq)                       :: dqdtdx
    ! end function dtx_eval_i

    ! pure function grad_eval_i( this, x, t ) result(gradq)
    !   use set_precision,  only : dp
    !   import func_h_t
    !   class(func_h_t),                          intent(in) :: this
    !   real(dp), dimension(:),                   intent(in) :: x
    !   real(dp),                       optional, intent(in) :: t
    !   real(dp), dimension(this%n_dim,this%n_eq)            :: gradq
    ! end function grad_eval_i

    ! pure function hess_eval_i( this, x, t ) result(hessq)
    !   use set_precision,  only : dp
    !   import func_h_t
    !   class(func_h_t),                          intent(in) :: this
    !   real(dp), dimension(:),                   intent(in) :: x
    !   real(dp),                       optional, intent(in) :: t
    !   real(dp), dimension(this%n_dim,this%n_dim,this%n_eq) :: hessq
    ! end function hess_eval_i

    pure elemental subroutine destroy_i(this)
      import func_h_t
      class(func_h_t), intent(inout) :: this
    end subroutine destroy_i
  end interface

contains
  subroutine initialize_super( this, n_eq, n_dim )
    class(func_h_t),  intent(inout) :: this
    integer,      intent(in)    :: n_eq, n_dim
    this%n_eq     = n_eq
    this%n_dim    = n_dim
    this%derivatives_implemented = .false.
  end subroutine initialize_super

  pure function test_eval( this, n_dim, n_var, x ) result(val)
    class(func_h_t),        intent(in) :: this
    integer,                intent(in) :: n_dim, n_var
    real(dp), dimension(:), intent(in) :: x
    real(dp), dimension(n_var)         :: val
    real(dp), dimension(this%n_eq)     :: tmp_val
    integer :: sz, i
    sz = min(n_var,this%n_eq)
    tmp_val = this%eval(x)
    val = zero
    do i = 1,sz
      val(i) = tmp_val(i)
    end do
  end function test_eval

  pure function not_implemented_dx_eval( this, x, t, order ) result(dqdx)
    class(func_h_t),                          intent(in) :: this
    real(dp), dimension(:),                   intent(in) :: x
    real(dp),                       optional, intent(in) :: t
    integer, dimension(this%n_dim), optional, intent(in) :: order
    real(dp), dimension(this%n_eq)                       :: dqdx
    dqdx = zero
  end function not_implemented_dx_eval

  pure function not_implemented_dt_eval( this, x, t ) result(dqdt)
    class(func_h_t),                          intent(in) :: this
    real(dp), dimension(:),                   intent(in) :: x
    real(dp),                       optional, intent(in) :: t
    real(dp), dimension(this%n_eq)                       :: dqdt
    dqdt = zero
  end function not_implemented_dt_eval

  pure function not_implemented_dtx_eval( this, x, t, order, t_order ) result(dqdtdx)
    class(func_h_t),                          intent(in) :: this
    real(dp), dimension(:),                   intent(in) :: x
    real(dp),                       optional, intent(in) :: t
    integer, dimension(this%n_dim), optional, intent(in) :: order
    integer,                        optional, intent(in) :: t_order
    real(dp), dimension(this%n_eq)                       :: dqdtdx
    dqdtdx = zero
  end function not_implemented_dtx_eval

  pure function not_implemented_grad_eval( this, x, t ) result(gradq)
    class(func_h_t),                          intent(in) :: this
    real(dp), dimension(:),                   intent(in) :: x
    real(dp),                       optional, intent(in) :: t
    real(dp), dimension(this%n_dim,this%n_eq)            :: gradq
    gradq = zero
  end function not_implemented_grad_eval

  pure function not_implemented_hess_eval( this, x, t ) result(hessq)
    class(func_h_t),                          intent(in) :: this
    real(dp), dimension(:),                   intent(in) :: x
    real(dp),                       optional, intent(in) :: t
    real(dp), dimension(this%n_dim,this%n_dim,this%n_eq) :: hessq
    hessq = zero
  end function not_implemented_hess_eval

end module function_holder_type

module test_function_1
  use function_holder_type, only : func_h_t
  use set_precision,        only : dp
  implicit none
  private
  public :: test_fun1_t

  real(dp), dimension(3), parameter :: coefs = [999.0_dp, 888.0_dp, 777.0_dp]

  type, extends(func_h_t) :: test_fun1_t
  contains
    procedure :: eval    => eval_test_fun1
    procedure :: destroy   => destroy_test_fun1
    procedure :: grad_eval => grad_eval_test_fun1
  end type test_fun1_t

  interface test_fun1_t
    procedure constructor
  end interface test_fun1_t
contains
  function constructor(n_dim,n_eq) result(this)
    integer, intent(in) :: n_dim, n_eq
    type(test_fun1_t)   :: this
    call this%destroy()
    call this%initialize_super(n_eq,n_dim)
  end function constructor

  pure elemental subroutine destroy_test_fun1(this)
    class(test_fun1_t), intent(inout) :: this
    continue
  end subroutine destroy_test_fun1

  pure function eval_test_fun1( this, x, t ) result(q)
    class(test_fun1_t),        intent(in) :: this
    real(dp), dimension(:), intent(in) :: x
    real(dp), optional,     intent(in) :: t
    real(dp), dimension(this%n_eq)     :: q
    
    ! q = 999.0_dp * x(1) - 888.0_dp * x(2) + 777.0_dp * x(3) - 666.0_dp
    q = dot_product(x,coefs(1:size(x))) - 666.0_dp
  end function eval_test_fun1

  pure function grad_eval_test_fun1( this, x, t ) result(gradq)
    class(test_fun1_t),        intent(in) :: this
    real(dp), dimension(:),                   intent(in) :: x
    real(dp),                       optional, intent(in) :: t
    real(dp), dimension(this%n_dim,this%n_eq)            :: gradq
    integer :: i
    do i = 1,this%n_eq
      gradq(:,i) = coefs(1:size(x))
    end do
  end function grad_eval_test_fun1

end module test_function_1

module test_function_2
  use function_holder_type, only : func_h_t
  use set_precision,        only : dp
  implicit none
  private
  public :: test_fun2_t

  type, extends(func_h_t) :: test_fun2_t
  contains
    procedure :: eval      => eval_test_fun2
    procedure :: destroy   => destroy_test_fun2
    procedure :: grad_eval => grad_eval_test_fun2
  end type test_fun2_t

  interface test_fun2_t
    procedure constructor
  end interface test_fun2_t
contains
  function constructor(n_dim,n_eq) result(this)
    integer, intent(in) :: n_dim, n_eq
    type(test_fun2_t)   :: this
    call this%destroy()
    call this%initialize_super(n_eq,n_dim)
  end function constructor

  pure elemental subroutine destroy_test_fun2(this)
    class(test_fun2_t), intent(inout) :: this
    continue
  end subroutine destroy_test_fun2

  pure function eval_test_fun2( this, x, t ) result(q)
    use set_constants, only : pi
    class(test_fun2_t),        intent(in) :: this
    real(dp), dimension(:), intent(in) :: x
    real(dp), optional,     intent(in) :: t
    real(dp), dimension(this%n_eq)     :: q
    integer :: i

    q = sin(pi*x(1))
    do i = 2,this%n_dim
      q = q*sin(pi*x(i))
    end do
  end function eval_test_fun2

  pure function grad_eval_test_fun2( this, x, t ) result(gradq)
    use set_constants, only : pi
    class(test_fun2_t),        intent(in) :: this
    real(dp), dimension(:),                   intent(in) :: x
    real(dp),                       optional, intent(in) :: t
    real(dp), dimension(this%n_dim,this%n_eq)            :: gradq
    integer :: n, i
    real(dp), dimension(this%n_dim) :: tmpq

    do n = 1,this%n_dim
      tmpq(n) = pi*cos(pi*x(n))
      do i = 1,n-1
        tmpq(n) = tmpq(n)*sin(pi*x(i))
      end do
      do i = n+1,this%n_dim
        tmpq(n) = tmpq(n)*sin(pi*x(i))
      end do
    end do

    do i = 1,this%n_eq
      gradq(:,i) = tmpq
    end do
  end function grad_eval_test_fun2

end module test_function_2


module test_function_3
  use set_precision, only : dp
  use function_holder_type, only : func_h_t
  implicit none
  private
  public :: test_fun3_t

  type, extends(func_h_t) :: test_fun3_t
    real(dp), dimension(:,:), allocatable :: dx, x0
  contains
    procedure :: eval    => eval_test_fun3
    procedure :: destroy => destroy_test_fun3
  end type test_fun3_t

  interface test_fun3_t
    procedure constructor
  end interface test_fun3_t
contains
  function constructor(n_dim,n_eq,space_scale,space_origin) result(this)
    use set_constants, only : zero, one, near_zero, half, fourth
    integer,                                   intent(in) :: n_dim, n_eq
    real(dp), dimension(n_dim,n_eq), optional, intent(in) :: space_scale, space_origin
    type(test_fun3_t)                                           :: this
    call this%destroy()
    call this%initialize_super(n_eq,n_dim)
    allocate( this%dx(n_dim,n_eq) )
    allocate( this%x0(n_dim,n_eq) )
    this%dx = fourth
    this%x0 = -half
    if ( present(space_scale) ) then
      this%dx = sign(one,space_scale) * max(near_zero,abs(space_scale))
    end if
    if ( present(space_origin) ) this%x0 = space_origin
  end function constructor

  pure elemental subroutine destroy_test_fun3(this)
    class(test_fun3_t), intent(inout) :: this
    if ( allocated(this%dx)   ) deallocate( this%dx   )
    if ( allocated(this%x0)   ) deallocate( this%x0   )
  end subroutine destroy_test_fun3

  pure function eval_test_fun3( this, x, t ) result(q)
    use set_constants, only : one, four, pi
    class(test_fun3_t),        intent(in) :: this
    real(dp), dimension(:), intent(in) :: x
    real(dp), optional,     intent(in) :: t
    real(dp), dimension(this%n_eq)     :: q
    integer :: i
    real(dp), dimension(this%n_dim,this%n_eq) :: x_bar
    do i = 1,this%n_eq
      x_bar(:,i) = (x(1:this%n_dim) - this%x0(:,i)) / this%dx(:,i)
    end do
    if (x_bar(1,1)<-four) then
        q = 3.857143_dp
    else
      q = one + 0.2_dp*sin(5.0_dp*x_bar(1,:))
    end if
  end function eval_test_fun3

end module test_function_3

module test_function_4
  use function_holder_type, only : func_h_t
  use monomial_basis_derived_type, only : monomial_basis_t
  use set_precision,        only : dp
  implicit none
  private
  public :: test_fun4_t

  type, extends(func_h_t) :: test_fun4_t
    type(monomial_basis_t) :: p
    real(dp), dimension(:,:), allocatable :: dx, x0
    real(dp), dimension(:,:), allocatable :: coefs
  contains
    procedure :: eval      => eval_test_fun4
    procedure :: dx_eval   => dx_eval_test_fun4
    procedure :: destroy   => destroy_test_fun4
  end type test_fun4_t

  interface test_fun4_t
    procedure constructor
  end interface test_fun4_t
contains
  function constructor(n_dim,n_eq,space_scale,space_origin) result(this)
    use set_constants, only : zero, one, near_zero
    integer, intent(in) :: n_dim, n_eq
    real(dp), dimension(n_dim,n_eq), optional, intent(in) :: space_scale, space_origin
    type(test_fun4_t)   :: this
    integer :: i
    call this%destroy()
    call this%initialize_super(n_eq,n_dim)
    this%derivatives_implemented = .true.
    allocate( this%dx(n_dim,n_eq) )
    allocate( this%x0(n_dim,n_eq) )
    this%dx = one
    this%x0 = zero
    if ( present(space_scale) ) then
      this%dx = sign(one,space_scale) * max(near_zero,abs(space_scale))
    end if
    if ( present(space_origin) ) this%x0 = space_origin
    this%p = monomial_basis_t(4,n_dim)
    allocate( this%coefs(this%p%n_terms,n_eq) )
    this%coefs      = zero
    this%coefs(1:2,:) = one
    do i = 2,this%p%total_degree
      this%coefs(this%p%idx(i-1)+1,:) = -one
    end do
  end function constructor

  pure elemental subroutine destroy_test_fun4(this)
    class(test_fun4_t), intent(inout) :: this
    if ( allocated(this%dx)   ) deallocate( this%dx   )
    if ( allocated(this%x0)   ) deallocate( this%x0   )
    if ( allocated(this%coefs) ) deallocate( this%coefs )
  end subroutine destroy_test_fun4

  pure function eval_test_fun4( this, x, t ) result(q)
    use set_constants, only : zero
    class(test_fun4_t),        intent(in) :: this
    real(dp), dimension(:), intent(in) :: x
    real(dp), optional,     intent(in) :: t
    real(dp), dimension(this%n_eq)     :: q, q_compare
    real(dp), dimension(this%n_dim,this%n_eq) :: x_bar
    real(dp) :: tmp_val
    integer :: i, v, coef
    do i = 1,this%n_eq
      x_bar(:,i) = x(1:this%n_dim)/this%dx(:,i) + this%x0(:,i)
    end do
    q = 1.0_dp + x_bar(:,1) - x_bar(:,1)**2 - x_bar(:,1)**3 - x_bar(:,1)**4
    q_compare = zero
    do i = 1,this%p%n_terms
      do v = 1,this%n_eq
        call this%p%eval_m(i,x_bar(:,v),tmp_val,coef)
        q_compare(v) = q_compare(v) + this%coefs(i,v)*tmp_val
      end do
    end do
    tmp_val = zero
  end function eval_test_fun4

  pure function dx_eval_test_fun4( this, x, t, order ) result(dqdx)
    class(test_fun4_t),        intent(in) :: this
    real(dp), dimension(:),                    intent(in) :: x
    real(dp),                        optional, intent(in) :: t
    integer,  dimension(this%n_dim), optional, intent(in) :: order
    real(dp), dimension(this%n_eq)                        :: dqdx, dqdx_compare
    integer :: tmp_order
    real(dp), dimension(this%n_dim,this%n_eq) :: x_bar
    integer :: i, v, coef, dcoef
    real(dp) :: tmp_val

    if (present(order)) then
      do i = 1,this%n_eq
        x_bar(:,i) = x(1:this%n_dim)/this%dx(:,i) + this%x0(:,i)
      end do
      tmp_order = order(1)
      select case(tmp_order)
      case(0)
        dqdx = 1.0_dp + 1.0_dp*x_bar(:,1) - 1.0_dp*x_bar(:,1)**2 - 1.0_dp*x_bar(:,1)**3 -  1.0_dp*x_bar(:,1)**4
      case(1)
        ! dqdx = one + x(1) - x(1)**2 - x(1)**3 - x(1)**4
        dqdx =          1.0_dp      - 2.0_dp*x_bar(:,1)    - 3.0_dp*x_bar(:,1)**2 -  4.0_dp*x_bar(:,1)**3
        dqdx = dqdx/this%dx(:,1)
      case(2)
        dqdx =                      - 2.0_dp         - 6.0_dp*x_bar(:,1)    - 12.0_dp*x_bar(:,1)**2
        dqdx = dqdx/( this%dx(:,1)**2 )
      case(3)
        dqdx =                                       - 6.0_dp         - 24.0_dp*x_bar(:,1)
        dqdx = dqdx/( this%dx(:,1)**3 )
      case(4)
        dqdx =                                                        - 24.0_dp
        dqdx = dqdx/( this%dx(:,1)**4 )
      case default
        dqdx = 0.0_dp
      end select

      dqdx_compare = 0.0_dp
      do i = 1,this%p%n_terms
        do v = 1,this%n_eq
          call this%p%deval_m(i,x_bar(:,v),order,tmp_val,dcoef,coef)
          tmp_val = tmp_val * real(dcoef,dp) / product( this%dx(:,v)**order )
          dqdx_compare(v) = dqdx_compare(v) + this%coefs(i,v)*tmp_val
        end do
      end do
      tmp_val = 0.0_dp
    else
      dqdx = this%eval(x,t=t)
    end if

  end function dx_eval_test_fun4

end module test_function_4

module cylinder_cone_bump
  use set_precision, only : dp
  use function_holder_type, only : func_h_t
  implicit none
  private
  public :: ccb_t
 
  type, extends(func_h_t) :: ccb_t
    real(dp), dimension(:),   allocatable :: dt, t0
    real(dp), dimension(:,:), allocatable :: dx, x0
  contains
    procedure :: eval    => eval_ccb
    procedure :: destroy => destroy_ccb
  end type ccb_t

  interface ccb_t
    procedure constructor
  end interface ccb_t
contains
  function constructor(n_dim,n_eq,space_scale,space_origin,time_scale,time_origin) result(this)
    use set_constants, only : zero, one, near_zero
    integer,                                   intent(in) :: n_dim, n_eq
    real(dp), dimension(n_dim,n_eq), optional, intent(in) :: space_scale, space_origin
    real(dp), dimension(n_eq),       optional, intent(in) :: time_scale,  time_origin
    type(ccb_t)                                           :: this
    call this%destroy()
    call this%initialize_super(n_eq,n_dim)
    allocate( this%dt(n_eq) )
    allocate( this%t0(n_eq) )
    allocate( this%dx(n_dim,n_eq) )
    allocate( this%x0(n_dim,n_eq) )
    this%dt = one
    this%dx = one
    this%t0 = zero
    this%x0 = 0.75_dp
    if ( present(space_scale) ) then
      this%dx = sign(one,space_scale) * max(near_zero,abs(space_scale))
    end if
    if ( present(space_origin) ) this%x0 = space_origin
    if ( present(time_scale) ) then
      this%dt = sign(one,time_scale) * max(near_zero,abs(time_scale))
    end if
    if ( present(time_origin) ) this%t0 = time_origin
  end function constructor

  pure elemental subroutine destroy_ccb(this)
    class(ccb_t), intent(inout) :: this
    if ( allocated(this%dt)   ) deallocate( this%dt   )
    if ( allocated(this%dx)   ) deallocate( this%dx   )
    if ( allocated(this%t0)   ) deallocate( this%t0   )
    if ( allocated(this%x0)   ) deallocate( this%x0   )
  end subroutine destroy_ccb

  pure elemental function radius(x,y,x0,y0) result(r)
    real(dp), parameter :: r0          = 0.15_dp
    real(dp), intent(in) :: x, y, x0, y0
    real(dp)             :: r
    r = sqrt( (x-x0)**2 + (y-y0)**2 ) / r0
  end function radius

  pure function slotted_cylinder(x,y) result(G)  
    use set_constants, only : one, zero
    real(dp),     intent(in) :: x, y
    real(dp)                 :: G
    logical :: in_circle, cond1, cond2
    real(dp),               parameter :: slot_width  = 0.025_dp
    real(dp),               parameter :: slot_height = 0.85_dp
    real(dp), dimension(2), parameter :: x0_cyl = [0.50_dp,0.75_dp]
    in_circle = radius(x,y,x0_cyl(1),x0_cyl(2)) <= one
    cond1 = abs(x-x0_cyl(1))>=slot_width
    cond2 = y>=slot_height
    G = merge(one,zero,in_circle.and.(cond1.or.cond2))
  end function slotted_cylinder

  pure function sharp_cone(x,y) result(G)
    use set_constants, only : one, zero
    real(dp),     intent(in) :: x, y
    real(dp)                 :: G
    real(dp) :: r
    logical :: in_circle
    real(dp), dimension(2), parameter :: x0_con = [0.50_dp,0.25_dp]
    r = radius(x,y,x0_con(1),x0_con(2))
    in_circle = r <= one
    G = merge(one-r,zero,in_circle)
  end function sharp_cone

  pure function smooth_bump(x,y) result(G)
    use set_constants, only : one, zero, pi, fourth
    real(dp),     intent(in) :: x, y
    real(dp)                 :: G
    real(dp) :: r
    logical :: in_circle
    real(dp), dimension(2), parameter :: x0_bum = [0.25_dp,0.50_dp]
    r = radius(x,y,x0_bum(1),x0_bum(2))
    in_circle = r <= one
    G = merge(fourth*(one+cos(pi*r)),zero,in_circle)
  end function smooth_bump

  pure function rotate(this,x,theta) result(G)
    class(ccb_t), intent(in) :: this
    real(dp), dimension(:), intent(in) :: x
    real(dp),               intent(in) :: theta
    real(dp)                           :: G
    real(dp), dimension(2) :: xy1, xy2
    integer :: i
    xy1  = 0.75_dp
    do i = 1,min(this%n_dim,2)
      xy1(i) = x(i)
    end do
    ! rotate the point
    xy2(1) = xy1(1) * cos(theta) - xy1(2) * sin(theta)
    xy2(2) = xy1(1) * sin(theta) + xy1(2) * cos(theta)
    G = slotted_cylinder( xy2(1), xy2(2) ) &
      + sharp_cone(       xy2(1), xy2(2) ) &
      + smooth_bump(      xy2(1), xy2(2) )
  end function rotate

  pure function eval_ccb( this, x, t ) result(q)
    use set_precision, only : dp
    use set_constants, only : zero, pi
    class(ccb_t),           intent(in) :: this
    real(dp), dimension(:), intent(in) :: x
    real(dp), optional,     intent(in) :: t
    real(dp), dimension(this%n_eq)     :: q
    real(dp), dimension(this%n_dim,this%n_eq) :: x_bar
    real(dp), dimension(this%n_eq)            :: t_bar
    integer :: coef, i, n
    ! do i = 1,this%n_eq
    !   x_bar(:,i) = (x(1:this%n_dim) - this%x0(:,i)) / this%dx(:,i)
    ! end do
    ! t_bar = zero
    ! if ( present(t) ) then
    !   t_bar = ( t - this%t0 ) / this%dt
    ! end if
    do i = 1,this%n_eq
      x_bar(:,i) = x(1:this%n_dim)/this%dx(:,i) + this%x0(:,i)
    end do
    t_bar = zero
    if ( present(t) ) then
      t_bar = t/this%dt + this%t0
    end if
    do i = 1,this%n_eq
      q(i) = rotate( this, x_bar(:,i) , pi * t_bar(i) )
    end do
  end function eval_ccb

end module cylinder_cone_bump

module cross_term_sinusoid
  use set_precision, only : dp
  use function_holder_type, only : func_h_t
  use monomial_basis_derived_type, only : monomial_basis_t
  implicit none
  private
  public :: cts_t

  type, extends(func_h_t) :: cts_t
    real(dp), dimension(:),   allocatable :: dt, t0, a, e, f, g
    real(dp), dimension(:,:), allocatable :: dx, x0, b, c, d
    type(monomial_basis_t)                :: mono
  contains
    procedure :: eval    => eval_cts
    procedure :: destroy => destroy_cts
  end type cts_t

  interface cts_t
    procedure constructor
  end interface cts_t
contains
  function constructor( n_dim, n_eq, mean, space_coefs, space_scale,           &
                        space_origin, time_coefs, time_scale, time_origin,     &
                        rand_coefs, rand_seed ) result(this)
    use set_constants, only : zero, one, two, pi, near_zero
    use combinatorics, only : nchoosek

    integer,                                        intent(in) :: n_dim, n_eq
    real(dp), dimension(n_eq),                                                 &
                                          optional, intent(in) :: mean
    real(dp), dimension(n_eq,3*nchoosek(2*n_dim,n_dim)),                       &
                                          optional, intent(in) :: space_coefs
    real(dp), dimension(n_dim,n_eq),      optional, intent(in) :: space_scale
    real(dp), dimension(n_dim,n_eq),      optional, intent(in) :: space_origin
    real(dp), dimension(n_eq,3),          optional, intent(in) :: time_coefs
    real(dp), dimension(n_eq),            optional, intent(in) :: time_scale
    real(dp), dimension(n_eq),            optional, intent(in) :: time_origin
    logical,                              optional, intent(in) :: rand_coefs
    integer,                              optional, intent(in) :: rand_seed
    type(cts_t)                                                :: this
    integer :: n, cnt
    integer :: total_degree, n_terms
    integer, dimension(:), allocatable :: seed

    call this%destroy()
    call this%initialize_super(n_eq,n_dim)
    total_degree = n_dim
    this%mono = monomial_basis_t(total_degree,n_dim)
    n_terms = this%mono%n_terms

    allocate( this%dt(n_eq) )
    allocate( this%t0(n_eq) )
    allocate( this%dx(n_dim,n_eq) )
    allocate( this%x0(n_dim,n_eq) )
    allocate( this%a(n_eq) )
    allocate( this%b(n_eq,n_terms) )
    allocate( this%c(n_eq,n_terms) )
    allocate( this%d(n_eq,n_terms) )
    allocate( this%e(n_eq) )
    allocate( this%f(n_eq) )
    allocate( this%g(n_eq) )
    
    this%dt = one
    this%dx = one
    this%t0 = zero
    this%x0 = zero
    this%a  = zero
    this%b  = one
    this%c  = one
    this%d  = one
    this%e  = one
    this%f  = one
    this%g  = one

    if (present(rand_coefs) ) then
      call random_init(.true.,.false.)
      if ( present(rand_seed) ) then
        call random_seed( size=n )
        allocate( seed(n) )
        seed = rand_seed
        call random_seed(put=seed)
        deallocate(seed)
      end if
      call random_number(this%a); this%a = two*this%a - one
      call random_number(this%b); this%b = two*this%b - one
      call random_number(this%c); this%c = two*this%c - one
      call random_number(this%d); this%d = two*this%d - one
      call random_number(this%e); this%e = two*this%e - one
      call random_number(this%f); this%f = two*this%f - one
      call random_number(this%g); this%g = two*this%g - one
    else
      if (present(mean)) this%a = mean
      if (present(space_coefs)) then
        cnt = 0
        do n = 1,n_terms
          cnt = cnt + 1
          this%b(:,n) = space_coefs(:,cnt)
        end do
        do n = 1,n_terms
          cnt = cnt + 1
          this%c(:,n) = space_coefs(:,cnt)
        end do
        do n = 1,n_terms
          cnt = cnt + 1
          this%d(:,n) = space_coefs(:,cnt)
        end do
      end if
      if ( present(time_coefs)   ) then
        this%e  = time_coefs(:,1)
        this%f  = time_coefs(:,2)
        this%g  = time_coefs(:,3)
      end if
    end if
    if ( present(space_scale) ) then
      this%dx = sign(one,space_scale) * max(near_zero,abs(space_scale))
    end if
    if ( present(space_origin) ) this%x0 = space_origin
    if ( present(time_scale) ) then
      this%dt = sign(one,time_scale) * max(near_zero,abs(time_scale))
    end if
    if ( present(time_origin) ) this%t0 = time_origin
  end function constructor

  pure elemental subroutine destroy_cts(this)
    class(cts_t), intent(inout) :: this
    if ( allocated(this%dt)   ) deallocate( this%dt   )
    if ( allocated(this%dx)   ) deallocate( this%dx   )
    if ( allocated(this%t0)   ) deallocate( this%t0   )
    if ( allocated(this%x0)   ) deallocate( this%x0   )
    if ( allocated(this%a)    ) deallocate( this%a    )
    if ( allocated(this%b)    ) deallocate( this%b    )
    if ( allocated(this%c)    ) deallocate( this%c    )
    if ( allocated(this%d)    ) deallocate( this%d    )
    if ( allocated(this%e)    ) deallocate( this%e    )
    if ( allocated(this%f)    ) deallocate( this%f    )
    if ( allocated(this%g)    ) deallocate( this%g    )
    call this%mono%destroy()
  end subroutine destroy_cts

  pure function eval_cts( this, x, t ) result(q)
    class(cts_t),        intent(in) :: this
    real(dp), dimension(:), intent(in) :: x
    real(dp), optional,     intent(in) :: t
    real(dp), dimension(this%n_eq)     :: q
    real(dp), dimension(this%n_dim,this%n_eq) :: x_bar
    real(dp), dimension(this%n_eq)            :: t_bar
    real(dp) :: tmp_val
    integer :: coef, i, n
      do i = 1,this%n_eq
        x_bar(:,i) = (x(1:this%n_dim) - this%x0(:,i)) / this%dx(:,i)
      end do
      q = this%a
      do n = 1,this%mono%n_terms
        do i = 1,this%n_eq
          call this%mono%eval_m(n,x_bar(:,i),tmp_val,coef)
          q(i) = q(i) + this%b(i,n) * sin( this%c(i,n) * tmp_val + this%d(i,n) )
        end do
      end do
      if ( present(t) ) then
        t_bar = ( t - this%t0 ) / this%dt
        do i = 1,this%n_eq
          q(i) = q(i) + this%e(i) * sin( this%f(i) * t_bar(i) + this%g(i) )
        end do
      end if
    end function eval_cts

end module cross_term_sinusoid

module zero_mean_basis_derived_type
  use set_precision, only : dp
  use quadrature_derived_type, only : quad_t
  use monomial_basis_derived_type, only : monomial_basis_t
  implicit none
  private
  public :: zero_mean_basis_t
  type :: zero_mean_basis_t
    private
    real(dp), public, allocatable, dimension(:)   :: moments
    real(dp), public, allocatable, dimension(:)   :: x_ref
    real(dp), public, allocatable, dimension(:)   :: h_ref
  contains
    private
    procedure, pass :: compute_grid_moments
    procedure, pass :: transform
    procedure, public, pass :: shift_moments   => compute_shifted_moments
    procedure, public, pass :: shift_moments_q => compute_shifted_moments_quad
    procedure, public, nopass :: length_scale  => get_length_scale_vector
    procedure, public, pass :: eval_b  => evaluate_basis
    procedure, public, pass :: deval_b => evaluate_basis_derivative
    procedure, public, pass :: rec_eval => evaluate_reconstruction
    procedure, public, pass :: drec_eval => evaluate_reconstruction_derivative
    procedure, public, pass :: scaled_basis_derivative
    procedure, public, pass :: transform_coefs, shift_coefs_from_nbor
    procedure, public, pass :: destroy => destroy_zero_mean_basis_t

  end type zero_mean_basis_t

  interface zero_mean_basis_t
    procedure constructor
  end interface

contains

pure function constructor( p, quad, h_ref ) result(this)
  type(monomial_basis_t), intent(in) :: p
  type(quad_t),           intent(in) :: quad
  real(dp), dimension(:), intent(in) :: h_ref
  type(zero_mean_basis_t)            :: this
  integer :: n

  call this%destroy()
  allocate( this%moments( p%n_terms ) )
  allocate( this%x_ref( p%n_dim ) )
  allocate( this%h_ref( p%n_dim ) )
  this%h_ref   = h_ref(1:p%n_dim)
  this%x_ref   = quad%integrate( p%n_dim, quad%quad_pts(1:p%n_dim,:) )         &
               / sum( quad%quad_wts )
  this%moments = this%compute_grid_moments(p,quad)
end function constructor

pure subroutine destroy_zero_mean_basis_t(this)
  class(zero_mean_basis_t), intent(inout) :: this
  if ( allocated(this%moments) ) deallocate( this%moments )
  if ( allocated(this%x_ref)   ) deallocate( this%x_ref   )
  if ( allocated(this%h_ref)   ) deallocate( this%h_ref   )
end subroutine destroy_zero_mean_basis_t

pure function transform(this,n_dim,x) result(x_bar)
  class(zero_mean_basis_t), intent(in) :: this
  integer,                  intent(in) :: n_dim
  real(dp), dimension(:),   intent(in) :: x
  real(dp), dimension(n_dim)           :: x_bar
  x_bar = (x(1:n_dim)-this%x_ref)/this%h_ref
end function transform

pure function compute_grid_moments(this,p,quad) result(moments)
  use set_constants, only : one
  class(zero_mean_basis_t), intent(in) :: this
  type(monomial_basis_t),   intent(in) :: p
  type(quad_t),             intent(in) :: quad
  real(dp), dimension(p%n_terms)       :: moments
  real(dp), dimension(quad%n_quad)         :: tmp
  real(dp), dimension(p%n_dim,quad%n_quad) :: xtmp
  integer :: n, q, coef
  do q = 1,quad%n_quad
    xtmp(:,q) = this%transform( p%n_dim, quad%quad_pts(:,q) )
  end do
  do n = 1,p%n_terms
    do q = 1,quad%n_quad
      call p%eval_m( n, xtmp(:,q), tmp(q), coef )
    end do
    moments(n) = quad%integrate(tmp)
  end do
  moments = moments / sum( quad%quad_wts )
end function compute_grid_moments

pure function compute_shifted_moments(this,p,nbor) result(moments)
  use set_constants,                only : one, zero
  use message,                      only : error_message
  use combinatorics,                only : nchoosek
  class(zero_mean_basis_t), intent(in) :: this
  type(monomial_basis_t),   intent(in) :: p
  class(zero_mean_basis_t), intent(in) :: nbor
  real(dp), dimension(p%n_terms)       :: moments
  real(dp), dimension(p%n_dim) :: xi_tmp, xj_tmp
  real(dp), dimension(p%n_dim,0:p%total_degree) :: delta1, delta2
  integer,  dimension(p%n_dim) :: alpha, beta, shift_exp
  integer :: n, m, d, shift_row
  integer :: comb_factor
  real(dp) :: grid_factor
  logical :: err, found_exp

  moments = zero

  ! first, compute all the needed point terms
  delta1      = zero
  delta1(:,0) = one
  delta1(:,1) = this%transform( p%n_dim, nbor%x_ref )
  delta2      = zero
  delta2(:,0) = one
  delta2(:,1) = nbor%h_ref / this%h_ref
  do m = 2,p%total_degree
    delta1(:,m) = delta1(:,m-1) * delta1(:,1)
    delta2(:,m) = delta2(:,m-1) * delta2(:,1)
  end do

  ! now, loop through, computing the shifted moments
  do n = 1,p%n_terms
    alpha = p%exponents(:,n)
    do m = 1,n
      beta = p%exponents(:,m)
      if (any( ( alpha - beta) < 0 ) ) cycle

      shift_exp = alpha - beta
      comb_factor = 1
      grid_factor = one
      do d = 1,p%n_dim
        comb_factor = comb_factor * nchoosek( alpha(d), beta(d) )
        grid_factor = grid_factor * delta1( d, beta(d) ) * delta2( d, shift_exp(d) )
      end do

      ! Find corresponding moment for shift_exp
      found_exp = .false.
      do shift_row = 1,p%n_terms
        found_exp = all( p%exponents(:,shift_row) == shift_exp )
        if ( found_exp ) exit
      end do

      ! Moment Hat
      moments(n) = moments(n) + real(comb_factor,dp) * grid_factor * nbor%moments(shift_row)
    end do
    ! remove the 
    moments(n) = moments(n) - this%moments(n)
  end do
end function compute_shifted_moments

pure function compute_shifted_moments_quad(this,p,quad) result(moments)
  use set_constants, only : zero, one
  class(zero_mean_basis_t), intent(in) :: this
  type(monomial_basis_t),   intent(in) :: p
  type(quad_t),             intent(in) :: quad
  real(dp), dimension(p%n_terms)       :: moments
  real(dp), dimension(quad%n_quad)         :: tmp
  integer :: n, q
  moments = zero
  do n = 2,p%n_terms
    do q = 1,quad%n_quad
      tmp(q) = this%eval_b(p,n,quad%quad_pts(:,q))
    end do
    moments(n) = quad%integrate(tmp)
  end do
  moments = moments / sum( quad%quad_wts )
end function compute_shifted_moments_quad

pure function evaluate_basis(this,p,term,point) result(B)
  use set_constants, only : one
  class(zero_mean_basis_t),     intent(in) :: this
  type(monomial_basis_t),       intent(in) :: p
  integer,                      intent(in) :: term
  real(dp), dimension(:),       intent(in) :: point
  real(dp) :: B
  integer :: coef
  B = one
  if (term == 1) return
  call p%eval_m( term, this%transform(p%n_dim,point), B, coef )
  B = B - this%moments(term)
end function evaluate_basis

pure function evaluate_basis_derivative(this,p,term,point,order) result(dB)
  use set_constants, only : zero, one
  class(zero_mean_basis_t), intent(in) :: this
  type(monomial_basis_t),   intent(in) :: p
  integer,                  intent(in) :: term
  integer,  dimension(:), intent(in) :: order
  real(dp), dimension(:), intent(in) :: point
  real(dp) :: dB
  integer :: dcoef,coef

  if (all(order==0)) then
    dB =  this%eval_b(p,term,point)
    return
  end if

  dB = zero
  if (term==1) return

  call p%deval_m( term, this%transform(p%n_dim,point), order, dB, dcoef, coef )
  dB = dB * real( dcoef, dp ) / product( this%h_ref ** order(1:p%n_dim) )
end function evaluate_basis_derivative

pure function get_length_scale_vector( order, scale ) result(L)
  use set_constants, only : one
  integer,  dimension(:),           intent(in) :: order
  real(dp), dimension(size(order)), intent(in) :: scale
  real(dp)                                     :: L
  integer :: n, d, den

  L   = one
  den = 1
  do d = 1,size(order)
    do n = order(d),1,-1
      L   = L * scale(d)
      den = den * n
    end do
  end do
  L = L / real(den,dp)

end function get_length_scale_vector

pure function scaled_basis_derivative( this, p, term_idx, diff_idx,            &
                                       point, scale ) result(derivative)
  class(zero_mean_basis_t), intent(in) :: this
  type(monomial_basis_t),   intent(in) :: p
  integer,                  intent(in) :: term_idx, diff_idx
  real(dp), dimension(:),   intent(in) :: point
  real(dp), dimension(:),   intent(in) :: scale
  real(dp)                             :: derivative
  real(dp) :: L
  derivative = this%deval_b( p, term_idx, point, p%exponents(:,diff_idx) )
  L = this%length_scale( p%exponents(:,diff_idx), scale )
  derivative = derivative * L
end function scaled_basis_derivative

pure function transform_coefs(this,p,coefs,n_terms,n_var,var_idx) result(tcoefs)
  use set_constants, only : one
  class(zero_mean_basis_t),   intent(in) :: this
  type(monomial_basis_t),     intent(in) :: p
  real(dp), dimension(:,:),   intent(in) :: coefs
  integer,                    intent(in) :: n_terms, n_var
  integer,  dimension(n_var), intent(in) :: var_idx
  real(dp), dimension(n_terms,n_var)     :: tcoefs
  real(dp), dimension(n_terms-1) :: xden
  integer :: n, v, coef

  tcoefs(1,:) = this%rec_eval(p,this%x_ref,coefs,n_terms,n_var,var_idx)
  do n = 2,n_terms
    call p%eval_m(n,this%h_ref,xden(n-1),coef)
  end do
  xden = one / xden
  do v = 1,n_var
    tcoefs(2:n_terms,v) = coefs(2:n_terms,var_idx(v)) * xden
  end do
end function transform_coefs


pure function shift_coefs_from_nbor(this,nbor,p,nbor_coefs,n_terms,n_var,var_idx) result(tcoefs)
  use set_constants, only : one
  class(zero_mean_basis_t),   intent(in) :: this, nbor
  type(monomial_basis_t),     intent(in) :: p
  real(dp), dimension(:,:),   intent(in) :: nbor_coefs
  integer,                    intent(in) :: n_terms, n_var
  integer,  dimension(n_var), intent(in) :: var_idx
  real(dp), dimension(n_terms,n_var)     :: tcoefs
  integer :: n
  integer :: dc,fact_coef
  real(dp) :: scale
  real(dp), dimension(p%n_dim) :: point
  integer, dimension(p%n_dim) :: order
  ! evaluate nbor reconstruction at cell centroid
  point = this%x_ref
  tcoefs(1,:) = nbor%rec_eval(p,point,nbor_coefs,n_terms,n_var,var_idx)
  do n = 2,n_terms
    call p%eval_m(n,this%h_ref,scale,fact_coef)
    scale = scale / real(fact_coef,dp)
    order = p%exponents(:,n)
    tcoefs(n,:) = nbor%drec_eval(p,point,nbor_coefs,n_terms,n_var,var_idx,order)
    tcoefs(n,:) = scale * tcoefs(n,:)
    tcoefs(1,:) = tcoefs(1,:) + tcoefs(n,:) * this%moments(n)
  end do
end function shift_coefs_from_nbor

pure function evaluate_reconstruction( this, p, point, coefs, n_terms, n_var, var_idx ) result(val)
    use set_constants, only : zero
    class(zero_mean_basis_t),   intent(in) :: this
    type(monomial_basis_t),     intent(in) :: p
    real(dp), dimension(:),     intent(in) :: point
    real(dp), dimension(:,:),   intent(in) :: coefs
    integer,                    intent(in) :: n_terms, n_var
    integer,  dimension(n_var), intent(in) :: var_idx
    real(dp), dimension(n_var)             :: val
    real(dp), dimension(n_terms) :: basis
    integer :: v, n
    val = zero
    do n = 1,n_terms
      basis(n) = this%eval_b(p,n,point)
    end do
    do v = 1,n_var
      val(v) = val(v) + dot_product( coefs(1:n_terms,var_idx(v)), basis )
    end do
  end function evaluate_reconstruction

  pure function evaluate_reconstruction_derivative( this, p, point, coefs, n_terms, n_var, var_idx, order ) result(val)
    use set_constants, only : zero, one
    class(zero_mean_basis_t),   intent(in) :: this
    type(monomial_basis_t),     intent(in) :: p
    real(dp), dimension(:),     intent(in) :: point
    real(dp), dimension(:,:),   intent(in) :: coefs
    integer,                    intent(in) :: n_terms, n_var
    integer,  dimension(n_var), intent(in) :: var_idx
    integer,  dimension(:),     intent(in) :: order
    real(dp), dimension(n_var)             :: val
    real(dp), dimension(n_terms) :: basis
    integer :: v, n
    real(dp), dimension(p%n_dim) :: scale
    scale = one
    val = zero
    do n = 1,n_terms
      basis(n) = this%deval_b(p,n,point,order)
    end do
    do v = 1,n_var
      val(v) = val(v) + dot_product( coefs(1:n_terms,var_idx(v)), basis )
    end do
  end function evaluate_reconstruction_derivative

end module zero_mean_basis_derived_type

module reconstruct_cell_derived_type
  use set_precision,                only : dp
  use monomial_basis_derived_type,  only : monomial_basis_t
  use zero_mean_basis_derived_type, only : zero_mean_basis_t
  implicit none
  private
  public :: rec_cell_t

  type :: rec_cell_t
    type(zero_mean_basis_t)                 :: basis
    real(dp), dimension(:),     allocatable :: col_scale
    real(dp), dimension(:,:),   allocatable :: coefs
    real(dp), dimension(:,:),   allocatable :: Ainv
    real(dp), dimension(:,:,:), allocatable :: Ainv_sec
    real(dp), dimension(:,:,:), allocatable :: coefs_sec
    real(dp), dimension(:,:),   allocatable :: TCI_mat
    integer :: n_vars
    integer :: self_idx
    integer :: self_block
    integer :: n_nbor, n_bnd, n_sec
    integer, dimension(:), allocatable :: nbor_block, nbor_idx, nbor_degree, bnd_idx, sec_idx
  contains
    private
    procedure, public, pass :: destroy => destroy_cell_rec
    procedure, public, pass :: eval_c    => evaluate_reconstruction
    procedure, public, pass :: deval_c   => evaluate_reconstruction_derivative
    procedure, public, pass :: set_cell_avg => set_cell_avg_func, set_cell_avg_val
    procedure, public, pass :: set_cell_coefs, get_cell_error
    procedure, public, pass :: get_sector_stencil_idx
    procedure,         pass :: linear_weights, modify_coefs, get_oscillation_indicators
    procedure, public, pass :: get_cweno_weights, get_cwenoz_weights, get_aicwenoz_weights
    procedure, public, pass :: apply_cweno, get_mu, troubled_cell_indicator, cweno
  end type rec_cell_t

  interface rec_cell_t
    module procedure constructor
  end interface rec_cell_t

contains
  pure elemental subroutine destroy_cell_rec(this)
    class(rec_cell_t), intent(inout) :: this
    call this%basis%destroy()
    if ( allocated(this%nbor_block ) ) deallocate( this%nbor_block )
    if ( allocated(this%nbor_idx   ) ) deallocate( this%nbor_idx   )
    if ( allocated(this%nbor_degree) ) deallocate( this%nbor_degree)
    if ( allocated(this%bnd_idx    ) ) deallocate( this%bnd_idx    )
    if ( allocated(this%sec_idx    ) ) deallocate( this%sec_idx    )
    if ( allocated(this%coefs      ) ) deallocate( this%coefs      )
    if ( allocated(this%Ainv       ) ) deallocate( this%Ainv       )
    if ( allocated(this%coefs_sec  ) ) deallocate( this%coefs_sec  )
    if ( allocated(this%Ainv_sec   ) ) deallocate( this%Ainv_sec   )
    if ( allocated(this%TCI_mat    ) ) deallocate( this%TCI_mat    )
    this%n_vars     = 0
    this%self_idx   = 0
    this%self_block = 0
    this%n_nbor     = 0
    this%n_bnd      = 0
    this%n_sec      = 0
  end subroutine destroy_cell_rec

  pure function constructor( p, self_block, self_idx, n_nbor, n_bnd, n_sec, nbor_block, nbor_idx, nbor_degree, bnd_idx, sec_idx, n_vars, quad, h_ref ) result(this)
    use set_constants, only : zero,one
    use quadrature_derived_type, only : quad_t
    use project_inputs,          only : use_cweno, use_tci
    type(monomial_basis_t), intent(in)    :: p
    integer,                intent(in)    :: self_block, self_idx, n_nbor, n_bnd, n_sec
    integer, dimension(:),  intent(in)    :: nbor_block, nbor_idx, nbor_degree, bnd_idx, sec_idx
    integer,                intent(in)    :: n_vars
    type(quad_t),           intent(in)    :: quad
    real(dp), dimension(:), intent(in)    :: h_ref
    type(rec_cell_t)                      :: this
    integer :: n_sec_cells, nn

    
    call this%destroy()
    this%basis = zero_mean_basis_t( p, quad, h_ref )
    allocate( this%nbor_block(  n_nbor ) )
    allocate( this%nbor_idx(    n_nbor ) )
    allocate( this%nbor_degree( n_nbor ) )
    allocate( this%bnd_idx(      n_bnd ) )
    allocate( this%coefs( p%n_terms,   n_vars ) )
    allocate( this%Ainv(  p%n_terms-1, n_nbor ) )

    this%n_vars      = n_vars
    this%self_idx    = self_idx
    this%self_block  = self_block
    this%n_nbor      = n_nbor
    this%n_bnd       = n_bnd
    this%nbor_block  = nbor_block(1:n_nbor)
    this%nbor_idx    = nbor_idx(1:n_nbor)
    this%nbor_degree = nbor_degree(1:n_nbor)
    this%bnd_idx     = bnd_idx(1:n_bnd)
    this%coefs       = zero
    this%Ainv        = zero

    if ( use_cweno ) then
      this%n_sec         = n_sec
      n_sec_cells = maxval(sec_idx(1:1+sec_idx(1)))
      allocate( this%sec_idx( n_sec ) )
      allocate( this%coefs_sec( p%idx(1)-1, n_vars,      sec_idx(1) ) )
      allocate( this%Ainv_sec(  p%idx(1)-1, n_sec_cells, sec_idx(1) ) )
      this%sec_idx       = sec_idx(1:n_sec)
      this%n_sec         = n_sec
      this%coefs_sec     = zero
      this%Ainv_sec      = zero
    else
      allocate( this%sec_idx(       0       ) )
      allocate( this%coefs_sec(     0, 0, 0 ) )
      allocate( this%Ainv_sec(      0, 0, 0 ) )
      this%n_sec = 0
    end if

    if ( use_tci ) then
      ! nn = 2*p%n_dim - n_bnd ! face neighbors
      nn = count( nbor_degree(1:n_nbor) == 1 )
      allocate( this%TCI_mat(  nn, p%n_terms-1 ) )
      this%TCI_mat = zero
    else
      allocate( this%TCI_mat( 0, 0 ) )
    end if

  end function constructor

  pure function evaluate_reconstruction( this, p, point, n_terms,              &
                                         n_var, var_idx ) result(val)
    use set_constants, only : zero
    class(rec_cell_t),          intent(in) :: this
    type(monomial_basis_t),     intent(in) :: p
    real(dp), dimension(:),     intent(in) :: point
    integer,                    intent(in) :: n_terms, n_var
    integer,  dimension(n_var), intent(in) :: var_idx
    real(dp), dimension(n_var)             :: val

    real(dp), dimension(n_terms) :: local_basis
    integer :: v, n

    val = zero

    do n = 1,n_terms
      local_basis(n) = this%basis%eval_b(p,n,point)
    end do
    do v = 1,n_var
      val(v) = val(v) + dot_product( this%coefs(1:n_terms,var_idx(v)), local_basis )
    end do
  end function evaluate_reconstruction

    pure function evaluate_reconstruction_derivative( this, p, point, n_terms,              &
                                         n_var, var_idx, order ) result(val)
    use set_constants, only : zero
    class(rec_cell_t),          intent(in) :: this
    type(monomial_basis_t),     intent(in) :: p
    real(dp), dimension(:),     intent(in) :: point
    integer,                    intent(in) :: n_terms, n_var
    integer,  dimension(n_var), intent(in) :: var_idx
    integer,  dimension(:),     intent(in) :: order
    real(dp), dimension(n_var)             :: val
    real(dp), dimension(n_terms) :: local_basis
    integer :: v, n

    val = zero

    do n = 1,n_terms
      local_basis(n) = this%basis%deval_b(p,n,point,order)
    end do

    do v = 1,n_var
      val(v) = val(v) + dot_product( this%coefs(1:n_terms,var_idx(v)), local_basis )
    end do
  end function evaluate_reconstruction_derivative

  pure function get_cell_avg( quad, n_dim, n_var, var_idx, eval_fun ) result(avg)
    use set_constants, only : zero
    use quadrature_derived_type, only : quad_t
    use function_holder_type,    only : func_h_t
    type(quad_t),           intent(in) :: quad
    integer,                intent(in) :: n_dim, n_var
    integer,  dimension(:), intent(in) :: var_idx
    class(func_h_t),        intent(in) :: eval_fun
    real(dp), dimension(n_var)         :: avg
    real(dp), dimension(n_var,quad%n_quad) :: tmp_val
    integer :: n, i
    tmp_val = zero
    do n = 1,quad%n_quad
      tmp_val(:,n) = eval_fun%test_eval( n_dim, n_var, quad%quad_pts(:,n) )
    end do
    avg = quad%integrate( n_var, tmp_val ) / sum( quad%quad_wts)
  end function get_cell_avg

  pure subroutine set_cell_avg_func( this, quad, n_dim, n_var, var_idx, eval_fun )
    use quadrature_derived_type, only : quad_t
    use function_holder_type,    only : func_h_t
    class(rec_cell_t),      intent(inout) :: this
    type(quad_t),           intent(in)    :: quad
    integer,                intent(in)    :: n_dim, n_var
    integer,  dimension(:), intent(in)    :: var_idx
    class(func_h_t),        intent(in)    :: eval_fun
    real(dp), dimension(n_var) :: tmp_val
    integer :: v, s
    tmp_val = get_cell_avg( quad, n_dim, n_var, var_idx, eval_fun )
    do v = 1,n_var
      this%coefs(1,var_idx(v)) = tmp_val(v)
    end do
  end subroutine set_cell_avg_func

  pure subroutine set_cell_coefs( this, n_coef, n_var, coef_idx, var_idx, vals )
    use function_holder_type,    only : func_h_t
    class(rec_cell_t),        intent(inout) :: this
    integer,                  intent(in)    :: n_coef, n_var
    integer,  dimension(:),   intent(in)    :: coef_idx, var_idx
    real(dp), dimension(:,:), intent(in)    :: vals
    integer :: v, n
    do v = 1,n_var
      do n = 1,n_coef
        this%coefs(coef_idx(n),var_idx(v)) = vals(n,v)
      end do
    end do
  end subroutine set_cell_coefs

  pure subroutine set_cell_avg_val( this, n_var, var_idx, var_val )
    class(rec_cell_t),      intent(inout) :: this
    integer,                intent(in)    :: n_var
    integer,  dimension(:), intent(in)    :: var_idx
    real(dp), dimension(:), intent(in)    :: var_val
    call this%set_cell_coefs( 1, n_var, [1], var_idx, spread(var_val,1,1) )
  end subroutine set_cell_avg_val

  pure function get_cell_error( this, p, quad, n_terms, norm, n_var, var_idx, eval_fun ) result(err)
    use set_constants,           only : zero, one
    use quadrature_derived_type, only : quad_t
    use function_holder_type,    only : func_h_t
    class(rec_cell_t),      intent(in) :: this
    type(monomial_basis_t), intent(in) :: p
    type(quad_t),           intent(in) :: quad
    integer,                intent(in) :: n_terms, norm, n_var
    integer, dimension(:),  intent(in) :: var_idx
    class(func_h_t),        intent(in)    :: eval_fun
    real(dp), dimension(n_var)            :: err
    real(dp), dimension(n_var) :: reconstructed, exact
    real(dp), dimension(n_var,quad%n_quad) :: tmp_val
    integer, parameter :: max_L_norm = 10
    integer :: n
    tmp_val = zero
    do n = 1,quad%n_quad
      exact = eval_fun%test_eval( p%n_dim, n_var, quad%quad_pts(:,n) )
      reconstructed = this%eval_c( p, quad%quad_pts(:,n), n_terms, n_var, var_idx )
      tmp_val(:,n) = abs( reconstructed - exact )
    end do
    if (norm>max_L_norm) then
      err = maxval(tmp_val,dim=2)
    else
      err = quad%integrate( size(var_idx), tmp_val**norm )**(one/real(norm,dp))
      err = err / sum( quad%quad_wts)**(one/real(norm,dp))
    end if
  end function get_cell_error

  pure function get_sector_stencil_idx(this,s,n) result(j)
    class(rec_cell_t),      intent(in) :: this
    integer,                intent(in) :: s, n
    integer                            :: j
    integer :: n_stencils, n_cells, offset

    j = -1
    n_stencils = this%sec_idx(1)
    if ( s < 1 .or. s > n_stencils ) return

    n_cells = this%sec_idx(1+s)
    if ( n < 1 .or. n > n_cells ) return

    offset = 1 + n_stencils + sum( this%sec_idx(2:s) )
    j = this%sec_idx( offset + n )
  end function get_sector_stencil_idx

  pure function linear_weights(this,lambda_0) result(lambda)
    use set_constants, only : one
    use project_inputs, only : lambda_0_cweno
    class(rec_cell_t),          intent(in) :: this
    real(dp), optional,         intent(in) :: lambda_0
    real(dp), dimension(0:this%sec_idx(1)) :: lambda

    lambda = one
    lambda(0) = lambda_0_cweno
    if (present(lambda_0)) lambda(0) = lambda_0
    lambda(0)  = one - one/lambda(0)
    lambda(1:) = ( one - lambda(0) )/real(this%sec_idx(1),dp)
  end function linear_weights

  pure function modify_coefs( this, p, lin_weights ) result(coefs_s0)
    class(rec_cell_t),                            intent(in)  :: this
    type(monomial_basis_t),                       intent(in)  :: p
    real(dp), dimension(0:this%sec_idx(1)),       intent(in)  :: lin_weights
    real(dp), dimension(p%n_terms-1,this%n_vars)              :: coefs_s0
    integer :: lin_terms
    integer :: v, s
    lin_terms = p%idx(1) - 1
    
    do v = 1,this%n_vars
      coefs_s0(:,v) = this%coefs(2:,v) / lin_weights(0)
      do s = 1,this%sec_idx(1)
          coefs_s0(1:lin_terms,v) = coefs_s0(1:lin_terms,v) - lin_weights(s)/lin_weights(0) * this%coefs_sec(:,v,s)
      end do
    end do
  end function modify_coefs

  pure function get_oscillation_indicators( this, p, coefs_s0 ) result(osc_ind)
    class(rec_cell_t),                                 intent(in)  :: this
    type(monomial_basis_t),                            intent(in)  :: p
    real(dp), dimension(p%n_terms-1,this%n_vars),      intent(in)  :: coefs_s0
    real(dp), dimension(0:this%sec_idx(1),this%n_vars)             :: osc_ind
    integer :: v, s
    do v = 1,this%n_vars
      osc_ind(0,v) = sum(coefs_s0(:,v)**2)
      do s = 1,this%sec_idx(1)
        osc_ind(s,v) = sum(this%coefs_sec(:,v,s)**2)
      end do
    end do
  end function get_oscillation_indicators

  pure function get_cweno_weights( this, p ) result(weights)
    use project_inputs, only : epsilon_cweno, r_cweno
    class(rec_cell_t),                      intent(in) :: this
    type(monomial_basis_t),                 intent(in) :: p
    real(dp), dimension(0:this%sec_idx(1),this%n_vars) :: weights
    real(dp), dimension(p%n_terms-1,this%n_vars)       :: coefs_s0
    real(dp), dimension(0:this%sec_idx(1))             :: lin_weights
    real(dp), dimension(0:this%sec_idx(1),this%n_vars) :: osc_ind
    integer :: v, s
    lin_weights = this%linear_weights()
    coefs_s0    = this%modify_coefs( p, lin_weights )
    osc_ind     = this%get_oscillation_indicators( p, coefs_s0 )
    do v = 1,this%n_vars
      weights(:,v) = lin_weights / ( osc_ind(:,v) + epsilon_cweno )**r_cweno
      weights(:,v) = weights(:,v) / sum( weights(:,v) )
    end do
  end function get_cweno_weights

  pure function get_cwenoz_weights( this, p ) result(weights)
    use set_constants,  only : one
    use project_inputs, only : epsilon_cweno, r_cweno
    class(rec_cell_t),                      intent(in) :: this
    type(monomial_basis_t),                 intent(in) :: p
    real(dp), dimension(0:this%sec_idx(1),this%n_vars) :: weights
    real(dp), dimension(p%n_terms-1,this%n_vars)       :: coefs_s0
    real(dp), dimension(0:this%sec_idx(1))             :: lin_weights
    real(dp), dimension(0:this%sec_idx(1),this%n_vars) :: osc_ind
    real(dp) :: tau
    integer :: v, s
    lin_weights = this%linear_weights()
    coefs_s0    = this%modify_coefs( p, lin_weights )
    osc_ind     = this%get_oscillation_indicators( p, coefs_s0 )
    do v = 1,this%n_vars
      tau = sum( abs( osc_ind(0,v) - osc_ind(1:,v) ) )/real(this%sec_idx(1),dp)
      weights(:,v) = lin_weights * ( one + ( tau /( osc_ind(:,v) + epsilon_cweno) )**r_cweno )
      weights(:,v) = weights(:,v) / sum( weights(:,v) )
    end do
  end function get_cwenoz_weights

  pure function get_aicwenoz_weights( this, p, mu ) result(weights)
    use set_constants, only : one
    use project_inputs, only : epsilon_cweno, r_cweno
    class(rec_cell_t),                      intent(in) :: this
    type(monomial_basis_t),                 intent(in) :: p
    real(dp), dimension(this%n_vars),       intent(in) :: mu
    real(dp), dimension(0:this%sec_idx(1),this%n_vars) :: weights
    
    real(dp), dimension(p%n_terms-1,this%n_vars)       :: coefs_s0
    real(dp), dimension(0:this%sec_idx(1))             :: lin_weights
    real(dp), dimension(0:this%sec_idx(1),this%n_vars) :: osc_ind
    real(dp) :: tau
    integer :: v, s
    lin_weights = this%linear_weights()
    coefs_s0    = this%modify_coefs( p, lin_weights )
    osc_ind     = this%get_oscillation_indicators( p, coefs_s0 )
    do v = 1,this%n_vars
      tau = sum( abs( osc_ind(0,v) - osc_ind(1:,v) ) )/real(this%sec_idx(1),dp)
      weights(:,v) = lin_weights * ( one + ( tau /( osc_ind(:,v) + epsilon_cweno*mu(v)**2 ) )**r_cweno )
      weights(:,v) = weights(:,v) / sum( weights(:,v) )
    end do
  end function get_aicwenoz_weights

  pure function get_mu(this,RHS) result(mu)
    use set_constants, only : zero
    class(rec_cell_t),                            intent(in) :: this
    real(dp), dimension(this%n_nbor,this%n_vars), intent(in) :: RHS
    real(dp), dimension(this%n_vars)                         :: mu
    real(dp), parameter :: epsilon = 1.0e-40_dp
    real(dp), dimension(this%n_nbor+1) :: vals
    real(dp) :: avg
    integer :: v, n_vals
    n_vals = this%n_nbor+1
    vals = zero
    do v = 1,this%n_vars
      vals     = this%coefs(1,v)
      vals(2:) = vals(2:) + RHS(:,v)
      avg = sum( vals ) / real(n_vals,dp)
      mu(v)  = sum( abs(vals - avg )) / real(n_vals,dp) + epsilon
    end do
  end function get_mu

  pure subroutine apply_cweno( this, p, weights )
    class(rec_cell_t),                                  intent(inout)  :: this
    type(monomial_basis_t),                             intent(in)  :: p
    real(dp), dimension(0:this%sec_idx(1),this%n_vars), intent(in) :: weights
    real(dp), dimension(p%n_terms-1,this%n_vars) :: coefs_s0
    integer :: v, s
    coefs_s0 = this%modify_coefs( p, this%linear_weights() )
    do v = 1,this%n_vars
      this%coefs(2:,v) = weights(0,v) * coefs_s0(:,v)
      do s = 1,this%sec_idx(1)
        this%coefs(2:p%idx(1),v) = this%coefs(2:p%idx(1),v) + weights(s,v) * this%coefs_sec(:,v,s)
      end do
    end do
  end subroutine apply_cweno

  pure subroutine cweno(this,p,mu)
    use set_constants,  only : zero, one
    use project_inputs, only : use_cwenoz
    class(rec_cell_t),                          intent(inout)  :: this
    type(monomial_basis_t),                     intent(in)  :: p
    real(dp), dimension(this%n_vars), optional, intent(in) :: mu
    real(dp), dimension(0:this%sec_idx(1),this%n_vars) ::weights

    if ( use_cwenoz ) then
      if ( present(mu) ) then
        weights = this%get_aicwenoz_weights(p,mu)
      else
        weights = this%get_cwenoz_weights(p)
      end if
    else
      weights = this%get_cwenoz_weights(p)
    end if
    call this%apply_cweno(p,weights)

  end subroutine cweno

  pure function troubled_cell_indicator( this, n_var, var_idx, RHS ) result(val)
    use set_constants, only : near_zero
    class(rec_cell_t),                      intent(in) :: this
    integer,                                intent(in) :: n_var
    integer,  dimension(n_var),             intent(in) :: var_idx
    real(dp), dimension(this%n_nbor,n_var), intent(in) :: RHS
    real(dp), dimension(n_var)                         :: val
    real(dp), dimension(size(this%TCI_mat,1),n_var) :: delta, RHS2
    real(dp) :: avg, nbor_avg_max, den
    integer :: j, v

    RHS2 = RHS( pack([(j,j=1,this%n_nbor)],this%nbor_degree == 1), var_idx )
    delta = matmul( this%TCI_mat, this%coefs(2:,var_idx) ) - RHS2
    do v = 1,n_var
      avg          = this%coefs( 1, var_idx(v) )
      nbor_avg_max = maxval( abs( RHS2(:,v) + avg ) )
      den          = max( abs(avg), nbor_avg_max ) + near_zero
      val(v) = sum( abs( delta(:,v) ) ) / den
    end do
  end function troubled_cell_indicator
  
end module reconstruct_cell_derived_type


module rec_block_derived_type
  use set_precision,                 only : dp
  use grid_derived_type,             only : grid_type
  use monomial_basis_derived_type,   only : monomial_basis_t
  use reconstruct_cell_derived_type, only : rec_cell_t
  implicit none
  private
  public :: rec_block_t
  type :: rec_block_t
    integer :: block_num, n_dim, degree, n_vars, n_cells_total
    integer,   dimension(3) :: n_skip
    integer,   dimension(:), allocatable :: n_cells
    type(rec_cell_t), dimension(:), allocatable :: cells
    type(monomial_basis_t) :: p
  contains
    private
    procedure, public, pass :: destroy => destroy_rec_block
    procedure, public, pass :: solve   => solve_block
    procedure, public, pass :: get_nbors
    procedure, public, pass :: eval_rb => evaluate_block_rec
    procedure, public, pass :: set_cell_avgs => set_cell_avgs_fun
    procedure,         pass :: get_cell_h_ref
    procedure,         pass :: init_cell, solve_cell
    procedure, public, pass :: init_cells
    procedure, public, pass :: get_cell_error, get_cell_derivative_error
    procedure,         pass :: get_cell_LHS, get_cell_RHS
    procedure, public, pass :: get_nbor_min_max
    procedure,         pass :: get_cell_LHS_sec, get_cell_RHS_sec
    procedure,         pass :: determine_maximum_degree, get_nbor_delta_quad
  end type rec_block_t

  interface rec_block_t
    module procedure constructor
  end interface rec_block_t

contains

  pure elemental subroutine destroy_rec_block( this )
    class(rec_block_t), intent(inout) :: this
    if ( allocated(this%n_cells) ) deallocate( this%n_cells )
    if ( allocated(this%cells) ) then
      call this%cells%destroy()
      deallocate( this%cells )
    end if
    this%block_num     = 0
    this%n_dim         = 0
    this%degree        = 0
    this%n_vars        = 0
    this%n_cells_total = 0
  end subroutine destroy_rec_block

  function constructor( grid, block_num, n_dim, degree, n_var, n_skip ) result(this)
    use math,              only : maximal_extents
    use index_conversion,  only : global2local_bnd, get_exterior_mask
    use grid_derived_type, only : grid_type
    use reconstruct_cell_derived_type, only : rec_cell_t
    implicit none
    type(grid_type),       intent(in) :: grid
    integer,               intent(in) :: block_num, n_dim, degree, n_var
    integer, dimension(3), intent(in) :: n_skip
    type(rec_block_t)                 :: this
    integer,  dimension(3)     :: idx_tmp, lo, hi
    real(dp), dimension(n_dim) :: h_ref
    integer :: i, n_interior, n_cell_nodes
    integer :: min_sz, max_sz, n_nbors, max_degree, n_bnd, n_sec
    integer, dimension(6) :: bnd_idx
    integer, dimension(:), allocatable :: nbor_block, nbor_idx, nbor_degree
    integer, dimension(4*n_dim**2+1) :: sec_idx
    call this%destroy()
    allocate( this%n_cells( n_dim ) )
    this%n_skip         = n_skip
    this%n_cells        = grid%gblock(block_num)%n_cells(1:n_dim) / n_skip(1:n_dim)
    this%n_cells_total  = product(this%n_cells)
    this%block_num      = block_num
    this%n_dim          = n_dim
    this%degree         = degree
    this%n_vars         = n_var
    this%p = monomial_basis_t( this%degree, this%n_dim )
    allocate( this%cells(this%n_cells_total) )

    min_sz = (3 * this%p%n_terms)/2
    max_sz = 6*min_sz
    allocate( nbor_block(max_sz), nbor_idx(max_sz), nbor_degree(max_sz) )

    lo = 1
    hi = 1
    hi(1:this%n_dim) = this%n_cells
    idx_tmp = 1
    do i = 1,this%n_cells_total
      idx_tmp(1:this%n_dim) = global2local_bnd( i, lo(1:this%n_dim), hi(1:this%n_dim) )
      h_ref = this%get_cell_h_ref( grid%gblock(block_num), idx_tmp )
      call this%get_nbors( grid, block_num, i, min_sz, n_nbors, nbor_block, nbor_idx, nbor_degree, n_bnd, bnd_idx, n_sec, sec_idx )
      associate( quad => grid%gblock(block_num)%grid_vars%quad( idx_tmp(1),    &
                                                                idx_tmp(2),    &
                                                                idx_tmp(3) ) )
        this%cells(i) = rec_cell_t( this%p,                                &
                                    block_num,                             &
                                    i,                                     &
                                    n_nbors,                               &
                                    n_bnd,                                 &
                                    n_sec,                                 &
                                    nbor_block,                            &
                                    nbor_idx,                              &
                                    nbor_degree,                           &
                                    bnd_idx,                               &
                                    sec_idx,                               &
                                    n_var,                                 &
                                    quad,                                  &
                                    h_ref )
      end associate
    end do
    deallocate( nbor_block, nbor_idx )
  end function constructor

  pure function evaluate_block_rec( this, cell_idx, x, vars, n_terms, order ) result(val)
    use index_conversion, only : local2global
    class(rec_block_t),     intent(in) :: this
    integer,  dimension(:), intent(in) :: cell_idx
    real(dp), dimension(:), intent(in) :: x
    integer,  dimension(:), intent(in) :: vars
    integer,  optional,     intent(in) :: n_terms
    integer,  dimension(:), optional,  intent(in) :: order
    real(dp), dimension(size(vars))    :: val
    integer :: n_terms_, n_vars, lin_idx

    lin_idx = local2global( cell_idx(1:this%n_dim), this%n_cells )
    n_vars = size(vars)
    n_terms_ = this%p%n_terms
    if ( present(n_terms) ) n_terms_ = max(min(n_terms_,n_terms),1)

    if ( present(order) ) then
      val = this%cells(lin_idx)%deval_c( this%p, x, n_terms_, n_vars, vars, order )
    else
      val = this%cells(lin_idx)%eval_c( this%p, x, n_terms_, n_vars, vars )
    end if
  end function evaluate_block_rec

  pure subroutine set_cell_avgs_fun( this, gblock, n_var, var_idx, eval_fun )
    use grid_derived_type,       only : grid_block
    use index_conversion,        only : global2local
    use function_holder_type,    only : func_h_t
    class(rec_block_t),     intent(inout) :: this
    type(grid_block),       intent(in)    :: gblock
    integer,                intent(in)    :: n_var
    integer,  dimension(:), intent(in)    :: var_idx
    class(func_h_t),        intent(in)    :: eval_fun
    real(dp), dimension(n_var) :: tmp_val
    integer,  dimension(3)     :: tmp_idx
    integer :: i, v
    tmp_idx = 1
    do i = 1, this%n_cells_total
      tmp_idx(1:this%n_dim) = global2local(i,this%n_cells)
      associate( quad => gblock%grid_vars%quad( tmp_idx(1),tmp_idx(2),tmp_idx(3) ) )
        call this%cells(i)%set_cell_avg(quad,this%n_dim,n_var,var_idx,eval_fun)
      end associate
    end do
  end subroutine set_cell_avgs_fun

  pure function get_cell_h_ref( this, gblock, idx ) result(h_ref)
    use set_constants,     only : one
    use math,              only : maximal_extents
    use grid_derived_type, only : grid_block, pack_cell_node_coords
    use project_inputs,    only : local_scaling
    class(rec_block_t),    intent(in) :: this
    type(grid_block),      intent(in) :: gblock
    integer, dimension(3), intent(in) :: idx
    real(dp), dimension(this%n_dim)   :: h_ref
    integer, dimension(3) :: lo, hi
    integer :: n_cell_nodes
    real(dp), dimension(:,:), allocatable :: nodes
    
    if ( local_scaling ) then
      n_cell_nodes = product(this%n_skip+1)

      allocate( nodes(3,n_cell_nodes) )
      lo = [1,1,1]
      hi = gblock%n_nodes
      nodes = pack_cell_node_coords( idx, lo, hi, this%n_skip, gblock%node_coords )
      h_ref = maximal_extents( this%n_dim, n_cell_nodes, nodes(1:this%n_dim,:) )
      deallocate( nodes )
    else
      h_ref = one
    end if
  end function get_cell_h_ref

  pure function get_cell_error( this, quad, lin_idx, n_terms, norm,            &
                                n_var, var_idx, eval_fun ) result(err)
    use set_constants,           only : zero, one
    use quadrature_derived_type, only : quad_t
    use function_holder_type,    only : func_h_t
    class(rec_block_t),     intent(in) :: this
    type(quad_t),           intent(in) :: quad
    integer,                intent(in) :: lin_idx, n_terms, norm, n_var
    integer, dimension(:),  intent(in) :: var_idx
    class(func_h_t),        intent(in) :: eval_fun
    real(dp), dimension(n_var)                    :: err
    real(dp), dimension(n_var) :: reconstructed, exact
    real(dp), dimension(n_var,quad%n_quad) :: tmp_val
    integer, parameter :: max_L_norm = 10
    integer :: n
    tmp_val = zero
    do n = 1,quad%n_quad
      exact = eval_fun%test_eval( this%p%n_dim, n_var, quad%quad_pts(:,n) )
      reconstructed = this%cells(lin_idx)%eval_c( this%p, quad%quad_pts(:,n),    &
                                                n_terms, n_var, var_idx )
      tmp_val(:,n) = abs( reconstructed - exact )
    end do
    if (norm>max_L_norm) then
      err = maxval(tmp_val,dim=2)
    else
      err = quad%integrate( size(var_idx), tmp_val**norm )**(one/real(norm,dp))
      err = err / sum( quad%quad_wts)**(one/real(norm,dp))
    end if
    
  end function get_cell_error

  pure function get_cell_derivative_error( this, quad, lin_idx, n_terms, norm,            &
                                           n_var, var_idx, eval_fun, order ) result(err)
    use set_constants,           only : zero, one
    use quadrature_derived_type, only : quad_t
    use function_holder_type,    only : func_h_t
    class(rec_block_t),     intent(in) :: this
    type(quad_t),           intent(in) :: quad
    integer,                intent(in) :: lin_idx, n_terms, norm, n_var
    integer, dimension(:),  intent(in) :: var_idx
    class(func_h_t),        intent(in) :: eval_fun
    integer, dimension(this%n_dim), intent(in) :: order
    real(dp), dimension(n_var)                    :: err
    real(dp), dimension(n_var) :: reconstructed, exact
    real(dp), dimension(n_var,quad%n_quad) :: tmp_val
    integer, parameter :: max_L_norm = 10
    integer :: n
    tmp_val = zero
    do n = 1,quad%n_quad
      exact = eval_fun%dx_eval( quad%quad_pts(:,n), order=order)
      reconstructed = this%cells(lin_idx)%deval_c( this%p, quad%quad_pts(:,n),    &
                                                n_terms, n_var, var_idx, order )
      tmp_val(:,n) = abs( reconstructed - exact )
    end do
    if (norm>max_L_norm) then
      err = maxval(tmp_val,dim=2)
    else
      err = quad%integrate( size(var_idx), tmp_val**norm )**(one/real(norm,dp))
      err = err / sum( quad%quad_wts)**(one/real(norm,dp))
    end if
  end function get_cell_derivative_error

  subroutine init_cell( this, LHS, lin_idx, term_end )
    use math,              only : compute_pseudo_inverse
    use project_inputs,    only : column_scaling, use_cweno, use_tci
    class(rec_block_t),       intent(inout) :: this
    real(dp), dimension(:,:), intent(inout) :: LHS
    integer,                  intent(in)    :: lin_idx
    integer,                  intent(in)    :: term_end
    real(dp), dimension(term_end-1) :: col_scale
    integer :: i, j, n, m
    integer :: k, n_nnbor
    i = lin_idx
    if ( column_scaling ) then
      ! call this%get_cell_LHS( i, term_end, m, n, LHS, col_scale=this%cells(i)%col_scale(1:term_end-1) )
      call this%get_cell_LHS( i, term_end, m, n, LHS, col_scale=col_scale )
    else
      call this%get_cell_LHS( i, term_end, m, n, LHS )
    end if

    if ( use_tci ) then
      ! n_nnbor = 2*this%p%n_dim - this%cells(i)%n_bnd
      ! idx = [(j,j=1,this%cells(i)%n_nbor)]
      n_nnbor = count( this%cells(i)%nbor_degree == 1 )
      ! this%cells(i)%TCI_mat = LHS(1:n_nnbor,1:n)
      this%cells(i)%TCI_mat = LHS( pack([(j,j=1,this%cells(i)%n_nbor)],this%cells(i)%nbor_degree == 1), 1:n )
      do j = 1,n_nnbor
        this%cells(i)%TCI_mat(j,:) = this%cells(i)%TCI_mat(j,:) / col_scale
      end do
    end if

    call compute_pseudo_inverse( m, n, LHS(1:m,1:n), this%cells(i)%Ainv(1:n,1:m) )
    if ( column_scaling ) then
      do j = 1,m
        ! this%cells(i)%Ainv(:,j) = this%cells(i)%col_scale * this%cells(i)%Ainv(:,j)
        this%cells(i)%Ainv(:,j) = col_scale * this%cells(i)%Ainv(:,j)
      end do
    end if

    if ( use_cweno ) then
      if ( column_scaling ) then
        do j = 1,this%cells(i)%sec_idx(1)
          n = this%p%idx(1)-1
          call this%get_cell_LHS_sec(i,j,m,n,LHS,col_scale=col_scale(1:n))
          call compute_pseudo_inverse( m, n, LHS(1:m,1:n), this%cells(i)%Ainv_sec(1:n,1:m,j) )
          do k = 1,m
            this%cells(i)%Ainv_sec(1:n,k,j) = col_scale(1:n) * this%cells(i)%Ainv_sec(1:n,k,j)
          end do
        end do
      else
        do j = 1,this%cells(i)%sec_idx(1)
          call this%get_cell_LHS_sec(i,j,m,n,LHS)
          call compute_pseudo_inverse( m, n, LHS(1:m,1:n), this%cells(i)%Ainv_sec(1:n,1:m,j) )
        end do
      end if
    end if
    
  end subroutine init_cell

  subroutine init_cells( this, grid, term_start, term_end )
    use grid_derived_type, only : grid_type 
    use string_stuff,      only : progress_line
    class(rec_block_t),     intent(inout) :: this
    type(grid_type),        intent(in)    :: grid
    integer,                intent(in)    :: term_start, term_end
    integer :: i, n, m
    real(dp), dimension(:,:), allocatable :: LHS

    m = maxval(this%cells%n_nbor)
    n = this%p%n_terms
    allocate( LHS(m,n) )
    do i = 1,this%n_cells_total
      call progress_line('initializing k-exact cell ',i,this%n_cells_total)
      call this%init_cell( LHS, i, term_end )
    end do
    write(*,*)
    deallocate( LHS )
  end subroutine init_cells

  function determine_maximum_degree( this, grid, blk, min_sz ) result(max_degree)
    use stencil_growing_routines, only : get_max_degree
    use index_conversion,         only : global2local_bnd
    class(rec_block_t),     intent(in)  :: this
    type(grid_type),        intent(in)  :: grid
    integer,                intent(in)  :: blk, min_sz
    integer                             :: max_degree
    integer, dimension(3) :: idx_tmp, lo, hi
    integer :: i
    max_degree = 0
    lo = 1
    hi = 1
    hi(1:this%n_dim) = this%n_cells
    idx_tmp = 1
    do i = 1,this%n_cells_total
      idx_tmp(1:this%n_dim) = global2local_bnd( i, lo(1:this%n_dim), hi(1:this%n_dim) )
      max_degree = max(max_degree,get_max_degree(blk,idx_tmp,hi,min_sz))
    end do
  end function determine_maximum_degree

  pure subroutine get_nbors( this, grid, blk, lin_idx, min_sz, n_nbors, nbor_block, nbor_idx, degree, n_bnd, bnd_idx, n_sec, sec_idx )
    use index_conversion,  only : global2local_bnd, local2global_bnd
    use stencil_growing_routines, only : grow_stencil_basic
    use grid_derived_type, only : grid_type
    class(rec_block_t),                    intent(in)  :: this
    type(grid_type),                       intent(in)  :: grid
    integer,                               intent(in)  :: blk, lin_idx, min_sz
    integer,                               intent(out) :: n_nbors
    integer, dimension(6*min_sz),          intent(out) :: nbor_block, nbor_idx, degree
    integer,                               intent(out) :: n_bnd
    integer, dimension(6),                 intent(out) :: bnd_idx
    integer,                               intent(out) :: n_sec
    integer, dimension(4*this%n_dim**2+1), intent(out) :: sec_idx
    integer, dimension(6*min_sz) :: tmp
    integer :: i
    logical, dimension(6,6*min_sz) :: on_boundary
    integer, dimension(3) :: idx_tmp, lo, hi
    integer, dimension(37) :: sec_idx_
    lo = 1
    hi = 1
    hi(1:this%n_dim) = this%n_cells
    idx_tmp = 1
    idx_tmp(1:this%n_dim) = global2local_bnd( lin_idx, lo(1:this%n_dim), hi(1:this%n_dim) )
    call grow_stencil_basic( blk, idx_tmp, hi, min_sz, n_nbors, nbor_block, nbor_idx, degree, on_boundary=on_boundary, n_sec=n_sec, sec_idx=sec_idx_ )
    sec_idx = sec_idx_(1:4*this%n_dim**2+1)

    !
    n_bnd   = count(on_boundary(1:2*this%p%n_dim,1))
    bnd_idx = 0
    bnd_idx(1:n_bnd) = pack( [(i,i=1,2*this%p%n_dim)],on_boundary(1:2*this%p%n_dim,1))


    ! don't include the central cell
    tmp = nbor_block
    nbor_block(1:n_nbors-1) = tmp(2:n_nbors)
    tmp = nbor_idx
    nbor_idx(1:n_nbors-1) = tmp(2:n_nbors)
    tmp = degree
    degree(1:n_nbors-1) = tmp(2:n_nbors)

    n_nbors = n_nbors - 1
  end subroutine get_nbors

  pure subroutine get_cell_LHS( this, lin_idx, term_end, LHS_m, LHS_n, LHS, col_scale )
    use set_constants, only : zero, one
    class(rec_block_t),   intent(in)  :: this
    integer,                  intent(in)  :: lin_idx, term_end
    integer,                  intent(out) :: LHS_m, LHS_n
    real(dp), dimension(:,:), intent(out) :: LHS
    real(dp), dimension(term_end-1), optional, intent(out) :: col_scale
    real(dp), dimension(this%p%n_terms) :: shifted_moments
    integer :: i, j

    i = lin_idx
    LHS_m = this%cells(i)%n_nbor
    LHS_n = term_end - 1
    LHS = zero
    do j = 1,LHS_m
      shifted_moments = this%cells(i)%basis%shift_moments( this%p, this%cells( this%cells(i)%nbor_idx(j) )%basis )
      LHS(j,1:LHS_n) = shifted_moments(2:term_end)
    end do

    if ( present(col_scale) ) then
      ! Determine Scaling Factor
      do i = 1, LHS_n
        col_scale(i) = sum( abs(LHS(1:LHS_m,i)) )
      end do
      col_scale = one / col_scale

      ! Scale the Matrix
      do i = 1, LHS_n
        LHS(1:LHS_m,i) = LHS(1:LHS_m,i) * col_scale(i)
      end do
    end if

  end subroutine get_cell_LHS

  pure subroutine get_cell_LHS_sec( this, lin_idx, s, LHS_m, LHS_n, LHS, col_scale )
    use set_constants, only : zero, one, near_zero, ten
    class(rec_block_t),       intent(in)  :: this
    integer,                  intent(in)  :: lin_idx, s
    integer,                  intent(out) :: LHS_m, LHS_n
    real(dp), dimension(:,:), intent(out) :: LHS
    real(dp), dimension(this%p%idx(1)-1), optional, intent(out) :: col_scale
    real(dp), dimension(this%p%n_terms) :: shifted_moments
    logical :: scale_
    integer :: term_end, n_nbors
    integer :: i, j, k

    i = lin_idx
    term_end = this%p%idx(1)
    n_nbors = this%cells(i)%sec_idx(1+s)
    LHS_m = n_nbors
    LHS_n = term_end-1
    LHS = zero
    
    do k = 1,LHS_m
      j = this%cells(i)%get_sector_stencil_idx(s,k)
      shifted_moments = this%cells(i)%basis%shift_moments( this%p, this%cells( this%cells(i)%nbor_idx(j) )%basis )
      LHS(k,1:LHS_n) = shifted_moments(2:term_end)
    end do

    if ( present(col_scale) ) then
      ! Determine Scaling Factor
      do i = 1, LHS_n
        col_scale(i) = sum( abs(LHS(1:LHS_m,i)) )
      end do
      col_scale = one / col_scale

      ! Scale the Matrix
      do i = 1, LHS_n
        LHS(1:LHS_m,i) = LHS(1:LHS_m,i) * col_scale(i)
      end do
    end if

  end subroutine get_cell_LHS_sec

  pure subroutine get_cell_RHS( this, lin_idx, n_vars, var_idx, n_nbors, RHS )
    class(rec_block_t), intent(in)  :: this
    integer,                intent(in)  :: lin_idx, n_vars
    integer, dimension(:),  intent(in)  :: var_idx
    integer,                intent(out) :: n_nbors
    real(dp), dimension(:,:), intent(out) :: RHS
    integer :: i, j, k, v
    i = lin_idx
    n_nbors = this%cells(i)%n_nbor
    do k = 1,n_nbors
      j = this%cells(i)%nbor_idx(k)
      ! do v = 1,n_vars
      !   RHS(k,var_idx(v)) = this%cells(j)%coefs(1,var_idx(v)) - this%cells(i)%coefs(1,var_idx(v))
      ! end do
      RHS(k,var_idx) = this%cells(j)%coefs(1,var_idx) - this%cells(i)%coefs(1,var_idx)
    end do
  end subroutine get_cell_RHS

  pure subroutine get_nbor_min_max( this, lin_idx, n_vars, var_idx, min_v, max_v )
    use set_constants, only : large
    class(rec_block_t), intent(in)  :: this
    integer,                intent(in)  :: lin_idx, n_vars
    integer, dimension(:),  intent(in)  :: var_idx
    real(dp), dimension(n_vars), intent(out) :: min_v, max_v
    real(dp), dimension(n_vars) :: tmp_val
    integer :: i, j, k
    i = lin_idx
    max_v = -large
    min_v =  large
    tmp_val = this%cells(i)%coefs(1,var_idx)
    min_v   = min(min_v,tmp_val)
    max_v   = max(max_v,tmp_val)
    do k = 1,this%cells(i)%n_nbor
      if ( this%cells(i)%nbor_degree(k)>1 ) return
      j = this%cells(i)%nbor_idx(k)
      tmp_val = this%cells(j)%coefs(1,var_idx)
      min_v   = min(min_v,tmp_val)
      max_v   = max(max_v,tmp_val)
    end do
  end subroutine get_nbor_min_max

  pure subroutine get_cell_RHS_sec( this, lin_idx, s, n_vars, var_idx, n_nbors, RHS )
    class(rec_block_t), intent(in)  :: this
    integer,                intent(in)  :: lin_idx, s, n_vars
    integer, dimension(:),  intent(in)  :: var_idx
    integer,                intent(out) :: n_nbors
    real(dp), dimension(:,:), intent(out) :: RHS
    integer :: i, j, k, v, offset
    i = lin_idx
    n_nbors = this%cells(i)%sec_idx(1+s)
    do k = 1,n_nbors
      j = this%cells(i)%nbor_idx( this%cells(i)%get_sector_stencil_idx(s,k) )
      ! do v = 1,n_vars
      !   RHS(k,var_idx(v)) = this%cells(j)%coefs(1,var_idx(v)) - this%cells(i)%coefs(1,var_idx(v))
      ! end do
      RHS(k,var_idx) = this%cells(j)%coefs(1,var_idx) - this%cells(i)%coefs(1,var_idx)
    end do
  end subroutine get_cell_RHS_sec

  pure subroutine solve_cell( this, lin_idx, term_end, n_var, var_idx )
    use set_constants, only : zero, near_zero
    use project_inputs, only : use_cweno, use_tci
    use grid_local,     only : grid
    use index_conversion, only : global2local
    class(rec_block_t), intent(inout) :: this
    integer,                intent(in)    :: lin_idx, term_end, n_var
    integer, dimension(:),  intent(in)    :: var_idx
    real(dp), dimension(:,:), allocatable :: RHS, nbor_avg, nbor_delta0, nbor_delta1, nbor_delta2
    real(dp), dimension(n_var) :: T
    real(dp), dimension(this%n_vars) :: mu
    integer :: i, s, v, n_nbors
    logical :: troubled
    i = lin_idx
    allocate( RHS(this%cells(i)%n_nbor,n_var) )
    call this%get_cell_RHS(i,n_var,var_idx,n_nbors,RHS)
    this%cells(i)%coefs(2:,var_idx) = matmul( this%cells(i)%Ainv,RHS(1:n_nbors,:) )    
    ! real(dp), dimension(:,:), allocatable :: nbor_delta0
    ! integer :: n_nnbor
    ! allocate( nbor_delta0(this%cells(i)%n_nbor,n_var) )
    ! n_nnbor = 2*this%n_dim - this%cells(i)%n_bnd
    ! n_nnbor = count( this%cells(i)%nbor_degree == 1 )
    ! call this%get_nbor_delta_quad(this,lin_idx,term_end,n_var,var_idx,nbor_delta0)
    ! do v = 1,n_var
    !   T1(v) = sum(abs(nbor_delta0(1:n_nnbor,v))) / ( maxval( abs( RHS(1:n_nnbor,v) + this%cells(i)%coefs( 1, var_idx(v) ) ) ) + near_zero )
    ! end do
    ! deallocate( nbor_delta0 )
    ! troubled = any( T1 > 0.0015_dp*2.0_dp**(this%n_dim - 1) )
    troubled = .true.
    if ( use_tci ) then
      T = this%cells(i)%troubled_cell_indicator(n_var,var_idx,RHS)
      ! troubled = any( T > 0.0015_dp*2.0_dp**(this%n_dim - 1) )
      troubled = any( T > ( 0.0015_dp/real(this%n_dim,dp)) * 2.0_dp**(this%p%total_degree - 1) )
    end if

    if ( troubled .and. use_cweno ) then
      do s = 1,this%cells(i)%sec_idx(1)
        call this%get_cell_RHS_sec( i, s, n_var, var_idx, n_nbors, RHS )
        this%cells(i)%coefs_sec(:,var_idx,s) = matmul( this%cells(i)%Ainv_sec(:,1:n_nbors,s),RHS(1:n_nbors,:) )
      end do
      call this%cells(i)%cweno(this%p,mu=this%cells(i)%get_mu(RHS))
    end if

    deallocate( RHS )
  end subroutine solve_cell

  pure subroutine get_nbor_delta_quad(this,lin_idx,term_end,n_var,var_idx,nbor_delta)
    use set_constants, only : zero
    use project_inputs, only : column_scaling, use_cweno
    use grid_local,     only : grid
    use index_conversion, only : global2local
    class(rec_block_t), intent(inout) :: this
    integer,                intent(in)    :: lin_idx, term_end, n_var
    integer, dimension(:),  intent(in)    :: var_idx
    real(dp), dimension(this%cells(lin_idx)%n_nbor,n_var), intent(out) :: nbor_delta
    real(dp), dimension(:,:), allocatable :: quad_array
    integer, dimension(3) :: idx
    integer :: i, j, k, q
    i = lin_idx
    do k = 1,this%cells(i)%n_nbor
      j = this%cells(i)%nbor_idx(k)
      idx = 1
      idx(1:this%n_dim) = global2local(j,this%n_cells)
      associate( quad => grid%gblock(1)%grid_vars%quad(idx(1),idx(2),idx(3)) )
        allocate( quad_array(n_var,quad%n_quad) )
        do q = 1, quad%n_quad
          quad_array(:,q) = this%cells(i)%eval_c(this%p,quad%quad_pts(:,q),term_end,n_var,var_idx)
        end do
        nbor_delta(k,:) = quad%integrate(n_var,quad_array) / sum(quad%quad_wts )
        nbor_delta(k,:) = nbor_delta(k,:) - this%cells(j)%coefs(1,:)
        deallocate( quad_array)
      end associate
    end do
  end subroutine get_nbor_delta_quad

  pure subroutine solve_block( this, term_end, n_var, var_idx )
    use set_constants, only : zero
    class(rec_block_t), intent(inout) :: this
    integer,                intent(in)    :: term_end, n_var
    integer, dimension(:),  intent(in)    :: var_idx
    integer :: i
    do i = 1,this%n_cells_total
      call this%solve_cell( i, term_end, n_var, var_idx )
    end do
  end subroutine solve_block

end module rec_block_derived_type

module rec_derived_type
  use set_precision,          only : dp
  use rec_block_derived_type, only : rec_block_t
  use function_holder_type,   only : func_h_t
  implicit none
  private
  public :: rec_t

  type :: rec_t
    private
    integer, public :: n_blocks, n_dim, n_vars, degree
    type(rec_block_t), dimension(:), allocatable, public :: b
  contains
    private
    procedure, public, pass :: destroy => destroy_rec_t
    procedure, public, pass :: solve   => solve_rec
    procedure, public, pass :: eval_rb => evaluate_reconstruction
    procedure, public, pass :: get_block_error_norms, get_block_derivative_error_norms
  end type rec_t

  interface rec_t
    module procedure constructor
  end interface rec_t
contains
  pure elemental subroutine destroy_rec_t( this )
    class(rec_t), intent(inout) :: this
    if ( allocated(this%b) ) then
      call this%b%destroy()
      deallocate( this%b )
    end if
    this%n_blocks = 0
    this%n_dim    = 0
    this%n_vars   = 0
    this%degree   = 0
  end subroutine destroy_rec_t

  function constructor( grid, n_dim, degree, n_vars, n_skip, ext_fun ) result(this)
    use grid_derived_type,      only : grid_type
    use rec_block_derived_type, only : rec_block_t
    use function_holder_type,   only : func_h_t
    type(grid_type),                 intent(in) :: grid
    integer,                         intent(in) :: n_dim, degree, n_vars
    integer, dimension(:), optional, intent(in) :: n_skip
    class(func_h_t),       optional, intent(in) :: ext_fun
    type(rec_t)                                 :: this
    integer, dimension(3) :: n_skip_
    integer :: i, n

    call this%destroy()

    n_skip_ = 0
    n_skip_(1:n_dim) = 1
    if ( present(n_skip) ) n_skip_(1:n_dim) = n_skip(1:n_dim)

    this%n_dim    = n_dim
    this%n_vars   = n_vars
    this%degree   = degree
    this%n_blocks = grid%n_blocks

    allocate( this%b(this%n_blocks) )
    do i = 1,this%n_blocks
      this%b(i) = rec_block_t( grid, i, n_dim, degree, n_vars, n_skip_ )
    end do

    if ( present(ext_fun) ) then
      do i = 1,this%n_blocks
        call this%b(i)%set_cell_avgs( grid%gblock(i), n_vars, [(n,n=1,n_vars)],&
                                      ext_fun )
      end do
    end if

  end function constructor

  pure function evaluate_reconstruction( this, blk, cell_idx,                  &
                                         x, vars, n_terms ) result(val)
    use index_conversion, only : local2global
    class(rec_t),           intent(in) :: this
    integer,                intent(in) :: blk
    integer, dimension(:),  intent(in) :: cell_idx
    real(dp), dimension(:), intent(in) :: x
    integer, dimension(:),  intent(in) :: vars
    integer, optional,      intent(in) :: n_terms
    real(dp), dimension(size(vars))    :: val
    integer :: n_vars

    n_vars = size(vars)
    if ( present(n_terms) ) then
      val = this%b(blk)%eval_rb(cell_idx, x, vars, n_terms=n_terms )
    else
      val = this%b(blk)%eval_rb(cell_idx, x, vars )
    end if
  end function evaluate_reconstruction

  pure function get_block_error_norms( this, grid, blk, var_idx, n_terms, norms,         &
                                      eval_fun ) result(err_norms)
    use set_constants,           only : zero
    use index_conversion,        only : global2local
    use grid_derived_type,       only : grid_type
    use quadrature_derived_type, only : quad_t
    use function_holder_type,    only : func_h_t
    class(rec_t),             intent(in) :: this
    type(grid_type),          intent(in) :: grid
    integer,                  intent(in) :: blk
    integer, dimension(:),    intent(in) :: var_idx
    integer,                  intent(in) :: n_terms
    integer, dimension(:),    intent(in) :: norms
    class(func_h_t),          intent(in) :: eval_fun
    real(dp), dimension(size(var_idx),size(norms)) :: err_norms
    integer, dimension(this%n_dim) :: cell_idx
    integer, dimension(3) :: tmp_idx
    integer, parameter :: max_L_norm = 10
    integer :: n, i, n_var
    n_var = size(var_idx)
    err_norms = zero
    do n = 1,size(norms)
      if (norms(n)>max_L_norm) then
        do i = 1,this%b(blk)%n_cells_total
          cell_idx = global2local(i,grid%gblock(blk)%n_cells(1:this%n_dim))
          tmp_idx = 1
          tmp_idx(1:this%n_dim) = cell_idx
          associate( quad => grid%gblock(blk)%grid_vars%quad( tmp_idx(1),       &
                                                    tmp_idx(2),                 &
                                                    tmp_idx(3) ) )
            err_norms(:,n) = max( err_norms(:,n),                               &
                                this%b(blk)%get_cell_error( quad,               &
                                                      i,                        &
                                                      n_terms,                  &
                                                      norms(n),                 &
                                                      n_var,                    &
                                                      var_idx,                  &
                                                      eval_fun ) )
          end associate
        end do
      else
        do i = 1,this%b(blk)%n_cells_total
          cell_idx = global2local( i, grid%gblock(blk)%n_cells(1:this%n_dim) )
          tmp_idx = 1
          tmp_idx(1:this%n_dim) = cell_idx
          associate( quad => grid%gblock(blk)%grid_vars%quad( tmp_idx(1),      &
                                                    tmp_idx(2),                &
                                                    tmp_idx(3) ) )
            err_norms(:,n) = err_norms(:,n) + this%b(blk)%get_cell_error( quad, &
                                                                    i,          &
                                                                    n_terms,    &
                                                                    norms(n),   &
                                                                    n_var,      &
                                                                    var_idx,    &
                                                                    eval_fun )
          end associate
        end do
        err_norms(:,n) = err_norms(:,n) / real( this%b(blk)%n_cells_total, dp )
      end if
    end do
  end function get_block_error_norms


  pure function get_block_derivative_error_norms( this, grid, blk, var_idx, n_terms, norms,         &
                                                  eval_fun, order ) result(err_norms)
    use set_constants,           only : zero
    use index_conversion,        only : global2local
    use grid_derived_type,       only : grid_type
    use quadrature_derived_type, only : quad_t
    use function_holder_type,    only : func_h_t
    class(rec_t),             intent(in) :: this
    type(grid_type),          intent(in) :: grid
    integer,                  intent(in) :: blk
    integer, dimension(:),    intent(in) :: var_idx
    integer,                  intent(in) :: n_terms
    integer, dimension(:),    intent(in) :: norms
    integer, dimension(this%n_dim), intent(in) :: order
    class(func_h_t),          intent(in) :: eval_fun
    real(dp), dimension(size(var_idx),size(norms)) :: err_norms
    integer, dimension(this%n_dim) :: cell_idx
    integer, dimension(3) :: tmp_idx
    integer, parameter :: max_L_norm = 10
    integer :: n, i, n_var
    n_var = size(var_idx)
    err_norms = zero
    do n = 1,size(norms)
      if (norms(n)>max_L_norm) then
        do i = 1,this%b(blk)%n_cells_total
          cell_idx = global2local(i,grid%gblock(blk)%n_cells(1:this%n_dim))
          tmp_idx = 1
          tmp_idx(1:this%n_dim) = cell_idx
          associate( quad => grid%gblock(blk)%grid_vars%quad( tmp_idx(1),      &
                                                    tmp_idx(2),                &
                                                    tmp_idx(3) ) )
            err_norms(:,n) = max( err_norms(:,n),                              &
                                this%b(blk)%get_cell_derivative_error( quad,   &
                                                      i,                       &
                                                      n_terms,                 &
                                                      norms(n),                &
                                                      n_var,                   &
                                                      var_idx,                 &
                                                      eval_fun,                &
                                                      order ) )
          end associate
        end do
      else
        do i = 1,this%b(blk)%n_cells_total
          cell_idx = global2local( i, grid%gblock(blk)%n_cells(1:this%n_dim) )
          tmp_idx = 1
          tmp_idx(1:this%n_dim) = cell_idx
          associate( quad => grid%gblock(blk)%grid_vars%quad( tmp_idx(1),      &
                                                    tmp_idx(2),                &
                                                    tmp_idx(3) ) )
            err_norms(:,n) = err_norms(:,n) +                                  &
                              this%b(blk)%get_cell_derivative_error( quad,     &
                                                                  i,           &
                                                                  n_terms,     &
                                                                  norms(n),    &
                                                                  n_var,       &
                                                                  var_idx,     &
                                                                  eval_fun,    &
                                                                  order )
          end associate
        end do
        err_norms(:,n) = err_norms(:,n) / real( this%b(blk)%n_cells_total, dp )
      end if
    end do
  end function get_block_derivative_error_norms

  subroutine solve_rec( this, grid, ext_fun, soln_name, output_quad_order, output_derivatives )
    use set_constants, only : zero, max_text_line_length
    use string_stuff,  only : write_integer_tuple
    use combinatorics, only : nchoosek
    use grid_derived_type, only : grid_type
    use function_holder_type, only : func_h_t
    class(rec_t),              intent(inout) :: this
    type(grid_type),           intent(in)    :: grid
    class(func_h_t), optional, intent(in)    :: ext_fun
    character(*),    optional, intent(in)    :: soln_name
    integer,         optional, intent(in)    :: output_quad_order
    logical,         optional, intent(in)    :: output_derivatives

    real(dp), dimension(this%n_vars, 3) :: error_norms
    integer :: i, blk, n, v, nd, nt
    integer :: term_start, term_end, n_vars
    integer :: quad_order
    logical :: old
    character(max_text_line_length) :: derivative_index
    integer, dimension(this%n_dim) :: d_idx

    old = .false.

    quad_order = 1
    if (present(output_quad_order)) quad_order = output_quad_order
    n_vars = this%n_vars

    term_start = 1
    term_end   = nchoosek( this%n_dim + this%degree, this%degree )

    do blk = 1, this%n_blocks
      call this%b(blk)%init_cells( grid, term_start, term_end )
      call this%b(blk)%solve( term_end, n_vars, [(n,n=1,n_vars)] )
      if ( present(ext_fun) ) then
        error_norms = this%get_block_error_norms( grid, blk, [(n,n=1,n_vars)], term_end, [1,2,huge(1)], ext_fun )
        write(*,'(A,I0,A)') 'Block ', blk, ' error norms: '
        do v = 1,n_vars
          write(*,'(I0,3(" ",ES18.12))') v, (error_norms(v,n), n = 1,3)
        end do
        if (ext_fun%derivatives_implemented) then
          do nd = 1,this%b(blk)%p%total_degree
            do nt = this%b(blk)%p%idx(nd-1)+1,this%b(blk)%p%idx(nd)
              d_idx = this%b(blk)%p%exponents(:,nt)
              error_norms = this%get_block_derivative_error_norms( grid, blk, [(n,n=1,n_vars)], term_end, [1,2,huge(1)], ext_fun, d_idx )
              call write_integer_tuple(d_idx,derivative_index)
              write(*,'(A,I0,A)') 'Block ', blk, ' derivative error norms: ('//trim(derivative_index)//')'
              do v = 1,n_vars
                write(*,'(I0,3(" ",ES18.12))') v, (error_norms(v,n), n = 1,3)
              end do
            end do
          end do
        end if
      end if
      if (present(soln_name)) then
        call output_block_reconstruction( grid%gblock(blk), this%b(blk),        &
                                          blk, soln_name, old,                  &
                                          quad_order=quad_order,                &
                                          ext_fun=ext_fun,                      &
                                          rec_out=.true.,                       &
                                          ext_out=.true.,                       &
                                          err_out=.true.,                       &
                                          diff_out=output_derivatives,          &
                                          strand_id=1,                          &
                                          solution_time=zero )
      end if
    end do
  end subroutine solve_rec

  subroutine output_block_reconstruction( gblock1, rec, blk, base_name, old, &
                                          n_skip, quad_order,                &
                                          ext_fun, rec_out, ext_out, err_out,&
                                          diff_out, n_terms, tag,            &
                                          solution_time, strand_id )
    use set_constants, only : max_text_line_length
    use index_conversion, only : global2local
    use rec_block_derived_type,     only : rec_block_t
    use grid_derived_type,          only : grid_block
    use function_holder_type,       only : func_h_t
    type(grid_block),                intent(in)    :: gblock1
    type(rec_block_t),               intent(in)    :: rec
    integer,                         intent(in)    :: blk
    character(*),                    intent(in)    :: base_name
    logical,                         intent(inout) :: old
    integer, dimension(:), optional, intent(in)    :: n_skip
    integer,               optional, intent(in)    :: quad_order
    class(func_h_t),       optional, intent(in)    :: ext_fun
    logical,               optional, intent(in)    :: rec_out, ext_out, err_out, diff_out
    integer,               optional, intent(in)    :: n_terms
    character(*),          optional, intent(in)    :: tag
    integer,               optional, intent(in)    :: strand_id
    real(dp),              optional, intent(in)    :: solution_time
    character(max_text_line_length) :: file_name
    integer :: i
    file_name = trim(base_name)//'-reconstructed'
    if (present(tag)) then
      file_name = trim(file_name)//'-'//trim(tag)//'.dat'
    else
      file_name = trim(file_name)//'.dat'
    end if
    do i = 1,rec%n_cells_total
      call output_cell_reconstruction( gblock1, rec, blk,                     &
                                      global2local(i,rec%n_cells),            &
                                      file_name, old,                         &
                                      n_skip=n_skip,                          &
                                      quad_order=quad_order,                  &
                                      ext_fun=ext_fun,                        &
                                      rec_out=rec_out,                        &
                                      ext_out=ext_out,                        &
                                      err_out=err_out,                        &
                                      diff_out=diff_out,                      &
                                      n_terms=n_terms,                        &
                                      strand_id=strand_id,                    &
                                      solution_time=solution_time )
    end do
  end subroutine output_block_reconstruction

  subroutine output_cell_reconstruction( gblock1, rec, blk, cell_idx,          &
                                         file_name, old, n_skip, quad_order,   &
                                         ext_fun, rec_out, ext_out, err_out, diff_out,  &
                                         n_terms, strand_id, solution_time )
    use index_conversion, only : local2global
    use quadrature_derived_type, only : num_quad_pts, quad_t
    use string_stuff,            only : write_integer_tuple
    use tecplot_output, only : write_tecplot_ordered_zone_header
    use tecplot_output, only : write_tecplot_ordered_zone_block_packed
    use rec_block_derived_type, only : rec_block_t
    use grid_derived_type,          only : grid_block
    use function_holder_type,       only : func_h_t
    type(grid_block),                intent(in)    :: gblock1
    type(rec_block_t),               intent(in)    :: rec
    integer,                         intent(in)    :: blk
    integer, dimension(:),           intent(in)    :: cell_idx
    character(*),                    intent(in)    :: file_name
    logical,                         intent(inout) :: old
    integer, dimension(:), optional, intent(in)    :: n_skip
    integer,               optional, intent(in)    :: quad_order
    class(func_h_t),       optional, intent(in)    :: ext_fun
    logical,               optional, intent(in)    :: rec_out, ext_out, err_out, diff_out
    integer,               optional, intent(in)    :: n_terms
    integer,               optional, intent(in)    :: strand_id
    real(dp),              optional, intent(in)    :: solution_time
    integer :: quad_order_, lin_idx
    integer,  dimension(rec%n_dim) :: n_nodes
    integer, dimension(3) :: n_skip_
    real(dp), dimension(rec%n_dim) :: x
    real(dp), dimension(0,0) :: CELL_DATA
    real(dp), dimension(:,:), allocatable :: NODE_DATA, tmp_var
    logical, dimension(6) :: opt
    type(quad_t) :: phys_quad
    character(*), dimension(6), parameter :: var_prefix = ['REC','EXT','ERR','DIF','DEX','DER']
    character(*), dimension(3), parameter :: xyz        = ['x','y','z']
    character(*), dimension(7), parameter :: var_suffix = ['rho',              &
                                                           'u  ',              &
                                                           'v  ',              &
                                                           'w  ',              &
                                                           'p  ',              &
                                                           't1 ',              &
                                                           't2 ']
    character(100), dimension(:), allocatable :: var_names
    character(100) :: zone_name, tmp_name
    

    integer :: n_dim, n_node_vars, n_cell_vars, n_vars, n_terms_
    integer :: fid, q, n, i, j, cnt
    logical :: file_exists, has_derivatives

    n_dim = rec%n_dim

    lin_idx = local2global(cell_idx(1:n_dim),rec%n_cells)

    n_skip_ = 1
    if ( present(n_skip) ) n_skip_(1:n_dim) = n_skip(1:n_dim)
    n_skip_(n_dim+1:) = 0

    quad_order_ = 1
    if ( present(quad_order) ) quad_order_ = quad_order

    call get_cell_quad( gblock1, cell_idx, n_skip_, quad_order_, phys_quad )

    n_nodes     = num_quad_pts(quad_order_,n_dim,.true.)

    opt = .false.
    has_derivatives = .false.
    if ( present(rec_out)  ) opt(1) = rec_out
    if ( present(ext_fun) ) then
      if ( present(ext_out) ) opt(2) = ext_out
      if ( present(err_out) ) opt(3) = err_out
      has_derivatives = ext_fun%derivatives_implemented
    end if
    if ( present(diff_out) ) opt(4) = diff_out
    opt(5) = opt(2).and.opt(4).and.has_derivatives
    opt(6) = opt(3).and.opt(4).and.has_derivatives

    n_terms_ = rec%p%n_terms
    if ( present(n_terms) ) n_terms_ = max(min(n_terms_,n_terms),1)

    
    n_node_vars = n_dim + count(opt(1:3))*rec%n_vars + count(opt(4:6))*rec%n_vars*(n_terms_-1)
    n_cell_vars = 0
    n_vars      = n_node_vars + n_cell_vars

    allocate( var_names( n_vars ) )
    allocate( tmp_var(   rec%n_vars, 3 ) )
    allocate( NODE_DATA( n_node_vars, phys_quad%n_quad ) )

    
    if (n_dim==1) then
      write(zone_name,'(A)') "('CELL:(',I0,',[',I0,'])')"
    else
      write(zone_name,'(A,I0,A)') "('CELL:(',I0,',[',I0,",n_dim-1,"(',',I0),'])')"
    end if
    write(zone_name,trim(zone_name)) blk, cell_idx(1:n_dim)

    cnt = 0
    do i = 1,n_dim
      cnt = cnt + 1
      var_names(cnt) = xyz(i)
    end do
    do j = 1,3
      if (.not. opt(j)) cycle
      do i = 1,rec%n_vars
        cnt = cnt + 1
        write(var_names(cnt),'(A)') var_prefix(j)//':'//trim(var_suffix(i))
      end do
    end do
    do j = 4,6
      if (.not. opt(j)) cycle
      do i = 1,rec%n_vars
        do n = 2,n_terms_
          cnt = cnt + 1
          call write_integer_tuple( rec%p%exponents(:,n), tmp_name )
          write(var_names(cnt),'(A)') var_prefix(j)//':'//trim(var_suffix(i))//'('//trim(tmp_name)//')'
        end do
      end do
    end do

    do q = 1,phys_quad%n_quad
      x = phys_quad%quad_pts(1:n_dim,q)
      NODE_DATA( 1:n_dim, q ) = x
      tmp_var(:,1) = rec%eval_rb( cell_idx, x, [(i,i=1,rec%n_vars)],              &
                               n_terms=n_terms_ )
      if ( opt(2) .or. opt(3) ) then
        tmp_var(:,2) = ext_fun%eval(x)
        tmp_var(:,3) = tmp_var(:,1) - tmp_var(:,2)
      end if
      cnt = n_dim
      do j = 1,3
        if (.not. opt(j)) cycle
        do i = 1,rec%n_vars
          cnt = cnt + 1
          NODE_DATA(cnt,q) = tmp_var(i,j)
        end do
      end do
      if (opt(4) ) then
        do i = 1,rec%n_vars
          do n = 2,n_terms_
            cnt = cnt + 1
            tmp_var(1:1,1) = rec%eval_rb( cell_idx, x, [i],              &
                               n_terms=n_terms_, order=rec%p%exponents(:,n) )
            NODE_DATA(cnt,q) = tmp_var(1,1)
          end do
        end do
      end if
      if (opt(5) ) then
        do i = 1,rec%n_vars
          do n = 2,n_terms_
            cnt = cnt + 1
            tmp_var(1:1,1) = ext_fun%dx_eval(x,order=rec%p%exponents(:,n))
            NODE_DATA(cnt,q) = tmp_var(1,1)
          end do
        end do
      end if
      if (opt(6) ) then
        do i = 1,rec%n_vars
          do n = 2,n_terms_
            cnt = cnt + 1
            tmp_var(1:1,1) = rec%eval_rb( cell_idx, x, [i],              &
                               n_terms=n_terms_, order=rec%p%exponents(:,n) ) &
                               - ext_fun%dx_eval(x,order=rec%p%exponents(:,n))
            NODE_DATA(cnt,q) = tmp_var(1,1)
          end do
        end do
      end if
    end do
    
    inquire( file=trim(file_name), exist=file_exists )
    if ( file_exists ) then
      if (.not.old) then
        open( newunit=fid, file=trim(file_name), status='unknown')
      else
        open( newunit=fid, file=trim(file_name), status='old',                 &
                                                 position='append' )
      end if
    else
      open( newunit=fid, file=trim(file_name), status='unknown')
    end if
    old = .true.

    call write_tecplot_ordered_zone_header( fid, n_dim, n_nodes,               &
                                           n_node_vars, n_cell_vars,           &
                                           zone_name=zone_name,                &
                                           var_names=var_names,                &
                                           data_packing='block',               &
                                           solution_time=solution_time,        &
                                           strand_id=strand_id )
    call write_tecplot_ordered_zone_block_packed( fid, n_nodes,                &
                                                  n_node_vars, n_cell_vars,    &
                                                  NODE_DATA, CELL_DATA )
    close(fid)
    deallocate( var_names, tmp_var, NODE_DATA )
  end subroutine output_cell_reconstruction

  subroutine get_cell_quad( gblock1, cell_idx, n_skip, quad_order, phys_quad )
    use grid_derived_type,       only : grid_block, get_cell_nodes
    use quadrature_derived_type, only : quad_t,                                &
                                        create_quad_ref_1D,                    &
                                        create_quad_ref_2D,                    &
                                        create_quad_ref_3D,                    &
                                        map_quad_ref_to_physical
    class(grid_block),                     intent(in)  :: gblock1
    integer,                               intent(in)  :: quad_order
    integer,  dimension(:),                intent(in)  :: cell_idx, n_skip
    type(quad_t),                          intent(out) :: phys_quad
    type(quad_t) :: ref_quad
    real(dp), dimension(n_skip(1)+1,n_skip(2)+1,n_skip(3)+1,3) :: coords_tmp
    integer :: n_quad, n_dim
    integer :: status
    integer, dimension(4) :: bnd_tmp
    integer, dimension(3) :: start_idx, bnd_min, bnd_max, sz, loc

    n_dim = gblock1%n_dim
    select case(n_dim)
    case(1)
      call create_quad_ref_1D( quad_order, ref_quad, include_ends=.true. )
    case(2)
      call create_quad_ref_2D( quad_order, ref_quad, include_ends=.true. )
    case(3)
      call create_quad_ref_3D( quad_order, ref_quad, include_ends=.true. )
    case default
      call create_quad_ref_1D( 1, ref_quad )
    end select
    start_idx = 1
    start_idx(1:n_dim) = (cell_idx(1:n_dim)-1)*n_skip(1:n_dim) + 1

    coords_tmp = get_cell_nodes(gblock1,start_idx,n_skip)

    loc = 2
    loc(n_dim+1:) = 0

    associate( X1 => coords_tmp(:,:,:,1), &
               X2 => coords_tmp(:,:,:,2), &
               X3 => coords_tmp(:,:,:,3), &
               gv => gblock1%grid_vars )
      call map_quad_ref_to_physical( X1, X2, X3, loc, gv%interp,               &
                                     ref_quad, phys_quad, status=status )
    end associate
  end subroutine get_cell_quad

end module rec_derived_type









module test_problem
  use set_precision, only : dp
  implicit none
  private
  ! public :: setup_grid_and_rec_2
  public :: setup_grid
  public :: setup_reconstruction
  public :: geom_space_wrapper
  public :: make_eval_function, make_grid, make_reconstruction
  interface setup_grid
    module procedure setup_grid_generate
    ! module procedure setup_grid_read
  end interface setup_grid
contains

  pure function geom_space_wrapper(x_in) result(x_out)
    use project_inputs, only : geom_space_r
    real(dp), dimension(:), intent(in) :: x_in
    real(dp), dimension(size(x_in))    :: x_out
    x_out = geom_space(x_in,geom_space_r)
  end function geom_space_wrapper

  pure function geom_space(x_in,r) result(x_out)
    use set_constants, only : zero, one, near_zero
    use linspace_helper, only : linspace
    real(dp), dimension(:), intent(in) :: x_in
    real(dp),               intent(in) :: r
    real(dp), dimension(size(x_in))    :: x_out
    real(dp) :: dx0, dx
    integer :: i, j, N
    N = size(x_in)
    x_out = zero
    if ( abs(r-one)<near_zero) then
      x_out = linspace(N,x_in(1),x_in(N))
    else
      x_out(1) = x_in(1)
      x_out(N) = x_in(N)
      dx0 = one
      do i = 1,N-2
        dx0 = dx0 + r**i
      end do
      dx0 = ( x_in(N) - x_in(1) ) / dx0
      do i = 2,N-1
        dx = dx0
        do j = 1,i-2
          dx = dx * r
        end do
        x_out(i) = x_out(i-1) + dx
      end do
    end if
  end function geom_space

  subroutine setup_grid_generate( n_dim, n_nodes, n_ghost, grid, delta,        &
                                  end_pts, x1_map, x2_map, x3_map )
    use grid_derived_type,    only : grid_type
    use linspace_helper,      only : cartesian_mesh, perturb_mesh, map_1D_fun
    integer, intent(in) :: n_dim
    integer, dimension(3), intent(in) :: n_nodes, n_ghost
    type(grid_type),       intent(out) :: grid
    real(dp), optional,    intent(in)  :: delta
    real(dp), dimension(3,2), optional, intent(in) :: end_pts
    procedure(map_1D_fun), optional    :: x1_map, x2_map, x3_map
    ! integer, dimension(3), optional, intent(in) :: n_blocks
    ! integer :: n_total_blocks
    ! n_total_blocks = 1
    ! if (present(n_blocks) ) then
    !   n_total_blocks = product(n_blocks)
    ! end if

    call grid%setup(1)
    call grid%gblock(1)%setup(n_dim,n_nodes,n_ghost)
    call grid%gblock(1)%set_nodes( cartesian_mesh( n_nodes(1),                 &
                                                   n_nodes(2),                 &
                                                   n_nodes(3),                 &
                                                   end_pts=end_pts,            &
                                                   x_fun=x1_map,               &
                                                   y_fun=x2_map,               &
                                                   z_fun=x3_map ) )
    if (present(delta) ) then
      call perturb_mesh( grid%gblock(1)%node_coords, delta )
    end if
    call grid%gblock(1)%grid_vars%setup( grid%gblock(1) )
  end subroutine setup_grid_generate

  subroutine setup_reconstruction( grid, n_dim, n_vars, degree, rec, eval_fun )
    use grid_derived_type,    only : grid_type
    use rec_derived_type,     only : rec_t
    use function_holder_type, only : func_h_t
    type(grid_type),           intent(in)  :: grid
    integer,                   intent(in)  :: n_dim, n_vars, degree
    type(rec_t),               intent(out) :: rec
    class(func_h_t), optional, intent(in)  :: eval_fun
    rec = rec_t( grid, n_dim, degree, n_vars, ext_fun=eval_fun )
  end subroutine setup_reconstruction

  subroutine make_eval_function(eval_fun)
    use project_inputs,       only : test_function, space_origin, space_scale, &
                                     time_origin, time_scale, rand_coefs,      &
                                     rand_seed, n_dim, n_rec_vars
    use function_holder_type, only : func_h_t
    use test_function_1,      only : test_fun1_t
    use test_function_2,      only : test_fun2_t
    use test_function_3,      only : test_fun3_t
    use test_function_4,      only : test_fun4_t
    use cross_term_sinusoid,  only : cts_t
    use cylinder_cone_bump,   only : ccb_t
    use message,              only : error_message
    class(func_h_t), allocatable, intent(out) :: eval_fun
    character(*), parameter :: routine_name = 'set_eval_function'
    logical :: err
    select case(test_function)
    case(1)
      allocate( eval_fun, source=test_fun1_t( n_dim, n_rec_vars ) )
    case(2)
      allocate( eval_fun, source=test_fun2_t( n_dim, n_rec_vars ) )
    case(3)
      allocate( eval_fun, source=test_fun3_t( n_dim, n_rec_vars, &
                                    space_scale=space_scale, &
                                    space_origin=space_origin) )
    case(4)
      allocate( eval_fun, source=test_fun4_t( n_dim, n_rec_vars, &
                                    space_scale=space_scale, &
                                    space_origin=space_origin) )
    case(5)
      allocate( eval_fun, source=cts_t( n_dim, n_rec_vars,       &
                                    rand_coefs=rand_coefs,   &
                                    rand_seed=rand_seed,     &
                                    space_scale=space_scale, &
                                    space_origin=space_origin) )
    case(6)
      allocate( eval_fun, source=ccb_t( n_dim, n_rec_vars,       &
                                    space_scale=space_scale, &
                                    space_origin=space_origin) )
    case default
      err = error_message(routine_name,'test_function must be 1-4')
    end select
  end subroutine make_eval_function

  subroutine make_grid( grid )
    use project_inputs,       only : n_dim, n_nodes, n_ghost, grid_perturb
    use grid_derived_type,    only : grid_type
    use grid_local,           only : grid_l => grid
    type(grid_type), intent(inout) :: grid
    call setup_grid( n_dim, n_nodes, n_ghost, grid, delta=grid_perturb, x1_map=geom_space_wrapper )

    ! copy of grid saved in a module to help with debugging
    call setup_grid( n_dim, n_nodes, n_ghost, grid_l, delta=grid_perturb, x1_map=geom_space_wrapper )
  end subroutine make_grid

  subroutine make_reconstruction( grid, eval_fun, rec )
    use grid_derived_type,    only : grid_type
    use function_holder_type, only : func_h_t
    use rec_derived_type,     only : rec_t
    use project_inputs,       only : n_dim, n_rec_vars, rec_degree
    type(grid_type),           intent(in)  :: grid
    class(func_h_t),           intent(in)  :: eval_fun
    type(rec_t),               intent(out) :: rec
    call setup_reconstruction( grid, n_dim, n_rec_vars, rec_degree, rec, eval_fun=eval_fun )
  end subroutine make_reconstruction

end module test_problem

program main
  use set_precision, only : dp
  use project_inputs, only : job_name, out_quad_order, out_derivatives, deallocate_inputs
  use timer_derived_type, only : basic_timer_t
  use namelist,       only : read_nml
  use test_problem,  only : make_eval_function, make_grid, make_reconstruction
  use grid_derived_type, only : grid_type
  use rec_derived_type, only : rec_t
  use function_holder_type, only : func_h_t
  use grid_local,           only : grid_l => grid
  implicit none
  type(grid_type) :: grid
  type(rec_t) :: rec
  class(func_h_t), allocatable :: eval_fun
  type(basic_timer_t) :: timer
  call timer%tic()
  call read_nml( )
  call make_eval_function( eval_fun)
  call make_grid( grid )
  call make_reconstruction( grid, eval_fun, rec )
  call rec%solve( grid,ext_fun=eval_fun,soln_name=job_name,output_quad_order=out_quad_order,output_derivatives=out_derivatives)
  call rec%destroy()
  call grid%destroy()
  call grid_l%destroy()
  
  if ( allocated(eval_fun) ) then
    call eval_fun%destroy()
    deallocate(eval_fun)
  end if
  call deallocate_inputs()
  write(*,*) 'Elapsed time: ', timer%toc()
end program main

! program main
!   use string_stuff,     only : write_integer_tuple
!   use stencil_indexing, only : sector_offsets
!   implicit none
!   integer, dimension(:,:), allocatable :: offsets
!   integer :: n_dim, dir
!   integer :: i, s, d
!   character(100) :: tmp_str

!   n_dim = 3
!   dir   = 1
!   allocate( offsets(n_dim,2*n_dim-1))
!   do d = 1,n_dim
!     do s = -1,1,2
!       write(*,*)
!       write(*,'(A,I0,A,SP,I2,A)') '(',d,',',s,')'
!       dir = d*s
!       call sector_offsets(dir,n_dim,offsets)
!       do i = 1,size(offsets,2)
!         call write_integer_tuple(offsets(:,i),tmp_str,plus=.true.)
!         write(*,'(I0,A)') i, ' : ('//trim(tmp_str)//')'
!       end do
!     end do
!   end do
!   deallocate( offsets )
! end program main

! program main
!   use set_precision, only : dp
!   use namelist,       only : read_nml
!   use project_inputs, only : n_dim, rec_degree, deallocate_inputs
!   use monomial_basis_derived_type, only : monomial_basis_t
!   implicit none
!   type(monomial_basis_t) :: p
!   call read_nml( )
!   p = monomial_basis_t( rec_degree, n_dim )
!   call p%check_gradient_indexing()
!   call p%destroy()
!   call deallocate_inputs()
! end program main