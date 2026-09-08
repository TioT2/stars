module stars
    use iso_fortran_env
    use iso_c_binding
    use sdl2
    implicit none

    type :: random_t
        unsigned(kind=uint64) :: v0, v1, v2, v3
    end type random_t

    type :: timer_t
        integer(kind=int64) :: freq, start, now
        real                :: time, dt

        integer(kind=int64) :: fps_duration, fps_last_measure
        integer             :: fps_frames
        real                :: fps
    end type timer_t

    type :: vec3_t
        real :: x, y, z
    end type vec3_t

    ! c-compatible projection star for being successfully c_qsort-ed
    type, bind(c) :: proj_star_t
        integer(kind=c_int32_t) :: x, y
        real(kind=c_float) :: d2
    end type proj_star_t

    type :: input_t
        real :: rot = 0, acc = 0, dx = 0, dy = 0
    end type input_t

    interface
        subroutine qsort(array, elem_count, elem_size, compare) bind(c, name="qsort")
            import :: c_ptr, c_size_t, c_funptr
            type(c_ptr), value :: array
            integer(c_size_t), value :: elem_count
            integer(c_size_t), value :: elem_size
            type(c_funptr), value :: compare
        end subroutine qsort
    end interface

contains
    pure function input_compose_csum(l, r) result(o)
        type(real), intent(in) :: l, r
        type(real) :: o
        o = max(-1.0, min(1.0, l + r))
    end function input_compose_csum

    pure function input_compose(l, r) result(o)
        type(input_t), intent(in) :: l, r
        type(input_t) :: o
        o%rot = input_compose_csum(l%rot, r%rot)
        o%acc = input_compose_csum(l%acc, r%acc)
        o%dx = input_compose_csum(l%dx, r%dx)
        o%dy = input_compose_csum(l%dy, r%dy)
    end function input_compose

    function compare_proj_stars(l, r) result(cmp) bind(c)
        type(c_ptr), value :: l, r
        type(proj_star_t), pointer :: lv, rv
        integer(c_int) :: cmp

        call c_f_pointer(l, lv)
        call c_f_pointer(r, rv)

        cmp = (lv%d2 > rv%d2 ? -1 : 1)
    end function compare_proj_stars

    pure function input_from_key(key, v) result(inp)
        integer(kind=c_int), intent(in) :: key
        real, intent(in) :: v
        type(input_t) :: inp

        select case (key)
        case (SDL_SCANCODE_Q)
            inp%rot = +v
        case (SDL_SCANCODE_E)
            inp%rot = -v
        case (SDL_SCANCODE_R)
            inp%acc = +v
        case (SDL_SCANCODE_F)
            inp%acc = -v
        case (SDL_SCANCODE_W)
            inp%dy = +v
        case (SDL_SCANCODE_S)
            inp%dy = -v
        case (SDL_SCANCODE_D)
            inp%dx = +v
        case (SDL_SCANCODE_A)
            inp%dx = -v
        end select
    end function input_from_key

    ! make timer
    function make_timer() result(res)
        type(timer_t) :: res

        res%freq = sdl_get_performance_frequency()
        res%start = sdl_get_performance_counter()
        res%now = res%start
        res%time = 0.01
        res%dt = 0.01

        res%fps_duration = res%freq * 3
        res%fps_last_measure = res%now - 1
        res%fps = 0.0
        res%fps_frames = 0
    end function make_timer

    pure function timer_dt(tm, beg, end) result(dt)
        type(timer_t), intent(in) :: tm
        integer(kind=int64), intent(in) :: beg, end
        real :: dt

        dt = real(end - beg) / tm%freq
    end function timer_dt

    pure function timer_fps_is_new(tm) result(inew)
        type(timer_t), intent(in) :: tm
        logical :: inew
        inew = (tm%fps_last_measure == tm%now)
    end function timer_fps_is_new

    subroutine timer_update(tm)
        type(timer_t), intent(inout) :: tm
        integer(kind=int64) :: now

        now = sdl_get_performance_counter()
        tm%time = timer_dt(tm, tm%start, now)
        tm%dt = timer_dt(tm, tm%now, now)
        tm%now = now

        tm%fps_frames = tm%fps_frames + 1
        if (now - tm%fps_last_measure > tm%fps_duration) then
            tm%fps = real(tm%fps_frames) / &
                timer_dt(tm, tm%fps_last_measure, now)
            tm%fps_last_measure = now
            tm%fps_frames = 0
        end if
    end subroutine timer_update

    subroutine rotate_stars(stars, angle)
        type(vec3_t), intent(inout) :: stars(:)
        real, intent(in) :: angle
        real :: sina, cosa, x, z
        integer :: i
        sina = sin(angle)
        cosa = cos(angle)

        do i = 1, size(stars)
            x = stars(i)%z * sina + stars(i)%x * cosa
            z = stars(i)%z * cosa - stars(i)%x * sina

            stars(i)%x = x
            stars(i)%z = z
        end do
    end subroutine rotate_stars

    subroutine move_stars(stars, rand, delta)
        type(vec3_t), intent(inout) :: stars(:)
        type(random_t), intent(inout) :: rand
        type(vec3_t), intent(in) :: delta
        type(vec3_t) :: s
        integer :: i

        do i = 1, size(stars)
            s = vec3_add(stars(i), delta)
            if (vec3_dot(s, s) >= 1) then
                s = random_next_unit_vec3(rand)
                if (vec3_dot(s, delta) >= 0) then
                    s = vec3_neg(s)
                end if
            end if
            stars(i) = s
        end do
    end subroutine move_stars

    pure function vec3_add(l, r) result(o)
        type(vec3_t), intent(in) :: l, r
        type(vec3_t) :: o

        o%x = l%x + r%x
        o%y = l%y + r%y
        o%z = l%z + r%z
    end function vec3_add

    pure function vec3_splat(c) result(v)
        real, intent(in) :: c
        type(vec3_t) :: v

        v%x = c
        v%y = c
        v%z = c
    end function vec3_splat

    pure function vec3_dot(l, r) result(d)
        type(vec3_t), intent(in) :: l, r
        real :: d

        d = l%x * r%x + l%y * r%y + l%z * r%z
    end function vec3_dot

    pure function vec3_neg(v) result(n)
        type(vec3_t), intent(in) :: v
        type(vec3_t) :: n
        n%x = -v%x
        n%y = -v%y
        n%z = -v%z
    end function vec3_neg

    pure function vec3_from_spherical(phi, theta) result(v)
        real, intent(in) :: phi, theta
        type(vec3_t) :: v

        v%x = cos(phi) * sin(theta)
        v%y = sin(phi) * sin(theta)
        v%z = cos(theta)
    end function vec3_from_spherical

    function splitmix64(state) result(r)
        unsigned(kind=uint64), intent(inout) :: state
        unsigned(kind=uint64) :: r

        state = state + uint(z'9E3779B97F4A7C15', kind=uint64)
        r = state
        r = ieor(r, shiftr(r, 30)) * uint(z'BF58476D1CE4E5B9', kind=uint64)
        r = ieor(r, shiftr(r, 27)) * uint(z'94D049BB133111EB', kind=uint64)
        r = ieor(r, shiftr(r, 31))
    end function splitmix64

    function make_random(seed) result(r)
        unsigned(kind=uint64), intent(in) :: seed
        unsigned(kind=uint64) :: state
        type(random_t) :: r
        state = seed
        r%v0 = splitmix64(state)
        r%v1 = splitmix64(state)
        r%v2 = splitmix64(state)
        r%v3 = splitmix64(state)
    end function make_random

    function random_next_uint64(r) result(res)
        type(random_t), intent(inout) :: r
        unsigned(kind=uint64) :: res, v, temp

        res = r%v0 + ishftc(r%v0 + r%v3, 23)
        temp = shiftl(r%v1, 17)

        r%v2 = ieor(r%v2, r%v0)
        r%v3 = ieor(r%v3, r%v1)
        r%v1 = ieor(r%v1, r%v2)
        r%v0 = ieor(r%v0, r%v3)

        r%v2 = ieor(r%v2, temp)
        r%v3 = ishftc(r%v3, 45)
    end function random_next_uint64

    function random_next_unit_real8(r) result(res)
        type(random_t), intent(inout) :: r
        real(kind = 8) :: res
        res = real(random_next_uint64(r), kind=8) / real(uint(z'FFFFFFFFFFFFFFFF', kind = 8), kind = 8)
    end function random_next_unit_real8

    function random_next_unit_real(r) result(res)
        type(random_t), intent(inout) :: r
        real :: res
        res = real(random_next_unit_real8(r), kind=4)
    end function random_next_unit_real

    function random_next_unit_vec3(r) result(res)
        type(random_t), intent(inout) :: r
        real, parameter :: pi = 4.0d0 * atan(1.0d0)
        real :: k1, k2
        type(vec3_t) :: res
        k1 = random_next_unit_real(r)
        k2 = random_next_unit_real(r)
        res = vec3_from_spherical(2 * pi * k1, acos(2 * k2 - 1))
    end function random_next_unit_vec3

    function random_next_sphere_vec3(r) result(res)
        type(random_t), intent(inout) :: r
        type(vec3_t) :: res
        do
            res%x = random_next_unit_real(r) * 2 - 1
            res%y = random_next_unit_real(r) * 2 - 1
            res%z = random_next_unit_real(r) * 2 - 1
            if (vec3_dot(res, res) <= 1) exit
        end do
    end function random_next_sphere_vec3
end module stars

program main
    use sdl2
    use sdl2_video
    use sdl2_surface
    use sdl2_timer
    use iso_c_binding
    use iso_fortran_env

    use stars
    implicit none

    ! just service variables
    integer       :: init, i
    type(c_ptr)   :: window
    logical       :: main_loop

    ! context
    type(timer_t) :: timer
    type(input_t) :: input
    type(random_t) :: random
    type(vec3_t), allocatable :: star_buffer(:)
    type(proj_star_t), allocatable, target :: proj_buffer(:)
    real :: movement_speed

    init = sdl_init(SDL_INIT_VIDEO)
    if (init /= 0) then
        write (error_unit, *) "window initialization failed: ", sdl_get_error()
        stop
    end if

    window = sdl_create_window("stars-fortran" // c_null_char, &
        SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 800, 600, 0)

    if (.not. c_associated(window)) then
        write (error_unit, *) "cannot create window: ", sdl_get_error()
        call sdl_quit()
        stop
    end if

    ! initialize everything
    allocate(star_buffer(65536))
    allocate(proj_buffer(size(star_buffer)))
    timer = make_timer()
    random = make_random(47u_8)
    input = input_t()
    movement_speed = 0.47

    do i = 1, size(star_buffer)
        star_buffer(i) = random_next_sphere_vec3(random)
    end do

    ! main loop
    main_loop = .true.
    do while (main_loop)
        ! event loop
        block
            type(sdl_event) :: event

            do while (sdl_poll_event(event) /= 0)
                select case (event%type)
                    case (SDL_QUITEVENT)
                        main_loop = .false.
                    case (SDL_KEYDOWN)
                        input = input_compose(input, &
                            input_from_key(event%key%key_sym%scan_code, 1.0))
                    case (SDL_KEYUP)
                        input = input_compose(input, &
                            input_from_key(event%key%key_sym%scan_code, -1.0))
                end select
            end do
        end block

        ! timer update
        call timer_update(timer)

        if (timer_fps_is_new(timer)) then
            write (*, '("FPS: ", G0)') timer%fps
        end if


        ! control
        block
            real, parameter :: acc_speed = 1, rot_speed = 1
            type(vec3_t) :: offset

            movement_speed = movement_speed + timer%dt * input%acc * acc_speed

            if (abs(input%rot) > 0.1) then
                call rotate_stars(star_buffer, input%rot * rot_speed * timer%dt)
            end if

            offset%x = -movement_speed * timer%dt * input%dx
            offset%y = -movement_speed * timer%dt * input%dy
            offset%z = -movement_speed * timer%dt * 1.0
            call move_stars(star_buffer, random, offset)
        end block

        ! rendering
        block
            integer :: upd, locked
            type(sdl_surface), pointer :: surface
            type(sdl_pixel_format), pointer :: pixel_format
            integer :: proj_i
            real :: half_w, half_h, clip, wh_scale, xy_mul
            unsigned(kind=uint8), pointer :: pixels(:)

            surface => sdl_get_window_surface(window)

            if (.not. associated(surface)) then
                write (error_unit, *) "cannot access window surface: ", &
                    sdl_get_error()
                cycle
            end if

            call c_f_pointer(surface%format, pixel_format)
            if (pixel_format%bytes_per_pixel /= 4) then
                write (error_unit, *) "window surface bpp must be 4, but is ", &
                    pixel_format%bytes_per_pixel
                cycle
            end if

            locked = sdl_lock_surface(surface)
            if (locked /= 0) then
                write (error_unit, *) "cannot lock surface: ", sdl_get_error()
                cycle
            end if

            clip = 0.5
            half_w = real(surface%w) / 2.0
            half_h = real(surface%h) / 2.0
            wh_scale = sqrt(real(surface%w * surface%w + surface%h * surface%h) &
                / (1.0 * clip * clip))
            xy_mul = clip * real(surface%w * surface%h) / wh_scale

            ! projection
            proj_i = 1
            do i = 1, size(star_buffer)
                block
                    type(vec3_t) :: s
                    type(proj_star_t) :: pt

                    s = star_buffer(i)
                    if (s%z <= 0) cycle

                    pt%x = int(half_w + xy_mul * s%x / s%z)
                    if (pt%x < 0 .or. pt%x > surface%w - 4) cycle

                    pt%y = int(half_h - xy_mul * s%y / s%z)
                    if (pt%y < 0 .or. pt%y > surface%h - 4) cycle

                    pt%d2 = vec3_dot(s, s)
                    proj_buffer(proj_i) = pt
                    proj_i = proj_i + 1
                end block
            end do

            ! sort projection buffer
            call qsort(c_loc(proj_buffer(1)), int(proj_i - 1, kind=c_size_t), &
                c_sizeof(proj_buffer(1)), c_funloc(compare_proj_stars))

            ! capture c pointer
            call c_f_pointer(surface%pixels, pixels, [surface%pitch * surface%h])

            pixels = 0u

            ! rendering
            do i = 1, proj_i - 1
                block
                    type(proj_star_t) :: star
                    unsigned(kind=uint8) :: color
                    integer :: y, off, size

                    star = proj_buffer(i)
                    if (star%d2 < 0.0025) then
                        size = 4
                    else if (star%d2 < 0.01) then
                        size = 3
                    else if (star%d2 < 0.09) then
                        size = 2
                    else
                        size = 1
                    end if

                    ! rendering loop
                    color = uint(255.0 * (1.0 - star%d2))
                    off = star%y * surface%pitch + star%x * 4 + 4
                    do y = 1, size
                        pixels(off:off + size * 4) = color
                        off = off + surface%pitch
                    end do
                end block
            end do

            call sdl_unlock_surface(surface)
            upd = sdl_update_window_surface(window)
        end block
    end do

    deallocate(star_buffer)
    deallocate(proj_buffer)

    call sdl_destroy_window(window)
    call sdl_quit()
end program main
