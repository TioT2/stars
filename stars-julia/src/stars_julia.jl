using SimpleDirectMediaLayer
using SimpleDirectMediaLayer.LibSDL2

struct Vec3{T}
    x::T
    y::T
    z::T
end

function from_spherical(phi::T, theta::T)::Vec3{T} where T <: AbstractFloat
    ps, pc = sincos(phi)
    ts, tc = sincos(theta)
    return Vec3{T}(pc * ts, ps * ts, tc)
end

dot(l::Vec3{T}, r::Vec3{T}) where T = l.x * r.x + l.y * r.y + l.z * r.z
Base.:+(l::Vec3{T}, r::Vec3{T}) where T = Vec3(l.x + r.x, l.y + r.y, l.z + r.z)
Base.:-(v::Vec3{T}) where T = Vec3(-v.x, -v.y, -v.z)

mutable struct RandState
    v0::UInt64
    v1::UInt64
    v2::UInt64
    v3::UInt64
end

function splitmix64(state::Ref{UInt64})::UInt64
    r = (state[] += 0x9E3779B97F4A7C15)
    r = (r ⊻ (r >> 30)) * 0xBF58476D1CE4E5B9
    r = (r ⊻ (r >> 27)) * 0x94D049BB133111EB
    return r ⊻ (r >> 31)
end

function RandState(seed::UInt64)::RandState
    seedref = Ref{UInt64}(seed)
    return RandState(
        splitmix64(seedref),
        splitmix64(seedref),
        splitmix64(seedref),
        splitmix64(seedref),
    )
end

function rand_uint64!(xs::RandState)::UInt64
    result = xs.v0 + bitrotate(xs.v3 + xs.v0, 23)
    temp = xs.v1 << 17

    xs.v2 ⊻= xs.v0
    xs.v3 ⊻= xs.v1
    xs.v1 ⊻= xs.v2
    xs.v0 ⊻= xs.v3

    xs.v2 ⊻= temp
    xs.v3 = bitrotate(xs.v3, 45)

    return result
end

rand_unit_float64!(xs::RandState)::Float64 = Float64(rand_uint64!(xs)) / Float64(0xFFFF_FFFF_FFFF_FFFF)
rand_unit_float32!(xs::RandState)::Float32 = Float32(rand_unit_float64!(xs))

rand_unit_vec3!(xs::RandState)::Vec3{Float32} = from_spherical(
    2.0f0 * Float32(Base.pi) * rand_unit_float32!(xs),
    acos(rand_unit_float32!(xs) * 2.0f0 - 1.0f0)
)

function rand_sphere_vec3!(xs::RandState)::Vec3{Float32}
    while true
        v = Vec3(
            rand_unit_float32!(xs) * 2 - 1,
            rand_unit_float32!(xs) * 2 - 1,
            rand_unit_float32!(xs) * 2 - 1
        )

        dot(v, v) <= 1 && return v
    end
end

mutable struct TimeCounter
    start::UInt64
    now::UInt64
    freq::UInt64
    delta_time::Float32
    time::Float32
    fps_frame_count::Int32
    fps_duration::UInt64
    fps_last_measure::UInt64
    fps::Float32
end

function TimeCounter()::TimeCounter
    now = SDL_GetPerformanceCounter()
    freq = SDL_GetPerformanceFrequency()
    return TimeCounter(now, now, freq, 0.01, 0.01, 0, freq * 3, now, NaN32)
end

timer_new_fps(timer::TimeCounter)::Union{Float32, Nothing} =
    timer.now == timer.fps_last_measure ? timer.fps : nothing

function timer_update!(timer::TimeCounter)
    dt(t0::UInt64, t1::UInt64)::Float32 = Float32(t1 - t0) / Float32(timer.freq)

    now = SDL_GetPerformanceCounter()
    timer.delta_time = dt(timer.now, now)
    timer.time = dt(timer.start, now)
    timer.now = now

    timer.fps_frame_count += 1
    if now - timer.fps_last_measure > timer.fps_duration
        timer.fps = Float32(timer.fps_frame_count) / dt(timer.fps_last_measure, now)
        timer.fps_frame_count = 0
        timer.fps_last_measure = now
    end
end

struct Input
    rot::Float32
    acc::Float32
    dx::Float32
    dy::Float32
end

Input(; rot = 0, acc = 0, dx = 0, dy = 0) = Input(rot, acc, dx, dy)
function Base.:+(l::Input, r::Input)::Input
    @inline csum(f::Symbol)::Float32 = clamp(getproperty(l, f) + getproperty(r, f), -1, 1)
    return Input(csum(:rot), csum(:acc), csum(:dx), csum(:dy))
end

function input_from_key(k::SDL_Scancode, pressed::Bool)::Input
    ks = pressed ? +1 : -1
    if     k == SDL_SCANCODE_Q Input(rot = +ks)
    elseif k == SDL_SCANCODE_E Input(rot = -ks)
    elseif k == SDL_SCANCODE_R Input(acc = +ks)
    elseif k == SDL_SCANCODE_F Input(acc = -ks)
    elseif k == SDL_SCANCODE_W Input(dy = +ks)
    elseif k == SDL_SCANCODE_S Input(dy = -ks)
    elseif k == SDL_SCANCODE_D Input(dx = +ks)
    elseif k == SDL_SCANCODE_A Input(dx = -ks)
    else                       Input()
    end
end

# Projected star
struct ProjStar
    x::UInt32
    y::UInt32
    d2::Float32
end

mutable struct Context
    window::Ptr{SDL_Window}
    rand::RandState
    input::Input
    stars::Vector{Vec3{Float32}}
    timer::TimeCounter
    speed::Float32
end

function move_stars!(context::Context, delta::Vec3{Float32})
    for i in eachindex(context.stars)
        context.stars[i] += delta
        if dot(context.stars[i], context.stars[i]) >= 1
            ns = rand_unit_vec3!(context.rand)
            context.stars[i] = dot(ns, delta) <= 0 ? ns : -ns
        end
    end
end

function rotate_stars!(context::Context, angle::Float32)
    as, ac = sincos(angle)
    for i in eachindex(context.stars)
        star = context.stars[i]
        context.stars[i] = Vec3(
            star.z * as + star.x * ac,
            star.y,
            star.z * ac - star.x * as
        )
    end
end

function c_arrow(p::Ptr{T}, s::Symbol) where {T}
    i = findfirst(==(s), fieldnames(T))
    i === nothing && throw(ArgumentError("$T does not have field $s"))
    return unsafe_load(Ptr{fieldtype(T, i)}(p + fieldoffset(T, i)))
end

function render!(context::Context)
    surface_ptr::Ptr{SDL_Surface} = SDL_GetWindowSurface(context.window)
    surface_ptr == C_NULL && return

    pixel_format_ptr::Ptr{SDL_PixelFormat} = c_arrow(surface_ptr, :format)
    bpp::UInt8 = c_arrow(pixel_format_ptr, :BytesPerPixel)
    bpp != 4 && return

    SDL_LockSurface(surface_ptr) != 0 && return

    w::Int32 = c_arrow(surface_ptr, :w)
    h::Int32 = c_arrow(surface_ptr, :h)

    whalf = w / 2.0f0
    hhalf = h / 2.0f0
    clip = 0.5f0
    wh_scale = sqrt(Float32(w * w + h * h) / (1 - clip * clip))
    xymul = clip * w * h / wh_scale

    # Project stars and sort them
    proj_stars::Vector{ProjStar} = []
    sizehint!(proj_stars, length(context.stars) / 2)
    for s::Vec3{Float32} in context.stars
        s.z <= 0 && continue
        xs = Base.unsafe_trunc(UInt32, whalf + xymul * s.x / s.z)
        xs >= w - 4 && continue
        ys = Base.unsafe_trunc(UInt32, hhalf - xymul * s.y / s.z)
        ys >= h - 4 && continue
        push!(proj_stars, ProjStar(xs, ys, dot(s, s)))
    end
    sort!(proj_stars, lt = (l, r) -> l.d2 > r.d2)

    pitch::Int32 = c_arrow(surface_ptr, :pitch)
    pixels::Ptr{UInt8} = convert(Ptr{UInt8}, c_arrow(surface_ptr, :pixels))

    # Clear screen
    Base.memset(pixels, 0, pitch * h)

    # Render!
    for s in proj_stars
        size = if s.d2 < 0.0025 4
        elseif s.d2 < 0.01 3
        elseif s.d2 < 0.09 2
        else 1 end

        color = round(UInt8, 255 * (1 - s.d2))

        pptr = pixels + (s.y * pitch + s.x * 4)
        for _ in 1:size
            Base.memset(pptr, color, size * 4)
            pptr += pitch
        end
    end

    SDL_UnlockSurface(surface_ptr)
    SDL_UpdateWindowSurface(context.window)
end

function run!(context::Context)
    do_quit = false

    evref = Ref{SDL_Event}()

    while !do_quit
        while SDL_PollEvent(evref) != 0
            event = evref[]
            
            if event.type == SDL_QUIT
                do_quit = true
            elseif event.type == SDL_KEYDOWN
                context.input += input_from_key(event.key.keysym.scancode, true)
            elseif event.type == SDL_KEYUP
                context.input += input_from_key(event.key.keysym.scancode, false)
            end
        end

        timer_update!(context.timer)

        drot_dt = 1
        dacc_dt = 1

        context.speed += context.timer.delta_time * context.input.acc * dacc_dt
        if abs(context.input.rot) > 0.1
            rotate_stars!(context, drot_dt * context.timer.delta_time * context.input.rot)
        end

        ds = -context.speed * context.timer.delta_time
        move_stars!(context, Vec3(context.input.dx * ds, context.input.dy * ds, ds))

        fps = timer_new_fps(context.timer)
        if fps !== nothing
            println("FPS: $(fps)")
        end

        render!(context)
    end
end

function (@main)(_)
    SDL_Init(SDL_INIT_VIDEO) != 0 && return

    # Create in 1000x750 to counter usage of system-wide sdl2-compat by other implementations
    window = SDL_CreateWindow("stars-julia", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 1000, 750, 0)
    # window = SDL_CreateWindow("stars-julia", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 800, 600, 0)

    window == C_NULL && return

    rand = RandState(UInt64(47))
    stars = [rand_sphere_vec3!(rand) for _ in 1:8192]

    run!(Context(
        window,
        rand,
        Input(),
        stars,
        TimeCounter(),
        0.47
    ))
    
    SDL_DestroyWindow(window)
    
    SDL_Quit()
end
