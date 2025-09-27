#include <SDL2/SDL.h>

#include <algorithm>
#include <cmath>
#include <chrono>
#include <cstring>
#include <print>
#include <vector>

/// 3-component floating-point vector
struct vec3 {
    float x;
    float y;
    float z;

    constexpr vec3( float x, float y, float z ) noexcept: x(x), y(y), z(z) {}
    explicit constexpr vec3( float t ) noexcept: x(t), y(t), z(t) {}

    static vec3 from_spherical( float phi, float theta ) {
        return vec3(
            std::cos(phi) * std::sin(theta),
            std::sin(phi) * std::sin(theta),
            std::cos(theta)
        );
    }

    constexpr float dot( vec3 rhs ) const noexcept {
        return x * rhs.x + y * rhs.y + z * rhs.z;
    }

    constexpr float length2( void ) const noexcept {
        return dot(*this);
    }

    constexpr vec3 operator*( float t ) const noexcept {
        return vec3(x * t, y * t, z * t);
    }

    constexpr vec3 operator+( vec3 r ) const noexcept {
        return vec3(x + r.x, y + r.y, z + r.z);
    }

    constexpr vec3 operator-( vec3 r ) const noexcept {
        return vec3(x - r.x, y - r.y, z - r.z);
    }
};

class random_generator {
    static std::uint64_t splitmix64_next( std::uint64_t &state ) noexcept {
        std::uint64_t r = (state += 0x9E3779B97F4A7C15);
        r = (r ^ (r >> 30)) * 0xBF58476D1CE4E5B9;
        r = (r ^ (r >> 27)) * 0x94D049BB133111EB;
        return r ^ (r >> 31);
    }

    std::uint64_t s0, s1, s2, s3;

public:
    random_generator( std::uint64_t seed ) noexcept {
        s0 = splitmix64_next(seed);
        s1 = splitmix64_next(seed);
        s2 = splitmix64_next(seed);
        s3 = splitmix64_next(seed);
    }

    std::uint64_t next_uint64( void ) noexcept {
        std::uint64_t result = s0 + std::rotl(s0 + s3, 23);
        std::uint64_t t = s1 << 17;

        s2 ^= s0;
        s3 ^= s1;
        s1 ^= s2;
        s0 ^= s3;

        s2 ^= t;
        s3 = std::rotl(s3, 45);

        return result;
    }

    float next_unit_float( void ) noexcept {
        return static_cast<double>(next_uint64()) / static_cast<double>(std::numeric_limits<uint64_t>::max());
    }

    vec3 next_unit_vec3( void ) {
        float ksi1 = next_unit_float();
        float ksi2 = next_unit_float();

        return vec3::from_spherical(2.0f * std::numbers::pi_v<float> * ksi1, std::acos(ksi2 * 2.0f - 1.0f));
    }

    vec3 next_sphere_vec3( void ) noexcept {
        vec3 v {0.0f};
        do {
            v = vec3(next_unit_float(), next_unit_float(), next_unit_float()) * 2.0f - vec3(1.0f);
        } while (v.length2() > 1);
        return v;
    }
};

class timer {
    using hrc = std::chrono::high_resolution_clock;

    hrc::time_point start = hrc::now();
    hrc::time_point now = hrc::now();
    float delta_time = 0.01f;
    float time = 0.01f;
    float fps = 0.0f;
    std::uint32_t fps_frame_count = 0;
    hrc::duration fps_duration = std::chrono::seconds(3);
    hrc::time_point fps_last_measure = hrc::now();
    bool fps_is_new = false;
public:
    void update( void ) {
        hrc::time_point new_now = hrc::now();
        delta_time = std::chrono::duration<double>(new_now - now).count();
        time = std::chrono::duration<double>(new_now - start).count();
        now = new_now;

        fps_frame_count++;
        fps_is_new = now - fps_last_measure > fps_duration;
        if (fps_is_new) {
            float fps_dt = std::chrono::duration<float>(now - fps_last_measure).count();
            fps = fps_frame_count / fps_dt;
            fps_last_measure = now;
            fps_frame_count = 0;
        }
    }

    float get_time( void ) const noexcept {
        return time;
    }

    float get_delta_time( void ) const noexcept {
        return delta_time;
    }

    bool is_fps_new( void ) const noexcept {
        return fps_is_new;
    }

    float get_fps( void ) const noexcept {
        return fps;
    }
};

struct input {
    float acceleration = 0.0;
    float rotation = 0.0;
    float move_x = 0.0;
    float move_y = 0.0;

    input compose( input rhs ) const {
        return input {
            .acceleration = std::clamp(acceleration + rhs.acceleration, -1.0f, 1.0f),
            .rotation     = std::clamp(rotation     + rhs.rotation    , -1.0f, 1.0f),
            .move_x       = std::clamp(move_x       + rhs.move_x      , -1.0f, 1.0f),
            .move_y       = std::clamp(move_y       + rhs.move_y      , -1.0f, 1.0f),
        };
    }
};


class context {
    struct vertex {
        std::uint32_t x;
        std::uint32_t y;
        float d2;
    };

    SDL_Window *window_;
    std::vector<vec3> stars_;
    std::vector<vertex> vertex_buffer_;
    random_generator random_;
    timer timer_;
    float speed_ = 0.47f;
    input input_;

    void on_key_changed( SDL_Scancode key, bool is_pressed ) {
        const float delta = (float)is_pressed * 2.0f - 1.0f;

        input in_delta;
        switch (key) {
        case SDL_SCANCODE_Q: in_delta.rotation     =  delta; break;
        case SDL_SCANCODE_E: in_delta.rotation     = -delta; break;
        case SDL_SCANCODE_R: in_delta.acceleration =  delta; break;
        case SDL_SCANCODE_F: in_delta.acceleration = -delta; break;
        case SDL_SCANCODE_W: in_delta.move_y       =  delta; break;
        case SDL_SCANCODE_S: in_delta.move_y       = -delta; break;
        case SDL_SCANCODE_D: in_delta.move_x       =  delta; break;
        case SDL_SCANCODE_A: in_delta.move_x       = -delta; break;
        default:
            break;
        }
        input_ = input_.compose(in_delta);
    }

    void rotate_stars_y( float angle ) {
        float sina = std::sin(angle);
        float cosa = std::cos(angle);
        for (auto &s : stars_)
            s = vec3(s.z * sina + s.x * cosa, s.y, s.z * cosa - s.x * sina);
    }

    void move_stars( vec3 off ) {
        for (auto &star : stars_) {
            star = star + off;
            if (star.length2() > 1.0f) {
                star = random_.next_unit_vec3();
                star = star * std::copysign(1.0f, -off.dot(star));
            }
        }
    }

    void render( void ) {
        SDL_Surface *surface = SDL_GetWindowSurface(window_);
        if (surface == NULL || surface->format->format != SDL_PIXELFORMAT_RGB888)
            return;

        constexpr float clip = 0.5;
        float half_w = surface->w / 2.0f;
        float half_h = surface->h / 2.0f;
        float wh_scale = std::sqrt((surface->w * surface->w + surface->h * surface->h) / (1.0f - clip * clip));
        float xy_mul = clip * surface->w * surface->h / wh_scale;

        for (auto star : stars_) {
            if (star.z <= 0.0f)
                continue;

            std::int32_t xs = half_w + xy_mul * star.x / star.z;
            std::int32_t ys = half_h - xy_mul * star.y / star.z;

            if (xs < 0 || ys < 0 || xs > surface->w - 4 || ys > surface->h - 4)
                continue;

            vertex_buffer_.push_back(vertex(xs, ys, star.dot(star)));
        }

        std::sort(vertex_buffer_.begin(), vertex_buffer_.end(),
            []( const vertex &lhs, const vertex &rhs ) { return rhs.d2 < lhs.d2; });

        if (SDL_MUSTLOCK(surface) && !SDL_LockSurface(surface))
            return;

        std::memset(surface->pixels, 0, surface->pitch * surface->h);

        for (auto &v : vertex_buffer_) {
            int size = v.d2 < 0.0025 ? 4 : v.d2 < 0.01 ? 3 : v.d2 < 0.09 ? 2 : 1;

            std::uint32_t color = (std::uint8_t)(255.0f * (1.0f - v.d2));
            color |= color << 8;
            color |= color << 16;

            std::byte *ptr = (std::byte *)surface->pixels + v.y * surface->pitch + v.x * 4;
            for (int y = 0; y < size; y++) {
                for (int x = 0; x < size; x++) {
                    std::memcpy(ptr, &color, 4);
                    ptr += 4;
                }
                ptr += surface->pitch - size * 4;
            }
        }

        vertex_buffer_.clear();

        if (SDL_MUSTLOCK(surface))
            SDL_UnlockSurface(surface);
        SDL_UpdateWindowSurface(window_);
    }

public:
    context(
        SDL_Window *window,
        std::uint64_t star_count,
        std::uint64_t random_seed = 47
    ):
        window_(window),
        random_(random_seed)
    {
        stars_.reserve(star_count);
        vertex_buffer_.reserve(star_count);
        for (std::uint64_t i = 0; i < star_count; i++)
            stars_.push_back(random_.next_sphere_vec3());
    }

    // Run main loop
    void main_loop( void ) {
        bool do_quit = false;

        while (!do_quit) {
            SDL_Event event;
            while (SDL_PollEvent(&event)) {
                switch (event.type) {
                case SDL_QUIT:
                    do_quit = true;
                    break;

                case SDL_KEYDOWN:
                    on_key_changed(event.key.keysym.scancode, true);
                    break;

                case SDL_KEYUP:
                    on_key_changed(event.key.keysym.scancode, false);
                    break;

                default:
                    break;
                    // Nothing
                }
            }

            constexpr float rotation_speed = 1.0f;
            constexpr float acceleration_speed = 1.0f;

            timer_.update();
            const float delta_time = timer_.get_delta_time();

            if (timer_.is_fps_new())
                std::println("FPS: {}", timer_.get_fps());

            speed_ += delta_time * input_.acceleration * acceleration_speed;
            if (std::abs(input_.rotation) > 0.1f)
                rotate_stars_y(input_.rotation * delta_time * rotation_speed);
            move_stars(vec3(input_.move_x, input_.move_y, 1.0f) * (-speed_ * delta_time));
            render();
        }
    }
};

int main( void ) {
    SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS);
    SDL_Window *window = SDL_CreateWindow("stars-cpp", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 800, 600, 0);

    context(window, 8192, 47).main_loop();

    SDL_DestroyWindow(window);
    SDL_Quit();
    return 0;
}
