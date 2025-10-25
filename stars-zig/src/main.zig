const std = @import("std");
const c = @cImport({
    @cInclude("SDL2/SDL.h");
});

// 3-component vector
const Vec3 = struct {
    x: f32,
    y: f32,
    z: f32,

    const Self = @This();

    pub fn init(x: f32, y: f32, z: f32) Self {
        return Self{ .x = x, .y = y, .z = z };
    }

    pub fn add(self: Self, rhs: Self) Self {
        return Self{
            .x = self.x + rhs.x,
            .y = self.y + rhs.y,
            .z = self.z + rhs.z,
        };
    }

    pub fn mul_f(self: Self, n: f32) Self {
        return Self{
            .x = self.x * n,
            .y = self.y * n,
            .z = self.z * n,
        };
    }

    pub fn dot(self: Self, rhs: Self) f32 {
        return self.x * rhs.x + self.y * rhs.y + self.z * rhs.z;
    }

    pub fn fromSpherical(phi: f32, theta: f32) Self {
        return Self{
            .x = std.math.cos(phi) * std.math.sin(theta),
            .y = std.math.sin(phi) * std.math.sin(theta),
            .z = std.math.cos(theta),
        };
    }
};

const RandomGenerator = struct {
    s0: u64,
    s1: u64,
    s2: u64,
    s3: u64,

    const Self = @This();

    fn splitMix64(state: *u64) u64 {
        var r: u64 = @addWithOverflow(state.*, 0x9E3779B97F4A7C15)[0];
        state.* = r;

        r = @addWithOverflow(r ^ (r >> 30), 0xBF58476D1CE4E5B9)[0];
        r = @addWithOverflow(r ^ (r >> 27), 0x94D049BB133111EB)[0];
        return r ^ (r >> 31);
    }

    /// Initialize self
    pub fn init(seed: u64) Self {
        var sm = seed;
        return Self{
            .s0 = splitMix64(&sm),
            .s1 = splitMix64(&sm),
            .s2 = splitMix64(&sm),
            .s3 = splitMix64(&sm),
        };
    }

    pub fn randomU64(self: *Self) u64 {
        const result = @addWithOverflow(
            self.s0,
            std.math.rotl(u64, @addWithOverflow(self.s0, self.s3)[0], 23),
        )[0];
        const temp = self.s1 << 17;

        self.s2 ^= self.s0;
        self.s3 ^= self.s1;
        self.s1 ^= self.s2;
        self.s0 ^= self.s3;

        self.s2 ^= temp;
        self.s3 ^= std.math.rotl(u64, self.s3, 45);

        return result;
    }

    pub fn randomUnitFloat(self: *Self) f32 {
        return @as(f32, @floatCast(@as(f64, @floatFromInt(self.randomU64())) / @as(f64, @floatFromInt(std.math.maxInt(u64)))));
    }

    pub fn randomUnitVec3(self: *Self) Vec3 {
        const ksi1 = self.randomUnitFloat();
        const ksi2 = self.randomUnitFloat();

        return Vec3.fromSpherical(2.0 * std.math.pi * ksi1, std.math.acos(2.0 * ksi2 - 1.0));
    }

    pub fn randomSphereVec3(self: *Self) Vec3 {
        var v: Vec3 = undefined;
        while (true) {
            v.x = self.randomUnitFloat() * 2 - 1;
            v.y = self.randomUnitFloat() * 2 - 1;
            v.z = self.randomUnitFloat() * 2 - 1;
            if (v.dot(v) <= 1.0)
                return v;
        }
    }
};

const Timer = struct {
    _frequency: u64,
    _start: u64,
    _now: u64,
    _fps_duration: u64,
    _fps_last_measure: u64,
    delta_time: f32,
    time: f32,
    _fps_frame_count: u64,
    fps: f32,
    fps_is_new: bool,

    const Self = @This();

    pub fn init() Self {
        const freq = c.SDL_GetPerformanceFrequency();
        const now = c.SDL_GetPerformanceCounter();

        return Self{
            ._frequency = freq,
            ._start = now,
            ._now = now,
            ._fps_duration = freq * 3,
            ._fps_last_measure = now,
            .delta_time = 0.00001,
            .time = 0.00001,
            ._fps_frame_count = 0,
            .fps = 0.0,
            .fps_is_new = false,
        };
    }

    fn duration_of(self: *Self, begin: u64, end: u64) f32 {
        return @as(f32, @floatFromInt(end - begin)) / @as(f32, @floatFromInt(self._frequency));
    }

    pub fn update(self: *Self) void {
        const now = c.SDL_GetPerformanceCounter();

        self.delta_time = self.duration_of(self._now, now);
        self.time = self.duration_of(self._start, now);
        self._now = now;

        self._fps_frame_count += 1;
        self.fps_is_new = now - self._fps_last_measure > self._fps_duration;
        if (self.fps_is_new) {
            self.fps = @as(f32, @floatFromInt(self._fps_frame_count)) / self.duration_of(self._fps_last_measure, now);
            self._fps_last_measure = now;
            self._fps_frame_count = 0;
        }
    }
};

const Input = struct {
    rotation: f32,
    acceleration: f32,
    move_x: f32,
    move_y: f32,

    const Self = @This();

    pub fn init() Self {
        return Self{
            .rotation = 0,
            .acceleration = 0,
            .move_x = 0,
            .move_y = 0,
        };
    }

    fn composeField(self: *Self, rhs: *const Self, comptime name: []const u8) void {
        @field(self, name) = std.math.clamp(@field(self, name) + @field(rhs, name), -1, 1);
    }

    pub fn compose(self: *Self, other: Self) void {
        composeField(self, &other, "rotation");
        composeField(self, &other, "acceleration");
        composeField(self, &other, "move_x");
        composeField(self, &other, "move_y");
    }
};

const Vertex = struct {
    x: u32,
    y: u32,
    d2: f32,

    fn sort_comparator(_: void, l: Vertex, r: Vertex) bool {
        return r.d2 < l.d2;
    }
};

const Context = struct {
    window: ?*c.SDL_Window,
    random: RandomGenerator,

    input: Input,
    timer: Timer,
    speed: f32,

    allocator: std.mem.Allocator,
    stars: []Vec3,
    vertex_buffer: []Vertex,

    const Self = @This();

    pub fn init(
        allocator: std.mem.Allocator,
        window: ?*c.SDL_Window,
        star_count: usize,
        seed: u64,
    ) !Self {
        const stars = try allocator.alloc(Vec3, star_count);
        errdefer allocator.free(stars);

        var random = RandomGenerator.init(seed);
        for (stars) |*star| star.* = random.randomSphereVec3();

        const vertex_buffer = try allocator.alloc(Vertex, star_count);
        errdefer allocator.free(vertex_buffer);

        return Self{
            .window = window,
            .random = random,
            .input = Input.init(),
            .timer = Timer.init(),
            .speed = 0.47,

            .allocator = allocator,
            .stars = stars,
            .vertex_buffer = vertex_buffer,
        };
    }

    fn onKey(self: *Self, key: c.SDL_Scancode, isPressed: bool) void {
        const delta: f32 = @as(f32, @floatFromInt(@intFromBool(isPressed))) * 2 - 1;
        var input = Input.init();

        switch (key) {
            c.SDL_SCANCODE_Q => input.rotation = delta,
            c.SDL_SCANCODE_E => input.rotation = -delta,
            c.SDL_SCANCODE_R => input.acceleration = delta,
            c.SDL_SCANCODE_F => input.acceleration = -delta,
            c.SDL_SCANCODE_W => input.move_y = delta,
            c.SDL_SCANCODE_S => input.move_y = -delta,
            c.SDL_SCANCODE_D => input.move_x = delta,
            c.SDL_SCANCODE_A => input.move_x = -delta,
            else => {},
        }

        self.input.compose(input);
    }

    fn rotateStars(self: *Self, alpha: f32) void {
        const sina = @sin(alpha);
        const cosa = @cos(alpha);

        for (self.stars) |*star| {
            const x = star.z * sina + star.x * cosa;
            const z = star.z * cosa - star.x * sina;

            star.x = x;
            star.z = z;
        }
    }

    fn moveStars(self: *Self, offset: Vec3) void {
        for (self.stars) |*star| {
            star.* = star.add(offset);
            if (star.dot(star.*) > 1) {
                star.* = self.random.randomUnitVec3();
                star.* = star.mul_f(-std.math.sign(star.dot(offset)));
            }
        }
    }

    fn update(self: *Self) void {
        const acceleration_speed: f32 = 1;
        const rotation_speed: f32 = 1;

        self.timer.update();
        if (self.timer.fps_is_new)
            std.debug.print("FPS: {d}\n", .{self.timer.fps});

        self.speed += self.timer.delta_time * self.input.acceleration * acceleration_speed;
        if (@abs(self.input.rotation) > 0.1)
            self.rotateStars(rotation_speed * self.input.rotation * self.timer.delta_time);
        self.moveStars(Vec3
            .init(self.input.move_x, self.input.move_y, 1)
            .mul_f(-self.speed * self.timer.delta_time));
    }

    // Build star vertex buffer
    fn build_vertex_buffer(self: *Self, surface_w: f32, surface_h: f32) []Vertex {
        const clip: f32 = 0.5;
        const half_w = surface_w / 2;
        const half_h = surface_h / 2;
        const wh_scale = @sqrt((surface_w * surface_w + surface_h * surface_h) / (1 - clip * clip));
        const xy_mul = clip * surface_w * surface_h / wh_scale;

        // Current vertex pointer
        var vt: [*]Vertex = self.vertex_buffer.ptr;

        for (self.stars) |star| {
            if (star.z < 0)
                continue;
            const vx = half_w + xy_mul * star.x / star.z;
            const vy = half_h - xy_mul * star.y / star.z;
            if (vx < 0 or vy < 0 or vx > surface_w or vy > surface_h)
                continue;
            vt[0] = Vertex{
                .x = @intFromFloat(vx),
                .y = @intFromFloat(vy),
                .d2 = Vec3.dot(star, star),
            };
            vt = vt + 1;
        }

        const used_vertex_count = (@intFromPtr(vt) - @intFromPtr(self.vertex_buffer.ptr)) / @sizeOf(Vertex);
        const vertices = self.vertex_buffer[0..used_vertex_count];
        std.sort.pdq(Vertex, vertices, {}, Vertex.sort_comparator);
        return vertices;
    }

    // Display vertex buffer contents
    fn draw_vertex_buffer(
        self: *Self,
        vertices: []Vertex,
        surface_w: u32,
        surface_h: u32,
        surface_pitch: u32,
        surface_pixels: [*]u8,
    ) void {
        _ = self;
        _ = surface_w;
        @memset(surface_pixels[0..@intCast(surface_pitch * surface_h)], 0);

        for (vertices) |vertex| {
            const size: u32 = getsize: {
                if (vertex.d2 < 0.0025) break :getsize 4;
                if (vertex.d2 < 0.01) break :getsize 3;
                if (vertex.d2 < 0.09) break :getsize 2;
                break :getsize 1;
            };

            const color: u8 = @intFromFloat(255 * (1 - vertex.d2));
            var pixel_ptr = surface_pixels + surface_pitch * vertex.y + vertex.x * 4;
            const pixel_end = pixel_ptr + surface_pitch * size;

            while (@intFromPtr(pixel_ptr) < @intFromPtr(pixel_end)) : (pixel_ptr += surface_pitch)
                @memset(pixel_ptr[0 .. size * 4], color);
        }
    }

    fn render(self: *Self) void {
        const surface = c.SDL_GetWindowSurface(self.window);

        if (c.SDL_MUSTLOCK(surface) and c.SDL_LockSurface(surface) == 0)
            return;

        if (surface.*.format.*.format != c.SDL_PIXELFORMAT_RGB888)
            return;

        // Render!
        self.draw_vertex_buffer(
            self.build_vertex_buffer(
                @as(f32, @floatFromInt(surface.*.w)) - 4,
                @as(f32, @floatFromInt(surface.*.h)) - 4,
            ),
            @intCast(surface.*.w),
            @intCast(surface.*.h),
            @intCast(surface.*.pitch),
            @as([*]u8, @ptrCast(surface.*.pixels)),
        );

        if (c.SDL_MUSTLOCK(surface))
            c.SDL_UnlockSurface(surface);
        _ = c.SDL_UpdateWindowSurface(self.window);
    }

    pub fn run(self: *Self) void {
        var do_quit = false;

        while (!do_quit) {
            // Handle events
            var event: c.SDL_Event = undefined;
            while (c.SDL_PollEvent(&event) == c.SDL_TRUE) {
                switch (event.type) {
                    c.SDL_QUIT => {
                        do_quit = true;
                    },
                    c.SDL_KEYDOWN => self.onKey(event.key.keysym.scancode, true),
                    c.SDL_KEYUP => self.onKey(event.key.keysym.scancode, false),
                    else => {},
                }
            }

            self.update();
            self.render();
        }
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.stars);
        self.allocator.free(self.vertex_buffer);
    }
};

pub fn main() !void {
    // Initialize SDL2 and defer quit
    _ = c.SDL_Init(c.SDL_INIT_VIDEO);
    defer c.SDL_Quit();

    const window = c.SDL_CreateWindow("stars-zig", c.SDL_WINDOWPOS_UNDEFINED, c.SDL_WINDOWPOS_UNDEFINED, 800, 600, 0);
    defer c.SDL_DestroyWindow(window);

    var context = try Context.init(std.heap.page_allocator, window, 8192, 47);
    defer Context.deinit(&context);

    context.run();
}
