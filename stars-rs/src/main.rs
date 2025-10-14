//! 'Stars' test project implementation main file

pub struct RandomGenerator(u64, u64, u64, u64);

impl RandomGenerator {
    fn splitmix64(state: &mut u64) -> u64 {
        *state = state.wrapping_add(0x9E3779B97F4A7C15);
        let r0 = *state;
        let r1 = (r0 ^ (r0 >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
        let r2 = (r1 ^ (r1 >> 27)).wrapping_mul(0x94D049BB133111EB);
        r2 ^ (r2 >> 31)
    }

    pub fn new(mut seed: u64) -> Self {
        Self(
            Self::splitmix64(&mut seed),
            Self::splitmix64(&mut seed),
            Self::splitmix64(&mut seed),
            Self::splitmix64(&mut seed)
        )
    }

    pub fn next_u64(&mut self) -> u64 {
        let result = self.3
            .wrapping_add(self.0)
            .rotate_left(23)
            .wrapping_add(self.0);
        let t = self.1 << 17;

        self.2 ^= self.0;
        self.3 ^= self.1;
        self.1 ^= self.2;
        self.0 ^= self.3;

        self.2 ^= t;
        self.3 = self.3.rotate_left(45);

        result
    }

    pub fn next_unit_f32(&mut self) -> f32 {
        (self.next_u64() as f64 / u64::MAX as f64) as f32
    }
}

#[derive(Copy, Clone)]
pub struct Vec3 {
    x: f32,
    y: f32,
    z: f32
}

impl Vec3 {
    pub fn new(x: f32, y: f32, z: f32) -> Self {
        Self { x, y, z }
    }

    pub fn dot(self, rhs: Vec3) -> f32 {
        self.x * rhs.x + self.y * rhs.y + self.z * rhs.z
    }

    pub fn from_spherical(phi: f32, theta: f32) -> Self {
        Self {
            x: phi.cos() * theta.sin(),
            y: phi.sin() * theta.sin(),
            z: theta.cos()
        }
    }
}

impl std::ops::Add<Self> for Vec3 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.x + rhs.x, self.y + rhs.y, self.z + rhs.z)
    }
}

impl std::ops::Mul<f32> for Vec3 {
    type Output = Self;

    fn mul(self, rhs: f32) -> Self::Output {
        Self::new(self.x * rhs, self.y * rhs, self.z * rhs)
    }
}

impl RandomGenerator {
    pub fn next_unit_vec3(&mut self) -> Vec3 {
        Vec3::from_spherical(
            2.0 * std::f32::consts::PI * self.next_unit_f32(),
            (self.next_unit_f32() * 2.0 - 1.0).acos()
        )
    }

    pub fn next_sphere_vec3(&mut self) -> Vec3 {
        loop {
            let v = Vec3::new(
                self.next_unit_f32() * 2.0 - 1.0,
                self.next_unit_f32() * 2.0 - 1.0,
                self.next_unit_f32() * 2.0 - 1.0
            );
            if v.dot(v) <= 1.0 {
                return v;
            }
        }
    }
}

pub struct Timer {
    start: std::time::Instant,
    now: std::time::Instant,
    delta: f32,
    time: f32,
    fps_frame_count: u32,
    fps_duration: std::time::Duration,
    fps_last_measure: std::time::Instant,
    fps: f32,
    fps_is_new: bool,
}

impl Timer {
    pub fn new() -> Self {
        let now = std::time::Instant::now();
        Self {
            start: now,
            now,
            delta: 0.01,
            time: 0.01,
            fps_frame_count: 0,
            fps_duration: std::time::Duration::from_secs_f32(3.0),
            fps_last_measure: now,
            fps: std::f32::NAN,
            fps_is_new: false,
        }
    }

    pub fn fps_is_new(&self) -> bool {
        self.fps_is_new
    }

    pub fn fps(&self) -> f32 {
        self.fps
    }

    pub fn delta_time(&self) -> f32 {
        self.delta
    }

    pub fn time(&self) -> f32 {
        self.time
    }

    pub fn update(&mut self) {
        let now = std::time::Instant::now();
        self.delta = now.duration_since(self.now).as_secs_f32();
        self.time = now.duration_since(self.start).as_secs_f32();
        self.now = now;

        self.fps_is_new = now.duration_since(self.fps_last_measure) > self.fps_duration;
        if self.fps_is_new {
            self.fps = self.fps_frame_count as f32 / now.duration_since(self.fps_last_measure).as_secs_f32();
            self.fps_frame_count = 0;
            self.fps_last_measure = now;
        } else {
            self.fps_frame_count += 1;
        }
    }
}

#[derive(Copy, Clone, Default)]
pub struct Input {
    pub rotation: f32,
    pub acceleration: f32,
    pub move_x: f32,
    pub move_y: f32,
}

impl Input {
    /// Compose two inputs
    pub fn compose(&self, addition: Self) -> Self {
        macro_rules! add_clamp {
            ($($name: ident),*) => {
                Self { $( $name: (self.$name + addition.$name).clamp(-1.0, 1.0)),* }
            }
        }
        add_clamp!(rotation, acceleration, move_x, move_y)
    }
}

pub struct Context {
    window: sdl2::video::Window,
    stars: Vec<Vec3>,
    projection_buffer: Vec<(u32, u32, f32)>,
    random: RandomGenerator,
    input: Input,
    timer: Timer,
    move_speed: f32,
    event_pump: sdl2::EventPump,
}

impl Context {
    pub fn move_stars(&mut self, off: Vec3) {
        for star in &mut self.stars {
            *star = *star + off;

            if Vec3::dot(*star, *star) > 1.0 {
                *star = self.random.next_unit_vec3();
                *star = *star * -off.dot(*star).signum();
            }
        }
    }

    pub fn rotate_stars_y(&mut self, angle: f32) {
        let (sin_a, cos_a) = angle.sin_cos();
        for star in &mut self.stars {
            *star = Vec3::new(
                star.z * sin_a + star.x * cos_a,
                star.y,
                star.z * cos_a - star.x * sin_a
            );
        }
    }

    fn on_key_changed(&mut self, key: sdl2::keyboard::Scancode, is_pressed: bool) {
        type Scancode = sdl2::keyboard::Scancode;
        let delta = (is_pressed as i32 * 2 - 1) as f32;
        let mut input = Input::default();

        match key {
            Scancode::Q => input.rotation     =  delta,
            Scancode::E => input.rotation     = -delta,
            Scancode::R => input.acceleration =  delta,
            Scancode::F => input.acceleration = -delta,
            Scancode::W => input.move_y       =  delta,
            Scancode::S => input.move_y       = -delta,
            Scancode::D => input.move_x       =  delta,
            Scancode::A => input.move_x       = -delta,
            _ => {}
        }

        self.input = self.input.compose(input);
    }

    pub fn render(&mut self) {
        let mut surface = self.window.surface(&self.event_pump).unwrap();

        if surface.pixel_format_enum() != sdl2::pixels::PixelFormatEnum::RGB888 {
            surface.update_window().unwrap();
            return;
        }

        let surface_w = surface.width() as isize;
        let surface_h = surface.height() as isize;
        let surface_pitch = surface.pitch() as usize;

        let clip = 0.5f32;
        let w = surface_w as f32;
        let h = surface_h as f32;
        let half_w = w / 2.0;
        let half_h = h / 2.0;
        let wh_scale = ((w * w + h * h) / (1.0 - clip * clip)).sqrt();
        let xy_mul = clip * w * h / wh_scale;

        'rendering: for star in &self.stars {
            if star.z <= 0.0 {
                continue 'rendering;
            }

            let xs = (half_w + xy_mul * star.x / star.z) as isize;
            let ys = (half_h - xy_mul * star.y / star.z) as isize;

            if xs < 0 || ys < 0 || xs > surface_w - 4 || ys > surface_h - 4 {
                continue 'rendering;
            }

            self.projection_buffer.push((xs as u32, ys as u32, star.dot(*star)));
        }
        self.projection_buffer.sort_by(|(_, _, dl), (_, _, dr)| f32::total_cmp(dr, dl));

        let vertex_iter = self.projection_buffer.drain(..);
        surface.with_lock_mut(move |pixels| {
            if pixels.len() < surface_pitch * surface_h as usize {
                return;
            }
            let surface_ptr = pixels.as_mut_ptr();

            unsafe { surface_ptr.write_bytes(0, surface_pitch * surface_h as usize) };

            for (xs, ys, d) in vertex_iter {
                let mut pixel_ptr = unsafe {
                    surface_ptr
                        .add(ys as usize * surface_pitch + xs as usize * 4)
                        .cast::<u32>()
                };
                let size = match d {
                    _ if d < 0.0025 => 4,
                    _ if d < 0.01   => 3,
                    _ if d < 0.09   => 2,
                    _               => 1,
                };
                let color = (255.0 * (1.0 - d)).round() as u8;
                for _ in 0..size {
                    unsafe { pixel_ptr.write_bytes(color, size) }
                    pixel_ptr = unsafe { pixel_ptr.byte_add(surface_pitch) };
                }
            }
        });

        surface.update_window().unwrap();
    }

    // Main loop
    pub fn main_loop(mut self) {
        'main_loop: loop {
            'event_loop: loop {
                let event = match self.event_pump.poll_event() {
                    Some(event) => event,
                    None => break 'event_loop
                };

                type Event = sdl2::event::Event;

                match event {
                    Event::Quit { .. } => break 'main_loop,
                    Event::KeyDown { scancode, repeat, .. } => {
                        if repeat {
                            continue 'event_loop;
                        }
                        let Some(scancode) = scancode else {
                            continue 'event_loop;
                        };
                        self.on_key_changed(scancode, true);
                    }
                    Event::KeyUp { scancode, repeat, .. } => {
                        if repeat {
                            continue 'event_loop;
                        }
                        let Some(scancode) = scancode else {
                            continue 'event_loop;
                        };
                        self.on_key_changed(scancode, false);
                    }
                    _ => {}
                }
            }

            let acceleration_speed = 1.0;
            let rotation_speed = 1.0;

            self.timer.update();
            self.move_speed += self.timer.delta_time() * self.input.acceleration * acceleration_speed;


            if self.input.rotation.abs() > 0.1 {
                self.rotate_stars_y(self.input.rotation * rotation_speed * self.timer.delta_time());
            }

            let star_coef = -self.move_speed * self.timer.delta_time();
            self.move_stars(Vec3::new(self.input.move_x, self.input.move_y, 1.0) * star_coef);

            if self.timer.fps_is_new() {
                println!("FPS: {}", self.timer.fps());
            }

            self.render();
        }
    }
}

fn main() {
    let sdl = sdl2::init().unwrap();
    let video = sdl.video().unwrap();
    let event_pump = sdl.event_pump().unwrap();

    let window = video.window("stars-rs", 800, 600).build().unwrap();

    let mut random = RandomGenerator::new(47);
    let stars = (0..8192).map(|_| random.next_sphere_vec3()).collect::<Vec<Vec3>>();

    Context {
        window,
        event_pump,
        random,
        projection_buffer: Vec::with_capacity(stars.len()),
        stars,
        input: Input::default(),
        timer: Timer::new(),
        move_speed: 0.47,
    }.main_loop()
}
