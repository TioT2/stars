package main

import (
	"fmt"
	"errors"
	"math"
	"math/bits"
	"github.com/veandco/go-sdl2/sdl"
)

type RandomGenerator struct {
	v0, v1, v2, v3 uint64
}

func Splitmix64(seed *uint64) uint64 {
	*seed += 0x9E3779B97F4A7C15
	v := *seed
	v = (v ^ (v >> 30)) * 0xBF58476D1CE4E5B9
	v = (v ^ (v >> 27)) * 0x94D049BB133111EB
	return v ^ (v >> 31)
}

func NewRandomGenerator(seed uint64) RandomGenerator {
	return RandomGenerator{
		Splitmix64(&seed),
		Splitmix64(&seed),
		Splitmix64(&seed),
		Splitmix64(&seed),
	}
}

func (self *RandomGenerator) NextUint64() uint64 {
	result := bits.RotateLeft64(self.v0 + self.v3, 23) + self.v0
	t := self.v1 << 17

	self.v2 ^= self.v0
	self.v3 ^= self.v1
	self.v1 ^= self.v2
	self.v0 ^= self.v3

	self.v2 ^= t
	self.v3 = bits.RotateLeft64(self.v3, 45)

	return result
}

func (self *RandomGenerator) NextUnitFloat64() float64 {
	return float64(self.NextUint64()) / float64(math.MaxUint64)
}

func (self *RandomGenerator) NextUnitVec3() Vec3 {
	p := 2 * math.Pi * self.NextUnitFloat64()
	t := math.Acos(self.NextUnitFloat64() * 2 - 1)
	return NewVec3FromSph(p, t)
}

func (self *RandomGenerator) NextSphereVec3() Vec3 {
	for {
		v := Vec3{
			float32(self.NextUnitFloat64()) * 2 - 1,
			float32(self.NextUnitFloat64()) * 2 - 1,
			float32(self.NextUnitFloat64()) * 2 - 1,
		}
		if v.Dot(v) <= 1.0 {
			return v
		}
	}
}

type Vec3 struct {
	X, Y, Z float32
}

func NewVec3FromSph(phi, theta float64) Vec3 {
	ps, pc := math.Sincos(phi)
	ts, tc := math.Sincos(theta)
	return Vec3{ float32(pc * ts), float32(ps * ts), float32(tc) }
}

func (l Vec3) Add(r Vec3) Vec3 {
	return Vec3{l.X + r.X, l.Y + r.Y, l.Z + r.Z}
}

func (l Vec3) Mul(r Vec3) Vec3 {
	return Vec3{l.X * r.X, l.Y * r.Y, l.Z * r.Z}
}

func (l Vec3) Dot(r Vec3) float32 {
	return l.X * r.X + l.Y * r.Y + l.Z * r.Z
}

type Timer struct {
	start uint64
	now uint64
	freq uint64
	deltaTime float32
	time float32
	fpsFrameCount uint
	fpsDuration uint64
	fpsLastMeasure uint64
	fps float32
}

func NewTimer() Timer {
	now := sdl.GetPerformanceCounter()
	freq := sdl.GetPerformanceFrequency()

	return Timer{
		start: now,
		now: now,
		freq: freq,
		deltaTime: 0,
		time: 0,
		fpsFrameCount: 0,
		fpsDuration: 3 * freq,
		fpsLastMeasure: now - 1,
		fps: 0,
	}
}

func (self *Timer) DurationBetween(start, end uint64) float32 {
	return float32(end - start) / float32(self.freq)
}

func (self *Timer) Update() {
	now := sdl.GetPerformanceCounter()
	self.deltaTime = self.DurationBetween(self.now, now)
	self.time = self.DurationBetween(self.start, now)
	self.now = now

	self.fpsFrameCount += 1
	if now - self.fpsLastMeasure > self.fpsDuration {
		dur := self.DurationBetween(self.fpsLastMeasure, now)
		self.fps = float32(self.fpsFrameCount) / dur
		self.fpsFrameCount = 0
		self.fpsLastMeasure = now
	}
}

func (self *Timer) FpsIsNew() bool {
	return self.fpsLastMeasure == self.now
}

func (self *Timer) GetFps() float32 {
	return self.fps
}

func (self *Timer) GetDeltaTime() float32 {
	return self.deltaTime
}

func (self *Timer) GetTime() float32 {
	return self.time
}

type Input struct {
	Rotation float32
	Acceleration float32
	DeltaX float32
	DeltaY float32
}

func NewInput() Input {
	return Input{0, 0, 0, 0}
}

func ComposeFields(l, r float32) float32 {
	sum := l + r
	if sum < -1 {
		return -1
	} else if sum > 1 {
		return 1
	} else {
		return sum
	}
}

func (self Input) Compose(othr Input) Input {
	return Input{
		Rotation: ComposeFields(self.Rotation, othr.Rotation),
		Acceleration: ComposeFields(self.Acceleration, othr.Acceleration),
		DeltaX: ComposeFields(self.DeltaX, othr.DeltaX),
		DeltaY: ComposeFields(self.DeltaY, othr.DeltaY),
	}
}

type ScreenPoint struct {
	X int32
	Y int32
	D2 float32
}

type Context struct {
	window *sdl.Window
	stars []Vec3
	projBuffer [] ScreenPoint
	rand RandomGenerator
	input Input
	timer Timer
	speed float32
}

func (self *Context) RotateStars(angle float32) {
	s64, c64 := math.Sincos(float64(angle))
	s, c := float32(s64), float32(c64)
	for i, v := range self.stars {
		self.stars[i] = Vec3 { v.Z * s + v.X * c, v.Y, v.Z * c - v.X * s }
	}
}

func (self *Context) MoveStars(delta Vec3) {
	for i, v := range self.stars {
		v = v.Add(delta)
		if v.Dot(v) > 1 {
			v = self.rand.NextUnitVec3()
			d := -v.Dot(delta)
			if d >= 0 {
				d = 1
			} else {
				d = -1
			}
			v = v.Mul(Vec3{d, d, d})
		}
		self.stars[i] = v
	}
}

func (self *Context) OnKeyEvent(key sdl.Scancode, isDown bool) {
	inp := NewInput()

	delta := float32(-1)
	if isDown {
		delta = float32(1)
	}

	switch key {
	case sdl.SCANCODE_Q: inp.Rotation     =  delta
	case sdl.SCANCODE_E: inp.Rotation     = -delta
	case sdl.SCANCODE_R: inp.Acceleration =  delta
	case sdl.SCANCODE_F: inp.Acceleration = -delta
	case sdl.SCANCODE_W: inp.DeltaY       =  delta
	case sdl.SCANCODE_S: inp.DeltaY       = -delta
	case sdl.SCANCODE_D: inp.DeltaX       =  delta
	case sdl.SCANCODE_A: inp.DeltaX       = -delta
	}

	self.input = self.input.Compose(inp)
}

func (self *Context) Render() error {
	surface, err := self.window.GetSurface()
	if err != nil {
		return err
	}

	pixelFormat := surface.Format.Format
	if pixelFormat != sdl.PIXELFORMAT_RGB888 {
		return errors.New(fmt.Sprintf("Cannot render to 0x%04X pixel format", pixelFormat))
	}

	clip := float32(0.5)
	w := float32(surface.W)
	h := float32(surface.H)
	halfW := w / 2.0
	halfH := h / 2.0
	whScale := float32(math.Sqrt(float64((w * w + h * h) / (1.0 - clip * clip))))
	xyMul := clip * w * h / whScale

	self.projBuffer = self.projBuffer[:0]
	for _, star := range self.stars {
		if star.Z <= 0 {
			continue
		}

		xs := int32(halfW + xyMul * star.X / star.Z)
		ys := int32(halfH - xyMul * star.Y / star.Z)

		if xs < 0 || ys < 0 || xs >= surface.W - 4 || ys >= surface.H - 4 {
			continue
		}

		self.projBuffer = append(self.projBuffer, ScreenPoint{
			xs,
			ys,
			star.Dot(star),
		})
	}

	surface.Lock()
	if err != nil {
		return err
	}

	pixels := surface.Pixels()
	for i, _ := range pixels {
		pixels[i] = 0
	}
	for _, pt := range self.projBuffer {
		size := 1
		if pt.D2 < 0.025 {
			size = 4
		} else if pt.D2 < 0.01 {
			size = 3
		} else if pt.D2 < 0.09 {
			size = 2
		}

		color := uint8(255 * (1 - pt.D2))
		row := pixels[surface.Pitch * pt.Y + pt.X * 4:]

		for range size {
			for x := range size * 4 {
				row[x] = color
			}
			row = row[surface.Pitch:]
		}
	}

	surface.Unlock()

	self.window.UpdateSurface()

	return nil
}

func (self *Context) Run() error {
	doQuit := false
	for !doQuit {
		for event := sdl.PollEvent(); event != nil; event = sdl.PollEvent() {
			switch ty := event.(type) {
			case sdl.QuitEvent:
				doQuit = true
			case sdl.KeyboardEvent:
				if ty.Repeat == 0 {
					self.OnKeyEvent(ty.Keysym.Scancode, ty.Type == sdl.KEYDOWN)
				}
			}
		}

		accSpeed := float32(1)
		rotSpeed := float32(1)

		self.timer.Update()
		self.speed += self.timer.GetDeltaTime() * self.input.Acceleration * accSpeed
		if self.input.Rotation > 0.1 || self.input.Rotation < -0.1 {
			self.RotateStars(self.input.Rotation * rotSpeed * self.timer.GetDeltaTime())
		}

		st := -self.speed * self.timer.GetDeltaTime()
		self.MoveStars(Vec3{self.input.DeltaX * st, self.input.DeltaY * st, st})

		if self.timer.FpsIsNew() {
			fmt.Printf("FPS: %f\n", self.timer.GetFps())
		}

		if err := self.Render(); err != nil {
			return err
		}
	}

	return nil
}

func main() {
	if err := sdl.Init(sdl.INIT_VIDEO); err != nil {
		panic(err)
	}
	defer sdl.Quit()

	win, err := sdl.CreateWindow("stars-go", sdl.WINDOWPOS_UNDEFINED, sdl.WINDOWPOS_UNDEFINED, 800, 600, sdl.WINDOW_SHOWN)
	if err != nil {
		panic(err)
	}
	defer win.Destroy()

	starCount := 8192
	rand := NewRandomGenerator(47)
	stars := make([]Vec3, 0, starCount)
	projBuffer := make([]ScreenPoint, 0, starCount)

	for range starCount {
		star := rand.NextSphereVec3()
		stars = append(stars, star)
	}

	context := Context{
		window: win,
		stars: stars,
		projBuffer: projBuffer,
		rand: rand,
		input: NewInput(),
		timer: NewTimer(),
		speed: 0.47,
	}

	context.Run()
}
