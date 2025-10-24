#include <SDL2/SDL.h>

typedef struct RandomGenerator_ {
    unsigned long s0;
    unsigned long s1;
    unsigned long s2;
    unsigned long s3;
} RandomGenerator;

static unsigned long splitMix64( unsigned long *state ) {
    unsigned long r = (*state += 0x9E3779B97F4A7C15);
    r = (r ^ (r >> 30)) * 0xBF58476D1CE4E5B9;
    r = (r ^ (r >> 27)) * 0x94D049BB133111EB;
    return r ^ (r >> 31);
}

void randomInit( RandomGenerator *self, unsigned long seed ) {
    self->s0 = splitMix64(&seed);
    self->s1 = splitMix64(&seed);
    self->s2 = splitMix64(&seed);
    self->s3 = splitMix64(&seed);
}

unsigned long rotl( unsigned long value, int count ) {
    return (value << count) | (value >> (64 - count));
}

unsigned long randomUint64( RandomGenerator *self ) {
    unsigned long result = self->s0 + rotl(self->s0 + self->s3, 23);
    unsigned long temp = self->s1 << 17;

    self->s2 ^= self->s0;
    self->s3 ^= self->s1;
    self->s1 ^= self->s2;
    self->s0 ^= self->s3;

    self->s2 ^= temp;
    self->s3 ^= rotl(self->s3, 45);

    return result;
}

float randomUnitFloat( RandomGenerator *self ) {
    return (double)randomUint64(self) / (double)0xFFFFFFFFFFFFFFFFlu;
}

typedef struct Vec3_ {
    float x;
    float y;
    float z;
} Vec3;

/// Calculate vec3 dot product
float vec3Dot( Vec3 lhs, Vec3 rhs ) {
    return lhs.x * rhs.x + lhs.y * rhs.y + lhs.z * rhs.z;
}

Vec3 vec3FromSpherical( float phi, float theta ) {
    Vec3 v;
    v.x = cos(phi) * sin(theta);
    v.y = sin(phi) * sin(theta);
    v.z = cos(theta);
    return v;
}

Vec3 randomUnitVec3( RandomGenerator *self ) {
    float ksi1 = randomUnitFloat(self);
    float ksi2 = randomUnitFloat(self);

    return vec3FromSpherical(2.0f * (float)M_PI * ksi1, acosf(2.0f * ksi2 - 1.0f));
}

Vec3 randomSphereVec3( RandomGenerator *self ) {
    Vec3 v;
    do {
        v.x = randomUnitFloat(self) * 2.0f - 1.0f;
        v.y = randomUnitFloat(self) * 2.0f - 1.0f;
        v.z = randomUnitFloat(self) * 2.0f - 1.0f;
    } while (vec3Dot(v, v) > 1.0f);
    return v;
}

typedef struct Timer_ {
    unsigned long frequency;
    unsigned long start;
    unsigned long now;
    unsigned long fpsDuration;
    unsigned long fpsLastMeasure;
    float deltaTime;
    float time;
    unsigned int fpsFrameCount;
    float fps;
    int fpsIsNew;
} Timer;

void timerInit( Timer *self ) {
    memset(self, 0, sizeof(Timer));

    self->frequency = SDL_GetPerformanceFrequency();
    self->start = SDL_GetPerformanceCounter();

    self->now = self->start;
    self->fpsLastMeasure = self->start;
    self->fpsDuration = self->frequency * 3;
}

void timerUpdate( Timer *self ) {
    unsigned long now = SDL_GetPerformanceCounter();

    self->deltaTime = (float)(now - self->now) / self->frequency;
    self->time = (float)(now - self->start) / self->frequency;
    self->now = now;

    self->fpsFrameCount++;
    self->fpsIsNew = now - self->fpsLastMeasure > self->fpsDuration;
    if (self->fpsIsNew) {
        self->fps = self->fpsFrameCount / ((float)(now - self->fpsLastMeasure) / self->frequency);
        self->fpsLastMeasure = now;
        self->fpsFrameCount = 0;
    }
}

typedef struct Input_ {
    float rotation;
    float acceleration;
    float move_x;
    float move_y;
} Input;

void inputInit( Input *self ) {
    memset(self, 0, sizeof(Input));
}

static float clampFloat( float value, float min, float max ) {
    return value < min ? min : value > max ? max : value;
}

void inputCompose( Input *self, const Input *rhs ) {
    self->rotation     = clampFloat(self->rotation     + rhs->rotation    , -1.0f, 1.0f);
    self->acceleration = clampFloat(self->acceleration + rhs->acceleration, -1.0f, 1.0f);
    self->move_x       = clampFloat(self->move_x       + rhs->move_x      , -1.0f, 1.0f);
    self->move_y       = clampFloat(self->move_y       + rhs->move_y      , -1.0f, 1.0f);
}

typedef struct Vertex_ {
    unsigned int x;
    unsigned int y;
    float d2;
} Vertex;

typedef struct Context_ {
    SDL_Window *window;
    RandomGenerator random;

    Input input;
    Timer timer;
    float speed;

    unsigned long starCount;
    Vec3 *stars;
    Vertex *vertex_buffer;
} Context;

void contextInitStars( Context *self ) {
    unsigned long i;
    for (i = 0; i < self->starCount; i++)
        self->stars[i] = randomSphereVec3(&self->random);
}

void contextMoveStars( Context *self, Vec3 offset ) {
    Vec3 *const starsEnd = self->stars + self->starCount;
    Vec3 *star;

    for (star = self->stars; star < starsEnd; star++) {
        star->x += offset.x;
        star->y += offset.y;
        star->z += offset.z;

        if (vec3Dot(*star, *star) > 1.0) {
            float sign;

            *star = randomUnitVec3(&self->random);
            sign = vec3Dot(*star, offset);
            sign /= -fabs(sign);
            star->x *= sign;
            star->y *= sign;
            star->z *= sign;
        }
    }
}

void contextRotateStars( Context *self, float alpha ) {
    float sina = sin(alpha);
    float cosa = cos(alpha);
    Vec3 *const starsEnd = self->stars + self->starCount;
    Vec3 *star;

    for (star = self->stars; star < starsEnd; star++) {
        float x = star->z * sina + star->x * cosa;
        float z = star->z * cosa - star->x * sina;

        star->x = x;
        star->z = z;
    }
}

void contextOnKeyDown( Context *self, SDL_Scancode key, int isPressed ) {
    Input input;
    float delta = isPressed * 2 - 1;
    memset(&input, 0, sizeof(input));

    switch (key) {
    case SDL_SCANCODE_Q: input.rotation     =  delta; break;
    case SDL_SCANCODE_E: input.rotation     = -delta; break;
    case SDL_SCANCODE_R: input.acceleration =  delta; break;
    case SDL_SCANCODE_F: input.acceleration = -delta; break;
    case SDL_SCANCODE_W: input.move_y       =  delta; break;
    case SDL_SCANCODE_S: input.move_y       = -delta; break;
    case SDL_SCANCODE_D: input.move_x       =  delta; break;
    case SDL_SCANCODE_A: input.move_x       = -delta; break;
    default: break;
    }
    inputCompose(&self->input, &input);
}

void contextUpdate( Context *self ) {
    const float accelerationSpeed = 1.0;
    const float rotationSpeed = 1.0;
    Vec3 starOffset;

    timerUpdate(&self->timer);

    if (self->timer.fpsIsNew)
        printf("FPS: %f\n", self->timer.fps);

    self->speed += self->timer.deltaTime * self->input.acceleration * accelerationSpeed;

    if (fabs(self->input.rotation) > 0.1)
        contextRotateStars(self, rotationSpeed * self->input.rotation * self->timer.deltaTime);

    starOffset.x = -self->speed * self->timer.deltaTime * self->input.move_x;
    starOffset.y = -self->speed * self->timer.deltaTime * self->input.move_y;
    starOffset.z = -self->speed * self->timer.deltaTime * 1.0f;
    contextMoveStars(self, starOffset);
}

static int vertex_qsort_cmp( const void *lhs, const void *rhs ) {
    return 1 - 2 * (int)signbit(((const Vertex *)rhs)->d2 - ((const Vertex *)lhs)->d2);
}

void contextRender( Context *self ) {
    SDL_Surface *surface = SDL_GetWindowSurface(self->window);
    const float clip = 0.5;
    float halfW = surface->w / 2.0f;
    float halfH = surface->h / 2.0f;
    float whScale = sqrt((surface->w * surface->w + surface->h * surface->h) / (1.0f - clip * clip));
    float xyMul = clip * surface->w * surface->h / whScale;
    Vertex *vt = self->vertex_buffer;
    Vertex *vtEnd;
    Vec3 *const starEnd = self->stars + self->starCount;
    Vec3 *star;

    if (SDL_MUSTLOCK(surface) && !SDL_LockSurface(surface))
        return;

    if (surface->format->format != SDL_PIXELFORMAT_RGB888)
        return;

    for (star = self->stars; star < starEnd; star++) {
        if (star->z <= 0.0)
            continue;
        vt->x = halfW + xyMul * star->x / star->z;
        vt->y = halfH - xyMul * star->y / star->z;
        // vt->x and vt->y cannot be < 0 due to their unsigned nature
        if (vt->x > surface->w - 4 || vt->y > surface->h - 4)
            continue;
        vt->d2 = vec3Dot(*star, *star);
        vt++;
    }

    vtEnd = vt;
    vt = self->vertex_buffer;
    qsort(vt, vtEnd - vt, sizeof(Vertex), vertex_qsort_cmp);

    memset(surface->pixels, 0, surface->pitch * surface->h);

    for (; vt < vtEnd; vt++) {
        const int size = vt->d2 < 0.0025f ? 4 : vt->d2 < 0.01f ? 3 : vt->d2 < 0.09 ? 2 : 1;
        const unsigned char color = (unsigned char)(255.0f * (1.0f - vt->d2));
        unsigned char *pixelPtr = (unsigned char *)surface->pixels + surface->pitch * vt->y + vt->x * 4;
        unsigned char *const pixelEnd = pixelPtr + surface->pitch * size;

        for (; pixelPtr < pixelEnd; pixelPtr += surface->pitch)
            memset(pixelPtr, color, size * 4);
    }

    if (SDL_MUSTLOCK(surface))
        SDL_UnlockSurface(surface);

    SDL_UpdateWindowSurface(self->window);
}

void contextMainLoop( Context *self ) {
    int doQuit = 0;

    while (!doQuit) {
        SDL_Event event;
        while (SDL_PollEvent(&event))
            switch (event.type) {
            case SDL_QUIT:
                doQuit = 1;
                break;
            case SDL_KEYDOWN:
                contextOnKeyDown(self, event.key.keysym.scancode, 1);
                break;
            case SDL_KEYUP:
                contextOnKeyDown(self, event.key.keysym.scancode, 0);
                break;
            default:
                break;
            }

        contextUpdate(self);
        contextRender(self);
    }
}

int main( void ) {
    Context context;
    unsigned char *allocation = NULL;
    memset(&context, 0, sizeof(Context));

    SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS);
    context.window = SDL_CreateWindow("stars-c", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 800, 600, 0);
    randomInit(&context.random, 47);
    inputInit(&context.input);
    timerInit(&context.timer);

    context.starCount = 8192;
    allocation = (unsigned char *)malloc(context.starCount * (sizeof(Vec3) + sizeof(Vertex)));
    if (allocation == NULL)
        goto main__deinit;

    context.stars = (Vec3 *)(allocation + 0);
    context.vertex_buffer = (Vertex *)(allocation + sizeof(Vec3) * context.starCount);
    context.speed = 0.47;
    contextInitStars(&context);

    contextMainLoop(&context);

main__deinit:
    free(allocation);
    SDL_DestroyWindowSurface(context.window);
    SDL_Quit();
    return 0;
}
