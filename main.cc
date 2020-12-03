/* A Chip-8 implementation, following the documentation at:
 * ``http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#3.1''.
 */
#include <algorithm>
#include <cassert>
#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <random>
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_ttf.h>
#include <SDL2/SDL_mixer.h>
#include <string>
#include <thread>

#define _DEBUG              1
#define _ENABLE_SUPER_CHIP8 1
#define NOT_IMPLEMENTED(msg) { fprintf(stderr, "%s\n", msg); SDL_Delay(2); }

static const char*                progname;
static std::random_device         rnd_dev;
static std::default_random_engine rnd_gen(rnd_dev());

static constexpr uint8_t sound_hz      = 60;

void warn(const char*, ...);
[[noreturn]] void error(const char*, ...);
[[noreturn]] void usage(void);

// Debugging of e.g. big-/little-endianess issues. Probably not used anywhere.
template <typename T>
void pr_bits(T word)
{
    static_assert(std::is_integral<T>::value,
                  "argument must have an integral type");
    for (int bit = sizeof(word)*8-1; bit >= 0; bit--) {
        uint8_t b = (word >> bit) & 0x1;
        if (b) fprintf(stderr, "1");
        else   fprintf(stderr, "0");
    }
    fprintf(stderr, "\n");
}

class Chip8 {
    static constexpr uint16_t mem_size           = 0x1000;
    static constexpr uint16_t stack_size         = 0x40;
    static constexpr uint16_t program_start      = 0x200;
    static constexpr uint16_t display_width      = 0x40; // or 0x80 pixels
    static constexpr uint16_t display_height     = 0x20; // or 0x40 pixels
    static constexpr uint16_t num_display_pixels = display_width *
                                                   display_height;

    /* There are 16 8-bit registers (v0 through vf) and 1 16-bit register (I),
     * which is usually used to store 12-bit memory addresses.
     */
    static constexpr uint8_t num_registers = 0x12;
    static constexpr uint8_t num_keys = 0x10;

    // special purpose registers, decremented at 60Hz if non-zero
    uint8_t delay_timer_reg = 0;
    uint8_t sound_timer_reg = 0;  // machine plays a sound if non-zero
    uint8_t timer_frequency = 60; // in Hz

    /* Keyboard state is kept in an array and updated every frame. `true'
     * indicates that a key is pressed, `false' that it isnt'.
     */
    bool keyboard[num_keys] = {};

    // non-accessable pseudo-registers
    uint16_t pc      = 0x200; // program counter, points at next instruction
    uint8_t  sp      = 0;     // stack pointer, points at next empty spot

    // TODO: for stepping back, we need to make this an array
    uint16_t prev_pc = pc;    // save last pc, e.g. to check for infinite loops

    /* Memory Map:
     * +---------------+= 0xFFF (4095) End of Chip-8 RAM
     * |               |
     * | 0x200 to 0xFFF|
     * |     Chip-8    |
     * | Program / Data|
     * |     Space     |
     * |               |
     * +- - - - - - - -+= 0x600 (1536) Start of ETI 660 Chip-8 programs
     * |               |
     * +---------------+= 0x200 (512) Start of most Chip-8 programs
     * | 0x000 to 0x1FF|
     * | Reserved for  |
     * |  interpreter  |
     * +---------------+= 0x000 (0) Start of Chip-8 RAM
     *
     * All multi-byte values are stored in big-endian order.
     * In addition, Chip-8 has a stack of 16 16-bit values which allows for 12
     * levels of nested subroutines.
     */
    uint8_t  memory[mem_size]  = {};
    uint16_t stack[stack_size] = {};

    static constexpr uint16_t builtin_font_stride    = 0x5;
    static constexpr uint16_t builtin_fontset_size   = 0x10*builtin_font_stride;
    static constexpr uint16_t builtin_font_mem_start = 0x0;
    static constexpr uint16_t builtin_font_mem_end   = 0x1ff;
    const uint8_t builtin_fontset[builtin_fontset_size] = {
        0xF0, 0x90, 0x90, 0x90, 0xF0, // 8x5 sprite for `0'
        0x20, 0x60, 0x20, 0x20, 0x70, // 8x5 sprite for `1'
        0xF0, 0x10, 0xF0, 0x80, 0xF0, // 8x5 sprite for `2'
        0xF0, 0x10, 0xF0, 0x10, 0xF0, // 8x5 sprite for `3'
        0x90, 0x90, 0xF0, 0x10, 0x10, // 8x5 sprite for `4'
        0xF0, 0x80, 0xF0, 0x10, 0xF0, // 8x5 sprite for `5'
        0xF0, 0x80, 0xF0, 0x90, 0xF0, // 8x5 sprite for `6'
        0xF0, 0x10, 0x20, 0x40, 0x40, // 8x5 sprite for `7'
        0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8x5 sprite for `8'
        0xF0, 0x90, 0xF0, 0x10, 0xF0, // 8x5 sprite for `9'
        0xF0, 0x90, 0xF0, 0x90, 0x90, // 8x5 sprite for `a'
        0xE0, 0x90, 0xE0, 0x90, 0xE0, // 8x5 sprite for `b'
        0xF0, 0x80, 0x80, 0x80, 0xF0, // 8x5 sprite for `c'
        0xE0, 0x90, 0x90, 0x90, 0xE0, // 8x5 sprite for `d'
        0xF0, 0x80, 0xF0, 0x80, 0xF0, // 8x5 sprite for `e'
        0xF0, 0x80, 0xF0, 0x80, 0x80, // 8x5 sprite for `f'
        // ... everything else is initialized to 0
    };

    /* While the original Chip-8 had a 64x32-pixel monochrome display, it seems
     * like most implementations support a 128x64-pixel mode. That's the one we
     * have, too. Also, the interpreter-area usually holds binary data for the
     * sprites of digits `0' through `F'.
     * Chip-8 pixels are just a single bit where 0 indicates black and 1
     * indicates white. To simplify copying data to the screen, we represent
     * every pixel with a byte anyways.
     */
    uint8_t display[display_width*display_height] = {};

    bool           halted;
    int8_t         waiting_for_key = -1;
    uint32_t       instr_delay     = 2; // ms delay after every instruction
    const uint32_t max_instr_delay = 500;

    // SDL attributes
    SDL_Window*   window         = nullptr;
    SDL_Renderer* renderer       = nullptr;
    Mix_Chunk*    beep_sound     = nullptr;
    const char*   title          = "Chip-8";
    uint8_t       pixel_size     = 0x14;
    uint32_t      win_x_pos      = 0;
    uint32_t      win_y_pos      = 0; // SDL_WINDOWPOS_CENTERED;
    uint32_t      win_width      = display_width * pixel_size;
    uint32_t      win_height     = display_height * pixel_size;
    bool          win_is_visible = true;

    int16_t  store_to_mem(uint16_t, uint8_t);
    void     store_to_mem_savely(uint16_t, uint8_t);
    int16_t  load_from_mem(uint16_t) const;
    uint8_t  load_from_mem_savely(uint16_t) const;
    int16_t  store_to_reg(uint8_t, uint8_t);
    void     store_to_reg_savely(uint8_t, uint8_t);
    int16_t  load_from_reg(uint8_t) const;
    uint8_t  load_from_reg_savely(uint8_t) const;
    void     store_to_reg_i(uint16_t);
    uint16_t load_from_reg_i(void) const;
    void     push_stack(uint16_t);
    uint16_t pop_stack(void);
    uint8_t  copy_byte_to_display(uint8_t, uint8_t, uint8_t);
    void     print_mem_range(uint16_t, uint16_t, uint8_t = 16) const;
    void     update_timers(void);
    void     update_keyboard(void);
    void     swap_display(void);

    bool    is_key_pressed(uint8_t) const;
    bool    any_key_pressed(void) const;
    uint8_t first_key_pressed(void) const;

    // Chip-8 instructions
    void cls(uint8_t);
    void ret(void);
    void jmp(uint16_t);
    void jmp_v0(uint16_t);
    void call(uint16_t);
    void skip_eq(uint8_t, uint8_t);
    void skip_neq(uint8_t, uint8_t);
    void skip_regs_eq(uint8_t, uint8_t);
    void skip_regs_neq(uint8_t, uint8_t);
    void skip_press_eq(uint8_t);
    void skip_press_neq(uint8_t);
    void ld(uint8_t, uint8_t);
    void ld_reg(uint8_t, uint8_t);
    void ld_from_dt(uint8_t);
    void ld_to_dt(uint8_t);
    void ld_to_st(uint8_t);
    void ld_wait_key(uint8_t);
    void ld_recv_key(void);
    void ld_sprite(uint8_t);
    void ld_bcd(uint8_t);
    void ld_regs(uint8_t);
    void save_regs(uint8_t);
    void add(uint8_t, uint8_t);
    void add_i(uint8_t);
    void or_reg(uint8_t, uint8_t);
    void and_reg(uint8_t, uint8_t);
    void xor_reg(uint8_t, uint8_t);
    void add_reg(uint8_t, uint8_t);
    void sub(uint8_t, uint8_t);
    void shr(uint8_t);
    void subn(uint8_t, uint8_t);
    void shl(uint8_t);
    void rnd(uint8_t, uint8_t);
    void drw(uint8_t, uint8_t, uint8_t);

    /* Super Chip-8 instructions
     * TODO:
     * 00Cn - SCD nibble
     * 00FB - SCR
     * 00FC - SCL
     * 00FE - LOW
     * 00FF - HIGH
     * Dxy0 - DRW Vx, Vy, 0
     * Fx30 - LD HF, Vx
     * Fx75 - LD R, Vx
     * Fx85 - LD Vx, R
     */
    void sc_exit(void);

    void    set_delay_timer(uint8_t v)  { delay_timer_reg = v; }
    void    set_sound_timer(uint8_t v)  { sound_timer_reg = v; }
    uint8_t get_delay_timer(void) const { return delay_timer_reg; }
    uint8_t get_sound_timer(void) const { return sound_timer_reg; }

    [[noreturn]] static void unknown_instr_error(uint16_t);
public:
    union {
        uint8_t register_block[num_registers] = {};
        struct {
            uint8_t v0, v1, v2, v3, v4, v5, v6, v7,
                    v8, v9, va, vb, vc, vd, ve, vf,
                    i0, i1; // `I' is actually a 16-bit register
        } registers;
    };

    Chip8(bool);
    ~Chip8(void);

    Chip8(const Chip8&) = delete;
    Chip8(Chip8&&)      = delete;
    Chip8& operator=(const Chip8&) = delete;
    Chip8& operator=(Chip8&&)      = delete;

    void run(void);
    void run_single_instr(bool = true);
    void load_program_to_mem(const char*);
    void toggle_visibility(void);
    void incr_clock_speed(void);
    void decr_clock_speed(void);
    void dump_mem(void) const;
    void dump_stack(void) const;
    void dump_regs(void) const;
    void dump_display(void) const;
    void dump_keyboard(void) const;
    void dump_properties(void) const;
    void dump_all(void) const;

    bool is_halted(void) const  { return halted; }
    void halt(void)             { warn("halting the machine"); halted = true; }
    void enter_run_state(void)  { warn("resume running"); halted = false; }
};

Chip8::Chip8(bool initially_halted) : halted(initially_halted)
{
    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) error(SDL_GetError());
    if (TTF_Init() != 0)                    error(TTF_GetError());

    int img_init_flags = IMG_INIT_JPG | IMG_INIT_PNG | IMG_INIT_TIF;
    if (IMG_Init(img_init_flags) != img_init_flags) error(IMG_GetError());

    // X11 usually pings windows to check if they're hung - which we don't need
    bool success = SDL_SetHint(SDL_HINT_VIDEO_X11_NET_WM_PING, "0");
    if (!success) warn("unable to set SDL window hint");

    window = SDL_CreateWindow(title, win_x_pos, win_y_pos, win_width,
                              win_height, SDL_WINDOW_SHOWN);
    if (!window) error(SDL_GetError());

    /* Enabling vsync (`SDL_RENDERER_PRESENTVSYNC') should limit the framerate
     * to whatever the video card is capable of. This doesn't work too well for
     * Chip-8, though. The game itself should either limit the framerate or
     * allow for adjusting the cpu speed.
     */
    uint32_t render_flags = SDL_RENDERER_ACCELERATED;
    renderer = SDL_CreateRenderer(window, -1, render_flags);
    if (!renderer) error(SDL_GetError());
    SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);

    if (Mix_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 2048) < 0)
        error(Mix_GetError());

    beep_sound = Mix_LoadWAV("assets/beep.wav");
    if (!beep_sound) warn(Mix_GetError());
}

Chip8::~Chip8(void)
{
    if (window)     SDL_DestroyWindow(window);
    if (renderer)   SDL_DestroyRenderer(renderer);
    if (beep_sound) Mix_FreeChunk(beep_sound);
    TTF_Quit();
    IMG_Quit();
    SDL_Quit();
}

// Run the entire program as loaded into memory. The machine cannot be halted.
void Chip8::run(void)
{
    while (true) run_single_instr();
}

/* Run a single Chip-8 instruction. In here, we delay the next instruction by
 * a constant amount of milliseconds which is adjustable.
 */
void Chip8::run_single_instr(bool skip_if_halted)
{
    if (is_halted() && skip_if_halted) return;

    // update status registers and act on a 0xfx0a instruction
    update_timers();
    update_keyboard();
    if (waiting_for_key >= 0) {
        SDL_Delay(instr_delay);
        return;
    }

    // fetch an instruction from memory (aligned at 2-byte boundaries)
    assert((pc & 0x1) == 0x0);
    uint8_t  upper_byte = memory[pc++];
    uint8_t  lower_byte = memory[pc++];
    uint16_t instr      = (upper_byte << 8) | lower_byte;

    // decode
    uint8_t  op_code = (upper_byte & 0xf0) >> 4;
    uint16_t addr    = instr & 0x0fff;
    uint8_t  reg     = upper_byte & 0x0f;
    uint8_t  reg2    = (lower_byte & 0xf0) >> 4;
    uint8_t  alu_op  = lower_byte & 0x0f;

#if defined(_DEBUG) && _DEBUG == 1
    fprintf(stderr, "pc=0x%04x op=0x%x instr=0x%04x\n", pc-2, op_code, instr);
#endif

    // execute
    switch (op_code) {
    case 0x0:
        switch (lower_byte) {
        case 0x00:
            // should 0x0000 really halt the machine?
            warn("encountered instr=0x0000, halting the machine");
            exit(0);
        case 0xfd: sc_exit(); break;
        case 0xe0: cls(0x0);  break;
        case 0xee: ret();     break;
        default:   unknown_instr_error(instr);
        }
        break;
    case 0x1: jmp(addr);  break;
    case 0x2: call(addr); break;
    case 0x3: skip_eq(reg, lower_byte);  break;
    case 0x4: skip_neq(reg, lower_byte); break;
    case 0x5: skip_regs_eq(reg, reg2);   break;
    case 0x6: ld(reg, lower_byte);  break;
    case 0x7: add(reg, lower_byte); break;
    case 0x8:
        switch (alu_op) {
        case 0x0: ld_reg(reg, reg2);  break;
        case 0x1: or_reg(reg, reg2);  break;
        case 0x2: and_reg(reg, reg2); break;
        case 0x3: xor_reg(reg, reg2); break;
        case 0x4: add_reg(reg, reg2); break;
        case 0x5: sub(reg, reg2);  break;
        case 0x6: shr(reg);        break;
        case 0x7: subn(reg, reg2); break;
        case 0xe: shl(reg);        break;
        default:  unknown_instr_error(instr);
        }
        break;
    case 0x9: skip_regs_neq(reg, reg2); break;
    case 0xa: store_to_reg_i(addr);     break;
    case 0xb: jmp_v0(addr);             break;
    case 0xc: rnd(reg, lower_byte);     break;
    case 0xd: drw(reg, reg2, alu_op);   break;
    case 0xe:
        switch (lower_byte) {
        case 0x9e: skip_press_eq(reg);  break;
        case 0xa1: skip_press_neq(reg); break;
        default:   unknown_instr_error(instr);
        }
        break;
    case 0xf:
        switch (lower_byte) {
        case 0x07: ld_from_dt(reg);  break;
        case 0x0a: ld_wait_key(reg); break;
        case 0x15: ld_to_dt(reg);    break;
        case 0x18: ld_to_st(reg);    break;
        case 0x1e: add_i(reg);       break;
        case 0x29: ld_sprite(reg);   break;
        case 0x33: ld_bcd(reg);      break;
        case 0x55: save_regs(reg);   break;
        case 0x65: ld_regs(reg);     break;
        default:   unknown_instr_error(instr);
        }
        break;
    default:  unknown_instr_error(instr);
    }

    // going beyond memory and an endless loop halt the machine
    if (pc == prev_pc || pc >= mem_size) {
        warn("entered an endless loop or exhausted memory");
        halt();
    }
    prev_pc = pc;
    SDL_Delay(instr_delay);
}

void Chip8::load_program_to_mem(const char* path)
{
    std::ifstream src_code(path, std::ios::in | std::ios::binary);
    if (!src_code.is_open()) {
        error("unable to load program from `%s'", path);
        exit(1);
    }

    uint16_t mem_ptr = program_start;
    while (!src_code.eof()) {
        char c;
        src_code.read(&c, 1);
        if (mem_ptr >= mem_size) {
            error("cannot load program, out of memory");
            exit(1);
        }
        uint8_t* uc = (uint8_t*)&c;
        if (store_to_mem(mem_ptr++, *uc) == -1)
            error("cannot store program at 0x%x", mem_ptr-1);
    }

    src_code.close();

    // load builtin font multiple times throughout the entire reserved mem
    mem_ptr = builtin_font_mem_start;
    uint16_t byte = 0;
    while (mem_ptr <= builtin_font_mem_end) {
        if (store_to_mem(mem_ptr++, builtin_fontset[byte]) == -1)
            error("cannot store font data at 0x%x", mem_ptr-1);
        byte = (byte+1) % builtin_fontset_size;
    }
}

// IMPROVE: Play music instead of a music chunk, pause if necessary.
void Chip8::update_timers(void)
{
    static uint32_t last_timer_ticks = SDL_GetTicks();
    uint32_t current_ticks = SDL_GetTicks();
    if ((current_ticks - last_timer_ticks) >= (1000 / timer_frequency)) {
        last_timer_ticks = current_ticks;
        if (delay_timer_reg > 0) delay_timer_reg--;
        if (sound_timer_reg > 0) {
            sound_timer_reg--;
            Mix_PlayChannelTimed(-1, beep_sound, 0, 1000 / sound_hz);
        }
    }
}

/* Update the CPU's keyboard state data. The return code indicates whether the
 * machine was instructed to terminate prematurely. It can savely be ignored.
 */
void Chip8::update_keyboard(void)
{
    SDL_PumpEvents();
    const uint8_t* state   = SDL_GetKeyboardState(NULL);
    bool           pressed = false;

    keyboard[0x0] = state[SDL_SCANCODE_0] ? (pressed = true) : false;
    keyboard[0x1] = state[SDL_SCANCODE_1] ? (pressed = true) : false;
    keyboard[0x2] = state[SDL_SCANCODE_2] ? (pressed = true) : false;
    keyboard[0x3] = state[SDL_SCANCODE_3] ? (pressed = true) : false;
    keyboard[0x4] = state[SDL_SCANCODE_4] ? (pressed = true) : false;
    keyboard[0x5] = state[SDL_SCANCODE_5] ? (pressed = true) : false;
    keyboard[0x6] = state[SDL_SCANCODE_6] ? (pressed = true) : false;
    keyboard[0x7] = state[SDL_SCANCODE_7] ? (pressed = true) : false;
    keyboard[0x8] = state[SDL_SCANCODE_8] ? (pressed = true) : false;
    keyboard[0x9] = state[SDL_SCANCODE_9] ? (pressed = true) : false;
    keyboard[0xa] = state[SDL_SCANCODE_A] ? (pressed = true) : false;
    keyboard[0xb] = state[SDL_SCANCODE_B] ? (pressed = true) : false;
    keyboard[0xc] = state[SDL_SCANCODE_C] ? (pressed = true) : false;
    keyboard[0xd] = state[SDL_SCANCODE_D] ? (pressed = true) : false;
    keyboard[0xe] = state[SDL_SCANCODE_E] ? (pressed = true) : false;
    keyboard[0xf] = state[SDL_SCANCODE_F] ? (pressed = true) : false;

    if (pressed && waiting_for_key >= 0) ld_recv_key();
}

void Chip8::swap_display(void)
{
    for (int pos = 0; pos < display_width*display_height; pos++) {
        if (display[pos] == 0x0)
            SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
        else if (display[pos] == 0x1)
            SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
        else
            error("invalid display color value");

        SDL_Rect rect;
        rect.w = rect.h = pixel_size;
        rect.x = (pos % display_width) * pixel_size;
        rect.y = pos == 0 ? 0 : (pos / display_width * pixel_size);
        SDL_RenderFillRect(renderer, &rect);
    }
    SDL_RenderPresent(renderer);
}

// Report upper and lower byte of a 2-byte instruction and exit the program.
[[noreturn]] void Chip8::unknown_instr_error(uint16_t instr)
{
    error("unknown instruction 0x%04x", instr);
}

void Chip8::cls(uint8_t pixel)
{
    for (int x = 0; x < display_width; x++)
        for (int y = 0; y < display_height; y++)
            display[y*display_width+x] = pixel;
}

void Chip8::jmp(uint16_t addr)
{
    pc = addr;
}

void Chip8::jmp_v0(uint16_t addr)
{
    uint8_t v0 = load_from_reg(0x0);
    pc = addr + v0;
}

void Chip8::call(uint16_t addr)
{
    push_stack(pc);
    pc = addr;
}

void Chip8::ret(void)
{
    pc = pop_stack();
}

void Chip8::skip_eq(uint8_t reg, uint8_t byte)
{
    if (load_from_reg_savely(reg) == byte)
        pc += 2;
}

void Chip8::skip_neq(uint8_t reg, uint8_t byte)
{
    if (load_from_reg_savely(reg) != byte)
        pc += 2;
}

void Chip8::skip_regs_eq(uint8_t reg1, uint8_t reg2)
{
    if (load_from_reg_savely(reg1) == load_from_reg_savely(reg2))
        pc += 2;
}

void Chip8::skip_press_eq(uint8_t reg)
{
    uint8_t val = load_from_reg_savely(reg);
    if (is_key_pressed(val))
        pc += 2;
}

void Chip8::skip_press_neq(uint8_t reg)
{
    uint8_t val = load_from_reg_savely(reg);
    if (!is_key_pressed(val))
        pc += 2;
}

void Chip8::ld(uint8_t reg, uint8_t val)
{
    store_to_reg_savely(reg, val);
}

void Chip8::ld_reg(uint8_t reg1, uint8_t reg2)
{
    uint8_t val = load_from_reg_savely(reg2);
    store_to_reg_savely(reg1, val);
}

void Chip8::ld_from_dt(uint8_t reg)
{
    store_to_reg_savely(reg, get_delay_timer());
}

void Chip8::ld_to_dt(uint8_t reg)
{
    uint8_t val = load_from_reg_savely(reg);
    set_delay_timer(val);
}

void Chip8::ld_to_st(uint8_t reg)
{
    uint8_t val = load_from_reg_savely(reg);
    set_sound_timer(val);
}

/* This instruction is a bit more complicated, we need to functions for it.
 * First, we save the register into which we have to load the key that was
 * pressed. Then, we check if we're waiting on every keyboard update. If we
 * are indeed waiting _and_ a key was pressed, we call `ld_recv_key()', save
 * the key to the correct register and reset `waiting_for_key'. See
 * `update_keyboard()', too.
 */
void Chip8::ld_wait_key(uint8_t reg)
{
    waiting_for_key = reg;
}

void Chip8::ld_recv_key(void)
{
    uint8_t reg = static_cast<uint8_t>(waiting_for_key);
    uint8_t key = first_key_pressed();
    fprintf(stderr, "reg=%d key=%d\n", reg, key);
    store_to_reg_savely(reg, key);
    waiting_for_key = -1;
}

void Chip8::ld_sprite(uint8_t reg)
{
    uint8_t  val        = load_from_reg_savely(reg);
    uint16_t sprite_pos = val * builtin_font_stride;
    store_to_reg_i(load_from_mem_savely(sprite_pos));
}

void Chip8::ld_bcd(uint8_t reg)
{
    uint16_t start_addr = load_from_reg_i();
    uint8_t  val        = load_from_reg_savely(reg);
    store_to_mem_savely(start_addr, val / 100);
    store_to_mem_savely(start_addr+1, (val % 100) / 10);
    store_to_mem_savely(start_addr+2, val % 10);
}

void Chip8::ld_regs(uint8_t reg)
{
    uint16_t start_addr = load_from_reg_i();
    for (int r = 0; r <= reg; r++) {
        uint8_t val = load_from_mem_savely(start_addr++);
        store_to_reg_savely(r, val);
    }
}

void Chip8::save_regs(uint8_t reg)
{
    uint16_t start_addr = load_from_reg_i();
    for (int r = 0; r <= reg; r++)
        store_to_mem_savely(start_addr++, load_from_reg_savely(r));
}

void Chip8::add(uint8_t reg, uint8_t val)
{
    // we practically implement a load store architecture
    uint8_t temp = load_from_reg_savely(reg);
    store_to_reg_savely(reg, temp + val);
}

void Chip8::add_i(uint8_t reg)
{
    uint8_t val_a = load_from_reg_i();
    uint8_t val_b = load_from_reg_savely(reg);
    store_to_reg_i(val_a + val_b);
}

void Chip8::or_reg(uint8_t reg1, uint8_t reg2)
{
    uint8_t val1 = load_from_reg_savely(reg1);
    uint8_t val2 = load_from_reg_savely(reg2);
    store_to_reg_savely(reg1, val1 | val2);
}

void Chip8::and_reg(uint8_t reg1, uint8_t reg2)
{
    uint8_t val1 = load_from_reg_savely(reg1);
    uint8_t val2 = load_from_reg_savely(reg2);
    store_to_reg_savely(reg1, val1 & val2);
}

void Chip8::xor_reg(uint8_t reg1, uint8_t reg2)
{
    uint8_t val1 = load_from_reg_savely(reg1);
    uint8_t val2 = load_from_reg_savely(reg2);
    store_to_reg_savely(reg1, val1 ^ val2);
}

void Chip8::add_reg(uint8_t reg1, uint8_t reg2)
{
    uint8_t  val1   = load_from_reg_savely(reg1);
    uint8_t  val2   = load_from_reg_savely(reg2);
    uint16_t result = val1 + val2;

    if (result > 0xff) store_to_reg(0xf, 0x1);
    else               store_to_reg(0xf, 0x0);

    store_to_reg_savely(reg1, result); // keep only lowest 8 bits of result
}

void Chip8::sub(uint8_t reg1, uint8_t reg2)
{
    uint8_t  val1   = load_from_reg_savely(reg1);
    uint8_t  val2   = load_from_reg_savely(reg2);

    if (val1 > val2) store_to_reg(0xf, 0x1);
    else             store_to_reg(0xf, 0x0);

    // BUG: not clear if this code is correct
    store_to_reg_savely(reg1, val1 - val2);
}

void Chip8::shr(uint8_t reg)
{
    uint8_t val = load_from_reg_savely(reg);
    store_to_reg(0xf, val & 0x1);
    store_to_reg_savely(reg, val / 2);
}

void Chip8::subn(uint8_t reg1, uint8_t reg2)
{
    uint8_t  val1   = load_from_reg_savely(reg1);
    uint8_t  val2   = load_from_reg_savely(reg2);

    if (val2 > val1) store_to_reg(0xf, 0x1);
    else             store_to_reg(0xf, 0x0);

    // BUG: not clear if this code is correct
    store_to_reg_savely(reg1, val2 - val1);
}

void Chip8::shl(uint8_t reg)
{
    uint8_t val = load_from_reg_savely(reg);
    store_to_reg(0xf, val & 0x1);
    store_to_reg_savely(reg, val * 2);
}

void Chip8::skip_regs_neq(uint8_t reg1, uint8_t reg2)
{
    if (load_from_reg_savely(reg1) != load_from_reg_savely(reg2))
        pc += 2;
}

void Chip8::rnd(uint8_t reg, uint8_t byte)
{
    std::uniform_int_distribution<uint8_t> dist(0, 255);
    uint8_t rnd = dist(rnd_gen);
    store_to_reg_savely(reg, rnd & byte);
}

// We only swap the display after a draw call.
void Chip8::drw(uint8_t reg1, uint8_t reg2, uint8_t n)
{
    // NOTE: sprites are _always_ 8 pixels wide
    uint16_t start_addr = load_from_reg_i();
    uint8_t  erased     = 0x0;
    uint8_t  x_pos      = load_from_reg_savely(reg1);
    uint8_t  y_pos      = load_from_reg_savely(reg2);
    for (int i = 0; i < n; i++) {
        uint8_t byte = load_from_mem_savely(start_addr + i);
        erased |= copy_byte_to_display(x_pos, y_pos++, byte);
    }
    store_to_reg(0xf, erased);
    swap_display();
}

/* Here, we handle wrapping of off-screen coordinates. It isn't totally clear
 * how Chip-8 handles this.
 */
uint8_t Chip8::copy_byte_to_display(uint8_t x, uint8_t y, uint8_t byte)
{
    x = x % display_width;
    y = y % display_height;
    uint8_t erased = 0x0;
    for (int i = 0; i < 8; i++) {
        uint16_t pos;
        if (x + i >= display_width) {
            uint8_t _y = y == 0 ? 0 : (y - 1);
            pos = _y * display_width + ((x + i) % display_width);
        } else {
            pos = y * display_width + x + i;
        }

        assert(pos < num_display_pixels);
        uint8_t prev_pixel = display[pos];
        display[pos] ^= (byte >> (7 - i)) & 0x1;
        if (display[pos] == 0x0 && prev_pixel != 0x0)
            erased = 0x1;
    }
    return erased;
}

void Chip8::sc_exit(void)
{
#if defined(_ENABLE_SUPER_CHIP8) && _ENABLE_SUPER_CHIP8 == 1
    exit(0);
#endif
}

// Returns -1 if memory access is somehow invalid.
int16_t Chip8::load_from_mem(uint16_t addr) const
{
    if ((addr >= builtin_font_mem_start && addr <= builtin_font_mem_end) ||
        addr >= program_start)
        return static_cast<int16_t>(memory[addr]);
    return -1;
}

uint8_t Chip8::load_from_mem_savely(uint16_t addr) const
{
    int16_t val = load_from_mem(addr);
    if (val < 0) error("invalid reading memory access at 0x%04x", addr);
    return static_cast<uint8_t>(val);
}

// Returns -1 if memory access is somehow invalid.
int16_t Chip8::store_to_mem(uint16_t addr, uint8_t value)
{
    if ((addr >= builtin_font_mem_start && addr <= builtin_font_mem_end) ||
        addr >= program_start) {
        memory[addr] = value;
        return 0;
    }
    return -1;
}

void Chip8::store_to_mem_savely(uint16_t addr, uint8_t value)
{
    int16_t result = store_to_mem(addr, value);
    if (result < 0) error("invalid writing memory access at 0x%04x", addr);
}

// Returns -1 if register access is somehow invalid.
int16_t Chip8::store_to_reg(uint8_t reg, uint8_t value)
{
    if (reg >= num_registers)
        return -1;
    register_block[reg] = value;
    return 0;
}

void Chip8::store_to_reg_savely(uint8_t reg, uint8_t value)
{
    if (store_to_reg(reg, value) < 0)
        error("invalid writing register access");
}

// Returns -1 if register access is somehow invalid.
int16_t Chip8::load_from_reg(uint8_t reg) const
{
    if (reg >= num_registers)
        return -1;
    return static_cast<int16_t>(register_block[reg]);
}

// Load from a register and check the result for errors. On error, we exit.
uint8_t Chip8::load_from_reg_savely(uint8_t reg) const
{
    int16_t value = load_from_reg(reg);
    if (value < 0) error("invalid reading register access");
    return static_cast<uint8_t>(value);
}

// Since `I' is meant for addresses, we only store unsigned values for now.
void Chip8::store_to_reg_i(uint16_t value)
{
    uint16_t* iptr = (uint16_t*)&registers.i0;
    *iptr = value;
}

// Since `I' is meant for addresses, we only load unsigned values for now.
uint16_t Chip8::load_from_reg_i(void) const
{
    uint16_t* iptr = (uint16_t*)&registers.i0;
    return *iptr;
}

// For now, we simply exit if an invalid stack access is attempted.
void Chip8::push_stack(uint16_t value)
{
    if (sp >= stack_size) error("stack overflow");
    stack[sp++] = value;
}

// For now, we simply exit if an invalid stack access is attempted.
uint16_t Chip8::pop_stack(void)
{
    if (sp == 0) error("cannot pop an empty stack");
    return stack[--sp];
}

void Chip8::dump_mem(void) const
{
    fprintf(stderr, "Interpreter Mem (0x%x-0x%x)\n", 0, program_start-1);
    print_mem_range(0, program_start-1);
    fprintf(stderr, "\nProgram Mem (0x%x-0x%x)\n", program_start, mem_size-1);
    print_mem_range(program_start, mem_size-1);
}

void Chip8::dump_stack(void) const
{
    fprintf(stderr, "Stack:\n");
    for (int i = 0; i < stack_size; i++)
        fprintf(stderr, "0x%04x ", stack[i]);
    fprintf(stderr, "\n");
}

void Chip8::dump_regs(void) const
{
    fprintf(stderr, "Registers:\n");
    for (uint8_t i = 0; i < num_registers - 2; i++) // don't display reg `I'
        fprintf(stderr, "V%x=0x%x ", i, load_from_reg_savely(i));
    fprintf(stderr, "\n");
}

void Chip8::dump_display(void) const
{
    fprintf(stderr, "Display:\n");
    for (int y = 0; y < display_height; y++) {
        for (int x = 0; x < display_width; x++)
            fprintf(stderr, "%d", display[y * display_width + x]);
        fprintf(stderr, "\n");
    }
}

void Chip8::dump_keyboard(void) const
{
    fprintf(stderr, "Keyboard:\n");

    for (int i = 0; i < num_keys; i++)
        fprintf(stderr, "|%x", i);
    fprintf(stderr, "|\n");

    for (int i = 0; i < num_keys; i++)
        fprintf(stderr, "|%d", keyboard[i]);
    fprintf(stderr, "|\n");
}

void Chip8::dump_properties(void) const
{
    fprintf(stderr, "VM Properties:\n");
    fprintf(stderr, "display width: 0x%x\n", display_width);
    fprintf(stderr, "display height: 0x%x\n", display_height);
    fprintf(stderr, "screen width: 0x%x\n", win_width);
    fprintf(stderr, "screen height: 0x%x\n", win_height);
    fprintf(stderr, "pixel size: 0x%x\n", pixel_size);
    fprintf(stderr, "available memory: 0x%x\n", mem_size);
    fprintf(stderr, "available stack space: 0x%x\n", stack_size);
    fprintf(stderr, "instruction delay: 0x%x\n", instr_delay);
    fprintf(stderr, "# of registers: 0x%x\n", num_registers);
    fprintf(stderr, "# of keys recognized: 0x%x\n", num_keys);
}

void Chip8::dump_all(void) const
{
    dump_display();
    dump_mem();
    dump_stack();
    dump_regs();
    dump_keyboard();
    dump_properties();
}

// Print memory contents in range `(from, to)'.
void Chip8::print_mem_range(uint16_t from, uint16_t to, uint8_t col_size) const
{
    uint8_t col = 0;
    for (int addr = from; addr <= to; addr++) {
        fprintf(stderr, "%02x ", memory[addr]);
        col++;
        if (col == col_size / 2) {
            fprintf(stderr, " ");
        } else if (col >= col_size) {
            fprintf(stderr, "\n");
            col = 0;
        }
    }
    fprintf(stderr, "\n");
}

bool Chip8::is_key_pressed(uint8_t key) const
{
    if (key >= num_keys) error("key 0x%x does not exist", key);
    return keyboard[key];
}

bool Chip8::any_key_pressed(void) const
{
    for (int i = 0; i < num_keys; i++)
        if (keyboard[i]) return true;
    return false;
}

// Always check if a key was pressed, first!
uint8_t Chip8::first_key_pressed(void) const
{
    for (int i = 0; i < num_keys; i++)
        if (keyboard[i]) return i;
    error("first_key_pressed(): no key was pressed");
}

/* BUG: Unfortunately, this doesn't really work with DWM. Maybe test under
 * another WM or DE?
 */
void Chip8::toggle_visibility(void)
{
    if (win_is_visible) {
        win_is_visible = false;
        SDL_HideWindow(window);
    } else {
        win_is_visible = true;
        SDL_RaiseWindow(window);
    }
}

void Chip8::incr_clock_speed(void)
{
    instr_delay = instr_delay == 0 ? 0 : instr_delay-1;
    fprintf(stderr, "instr_delay=%d\n", instr_delay);
}

// Increased delay means decreased clock speed.
void Chip8::decr_clock_speed(void)
{
    instr_delay = instr_delay == max_instr_delay ? instr_delay : instr_delay+1;
    fprintf(stderr, "instr_delay=%d\n", instr_delay);
}

[[noreturn]] void usage(void)
{
    fprintf(stderr, "Usage:\n\tchip8 <path_to_src>\n");
    exit(0);
}

void warn(const char* fmt, ...)
{
    va_list ap;
    char buf[BUFSIZ];

    va_start(ap, fmt);
    vsnprintf(buf, BUFSIZ-1, fmt, ap);
    va_end(ap);

    fprintf(stderr, "%s: warning: %s.\n", progname, buf);
}

[[noreturn]] void error(const char* fmt, ...)
{
    va_list ap;
    char buf[BUFSIZ];

    va_start(ap, fmt);
    vsnprintf(buf, BUFSIZ-1, fmt, ap);
    va_end(ap);

    fprintf(stderr, "%s: error: %s.\n", progname, buf);
    exit(1);
}

int main(int argc, char** argv)
{
    progname = *argv;
    if (argc != 2) usage();

    Chip8 comp(false);
    comp.load_program_to_mem(*(argv+1));

    bool quit  = false;
    while (!quit) {
        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            switch (event.type) {
            case SDL_KEYDOWN:
                switch (event.key.keysym.scancode) {
                case SDL_SCANCODE_R: comp.enter_run_state();       break;
                case SDL_SCANCODE_W: comp.toggle_visibility();     break;
                case SDL_SCANCODE_N: comp.run_single_instr(false); break;
                case SDL_SCANCODE_Q: quit = true;     break;
                case SDL_SCANCODE_H: comp.halt();     break;
                case SDL_SCANCODE_I: comp.dump_all(); break;
                case SDL_SCANCODE_UP:   comp.incr_clock_speed(); break;
                case SDL_SCANCODE_DOWN: comp.decr_clock_speed(); break;
                default:                        /* do nothing */ break;
                }
                break;
            case SDL_QUIT: quit = true; break;
            default:   /* do nothing */ break;
            }
        }
        comp.run_single_instr(true);
    }

    return 0;
}
