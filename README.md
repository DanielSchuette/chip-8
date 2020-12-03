# Chip-8
_The_ most simple emulator to write. All opcodes are implemented following [this reference](http://devernay.free.fr/hacks/chip8/chip8def.htm), sound and graphics I/O work via SDL. Running `make test` uses [this awesome ROM](https://github.com/corax89/chip8-test-rom) to test a bunch of instructions at once.

# Compilation
The only dependencies are `gcc`, `make` and `SDL2`. You should probably have those. If there is no `assets/beep.wav`, the interpreter will complain once and simply not play any sound effects.

```bash
make all
make test
```

# Keys
Chip-8 accepts input from keys `0 - 9, a, b, c, d, e, f`. A few other keys instruct the interpreter to do things that aren't part of Chip-8 but are useful:

| Key       | Action                                     |
| --------- | ------------------------------------------ |
| `h/r`     | halt/resume the emulator                   |
| `n`       | when halted, run the next instruction only |
| `i`       | display machine state information          |
| `up/down` | increase/decrease CPU clock speed          |

# License
All code except for the test ROM (see above) is licensed under an [MIT license](./LICENSE.md).
