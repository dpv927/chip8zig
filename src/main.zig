const std = @import("std");
const raylib = @import("raylib");

const chip8cpu = @import("chip8cpu.zig");
const Chip8CPU = chip8cpu.Chip8CPU;
const ExecError = chip8cpu.ExecError;
const FetchError = chip8cpu.FetchError;

pub fn main() !void {
    var cpu: Chip8CPU = Chip8CPU.init();
    const scale: i32 = 30;
    const fps: i32 = 60;

    // Load the ROM file to memory
    cpu.loadToROM("/home/tux/Downloads/program(1).c8") catch |err| {
        std.debug.print("ROM could not be loaded: {}", .{err});
        return;
    };

    // Load beep sound
    raylib.initAudioDevice();
    const sound = raylib.loadSound("/home/tux/Desktop/chip8asm/src/resources/beep.wav");
    defer raylib.unloadSound(sound);
    defer raylib.closeAudioDevice();

    // Create a new window (64x32 screen)
    raylib.initWindow(64 * scale, 32 * scale, "chip8zig");
    defer raylib.closeWindow();
    raylib.setTargetFPS(fps);

    while (!raylib.windowShouldClose()) {
        if (cpu.st > 0) {
            cpu.st -= 1;
            if (!raylib.isSoundPlaying(sound)) {
                raylib.playSound(sound);
            }
        } else {
            if (raylib.isSoundPlaying(sound)) {
                raylib.stopSound(sound);
            }
        }
        if (cpu.dt > 0)
            cpu.dt -= 1;

        // Capture the pressed keys
        cpu.get_pressed_keys();

        // Execute the next instruction. We can get two types of errors here:
        // The ones that comes from decoding the next instruction and founding
        // an unrecognized operation code, and the ones that comes from a
        // particular instruction.
        cpu.exec_next_cicle() catch |err| {
            // Se if it was a unrecognized opcode
            if (@TypeOf(err) == FetchError) {
                var opcode: u16 = @intCast(cpu.ram[cpu.pc]);
                opcode <<= 8;
                opcode |= @intCast(cpu.ram[cpu.pc + 1]);
                std.debug.print("Error: Invalid opcode detected: {}", .{opcode});
            }
            // The error was caused by an instruction
            else if (@TypeOf(err) == ExecError) {
                switch (err) {
                    ExecError.StackOverflow => {
                        // When doing a lot of subroutine calls, we can get a
                        // stack overflow error, meaning that we can save another
                        // program counter into the stack.
                        std.debug.print("Error: A call caused stack overflow.");
                    },
                    ExecError.JumpOutOfBounds => {
                        // We can get this error if we try to point out of the
                        // chip8's last legal address (0xfff).
                        std.debug.print("Error: Jump out of memory (>0xfff)");
                    },
                    ExecError.ReturnWithEmptyStack => {
                        // You can't do a return with the stack pointer at 0
                        // (bottom of the stack), because there is no address
                        // to recover from the stack.
                        std.debug.print("Error: Return with stack pointer = 0");
                    },
                    ExecError.JumpToInterpreterMemory => {
                        // This error occurs if we try to access the chip8's
                        // interpreter reserved memory section (0x0-0x200-1)
                        std.debug.print("Error: Jump to interpreter memory (<0x200)");
                    },
                }
            }
            break; // End loop due to error
        };

        // Draw the screen
        raylib.beginDrawing();
        defer raylib.endDrawing();
        raylib.clearBackground(raylib.Color.black);

        for (0..2048) |it| {
            if (cpu.display[it] != 0) {
                raylib.drawRectangle(
                    @intCast(it % 64 * 30),
                    @intCast((it / 64) * 30),
                    30,
                    30,
                    raylib.Color.white,
                );
            }
        }
    }
}
