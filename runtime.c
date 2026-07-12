#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/types.h>

void write_bool(bool x) { printf("%d", x); }

void write_char(unsigned char x) { printf("%c", x); }

void write_u8(uint8_t x) { printf("%u", x); }

void write_s32(int x) { printf("%d", x); }

void write_u32(uint32_t x) { printf("%d", x); }

void write_u16(uint16_t x) { printf("%d", x); }

void write_f32(float x) { printf("%f", x); }

void write_f64(double x) { printf("%f", x); }
