#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/types.h>

void write_bool(bool x) { printf("%d", x); }

void write_char(unsigned char x) { printf("%c", x); }

void write_u8(uint8_t x) { printf("%u", x); }

void write_s8(int8_t x) { printf("%u", x); }

void write_u16(uint16_t x) { printf("%d", x); }

void write_s16(int16_t x) { printf("%d", x); }

void write_s32(int32_t x) { printf("%d", x); }

void write_u32(uint32_t x) { printf("%d", x); }

void write_s64(int64_t x) { printf("%ld", x); }

void write_u64(uint64_t x) { printf("%lu", x); }

void write_usize(size_t x) { printf("%ld", x); }

void write_ssize(size_t x) { printf("%lu", x); }

void write_f32(float x) { printf("%f", x); }

void write_f64(double x) { printf("%f", x); }
