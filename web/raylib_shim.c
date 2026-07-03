#include "raylib.h"

void cf_init_window(int width, int height, const char *title) {
    InitWindow(width, height, title);
}

void cf_clear_background(unsigned char r, unsigned char g, unsigned char b, unsigned char a) {
    ClearBackground((Color){r, g, b, a});
}

void cf_draw_rectangle(int x, int y, int w, int h,
                       unsigned char r, unsigned char g, unsigned char b, unsigned char a) {
    DrawRectangle(x, y, w, h, (Color){r, g, b, a});
}

void cf_draw_text(const char *text, int x, int y, int font_size,
                  unsigned char r, unsigned char g, unsigned char b, unsigned char a) {
    DrawText(text, x, y, font_size, (Color){r, g, b, a});
}

int cf_check_collision_recs(float ax, float ay, float aw, float ah,
                            float bx, float by, float bw, float bh) {
    return CheckCollisionRecs((Rectangle){ax, ay, aw, ah}, (Rectangle){bx, by, bw, bh});
}
