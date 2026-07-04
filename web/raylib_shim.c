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

// Textures. LoadTexture returns a Texture2D by value; write its 5 ints to an
// out buffer the bridge reads back. Draw/Unload reconstruct the struct from
// scalars the bridge unpacked out of c-flat memory.
void cf_load_texture(const char *path, int *out) {
    Texture2D t = LoadTexture(path);
    out[0] = (int)t.id;
    out[1] = t.width;
    out[2] = t.height;
    out[3] = t.mipmaps;
    out[4] = t.format;
}

void cf_unload_texture(unsigned int id, int width, int height, int mipmaps, int format) {
    Texture2D t = {id, width, height, mipmaps, format};
    UnloadTexture(t);
}

void cf_draw_texture(unsigned int id, int width, int height, int mipmaps, int format,
                     int x, int y,
                     unsigned char r, unsigned char g, unsigned char b, unsigned char a) {
    Texture2D t = {id, width, height, mipmaps, format};
    DrawTexture(t, x, y, (Color){r, g, b, a});
}
