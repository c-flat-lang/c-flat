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

void cf_draw_poly(
    // Vector2 center
    float x, float y,
    // Normal Args
    int sides, float radius, float rotation,
    // Color color
    unsigned char r, unsigned char g, unsigned char b, unsigned char a
) {
    Vector2 center = {x, y};
    Color color = {r, g, b, a};
    DrawPoly(center, sides, radius, rotation, color);
}

void cf_draw_poly_lines(
    // Vector2 center
    float x, float y,
    // Normal Args
    int sides, float radius, float rotation,
    // Color color
    unsigned char r, unsigned char g, unsigned char b, unsigned char a
) {
    Vector2 center = {x, y};
    Color color = {r, g, b, a};
    DrawPolyLines(center, sides, radius, rotation, color);
}

void cf_draw_poly_lines_ex(
    // Vector2 center
    float x, float y,
    // Normal Args
    int sides, float radius, float rotation, float lineThick,
    // Color color
    unsigned char r, unsigned char g, unsigned char b, unsigned char a
) {
    Vector2 center = {x, y};
    Color color = {r, g, b, a};
    DrawPolyLinesEx(center, sides, radius, rotation, lineThick, color);
}

#define CF_MAX_SOUNDS 256
static Sound cf_sounds[CF_MAX_SOUNDS];
static int cf_sound_count = 0;

static int cf_sound_valid_handle(int h) {
    return h >= 0 && h < cf_sound_count;
}

int cf_load_sound(const char *path) {
    if (cf_sound_count >= CF_MAX_SOUNDS) return -1;
    Sound s = LoadSound(path);
    int h = cf_sound_count++;
    cf_sounds[h] = s;
    return h;
}

void cf_play_sound(int h) {
    if (cf_sound_valid_handle(h)) PlaySound(cf_sounds[h]);
}

void cf_stop_sound(int h) {
    if (cf_sound_valid_handle(h)) StopSound(cf_sounds[h]);
}

void cf_pause_sound(int h) {
    if (cf_sound_valid_handle(h)) PauseSound(cf_sounds[h]);
}

void cf_resume_sound(int h) {
    if (cf_sound_valid_handle(h)) ResumeSound(cf_sounds[h]);
}

int cf_is_sound_playing(int h) {
    return cf_sound_valid_handle(h) ? IsSoundPlaying(cf_sounds[h]) : 0;
}

void cf_set_sound_volume(int h, float volume) {
    if (cf_sound_valid_handle(h)) SetSoundVolume(cf_sounds[h], volume);
}

void cf_set_sound_pitch(int h, float pitch) {
    if (cf_sound_valid_handle(h)) SetSoundPitch(cf_sounds[h], pitch);
}

void cf_set_sound_pan(int h, float pan) {
    if (cf_sound_valid_handle(h)) SetSoundPan(cf_sounds[h], pan);
}

void cf_unload_sound(int h) {
    if (cf_sound_valid_handle(h)) UnloadSound(cf_sounds[h]);
}

int cf_is_sound_valid(int h) {
    return cf_sound_valid_handle(h) ? IsSoundValid(cf_sounds[h]) : 0;
}
