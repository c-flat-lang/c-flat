# ---- Config ----
TARGET      := x86_64-linux
SRC         := src/main.cb
BUILD_DIR   := bin
OUT         := $(BUILD_DIR)/main

# ---- Default ----
all: $(OUT)

# ---- Build ----
$(OUT): $(SRC) | $(BUILD_DIR)
	cflat --target=$(TARGET) \
	      $<

# Ensure build dir exists (order-only dependency)
$(BUILD_DIR):
	mkdir -p $@

# ---- Run ----
run: $(OUT)
	$<

# ---- Clean ----
clean:
	rm -rf $(BUILD_DIR)

.PHONY: all run clean
