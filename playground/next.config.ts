import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  turbopack: {},
  // Serve .wasm files from public/pkg with correct MIME type
  headers: async () => [
    {
      source: "/pkg/:path*",
      headers: [
        { key: "Content-Type", value: "application/wasm" },
      ],
    },
  ],
};

export default nextConfig;
