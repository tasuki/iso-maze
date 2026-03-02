import { defineConfig } from 'vite';
import { visualizer } from 'rollup-plugin-visualizer';
import elmPlugin from 'vite-plugin-elm';

export default defineConfig({
    base: '/',
    plugins: [
        elmPlugin({ debug: false }),
        // visualizer({
        //     open: true,
        //     gzipSize: true,
        //     filename: "stats.html",
        // }),
    ],
    server: {
        port: 8423,
        // hmr: false, // breaks elm reload
    },
});
