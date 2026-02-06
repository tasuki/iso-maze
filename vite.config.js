import { defineConfig } from 'vite';
import { viteSingleFile } from 'vite-plugin-singlefile'
import { visualizer } from 'rollup-plugin-visualizer';
import elmPlugin from 'vite-plugin-elm';

export default defineConfig({
    plugins: [
        elmPlugin(),
        viteSingleFile(),
        // visualizer({
        //     open: true,
        //     gzipSize: true,
        //     filename: "stats.html",
        // }),
    ],
    server: {
        port: 8423,
        hmr: false,
    },
});
