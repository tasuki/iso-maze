import { defineConfig } from 'vite';
import { visualizer } from 'rollup-plugin-visualizer';
import elmPlugin from 'vite-plugin-elm';
import { threeMinifier } from '@yushijinhun/three-minifier-rollup';

const glslMinifier = () => {
    return {
        name: 'glsl-minifier',
        enforce: 'post',
        generateBundle(options, bundle) {
            for (const fileName in bundle) {
                const chunk = bundle[fileName];
                if (chunk.type === 'chunk' && fileName.endsWith('.js')) {
                    chunk.code = chunk.code.replace(/([\`\"\'])([\s\S]*?void\s+main[\s\S]*?)\1/g, (match, quote, content) => {
                        if (content.includes('gl_Position') || content.includes('gl_FragColor') || content.includes('varying') || content.includes('uniform')) {
                            let minified = content
                                .replace(/\/\*[\s\S]*?\*\//g, '') // Remove multi-line comments
                                .replace(/\/\/.*/g, '')          // Remove single-line comments
                                .split('\n')
                                .map(line => line.trim())
                                .filter(line => line.length > 0)
                                .join('\n');

                            const lines = minified.split('\n');
                            const processedLines = lines.map(line => {
                                if (line.startsWith('#')) return line;
                                return line.replace(/\s*([{}();,=+*/<>!&|])\s*/g, '$1');
                            });

                            // For final bundle, we should keep the newlines if it's a template literal or use \n if it's a string
                            // But actually, we can just join with a space for non-preprocessor lines to be safe
                            let result = '';
                            for (let i = 0; i < processedLines.length; i++) {
                                const line = processedLines[i];
                                if (line.startsWith('#')) {
                                    result += line + '\n';
                                } else {
                                    result += line;
                                }
                            }
                            return quote + result.trim() + quote;
                        }
                        return match;
                    });
                }
            }
        }
    };
};

export default defineConfig({
    base: '/',
    plugins: [
        threeMinifier(),
        glslMinifier(),
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
