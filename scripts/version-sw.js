const fs = require('fs');
const path = require('path');

const swPath = path.join(__dirname, '..', 'dist', 'sw.js');
if (!fs.existsSync(swPath)) {
    console.error(`Error: ${swPath} not found. Make sure to run 'vite build' first.`);
    process.exit(1);
}

const content = fs.readFileSync(swPath, 'utf8');

const timestamp = new Date().toISOString().replace(/[-:T]/g, '').split('.')[0];
const newCacheName = `v_${timestamp}`;

const updatedContent = content.replace(/const CACHE_NAME = '[^']+';/, `const CACHE_NAME = '${newCacheName}';`);

fs.writeFileSync(swPath, updatedContent);
console.log(`Updated CACHE_NAME to ${newCacheName} in dist/sw.js`);
