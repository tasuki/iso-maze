const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const ELM_FILE = 'src/Analyzer.elm';
const JS_FILE = 'analyzer-compiled.js';

try {
    execSync(`npx elm make ${ELM_FILE} --output=${JS_FILE} --optimize`, { stdio: ['ignore', 'ignore', 'inherit'] });
    const Elm = require('./' + JS_FILE);
    const app = Elm.Elm.Analyzer.init();

    app.ports.output.subscribe((csv) => {
        if (csv.startsWith('ERROR:')) {
            console.error(csv);
            if (fs.existsSync(JS_FILE)) fs.unlinkSync(JS_FILE);
            process.exit(1);
        }
        console.log(csv);
        if (fs.existsSync(JS_FILE)) fs.unlinkSync(JS_FILE);
        process.exit(0);
    });

    // short delay to ensure the subscription is ready
    setTimeout(() => {
        if (app.ports.input) {
            const mazeStr = process.argv[2] || null;
            app.ports.input.send(mazeStr);
        } else {
            console.error('Error: input port not found');
            process.exit(1);
        }
    }, 10);
} catch (error) {
    console.error('Error running analyzer:', error);
    if (fs.existsSync(JS_FILE)) fs.unlinkSync(JS_FILE);
    process.exit(1);
}
