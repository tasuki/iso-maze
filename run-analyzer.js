const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const ELM_FILE = 'src/Analyzer.elm';
const JS_FILE = 'analyzer-compiled.js';

try {
    // Compile Elm to JS, suppressing STDOUT to keep the CSV clean
    execSync(`npx elm make ${ELM_FILE} --output=${JS_FILE} --optimize`, { stdio: ['ignore', 'ignore', 'inherit'] });

    // Load the compiled Elm
    const Elm = require('./' + JS_FILE);

    // Initialize the Elm worker
    const app = Elm.Elm.Analyzer.init();

    // Subscribe to the output port
    app.ports.output.subscribe((csv) => {
        console.log(csv);
        // Clean up and exit after receiving the result
        if (fs.existsSync(JS_FILE)) fs.unlinkSync(JS_FILE);
        process.exit(0);
    });

    // Trigger the analysis after a short delay to ensure the subscription is ready
    setTimeout(() => {
        if (app.ports.input) {
            app.ports.input.send(null);
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
