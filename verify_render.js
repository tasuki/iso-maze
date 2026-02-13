const { chromium } = require('playwright');
(async () => {
  const browser = await chromium.launch();
  const page = await browser.newPage();
  await page.setViewportSize({ width: 1280, height: 720 });
  await page.goto('http://localhost:8423');
  await page.waitForTimeout(2000); // Wait for Elm and Three.js to initialize
  await page.screenshot({ path: 'simplified_render_v2.png' });
  await browser.close();
})();
