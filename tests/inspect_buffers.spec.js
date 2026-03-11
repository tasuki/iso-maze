
import { test, expect } from '@playwright/test';

test('inspect buffers', async ({ page }) => {
    await page.goto('http://localhost:8423');
    await page.waitForTimeout(2000);

    const result = await page.evaluate(() => {
        const renderer = window.staticComposer.getRenderer();
        const target = renderer.getRenderTarget();
        const readBuffer = window.staticComposer.readBuffer;
        const writeBuffer = window.staticComposer.writeBuffer;

        return {
            hasTarget: !!target,
            targetTexture: target ? !!target.texture : false,
            readBufferTexture: !!readBuffer.texture,
            writeBufferTexture: !!writeBuffer.texture,
            targetIsRead: target === readBuffer,
            targetIsWrite: target === writeBuffer
        };
    });

    console.log('Buffer Inspection:', JSON.stringify(result, null, 2));
});
