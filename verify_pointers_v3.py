import asyncio
from playwright.async_api import async_playwright
import os

async def run():
    async with async_playwright() as p:
        browser = await p.chromium.launch()
        # Use a mobile-like viewport to test touch-action if needed,
        # but here we are testing pointer events which should work on desktop too.
        context = await browser.new_context(
            viewport={'width': 800, 'height': 600},
            has_touch=True
        )
        page = await context.new_page()

        # Navigate to the app
        await page.goto("http://localhost:8423")

        # Wait for the container to be present
        await page.wait_for_selector("#three-container")
        await asyncio.sleep(1) # wait for everything to settle

        await page.screenshot(path="01_initial.png")
        print("Initial screenshot saved.")

        # Test Running mode movement
        # Click in the top-left quadrant should move NW (if it follows the logic)
        # movePlayer logic:
        # up = dc.y * 2 < height (True for y=100 in 600 height)
        # left = dc.x * 2 < width (True for x=100 in 800 width)
        # NW = up and left
        await page.mouse.move(100, 100)
        await page.mouse.down()
        await page.mouse.up()
        await asyncio.sleep(0.5)
        await page.screenshot(path="02_after_click_nw.png")
        print("Screenshot after NW click saved.")

        # Toggle to Edit mode using 'e' key
        await page.keyboard.press("e")
        await asyncio.sleep(0.5)
        print("Toggled to Edit mode.")

        # Test Orbiting in Edit mode
        # Start drag at center
        await page.mouse.move(400, 300)
        await page.mouse.down()
        # Drag to the right
        await page.mouse.move(600, 300, steps=10)
        await page.mouse.up()
        await asyncio.sleep(0.5)
        await page.screenshot(path="03_after_orbit.png")
        print("Screenshot after orbit saved.")

        # Test touch events (emulated as pointers)
        # Playwright's tap/touch should trigger pointer events if configured
        await page.touchscreen.tap(100, 100)
        await asyncio.sleep(0.5)
        await page.screenshot(path="04_after_touch.png")
        print("Screenshot after touch saved.")

        await browser.close()

if __name__ == "__main__":
    asyncio.run(run())
