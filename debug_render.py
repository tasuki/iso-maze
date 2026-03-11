import asyncio
from playwright.async_api import async_playwright

async def run():
    async with async_playwright() as p:
        browser = await p.chromium.launch()
        page = await browser.new_page()

        page.on("console", lambda msg: print(f"CONSOLE: {msg.type}: {msg.text}"))
        page.on("pageerror", lambda exc: print(f"PAGE ERROR: {exc}"))

        try:
            # Wait for the dev server to be ready
            await page.goto("http://localhost:8423", wait_until="networkidle")
            # Wait a bit for rendering
            await asyncio.sleep(5)
            await page.screenshot(path="/home/jules/verification/debug_screenshot.png")
            print("Screenshot taken")
        except Exception as e:
            print(f"Error: {e}")
        finally:
            await browser.close()

if __name__ == "__main__":
    asyncio.run(run())
