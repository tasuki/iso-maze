from playwright.sync_api import Page, expect, sync_playwright
import time

def test_help_overlay_scrollable(page: Page):
    page.goto("http://localhost:8425/")

    # Click the lightbulb icon to open the help overlay
    # The lightbulb is the 3rd icon in the menu
    page.locator("#menu .icon").nth(2).click()

    # Wait for the overlay to be visible
    expect(page.locator(".modal-content")).to_be_visible()

    # Take a screenshot of the help overlay
    page.screenshot(path="help_overlay_scrollable.png")

    # Try clicking inside the content - it should NOT close
    page.locator(".modal-content").click()
    expect(page.locator(".modal-content")).to_be_visible()

    # Try clicking the backdrop - it SHOULD close
    # We click near the top left corner of the page where the backdrop should be
    page.mouse.click(10, 10)
    expect(page.locator(".modal-content")).not_to_be_visible()

if __name__ == "__main__":
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page()
        try:
            test_help_overlay_scrollable(page)
        finally:
            browser.close()
