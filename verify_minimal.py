from playwright.sync_api import sync_playwright

def verify_minimal():
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page()
        try:
            page.goto("http://localhost:8423", wait_until="networkidle")
            page.wait_for_selector("canvas")
            page.wait_for_timeout(3000)
            page.screenshot(path="verification_minimal.png")
            print("Screenshot saved to verification_minimal.png")
        except Exception as e:
            print(f"Error: {e}")
        finally:
            browser.close()

if __name__ == "__main__":
    verify_minimal()
