const CACHE_NAME = 'v7';
const ASSETS_TO_CACHE = [
    '/',
    '/manifest.json',
    '/favicon.svg',
    '/assets/index.js',
    '/assets/vendor.js',
];

// Track which assets have been checked for updates in the current session
const checkedInSession = new Set();

self.addEventListener('install', (event) => {
    event.waitUntil(
        caches.open(CACHE_NAME).then((cache) => cache.addAll(ASSETS_TO_CACHE))
    );
});

self.addEventListener('activate', (event) => {
    event.waitUntil(
        caches.keys().then((keys) => Promise.all(
            keys.map((k) => k !== CACHE_NAME && caches.delete(k))
        ))
    );
});

self.addEventListener('fetch', (event) => {
    const url = new URL(event.request.url);
    const path = url.pathname;

    // Reset session checks on every new page navigation
    if (event.request.mode === 'navigate') {
        checkedInSession.clear();
    }

    // Determine the cache key: fall back to '/' for navigation requests
    const isAsset = ASSETS_TO_CACHE.includes(path);
    const cacheKey = isAsset ? path : (event.request.mode === 'navigate' ? '/' : null);

    if (!cacheKey) return;

    event.respondWith(handleRequest(cacheKey, event.request));
});

async function handleRequest(cacheKey, request) {
    const cachedResponse = await caches.match(cacheKey);

    // If we've already checked this asset in this session, return from cache immediately
    if (checkedInSession.has(cacheKey) && cachedResponse) {
        return cachedResponse;
    }

    checkedInSession.add(cacheKey);

    // Try a network-first fetch with a 3-second timeout
    const fetchPromise = fetchAndRefreshCache(cacheKey, request);
    const timeoutPromise = new Promise((_, reject) => setTimeout(reject, 3000));

    try {
        const networkResponse = await Promise.race([fetchPromise, timeoutPromise]);
        if (networkResponse && networkResponse.ok) {
            return networkResponse;
        }
    } catch (e) {
        // Fallback to cache on timeout or network error
    }

    // If network failed or timed out, use the cached version or wait for the network to finish
    return cachedResponse || fetchPromise;
}

async function fetchAndRefreshCache(cacheKey, request) {
    try {
        // Use cache: 'reload' to bypass intermediate caches
        const response = await fetch(request, { cache: 'reload' });
        if (response.ok) {
            const cache = await caches.open(CACHE_NAME);
            cache.put(cacheKey, response.clone());
        }
        return response;
    } catch (e) {
        return null;
    }
}
