const CACHE_NAME = 'v7';
const ASSETS_TO_CACHE = [
    '/',
    '/manifest.json',
    '/favicon.svg',
    '/assets/index.js',
    '/assets/vendor.js',
];

const checkedAssets = new Set();

self.addEventListener('install', (event) => {
    event.waitUntil(
        caches.open(CACHE_NAME).then((cache) => {
            return cache.addAll(ASSETS_TO_CACHE);
        })
    );
});

self.addEventListener('activate', (event) => {
    event.waitUntil(
        caches.keys().then((cacheNames) => Promise.all(
            cacheNames.map((key) => key !== CACHE_NAME && caches.delete(key))
        ))
    );
});

self.addEventListener('fetch', (event) => {
    const url = new URL(event.request.url);
    const path = url.pathname;

    // Navigation requests (HTML)
    if (event.request.mode === 'navigate') {
        checkedAssets.clear(); // Start of a new session/load
        event.respondWith(handleRequest('/', event.request));
        return;
    }

    // Known static assets
    if (ASSETS_TO_CACHE.includes(path)) {
        event.respondWith(handleRequest(path, event.request));
    }
});

async function handleRequest(cacheKey, request) {
    if (!checkedAssets.has(cacheKey)) {
        checkedAssets.add(cacheKey);
        try {
            const networkResponse = await Promise.race([
                fetchAndRefreshCache(cacheKey, request),
                timeout(3000),
            ]);
            if (networkResponse && networkResponse.ok) {
                return networkResponse;
            }
        } catch (e) {
            // Fallback to cache on timeout or network error
        }
    }

    const cachedResponse = await caches.match(cacheKey);
    return cachedResponse || fetch(request);
}

async function fetchAndRefreshCache(cacheKey, request) {
    try {
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

function timeout(ms) {
    return new Promise((_, reject) =>
        setTimeout(() => reject(new Error('Timeout')), ms)
    );
}
