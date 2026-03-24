const CACHE_NAME = 'v7';
const ASSETS_TO_CACHE = [
    '/',
    '/manifest.json',
    '/favicon.svg',
    '/style.css',
    '/assets/index.js',
    '/assets/vendor.js',
];

const checkedInSession = new Set();

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
    const request = ASSETS_TO_CACHE.includes(url.pathname) ? event.request : '/';

    if (!checkedInSession.has(request)) {
        checkedInSession.add(request);
        event.respondWith(
            Promise.race([
                fetchAndRefreshCache(request),
                timeout(3000),
            ]).catch(() => caches.match(request))
        );
    } else {
        event.respondWith(caches.match(request));
    }
});

async function fetchAndRefreshCache(request) {
    const response = await fetch(request);
    if (response.ok) {
        const cache = await caches.open(CACHE_NAME);
        cache.put(request, response.clone());
    }
    return response;
}

function timeout(ms) {
    return new Promise((_, reject) =>
        setTimeout(() => reject(new Error('Timeout')), ms)
    );
}
