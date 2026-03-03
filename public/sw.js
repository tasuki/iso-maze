const CACHE_NAME = 'v4';
const ASSETS_TO_CACHE = [
    '/',
    '/manifest.json',
    '/favicon.svg',
    '/assets/index.js',
    '/assets/vendor.js',
];

self.addEventListener('install', (event) => {
    self.skipWaiting();
    event.waitUntil(
        caches.open(CACHE_NAME).then((cache) => {
            return cache.addAll(ASSETS_TO_CACHE);
        })
    );
});

self.addEventListener('activate', (event) => {
    self.clients.claim();
    event.waitUntil(
        caches.keys().then((cacheNames) => Promise.all(
            cacheNames.map((key) => key !== CACHE_NAME && caches.delete(key))
        ))
    );
});

self.addEventListener('fetch', (event) => {
    const response = event.request.mode === 'navigate'
        ? caches.match('/')
        : caches.match(event.request).then(res => res || fetch(event.request));

    event.respondWith(response);
});
