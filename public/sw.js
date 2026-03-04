const CACHE_NAME = 'v7';
const ASSETS = ['/', '/manifest.json', '/favicon.svg', '/assets/index.js', '/assets/vendor.js'];
const seen = new Set();

self.addEventListener('install', e => e.waitUntil(caches.open(CACHE_NAME).then(c => c.addAll(ASSETS))));
self.addEventListener('activate', e => e.waitUntil(caches.keys().then(ks => Promise.all(ks.map(k => k != CACHE_NAME && caches.delete(k))))));

self.addEventListener('fetch', e => {
  const path = new URL(e.request.url).pathname;
  if (e.request.mode === 'navigate') seen.clear();
  const key = ASSETS.includes(path) ? path : (e.request.mode === 'navigate' ? '/' : null);
  if (!key) return;

  e.respondWith(caches.match(key).then(cached => {
    if (seen.has(key) && cached) return cached;
    seen.add(key);
    const net = fetch(key === '/' ? '/' : e.request, { cache: 'reload' }).then(r => {
      if (r.ok) caches.open(CACHE_NAME).then(c => c.put(key, r.clone()));
      return r;
    });
    return Promise.race([net, new Promise((_, rej) => setTimeout(rej, 3000))]).catch(() => cached || net);
  }));
});
