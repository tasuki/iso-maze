import * as THREE from 'three';
import { RoundedBoxGeometry } from 'three/examples/jsm/geometries/RoundedBoxGeometry.js';
import * as PP from 'postprocessing';
import { N8AOPostPass } from 'n8ao';

import { Elm } from './src/Main.elm';
const app = Elm.Main.init({
    flags: {
        dpr: getDpr(),
        finishedLevels: JSON.parse(localStorage.getItem('finishedLevels') || '[]')
    }
});

let staticScene, dynamicScene, camera, renderer, container, dynamicComposer, staticComposer;
let backgroundScene, backgroundCamera, backgroundQuad;
let groundPlane;

// Z is up
THREE.Object3D.DEFAULT_UP.set(0, 0, 1);

// Caches
const materials = {
    base: new THREE.MeshStandardMaterial({ color: 0xffffff, roughness: 0.8, metalness: 0.2 }),
    stairs: new THREE.MeshStandardMaterial({ color: 0xffccaa, roughness: 0.8, metalness: 0.2 }),
    bridge: new THREE.MeshStandardMaterial({ color: 0xcc6666, roughness: 0.8, metalness: 0.2 }),
    player: new THREE.MeshStandardMaterial({ color: 0x66ffff, emissive: 0xbbdddd, emissiveIntensity: 1, roughness: 0.5, metalness: 0.5 }),
    goal: new THREE.MeshStandardMaterial({ color: 0x555555, roughness: 0.8, metalness: 0.2 }),
    focus: new THREE.MeshStandardMaterial({ color: 0xffcc00, emissive: 0xffcc00, emissiveIntensity: 4, roughness: 0.5, metalness: 0.5 }),
    occlusion: new THREE.MeshBasicMaterial({ colorWrite: false }),
};

const geometryCache = new Map();
function getBoxGeometry(sx, sy, sz) {
    const key = `box_${sx}_${sy}_${sz}`;
    if (!geometryCache.has(key)) {
        const radius = Math.min(sx, sy, sz) * 0.1;
        geometryCache.set(key, new RoundedBoxGeometry(sx, sy, sz, 4, radius));
    }
    return geometryCache.get(key);
}

function getSphereGeometry(r) {
    const key = `sphere_${r}`;
    if (!geometryCache.has(key)) {
        geometryCache.set(key, new THREE.SphereGeometry(r, 16, 16));
    }
    return geometryCache.get(key);
}

const staticMeshCache = new Map();
const dynamicMeshCache = new Map();

// Scenes & Lights
function createLights() {
    return {
        left: new THREE.PointLight(0xffcc99, 30),
        right: new THREE.PointLight(0x66bbff, 15),
        above: new THREE.PointLight(0xffffff, 40),
    };
}

function addLightsToScene(s, ls) {
    ls.left.position.set(-2, 0, 3);
    s.add(ls.left);
    ls.right.position.set(0, -2, 3);
    s.add(ls.right);
    ls.above.position.set(2, 2, 6);
    s.add(ls.above);
}

function parseHex(hex) {
    if (hex.length === 3) {
        return new THREE.Color('#' + hex[0] + hex[0] + hex[1] + hex[1] + hex[2] + hex[2]);
    }
    return new THREE.Color('#' + hex);
}

staticScene = new THREE.Scene();
const defaultBg = parseHex('689');
staticScene.background = defaultBg;
const staticLights = createLights();
addLightsToScene(staticScene, staticLights);

groundPlane = new THREE.Mesh(
    new THREE.PlaneGeometry(200, 200),
    new THREE.MeshStandardMaterial({ color: defaultBg, roughness: 1, metalness: 0 })
);
// Z is up in this project, and PlaneGeometry is in the XY plane by default, so it's already a floor.
groundPlane.position.z = -0.15;
staticScene.add(groundPlane);

dynamicScene = new THREE.Scene();
const dynamicLights = createLights();
addLightsToScene(dynamicScene, dynamicLights);

// Background scene
backgroundScene = new THREE.Scene();
backgroundCamera = new THREE.OrthographicCamera(-1, 1, 1, -1, 2, 20);
backgroundCamera.position.z = 5;
backgroundQuad = new THREE.Mesh(
    new THREE.PlaneGeometry(2, 2),
    new THREE.MeshBasicMaterial({ color: 0xffffff, depthTest: false, depthWrite: false })
);
backgroundScene.add(backgroundQuad);

// Camera
let lastViewSize = 1.4;
let lastFocalPoint = new THREE.Vector3(0, 0, 0.55);

function updateCamera() {
    const aspect = container.clientWidth / container.clientHeight;
    camera.left = -aspect * lastViewSize / 2;
    camera.right = aspect * lastViewSize / 2;
    camera.top = lastViewSize / 2;
    camera.bottom = -lastViewSize / 2;
    camera.updateProjectionMatrix();
}
container = document.getElementById('three-container');
camera = new THREE.OrthographicCamera(0, 0, 0, 0, 2, 20);
camera.up.set(0, 0, 1);

// Renderer
function getDpr() {
    return Math.min(window.devicePixelRatio || 1, 2);
}
renderer = new THREE.WebGLRenderer({ antialias: false });
renderer.autoClear = false;
container.appendChild(renderer.domElement);

// Static pass
const n8aoPass = new N8AOPostPass(
    staticScene, camera, container.clientWidth, container.clientHeight
);
n8aoPass.configuration.aoRadius = 0.5;
n8aoPass.configuration.distanceFalloff = 1.5;
n8aoPass.configuration.intensity = 7.0;
n8aoPass.setQualityMode("High");

staticComposer = new PP.EffectComposer(renderer, { frameBufferType: THREE.HalfFloatType });
staticComposer.autoRenderToScreen = false;
staticComposer.addPass(new PP.RenderPass(staticScene, camera));
staticComposer.addPass(n8aoPass);
staticComposer.addPass(new PP.EffectPass(camera, new PP.SMAAEffect({
    preset: PP.SMAAPreset.HIGH,
})));

// Dynamic pass
const dynamicRenderPass = new PP.RenderPass(dynamicScene, camera);
dynamicRenderPass.clear = false;
const bloomEffectPass = new PP.EffectPass(camera, new PP.BloomEffect({
    intensity: 5,
    luminanceThreshold: 1,
    mipmapBlur: true,
}));

dynamicComposer = new PP.EffectComposer(renderer, { frameBufferType: THREE.HalfFloatType });
dynamicComposer.addPass(new PP.RenderPass(backgroundScene, backgroundCamera));
dynamicComposer.addPass(dynamicRenderPass);
dynamicComposer.addPass(bloomEffectPass);
dynamicComposer.addPass(new PP.EffectPass(camera, new PP.SMAAEffect({
    preset: PP.SMAAPreset.LOW,
})));

// Updates and Listeners
function updateSize() {
    const w = container.clientWidth;
    const h = container.clientHeight;
    renderer.setPixelRatio(getDpr());
    renderer.setSize(w, h);
    dynamicComposer.setSize(w, h);
    staticComposer.setSize(w, h);
}
updateSize();

window.addEventListener('resize', () => {
    updateSize();
    updateCamera();
    app.ports.updateDpr.send(getDpr());
});

window.addEventListener('keydown', (e) => {
    if (['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight', ' '].includes(e.key)) {
        e.preventDefault();
    }
});

let rafId = null;
let latestData = null;
let lastRenderTimeReport = 0;
let needsStaticRender = true;
let pendingStatic = null;

app.ports.saveFinishedLevels.subscribe(levels => {
    localStorage.setItem('finishedLevels', JSON.stringify(levels));
});

app.ports.renderThreeJS.subscribe(data => {
    const unitScale = 0.01;
    latestData = data;
    if (data.staticUpdate && data.static) {
        needsStaticRender = true;
        pendingStatic = data.static;
    }

    if (data.staticUpdate) {
        const c = data.config;
        [staticLights, dynamicLights].forEach(ls => {
            ls.left.color.copy(parseHex(c.left.color));
            ls.left.intensity = c.left.intensity;
            ls.left.position.set(c.left.position.x * unitScale, c.left.position.y * unitScale, c.left.position.z * unitScale);

            ls.right.color.copy(parseHex(c.right.color));
            ls.right.intensity = c.right.intensity;
            ls.right.position.set(c.right.position.x * unitScale, c.right.position.y * unitScale, c.right.position.z * unitScale);

            ls.above.color.copy(parseHex(c.above.color));
            ls.above.intensity = c.above.intensity;
            ls.above.position.set(c.above.position.x * unitScale, c.above.position.y * unitScale, c.above.position.z * unitScale);
        });
        const bgColor = parseHex(c.bg);
        staticScene.background.copy(bgColor);
        groundPlane.material.color.copy(bgColor);
    }


    if (!rafId) {
        rafId = requestAnimationFrame(() => {
            const t0 = performance.now();
            updateScene(latestData, unitScale);

            if (needsStaticRender) {
                staticComposer.render();
                // weird parity of passes, watch out for inputBuffer vs outputBuffer
                // llms say set needsSwap, but no that doesn't help
                backgroundQuad.material.map = staticComposer.inputBuffer.texture;
                backgroundQuad.material.needsUpdate = true;
                needsStaticRender = false;
                pendingStatic = null;
            }

            renderer.setRenderTarget(null);
            dynamicComposer.render();

            const t1 = performance.now();
            if (t1 - lastRenderTimeReport > 1000) {
                app.ports.updateRenderTime.send(t1 - t0);
                lastRenderTimeReport = t1;
            }
            rafId = null;
        });
    }
});

function getRenderableKey(r, prefix) {
    if (r.type === 'box') {
        return `${prefix}_box_${r.x}_${r.y}_${r.z}_${r.sizeX}_${r.sizeY}_${r.sizeZ}_${r.material}_${r.rotationZ || 0}`;
    } else {
        return `${prefix}_sphere_${r.x}_${r.y}_${r.z}_${r.radius}_${r.material}`;
    }
}

function createMesh(r, unitScale, useOcclusion = false) {
    let geo;
    if (r.type === 'box') {
        geo = getBoxGeometry(r.sizeX * unitScale, r.sizeY * unitScale, r.sizeZ * unitScale);
    } else {
        geo = getSphereGeometry(r.radius * unitScale);
    }
    const mat = useOcclusion ? materials.occlusion : materials[r.material];
    const mesh = new THREE.Mesh(geo, mat);
    updateMesh(mesh, r, unitScale);
    return mesh;
}

function updateMesh(mesh, r, unitScale) {
    let geo;
    if (r.type === 'box') {
        geo = getBoxGeometry(r.sizeX * unitScale, r.sizeY * unitScale, r.sizeZ * unitScale);
        mesh.rotation.z = (r.rotationZ || 0) * Math.PI / 180;
    } else {
        geo = getSphereGeometry(r.radius * unitScale);
    }
    if (mesh.geometry !== geo) mesh.geometry = geo;
    mesh.position.set(r.x * unitScale, r.y * unitScale, r.z * unitScale);

    // correct render order, render occlusion first
    mesh.renderOrder = r.z - r.x - r.y;
    if (mesh.material === materials.occlusion) {
        mesh.renderOrder -= 10000;
    }
}

function updateScene(data, unitScale) {
    const staticToUse = pendingStatic || (data.staticUpdate ? data.static : null);
    if (staticToUse) {
        const currentStaticKeys = new Set();
        staticToUse.forEach((r) => {
            const key = getRenderableKey(r, 'static');
            currentStaticKeys.add(key);

            if (!staticMeshCache.has(key)) {
                const mesh = createMesh(r, unitScale);
                staticScene.add(mesh);
                staticMeshCache.set(key, mesh);

                const occMesh = createMesh(r, unitScale, true);
                dynamicScene.add(occMesh);
                dynamicMeshCache.set(key, occMesh);
            }
        });

        for (const [key, mesh] of staticMeshCache.entries()) {
            if (!currentStaticKeys.has(key)) {
                staticScene.remove(mesh);
                staticMeshCache.delete(key);
                const occMesh = dynamicMeshCache.get(key);
                if (occMesh) {
                    dynamicScene.remove(occMesh);
                    dynamicMeshCache.delete(key);
                }
            }
        }
    }

    const currentDynamicKeys = new Set();
    data.dynamic.forEach((r, i) => {
        const key = `dyn_${r.type}_${r.material}_${i}`;
        currentDynamicKeys.add(key);

        let mesh = dynamicMeshCache.get(key);
        if (!mesh) {
            mesh = createMesh(r, unitScale);
            dynamicScene.add(mesh);
            dynamicMeshCache.set(key, mesh);
        } else {
            updateMesh(mesh, r, unitScale);
        }
    });

    for (const [key, mesh] of dynamicMeshCache.entries()) {
        if (key.startsWith('dyn_') && !currentDynamicKeys.has(key)) {
            dynamicScene.remove(mesh);
            dynamicMeshCache.delete(key);
        }
    }

    lastFocalPoint.set(data.camera.focalPoint.x, data.camera.focalPoint.y, data.camera.focalPoint.z);
    camera.position.set(data.camera.position.x, data.camera.position.y, data.camera.position.z);
    camera.lookAt(lastFocalPoint);
    lastViewSize = data.camera.viewSize;

    updateCamera();
}
