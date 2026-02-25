import * as THREE from 'three';
import * as PP from 'postprocessing';
import { N8AOPostPass } from 'n8ao';

import { Elm } from './src/Main.elm';
const app = Elm.Main.init({
    flags: getDpr()
});

let staticScene, dynamicScene, camera, renderer, container, dynamicComposer, staticComposer;
let backgroundScene, backgroundCamera, backgroundQuad;

// Z is up
THREE.Object3D.DEFAULT_UP.set(0, 0, 1);

// Caches
const materials = {
    base: new THREE.MeshLambertMaterial({ color: 0xffffff }),
    stairs: new THREE.MeshLambertMaterial({ color: 0xffccaa }),
    bridge: new THREE.MeshLambertMaterial({ color: 0xcc6666 }),
    player: new THREE.MeshLambertMaterial({ color: 0x66ffff, emissive: 0xbbdddd, emissiveIntensity: 1 }),
    goal: new THREE.MeshLambertMaterial({ color: 0x222222 }),
    focus: new THREE.MeshLambertMaterial({ color: 0xffcc00, emissive: 0xffcc00, emissiveIntensity: 4 }),
    occlusion: new THREE.MeshBasicMaterial({ colorWrite: false }),
};

const geometryCache = new Map();
function getBoxGeometry(sx, sy, sz) {
    const key = `box_${sx}_${sy}_${sz}`;
    if (!geometryCache.has(key)) {
        geometryCache.set(key, new THREE.BoxGeometry(sx, sy, sz));
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
function addLights(s) {
    const fillLeft = new THREE.PointLight(0xffcc99, 30);
    fillLeft.position.set(-2, 0, 3);
    s.add(fillLeft);

    const fillRight = new THREE.PointLight(0x66bbff, 15);
    fillRight.position.set(0, -2, 3);
    s.add(fillRight);

    const fillAbove = new THREE.PointLight(0xffffff, 40);
    fillAbove.position.set(2, 2, 6);
    s.add(fillAbove);
}

staticScene = new THREE.Scene();
staticScene.background = new THREE.Color(0x668899);
addLights(staticScene);
dynamicScene = new THREE.Scene();
addLights(dynamicScene);

// Background scene
backgroundScene = new THREE.Scene();
backgroundCamera = new THREE.OrthographicCamera(-1, 1, 1, -1, 0, 10);
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
camera = new THREE.OrthographicCamera(0, 0, 0, 0, 1, 5);
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

staticComposer = new PP.EffectComposer(renderer);
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

app.ports.renderThreeJS.subscribe(data => {
    latestData = data;
    if (data.staticUpdate && data.static) {
        needsStaticRender = true;
        pendingStatic = data.static;
    }

    if (!rafId) {
        rafId = requestAnimationFrame(() => {
            const t0 = performance.now();
            updateScene(latestData);

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

    // Sorting: z - x - y is back-to-front for Azimuth -135
    mesh.renderOrder = r.z - r.x - r.y;
    if (mesh.material === materials.occlusion) {
        mesh.renderOrder -= 10000;
    } else if (mesh.material === materials.player || mesh.material === materials.goal || mesh.material === materials.focus) {
        mesh.renderOrder += 10000;
    }
}

function updateScene(data) {
    const unitScale = 0.01;

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
