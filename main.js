import * as THREE from 'three';
import * as PP from 'postprocessing';
import { N8AOPostPass } from 'n8ao';

import { Elm } from './src/Main.elm';
const app = Elm.Main.init({
    flags: getDpr()
});

let staticScene, dynamicScene, camera, renderer, container, composer, staticComposer;
let backgroundScene, backgroundCamera, backgroundQuad;

// Set Z as up
THREE.Object3D.DEFAULT_UP.set(0, 0, 1);

// Reuse Materials
const materials = {
    base: new THREE.MeshLambertMaterial({ color: 0xffffff }),
    stairs: new THREE.MeshLambertMaterial({ color: 0xffccaa }),
    bridge: new THREE.MeshLambertMaterial({ color: 0xcc6666 }),
    player: new THREE.MeshLambertMaterial({ color: 0xddffff, emissive: 0xddffff, emissiveIntensity: 0.7 }),
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
staticScene.background = new THREE.Color(0xaaddee);
addLights(staticScene);
dynamicScene = new THREE.Scene();
addLights(dynamicScene);

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

// Background Quad
backgroundScene = new THREE.Scene();
backgroundCamera = new THREE.OrthographicCamera(-1, 1, 1, -1, 0, 10);
backgroundCamera.position.z = 5;
backgroundQuad = new THREE.Mesh(
    new THREE.PlaneGeometry(2, 2),
    new THREE.MeshBasicMaterial({ color: 0xffffff, depthTest: false, depthWrite: false })
);
backgroundScene.add(backgroundQuad);

// Postprocessing
const staticRenderPass = new PP.RenderPass(staticScene, camera);
const staticAoPass = new N8AOPostPass(staticScene, camera, container.clientWidth, container.clientHeight);
staticAoPass.configuration.aoRadius = 0.5;
staticAoPass.configuration.distanceFalloff = 1.5;
staticAoPass.configuration.intensity = 7.0;
staticAoPass.setQualityMode("Medium");

staticComposer = new PP.EffectComposer(renderer, {
    frameBufferType: THREE.HalfFloatType
});
staticComposer.renderToScreen = false;
staticComposer.addPass(staticRenderPass);
staticComposer.addPass(staticAoPass);

// We'll update the map in the render loop to be safe

const backgroundPass = new PP.RenderPass(backgroundScene, backgroundCamera);
const dynamicRenderPass = new PP.RenderPass(dynamicScene, camera);
dynamicRenderPass.clear = false;
const bloomEffect = new PP.BloomEffect({
    intensity: 5,
    luminanceThreshold: 1,
    // luminanceSmoothing: 0.1,
    mipmapBlur: true,
});

composer = new PP.EffectComposer(renderer, {
    frameBufferType: THREE.HalfFloatType
});
composer.addPass(backgroundPass);
composer.addPass(dynamicRenderPass);
composer.addPass(new PP.EffectPass(camera, bloomEffect));

function updateSize() {
    const w = container.clientWidth;
    const h = container.clientHeight;
    renderer.setPixelRatio(getDpr());
    renderer.setSize(w, h);
    composer.setSize(w, h);
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
let pendingBoxes = null;

app.ports.renderThreeJS.subscribe(data => {
    latestData = data;
    if (data.staticUpdate && data.boxes) {
        needsStaticRender = true;
        pendingBoxes = data.boxes;
    }

    if (!rafId) {
        rafId = requestAnimationFrame(() => {
            const t0 = performance.now();
            updateScene(latestData);

            if (needsStaticRender) {
                staticComposer.render();
                // In postprocessing, the result is in inputBuffer after swap
                backgroundQuad.material.map = staticComposer.inputBuffer.texture;
                backgroundQuad.material.needsUpdate = true;
                needsStaticRender = false;
                pendingBoxes = null;
            }

            renderer.setRenderTarget(null);
            composer.render();

            const t1 = performance.now();
            if (t1 - lastRenderTimeReport > 1000) {
                app.ports.updateRenderTime.send(t1 - t0);
                lastRenderTimeReport = t1;
            }
            rafId = null;
        });
    }
});

function updateScene(data) {
    const unitScale = 0.01;

    const boxesToUse = pendingBoxes || (data.staticUpdate ? data.boxes : null);
    if (boxesToUse) {
        const currentStaticKeys = new Set();
        boxesToUse.forEach((b) => {
            const key = `box_${b.x}_${b.y}_${b.z}_${b.sizeX}_${b.sizeY}_${b.sizeZ}_${b.material}_${b.rotationZ || 0}`;
            currentStaticKeys.add(key);

            if (!staticMeshCache.has(key)) {
                const geo = getBoxGeometry(b.sizeX * unitScale, b.sizeY * unitScale, b.sizeZ * unitScale);
                const mesh = new THREE.Mesh(geo, materials[b.material]);
                mesh.position.set(b.x * unitScale, b.y * unitScale, b.z * unitScale);
                if (b.rotationZ) mesh.rotation.z = b.rotationZ * Math.PI / 180;
                mesh.renderOrder = -(b.x + b.y);
                staticScene.add(mesh);
                staticMeshCache.set(key, mesh);

                const occMesh = new THREE.Mesh(geo, materials.occlusion);
                occMesh.position.copy(mesh.position);
                occMesh.rotation.copy(mesh.rotation);
                occMesh.renderOrder = mesh.renderOrder;
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
    data.spheres.forEach((s, i) => {
        const key = `sphere_${s.material}_${i}`;
        currentDynamicKeys.add(key);

        let mesh = dynamicMeshCache.get(key);
        if (!mesh) {
            const geo = getSphereGeometry(s.radius * unitScale);
            mesh = new THREE.Mesh(geo, materials[s.material]);
            dynamicScene.add(mesh);
            dynamicMeshCache.set(key, mesh);
        } else {
            const geo = getSphereGeometry(s.radius * unitScale);
            if (mesh.geometry !== geo) mesh.geometry = geo;
        }
        mesh.position.set(s.x * unitScale, s.y * unitScale, s.z * unitScale);
    });

    for (const [key, mesh] of dynamicMeshCache.entries()) {
        if (!key.startsWith('box_') && !currentDynamicKeys.has(key)) {
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
