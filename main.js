import * as THREE from 'three';
import * as PP from 'postprocessing';
import { N8AOPostPass } from 'n8ao';

import { Elm } from './src/Main.elm';
const app = Elm.Main.init();

let scene, camera, renderer, container, composer;

// Set Z as up
THREE.Object3D.DEFAULT_UP.set(0, 0, 1);

// Reuse Materials
const materials = {
    base: new THREE.MeshLambertMaterial({ color: 0xffffff }),
    stairs: new THREE.MeshLambertMaterial({ color: 0xffccaa }),
    bridge: new THREE.MeshLambertMaterial({ color: 0xcc6666 }),
    railing: new THREE.MeshLambertMaterial({ color: 0xcccccc }),
    player: new THREE.MeshLambertMaterial({ color: 0xddffff, emissive: 0xddffff, emissiveIntensity: 0.5 }),
    goal: new THREE.MeshLambertMaterial({ color: 0x222222 }),
    focus: new THREE.MeshLambertMaterial({ color: 0xffcc00, emissive: 0xffcc00, emissiveIntensity: 10 }),
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

const meshCache = new Map();

// Scene & Lights
scene = new THREE.Scene();
scene.background = new THREE.Color(0xaaddee);

const fillLeft = new THREE.PointLight(0xffcc99, 30);
fillLeft.position.set(-2, 0, 3);
scene.add(fillLeft);

const fillRight = new THREE.PointLight(0x66bbff, 15);
fillRight.position.set(0, -2, 3);
scene.add(fillRight);

const fillAbove = new THREE.PointLight(0xffffff, 40);
fillAbove.position.set(2, 2, 6);
scene.add(fillAbove);

const playerLight = new THREE.PointLight(0xffffff, 0.03);
scene.add(playerLight);


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
updateCamera();

// Renderer
function getDpr() {
    return Math.min(window.devicePixelRatio || 1, 2);
}
renderer = new THREE.WebGLRenderer({ antialias: false });
container.appendChild(renderer.domElement);

// Ambient Occlusion
const n8aoPass = new N8AOPostPass(scene, camera, container.clientWidth, container.clientHeight);
n8aoPass.configuration.aoRadius = 0.5;
n8aoPass.configuration.distanceFalloff = 1.5;
n8aoPass.configuration.intensity = 7.0;
n8aoPass.setQualityMode("Medium");

const bloomEffect = new PP.BloomEffect({
    intensity: 2,
    luminanceThreshold: 1,
    mipmapBlur: true,
});

// Postprocessing
composer = new PP.EffectComposer(renderer, { frameBufferType: THREE.HalfFloatType });
composer.addPass(new PP.RenderPass(scene, camera));
composer.addPass(n8aoPass);
composer.addPass(new PP.EffectPass(camera, bloomEffect));
if (getDpr() <= 1.2) {
    composer.addPass(new PP.EffectPass(camera, new PP.SMAAEffect({ preset: PP.SMAAPreset.LOW })));
}

function updateSize() {
    renderer.setPixelRatio(getDpr());
    renderer.setSize(container.clientWidth, container.clientHeight);
    composer.setSize(container.clientWidth, container.clientHeight);
}
updateSize();

let frameCount = 0;
let fpsStartTime = performance.now();
window.currentActualFPS = 0;

function measureFPS() {
    frameCount++;
    const now = performance.now();
    if (now - fpsStartTime >= 1000) {
        window.currentActualFPS = frameCount;
        frameCount = 0;
        fpsStartTime = now;
    }
}

window.addEventListener('resize', () => {
    updateSize();
    updateCamera();
});

window.addEventListener('keydown', (e) => {
    if (['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight', ' '].includes(e.key)) {
        e.preventDefault();
    }
}, { passive: false });

let rafId = null;
app.ports.renderThreeJS.subscribe(data => {
    updateScene(data);
    if (!rafId) {
        rafId = requestAnimationFrame(() => {
            composer.render();
            measureFPS();
            rafId = null;
        });
    }
});

function updateScene(data) {
    const unitScale = 0.01;
    const currentMeshKeys = new Set();

    // Boxes
    data.boxes.forEach((b) => {
        const key = `box_${b.x}_${b.y}_${b.z}_${b.sizeX}_${b.sizeY}_${b.sizeZ}_${b.material}_${b.rotationZ || 0}`;
        currentMeshKeys.add(key);

        if (!meshCache.has(key)) {
            const geo = getBoxGeometry(
                b.sizeX * unitScale,
                b.sizeY * unitScale,
                b.sizeZ * unitScale,
            );
            const mesh = new THREE.Mesh(geo, materials[b.material]);
            mesh.position.set(
                b.x * unitScale,
                b.y * unitScale,
                b.z * unitScale,
            );
            if (b.rotationZ) {
                mesh.rotation.z = b.rotationZ * Math.PI / 180;
            }
            mesh.renderOrder = -(b.x + b.y); // no tears
            scene.add(mesh);
            meshCache.set(key, mesh);
        }
    });

    // Spheres
    data.spheres.forEach((s, i) => {
        // We use index because player spheres are always in the same order
        // and we want to be able to move them.
        const key = `sphere_${s.material}_${i}`;
        currentMeshKeys.add(key);

        let mesh = meshCache.get(key);
        if (!mesh) {
            const geo = getSphereGeometry(s.radius * unitScale);
            mesh = new THREE.Mesh(geo, materials[s.material]);
            scene.add(mesh);
            meshCache.set(key, mesh);
        } else {
            // Update geometry if radius changed (though it shouldn't for player)
            const geo = getSphereGeometry(s.radius * unitScale);
            if (mesh.geometry !== geo) {
                mesh.geometry = geo;
            }
        }

        mesh.position.set(s.x * unitScale, s.y * unitScale, s.z * unitScale);
    });

    // Player Light
    if (data.playerLight) {
        playerLight.position.set(
            data.playerLight.x * unitScale,
            data.playerLight.y * unitScale,
            data.playerLight.z * unitScale
        );
    }

    // Cleanup
    for (const [key, mesh] of meshCache.entries()) {
        if (!currentMeshKeys.has(key)) {
            scene.remove(mesh);
            meshCache.delete(key);
            // Geometries and materials are cached/reused, so don't dispose here
        }
    }


    // Camera
    lastFocalPoint.set(
        data.camera.focalPoint.x,
        data.camera.focalPoint.y,
        data.camera.focalPoint.z
    );
    camera.position.set(
        data.camera.position.x,
        data.camera.position.y,
        data.camera.position.z
    );
    camera.lookAt(lastFocalPoint);
    lastViewSize = data.camera.viewSize;

    updateCamera();
}
