import * as THREE from 'three';
import * as PP from 'postprocessing';
import { N8AOPostPass } from 'n8ao';

import { Elm } from './src/Main.elm';
const app = Elm.Main.init({
    flags: {
        dpr: getDpr(),
        finishedLevels: JSON.parse(localStorage.getItem('finishedLevels') || '[]'),
        performance: localStorage.getItem('performance') || 'normal',
        leashEnabled: localStorage.getItem('leashEnabled') === 'true' ? true : false,
    }
});

let staticScene, dynamicScene, camera, renderer, container, dynamicComposer, staticComposer;
let backgroundScene, backgroundCamera, backgroundQuad;
let groundPlane;

// Z is up
THREE.Object3D.DEFAULT_UP.set(0, 0, 1);

// Caches
function createHaloTexture() {
    const size = 128;
    const canvas = document.createElement('canvas');
    canvas.width = size;
    canvas.height = size;
    const ctx = canvas.getContext('2d');
    const grad = ctx.createRadialGradient(size / 2, size / 2, 0, size / 2, size / 2, size / 2);
    grad.addColorStop(0, 'rgba(0, 204, 255, 0)');
    grad.addColorStop(0.8, 'rgba(0, 204, 255, 0.2)');
    grad.addColorStop(0.9, 'rgba(0, 204, 255, 0.4)');
    grad.addColorStop(1, 'rgba(0, 204, 255, 0)');
    ctx.fillStyle = grad;
    ctx.fillRect(0, 0, size, size);
    const texture = new THREE.CanvasTexture(canvas);
    return texture;
}

const materials = {
    base: new THREE.MeshStandardMaterial({ color: 0xffffff, roughness: 0.8, metalness: 0.2 }),
    stairs: new THREE.MeshStandardMaterial({ color: 0xffccaa, roughness: 0.8, metalness: 0.2 }),
    bridge: new THREE.MeshStandardMaterial({ color: 0xcc6666, roughness: 0.8, metalness: 0.2 }),
    greenery: new THREE.MeshStandardMaterial({ color: 0x669900, roughness: 0.6, metalness: 0.9 }),
    player: new THREE.MeshStandardMaterial({ color: 0x66ffff, emissive: 0xbbdddd, emissiveIntensity: 1.4, roughness: 0.5, metalness: 0.2 }),
    goal: new THREE.MeshStandardMaterial({ color: 0x555555, roughness: 0.8, metalness: 0.2 }),
    halo: new THREE.MeshBasicMaterial({ map: createHaloTexture(), transparent: true, depthTest: false, depthWrite: false, blending: THREE.AdditiveBlending }),
    focus: new THREE.MeshStandardMaterial({ color: 0xff9900, emissive: 0xff9900, emissiveIntensity: 3, roughness: 0.5, metalness: 0.5 }),
    occlusion: new THREE.MeshBasicMaterial({ colorWrite: false }),
    debugUnreachable: new THREE.MeshStandardMaterial({ color: 0x555555, roughness: 0.8, metalness: 0.2 }),
    debugOccluding: new THREE.MeshStandardMaterial({ color: 0xcc6666, roughness: 0.8, metalness: 0.2 }),
    debugHanging: new THREE.MeshStandardMaterial({ color: 0xffff00, roughness: 0.8, metalness: 0.2 }),
};

const geometryCache = new Map();
function getUnitBox() {
    if (!geometryCache.has('unit_box')) {
        geometryCache.set('unit_box', new THREE.BoxGeometry(1, 1, 1));
    }
    return geometryCache.get('unit_box');
}

function getUnitSphere() {
    if (!geometryCache.has('unit_sphere')) {
        geometryCache.set('unit_sphere', new THREE.SphereGeometry(1, 16, 16));
    }
    return geometryCache.get('unit_sphere');
}

function getUnitPlane() {
    if (!geometryCache.has('unit_plane')) {
        geometryCache.set('unit_plane', new THREE.PlaneGeometry(1, 1));
    }
    return geometryCache.get('unit_plane');
}

const MAX_INSTANCES = 5000;

class BatchManager {
    constructor(scene, materials, baseRenderOrder = 0) {
        this.scene = scene;
        this.materials = materials;
        this.baseRenderOrder = baseRenderOrder;
        this.batches = new Map(); // key: type_material
        this.tempMatrix = new THREE.Matrix4();
        this.tempPosition = new THREE.Vector3();
        this.tempQuaternion = new THREE.Quaternion();
        this.tempScale = new THREE.Vector3();
        this.tempEuler = new THREE.Euler();
        this.upVector = new THREE.Vector3(0, 0, 1);
    }

    getBatch(type, materialName) {
        const key = `${type}_${materialName}`;
        if (!this.batches.has(key)) {
            const material = this.materials[materialName];
            let geometry;
            if (type === 'box') geometry = getUnitBox();
            else if (type === 'sphere') geometry = getUnitSphere();
            else if (type === 'plane') geometry = getUnitPlane();

            const mesh = new THREE.InstancedMesh(geometry, material, MAX_INSTANCES);
            mesh.count = 0;
            mesh.renderOrder = this.baseRenderOrder;
            if (materialName === 'halo') mesh.renderOrder += 10;
            if (materialName === 'player') mesh.renderOrder += 20;
            if (materialName === 'goal') mesh.renderOrder += 20;

            mesh.instanceMatrix.setUsage(THREE.DynamicDrawUsage);
            this.scene.add(mesh);
            this.batches.set(key, mesh);
        }
        return this.batches.get(key);
    }

    reset() {
        for (const mesh of this.batches.values()) {
            mesh.count = 0;
        }
    }

    add(r) {
        const batch = this.getBatch(r.type, r.material);
        if (batch.count >= MAX_INSTANCES) return;

        this.tempPosition.set(r.x, r.y, r.z);

        if (r.type === 'box') {
            const rot = (r.rotationZ || 0) * Math.PI / 180;
            this.tempQuaternion.setFromAxisAngle(this.upVector, rot);
            this.tempScale.set(r.sizeX, r.sizeY, r.sizeZ);
        } else if (r.type === 'sphere') {
            this.tempQuaternion.set(0, 0, 0, 1);
            this.tempScale.set(r.radius, r.radius, r.radius);
        } else if (r.type === 'plane') {
            this.tempEuler.set(
                (r.rotationX || 0) * Math.PI / 180,
                (r.rotationY || 0) * Math.PI / 180,
                (r.rotationZ || 0) * Math.PI / 180
            );
            this.tempQuaternion.setFromEuler(this.tempEuler);
            this.tempScale.set(r.sizeX, r.sizeY, 1);
        }

        this.tempMatrix.compose(this.tempPosition, this.tempQuaternion, this.tempScale);
        batch.setMatrixAt(batch.count, this.tempMatrix);
        batch.count++;
    }

    update() {
        for (const mesh of this.batches.values()) {
            mesh.instanceMatrix.needsUpdate = true;
            mesh.computeBoundingSphere();
        }
    }

    getTotalCount() {
        let total = 0;
        for (const mesh of this.batches.values()) {
            total += mesh.count;
        }
        return total;
    }
}

// Scenes & Lights
function createLights() {
    return {
        left: new THREE.PointLight(),
        right: new THREE.PointLight(),
        above: new THREE.PointLight(),
    };
}

function addLightsToScene(s, ls) {
    s.add(ls.left);
    s.add(ls.right);
    s.add(ls.above);
}

staticScene = new THREE.Scene();
const defaultBg = parseHex('689');
staticScene.background = defaultBg;
const staticLights = createLights();
addLightsToScene(staticScene, staticLights);

groundPlane = new THREE.Mesh(
    new THREE.PlaneGeometry(3000, 3000),
    new THREE.MeshLambertMaterial({ color: defaultBg })
);
groundPlane.position.z = -14;

dynamicScene = new THREE.Scene();
const dynamicLights = createLights();
addLightsToScene(dynamicScene, dynamicLights);

const staticBatchManager = new BatchManager(staticScene, materials);
const occlusionBatchManager = new BatchManager(dynamicScene, { occlusion: materials.occlusion }, -10000);
const dynamicBatchManager = new BatchManager(dynamicScene, materials);

// Background scene
backgroundScene = new THREE.Scene();
backgroundCamera = new THREE.OrthographicCamera(-1, 1, 1, -1, 1, 20);
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
camera = new THREE.OrthographicCamera(0, 0, 0, 0, 1, 2000);
camera.up.set(0, 0, 1);

// Renderer
function getDpr() {
    return Math.min(window.devicePixelRatio || 1, 2);
}
renderer = new THREE.WebGLRenderer({ antialias: false });
renderer.autoClear = false;
renderer.info.autoReset = false;
container.appendChild(renderer.domElement);

// Static pass
const n8aoPass = new N8AOPostPass(
    staticScene, camera, container.clientWidth, container.clientHeight
);
n8aoPass.configuration.aoRadius = 30;
n8aoPass.configuration.distanceFalloff = 1;
n8aoPass.configuration.intensity = 5;
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

dynamicComposer = new PP.EffectComposer(renderer, { frameBufferType: THREE.HalfFloatType });
dynamicComposer.addPass(new PP.RenderPass(backgroundScene, backgroundCamera));
dynamicComposer.addPass(dynamicRenderPass);
dynamicComposer.addPass(new PP.EffectPass(camera,
    new PP.BloomEffect({
        intensity: 5,
        luminanceThreshold: 1,
        mipmapBlur: true,
    }),
    new PP.SMAAEffect({
        preset: PP.SMAAPreset.LOW,
    })
));

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
let lastStaticStats = { calls: 0, triangles: 0 };

app.ports.saveFinishedLevels.subscribe(levels => {
    localStorage.setItem('finishedLevels', JSON.stringify(levels));
});

app.ports.savePerformance.subscribe(perf => {
    localStorage.setItem('performance', perf);
});

app.ports.saveLeashEnabled.subscribe(enabled => {
    localStorage.setItem('leashEnabled', enabled);
});

app.ports.renderThreeJS.subscribe(data => {
    latestData = data;
    if (data.performance === 'potato') {
        if (groundPlane.parent === staticScene) {
            staticScene.remove(groundPlane);
            needsStaticRender = true;
        }
    } else {
        if (groundPlane.parent !== staticScene) {
            staticScene.add(groundPlane);
            needsStaticRender = true;
        }
    }

    if (data.staticUpdate && data.static) {
        needsStaticRender = true;
        pendingStatic = data.static;
    }

    if (data.staticUpdate) {
        const c = data.config;
        [staticLights, dynamicLights].forEach(ls => {
            const intensityScale = 10000;
            ls.left.color.copy(parseHex(c.left.color));
            ls.left.intensity = c.left.intensity * intensityScale;
            ls.left.position.set(c.left.position.x, c.left.position.y, c.left.position.z);

            ls.right.color.copy(parseHex(c.right.color));
            ls.right.intensity = c.right.intensity * intensityScale;
            ls.right.position.set(c.right.position.x, c.right.position.y, c.right.position.z);

            ls.above.color.copy(parseHex(c.above.color));
            ls.above.intensity = c.above.intensity * intensityScale;
            ls.above.position.set(c.above.position.x, c.above.position.y, c.above.position.z);
        });
        const bgColor = parseHex(c.bg);
        staticScene.background.copy(bgColor);
        groundPlane.material.color.copy(bgColor);
    }


    if (!rafId) {
        rafId = requestAnimationFrame(() => {
            const t0 = performance.now();
            updateScene(latestData);

            if (needsStaticRender) {
                renderer.info.reset();
                staticComposer.render();
                lastStaticStats = {
                    calls: renderer.info.render.calls,
                    triangles: renderer.info.render.triangles
                };
                // a LLM says this is sometimes empty and needs to fall back to
                // staticComposer.readBuffer.texture; haven't managed to replicate
                backgroundQuad.material.map = staticComposer.getRenderer().getRenderTarget().texture;
                backgroundQuad.material.needsUpdate = true;
                needsStaticRender = false;
                pendingStatic = null;
            }

            renderer.info.reset();
            renderer.setRenderTarget(null);
            dynamicComposer.render();
            const dynamicStats = {
                calls: renderer.info.render.calls,
                triangles: renderer.info.render.triangles
            };

            const t1 = performance.now();
            if (t1 - lastRenderTimeReport > 1000 || lastRenderTimeReport === 0) {
                const stats = {
                    duration: t1 - t0,
                    staticMeshes: staticBatchManager.getTotalCount(),
                    dynamicMeshes: dynamicBatchManager.getTotalCount(),
                    staticDrawCalls: lastStaticStats.calls,
                    staticTriangles: lastStaticStats.triangles,
                    dynamicDrawCalls: dynamicStats.calls,
                    dynamicTriangles: dynamicStats.triangles,
                    geometries: renderer.info.memory.geometries,
                    textures: renderer.info.memory.textures
                };
                app.ports.updateRenderTime.send(stats);
                lastRenderTimeReport = t1;
            }
            rafId = null;
        });
    }
});

function parseHex(hex) {
    return new THREE.Color(
        '#' + hex.split('').map(char => char + char).join('')
    );
}

function updateScene(data) {
    const staticToUse = pendingStatic || (data.staticUpdate ? data.static : null);
    if (staticToUse) {
        staticBatchManager.reset();
        occlusionBatchManager.reset();

        // Sorting helps with render order even within batches if using transparency
        const sortedStatic = [...staticToUse].sort((a, b) => {
            return (a.z - a.x - a.y) - (b.z - b.x - b.y);
        });

        sortedStatic.forEach(r => {
            staticBatchManager.add(r);
            occlusionBatchManager.add({...r, material: 'occlusion'});
        });

        staticBatchManager.update();
        occlusionBatchManager.update();
    }

    dynamicBatchManager.reset();
    const sortedDynamic = [...data.dynamic].sort((a, b) => {
        return (a.z - a.x - a.y) - (b.z - b.x - b.y);
    });
    sortedDynamic.forEach(r => {
        dynamicBatchManager.add(r);
    });
    dynamicBatchManager.update();

    lastFocalPoint.set(data.camera.focalPoint.x, data.camera.focalPoint.y, data.camera.focalPoint.z);
    camera.position.set(data.camera.position.x, data.camera.position.y, data.camera.position.z);
    camera.lookAt(lastFocalPoint);
    lastViewSize = data.camera.viewSize;

    updateCamera();
}
