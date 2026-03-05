import * as THREE from 'three';
import * as PP from 'postprocessing';

import { Elm } from './src/Main.elm';

class SnowEffect extends PP.Effect {
    constructor() {
        super('SnowEffect', `
            uniform float time;
            uniform float aspect;

            float snow(vec2 uv, float scale) {
                float t = time * 0.2;
                uv.x += sin(t * 0.2 + uv.y * 1.0) * 0.1;
                uv.y += t * (1.0 + scale * 0.05);
                uv *= scale;
                vec2 grid = fract(uv) - 0.5;
                vec2 id = floor(uv);
                float r = fract(sin(dot(id, vec2(12.9898, 78.233))) * 43758.5453);
                if (r < 0.9) return 0.0;
                float size = 0.01 + 0.03 * r;
                float dist = length(grid);
                return smoothstep(size, size - 0.01, dist) * (r - 0.9) * 10.0;
            }

            void mainImage(const in vec4 inputColor, const in vec2 uv, out vec4 outputColor) {
                vec2 aspectUV = uv;
                aspectUV.x *= aspect;
                float s = 0.0;
                s += snow(aspectUV, 5.0) * 0.2;
                s += snow(aspectUV, 10.0) * 0.4;
                s += snow(aspectUV, 15.0) * 0.6;
                outputColor = vec4(mix(inputColor.rgb, vec3(0.95, 0.97, 1.0), s * 0.5), inputColor.a);
            }
        `, {
            uniforms: new Map([
                ['time', new THREE.Uniform(0.0)],
                ['aspect', new THREE.Uniform(1.0)]
            ])
        });
    }

    update(renderer, inputBuffer, deltaTime) {
        this.uniforms.get('time').value += deltaTime;
        this.uniforms.get('aspect').value = inputBuffer.width / inputBuffer.height;
    }
}

const app = Elm.Main.init({
    flags: {
        dpr: getDpr(),
        finishedLevels: JSON.parse(localStorage.getItem('finishedLevels') || '[]')
    }
});

let staticScene, dynamicScene, camera, renderer, container, mainComposer, snowComposer, textureEffect;

// Z is up
THREE.Object3D.DEFAULT_UP.set(0, 0, 1);

// Caches
const materials = {
    base: new THREE.MeshLambertMaterial({ color: 0xffffff }),
    stairs: new THREE.MeshLambertMaterial({ color: 0xffccaa }),
    bridge: new THREE.MeshLambertMaterial({ color: 0xcc6666 }),
    player: new THREE.MeshLambertMaterial({ color: 0x66ffff, emissive: 0xbbdddd, emissiveIntensity: 1 }),
    goal: new THREE.MeshLambertMaterial({ color: 0x555555 }),
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

staticScene = new THREE.Scene();
staticScene.background = new THREE.Color(0x668899);
const staticLights = createLights();
addLightsToScene(staticScene, staticLights);

dynamicScene = new THREE.Scene();
const dynamicLights = createLights();
addLightsToScene(dynamicScene, dynamicLights);

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
camera = new THREE.OrthographicCamera(0, 0, 0, 0, 0.1, 1000);
camera.up.set(0, 0, 1);

// Renderer
function getDpr() {
    return Math.min(window.devicePixelRatio || 1, 2);
}
renderer = new THREE.WebGLRenderer({ antialias: false });
renderer.autoClear = false;
container.appendChild(renderer.domElement);

mainComposer = new PP.EffectComposer(renderer, {
    frameBufferType: THREE.HalfFloatType
});
mainComposer.addPass(new PP.RenderPass(staticScene, camera));
const dynamicRenderPass = new PP.RenderPass(dynamicScene, camera);
dynamicRenderPass.clear = false;
mainComposer.addPass(dynamicRenderPass);
mainComposer.addPass(new PP.EffectPass(camera, new PP.BloomEffect({
    intensity: 5,
    luminanceThreshold: 1,
    mipmapBlur: true,
})));
mainComposer.addPass(new PP.EffectPass(camera, new PP.SMAAEffect({
    preset: PP.SMAAPreset.HIGH,
})));

snowComposer = new PP.EffectComposer(renderer, { frameBufferType: THREE.HalfFloatType });
textureEffect = new PP.TextureEffect({ texture: mainComposer.outputBuffer.texture, blendFunction: PP.BlendFunction.SET });
snowComposer.addPass(new PP.EffectPass(camera, textureEffect, new SnowEffect()));

// Updates and Listeners
function updateSize() {
    const w = container.clientWidth;
    const h = container.clientHeight;
    const dpr = getDpr();
    renderer.setPixelRatio(dpr);
    renderer.setSize(w, h);
    mainComposer.setSize(w, h);
    snowComposer.setSize(w, h);
    textureEffect.texture = mainComposer.outputBuffer.texture;
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

let latestData = null;
let lastRenderTimeReport = 0;
let needsDynamicRender = false;
let lastT = performance.now();

app.ports.saveFinishedLevels.subscribe(levels => {
    localStorage.setItem('finishedLevels', JSON.stringify(levels));
});

app.ports.renderThreeJS.subscribe(data => {
    const unitScale = 0.01;
    latestData = data;
    needsDynamicRender = true;

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
        staticScene.background.copy(parseHex(c.bg));
    }
});

function renderLoop() {
    const t0 = performance.now();
    const dt = (t0 - lastT) / 1000;
    lastT = t0;
    const unitScale = 0.01;

    if (latestData) {
        if (needsDynamicRender) {
            updateScene(latestData, unitScale);
            mainComposer.render(dt);
            needsDynamicRender = false;
        }
        renderer.setRenderTarget(null);
        snowComposer.render(dt);
    }

    const t1 = performance.now();
    if (t1 - lastRenderTimeReport > 1000) {
        app.ports.updateRenderTime.send(t1 - t0);
        lastRenderTimeReport = t1;
    }

    requestAnimationFrame(renderLoop);
}
requestAnimationFrame(renderLoop);

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

function parseHex(hex) {
    return new THREE.Color(
        '#' + hex.split('').map(char => char + char).join('')
    );
}

function updateScene(data, unitScale) {
    if (data.staticUpdate && data.static) {
        const currentStaticKeys = new Set();
        data.static.forEach((r) => {
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
