import * as THREE from 'three';
import * as PP from 'postprocessing';
import { sdfVertexShader, sdfFragmentShader } from './sdfShader.js';
import { Elm } from './src/Main.elm';

const app = Elm.Main.init({
    flags: {
        dpr: getDpr(),
        finishedLevels: JSON.parse(localStorage.getItem('finishedLevels') || '[]'),
        performance: localStorage.getItem('performance') || 'normal',
        leashEnabled: localStorage.getItem('leashEnabled') === 'true' ? true : false,
    }
});

function getDpr() {
    return Math.min(window.devicePixelRatio || 1, 2);
}

// SDF Setup
const dataArray = new Float32Array(128 * 128 * 4);
const dataTexture = new THREE.DataTexture(dataArray, 128, 128, THREE.RGBAFormat, THREE.FloatType);
dataTexture.needsUpdate = true;

const sdfUniforms = {
    uData: { value: dataTexture },
    uCount: { value: 0 },
    uBgColor: { value: new THREE.Color(0x87ceeb) },
    uCameraPosition: { value: new THREE.Vector3() },
    uCameraRight: { value: new THREE.Vector3() },
    uCameraUp: { value: new THREE.Vector3() },
    uViewSize: { value: 1.0 }
};

const sdfMaterial = new THREE.ShaderMaterial({
    glslVersion: THREE.GLSL3,
    uniforms: sdfUniforms,
    vertexShader: sdfVertexShader,
    fragmentShader: sdfFragmentShader,
    depthTest: false,
    depthWrite: false
});

const staticScene = new THREE.Scene();
staticScene.add(new THREE.Mesh(new THREE.PlaneGeometry(2, 2), sdfMaterial));
const sdfCamera = new THREE.OrthographicCamera(-1, 1, 1, -1, 0, 10);
sdfCamera.position.z = 5;

const staticRenderTarget = new THREE.WebGLRenderTarget(1, 1, { type: THREE.HalfFloatType });

// Final scene setup
const backgroundScene = new THREE.Scene();
const backgroundQuad = new THREE.Mesh(new THREE.PlaneGeometry(2, 2), new THREE.MeshBasicMaterial({ map: staticRenderTarget.texture }));
backgroundScene.add(backgroundQuad);
const backgroundCamera = new THREE.OrthographicCamera(-1, 1, 1, -1, 0, 10);
backgroundCamera.position.z = 5;

const dynamicScene = new THREE.Scene();
const camera = new THREE.OrthographicCamera(-1, 1, 1, -1, 1, 2000);
camera.up.set(0, 0, 1);

const materialColors = {
    base: new THREE.Color(0x99cc66),
    greenery: new THREE.Color(0x669933),
    stairs: new THREE.Color(0x99cc66),
    bridge: new THREE.Color(0x8b5a2b),
    debugUnreachable: new THREE.Color(0x000000),
    debugOccluding: new THREE.Color(0xff0000),
    debugHanging: new THREE.Color(0xffff00),
};

const geometryCache = new Map();
function getUnitBox() {
    if (!geometryCache.has('unit_box')) geometryCache.set('unit_box', new THREE.BoxGeometry(1, 1, 1));
    return geometryCache.get('unit_box');
}
function getUnitSphere() {
    if (!geometryCache.has('unit_sphere')) geometryCache.set('unit_sphere', new THREE.SphereGeometry(1, 16, 16));
    return geometryCache.get('unit_sphere');
}
function getUnitPlane() {
    if (!geometryCache.has('unit_plane')) geometryCache.set('unit_plane', new THREE.PlaneGeometry(1, 1));
    return geometryCache.get('unit_plane');
}

const materials = {
    player: new THREE.MeshStandardMaterial({ color: 0xffffff, emissive: 0xffffff, emissiveIntensity: 1.4, metalness: 0.2 }),
    halo: new THREE.MeshBasicMaterial({ color: 0x00ccff, transparent: true, opacity: 0.5, blending: THREE.AdditiveBlending, depthWrite: false }),
    goal: new THREE.MeshStandardMaterial({ color: 0xffd700, emissive: 0xffd700, emissiveIntensity: 0.5 }),
    focus: new THREE.MeshBasicMaterial({ color: 0xffffff, wireframe: true }),
    joystickDot: new THREE.MeshBasicMaterial({ color: 0x00ffff, transparent: true, opacity: 0.8, depthTest: false, depthWrite: false }),
    default: new THREE.MeshStandardMaterial({ color: 0xffffff })
};

class BatchManager {
    constructor(scene) {
        this.scene = scene;
        this.batches = new Map();
        this.tempMatrix = new THREE.Matrix4();
        this.tempPosition = new THREE.Vector3();
        this.tempQuaternion = new THREE.Quaternion();
        this.tempScale = new THREE.Vector3();
        this.tempEuler = new THREE.Euler();
    }
    getBatch(type, materialName) {
        const key = `${type}_${materialName}`;
        if (!this.batches.has(key)) {
            const material = materials[materialName] || materials.default;
            let geometry = type === 'box' ? getUnitBox() : (type === 'sphere' ? getUnitSphere() : getUnitPlane());
            const mesh = new THREE.InstancedMesh(geometry, material, 1000);
            mesh.count = 0;
            if (materialName === 'halo') mesh.renderOrder = 10;
            else if (materialName === 'player' || materialName === 'goal') mesh.renderOrder = 20;
            else if (materialName === 'joystickDot') mesh.renderOrder = 25;
            this.scene.add(mesh);
            this.batches.set(key, mesh);
        }
        return this.batches.get(key);
    }
    reset() { this.batches.forEach(m => m.count = 0); }
    add(obj) {
        const batch = this.getBatch(obj.type, obj.material);
        this.tempPosition.set(obj.x, obj.y, obj.z);
        if (obj.type === 'box') {
            this.tempQuaternion.setFromAxisAngle(new THREE.Vector3(0, 0, 1), obj.rotationZ || 0);
            this.tempScale.set(obj.sizeX, obj.sizeY, obj.sizeZ);
        } else if (obj.type === 'sphere') {
            this.tempQuaternion.set(0, 0, 0, 1);
            this.tempScale.set(obj.radius, obj.radius, obj.radius);
        } else if (obj.type === 'plane') {
            this.tempEuler.set(obj.rotationX || 0, obj.rotationY || 0, obj.rotationZ || 0);
            this.tempQuaternion.setFromEuler(this.tempEuler);
            this.tempScale.set(obj.sizeX, obj.sizeY, 1);
        }
        this.tempMatrix.compose(this.tempPosition, this.tempQuaternion, this.tempScale);
        batch.setMatrixAt(batch.count++, this.tempMatrix);
    }
    update() { this.batches.forEach(m => {
        m.instanceMatrix.needsUpdate = true;
        m.computeBoundingSphere();
    }); }
}

const dynamicBatch = new BatchManager(dynamicScene);
const ambientLight = new THREE.AmbientLight(0xffffff, 0.5);
dynamicScene.add(ambientLight);
const directionalLight = new THREE.DirectionalLight(0xffffff, 1.0);
directionalLight.position.set(50, 50, 100);
dynamicScene.add(directionalLight);

const container = document.getElementById('three-container');
const renderer = new THREE.WebGLRenderer({ antialias: false });
renderer.autoClear = false;
container.appendChild(renderer.domElement);

const dynamicComposer = new PP.EffectComposer(renderer, { frameBufferType: THREE.HalfFloatType });
dynamicComposer.addPass(new PP.RenderPass(backgroundScene, backgroundCamera));
const dynamicPass = new PP.RenderPass(dynamicScene, camera);
dynamicPass.clear = false;
dynamicComposer.addPass(dynamicPass);
dynamicComposer.addPass(new PP.EffectPass(camera,
    new PP.BloomEffect({ intensity: 1.5, luminanceThreshold: 0.8, mipmapBlur: true }),
    new PP.SMAAEffect({ preset: PP.SMAAPreset.LOW })
));

function updateSize() {
    const w = container.clientWidth, h = container.clientHeight, dpr = getDpr();
    renderer.setPixelRatio(dpr); renderer.setSize(w, h);
    staticRenderTarget.setSize(w * dpr, h * dpr);
    dynamicComposer.setSize(w, h);
}
updateSize();
window.addEventListener('resize', () => {
    updateSize();
    app.ports.updateDpr.send(getDpr());
});

app.ports.saveFinishedLevels.subscribe(levels => localStorage.setItem('finishedLevels', JSON.stringify(levels)));
app.ports.savePerformance.subscribe(perf => localStorage.setItem('performance', perf));
app.ports.saveLeashEnabled.subscribe(enabled => localStorage.setItem('leashEnabled', enabled));

app.ports.renderThreeJS.subscribe(data => {
    const t0 = performance.now();
    if (data.staticUpdate) {
        let count = 0;
        data.static.forEach(obj => {
            const idx = count * 12;
            dataArray[idx + 0] = obj.x;
            dataArray[idx + 1] = obj.y;
            dataArray[idx + 2] = obj.z;
            if (obj.type === 'box') {
                dataArray[idx + 3] = 1.0;
                dataArray[idx + 4] = obj.sizeX;
                dataArray[idx + 5] = obj.sizeY;
                dataArray[idx + 6] = obj.sizeZ;
                dataArray[idx + 7] = obj.rotationZ;
            } else if (obj.type === 'sphere') {
                dataArray[idx + 3] = 2.0;
                dataArray[idx + 4] = obj.radius;
                dataArray[idx + 5] = 0; dataArray[idx + 6] = 0; dataArray[idx + 7] = 0;
            }
            const color = materialColors[obj.material] || new THREE.Color(0xffffff);
            dataArray[idx + 8] = color.r; dataArray[idx + 9] = color.g; dataArray[idx + 10] = color.b; dataArray[idx + 11] = 0;
            count++;
        });
        sdfUniforms.uCount.value = count;
        dataTexture.needsUpdate = true;
        if (data.config && data.config.bg) {
            const hex = data.config.bg;
            sdfUniforms.uBgColor.value.setRGB(parseInt(hex[0], 16) / 15, parseInt(hex[1], 16) / 15, parseInt(hex[2], 16) / 15);
        }
    }

    dynamicBatch.reset();
    (data.dynamic || []).forEach(obj => dynamicBatch.add(obj));
    dynamicBatch.update();

    const cam = data.camera;
    camera.position.set(cam.position.x, cam.position.y, cam.position.z);
    camera.lookAt(cam.focalPoint.x, cam.focalPoint.y, cam.focalPoint.z);
    const aspect = container.clientWidth / container.clientHeight;
    camera.left = -cam.viewSize * aspect / 2;
    camera.right = cam.viewSize * aspect / 2;
    camera.top = cam.viewSize / 2;
    camera.bottom = -cam.viewSize / 2;
    camera.updateProjectionMatrix();

    sdfUniforms.uCameraPosition.value.copy(camera.position);
    sdfUniforms.uCameraRight.value.copy(new THREE.Vector3(1, 0, 0).applyQuaternion(camera.quaternion));
    sdfUniforms.uCameraUp.value.copy(new THREE.Vector3(0, 1, 0).applyQuaternion(camera.quaternion));
    sdfUniforms.uViewSize.value = cam.viewSize;

    renderer.setRenderTarget(staticRenderTarget);
    renderer.render(staticScene, sdfCamera);
    renderer.setRenderTarget(null);

    dynamicComposer.render();

    const t1 = performance.now();
    app.ports.updateRenderTime.send({
        duration: t1 - t0,
        staticMeshes: 0,
        dynamicMeshes: (data.dynamic || []).length,
        staticDrawCalls: 1,
        staticTriangles: 2,
        dynamicDrawCalls: (data.dynamic || []).length > 0 ? 1 : 0,
        dynamicTriangles: 0,
        geometries: renderer.info.memory.geometries,
        textures: renderer.info.memory.textures
    });
});
