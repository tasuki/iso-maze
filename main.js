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
    base: new THREE.MeshStandardMaterial({ color: 0xffffff, roughness: 1.0 }),
    stairs: new THREE.MeshStandardMaterial({ color: 0xffccaa, roughness: 1.0 }),
    bridge: new THREE.MeshStandardMaterial({ color: 0xcc6666, roughness: 1.0 }),
    railing: new THREE.MeshStandardMaterial({ color: 0xcccccc, roughness: 1.0 }),
    player: new THREE.MeshStandardMaterial({ color: 0xffffff, emissive: 0xffffff, emissiveIntensity: 1 }),
    goal: new THREE.MeshStandardMaterial({ color: 0x222222, roughness: 0.5, metalness: 0.5 }),
    focus: new THREE.MeshStandardMaterial({ color: 0xff9900, emissive: 0xff9900, emissiveIntensity: 1 }),
};

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
function updateCamera() {
    const aspect = container.clientWidth / container.clientHeight;
    const viewSize = 1.4;
    camera.left = -aspect * viewSize / 2;
    camera.right = aspect * viewSize / 2;
    camera.top = viewSize / 2;
    camera.bottom = -viewSize / 2;
    camera.updateProjectionMatrix();
    // composer.setSize(container.clientWidth, container.clientHeight);
}
container = document.getElementById('three-container');
camera = new THREE.OrthographicCamera(0, 0, 0, 0, 1, 1000);
updateCamera();
camera.up.set(0, 0, 1);

// Renderer
function updateRenderer() {
    const dpr = Math.min(window.devicePixelRatio || 1, 2);
    renderer.setPixelRatio(dpr);
    renderer.setSize(container.clientWidth, container.clientHeight);
}
renderer = new THREE.WebGLRenderer({ antialias: false });
updateRenderer();
container.appendChild(renderer.domElement);

// Ambient Occlusion
const n8aoPass = new N8AOPostPass(scene, camera, container.clientWidth, container.clientHeight);
n8aoPass.configuration.aoRadius = 0.5;
n8aoPass.configuration.distanceFalloff = 1.5;
n8aoPass.configuration.intensity = 7.0;

// Postprocessing
composer = new PP.EffectComposer(renderer);
composer.setSize(container.clientWidth, container.clientHeight, false);
composer.addPass(new PP.RenderPass(scene, camera));
composer.addPass(n8aoPass);
composer.addPass(new PP.EffectPass(camera, new PP.SMAAEffect({ preset: PP.SMAAPreset.ULTRA })));

function render() { composer.render(); }
// somehow this fixes aliasing on first frame...
setTimeout(function() { render(); }, 1);

window.addEventListener('resize', () => {
    updateCamera();
    updateRenderer();
    render();
});


app.ports.renderThreeJS.subscribe(data => {
    updateScene(data);
});

function updateScene(data) {
    // Remove and dispose of old meshes and geometries
    const toRemove = [];
    scene.traverse(child => {
        if (child.isMesh) {
            toRemove.push(child);
            if (child.geometry) child.geometry.dispose();
            // Materials are reused, so don't dispose them here
        }
    });
    toRemove.forEach(m => scene.remove(m));

    const unitScale = 0.01;

    // Boxes
    data.boxes.forEach(b => {
        const geo = new THREE.BoxGeometry(b.sizeX * unitScale, b.sizeY * unitScale, b.sizeZ * unitScale);
        const mesh = new THREE.Mesh(geo, materials[b.material]);
        mesh.position.set(b.x * unitScale, b.y * unitScale, b.z * unitScale);
        if (b.rotationZ) {
            mesh.rotation.z = b.rotationZ * Math.PI / 180;
        }
        scene.add(mesh);
    });

    // Spheres
    data.spheres.forEach(s => {
        const geo = new THREE.SphereGeometry(s.radius * unitScale, 16, 16);
        const mesh = new THREE.Mesh(geo, materials[s.material]);
        mesh.position.set(s.x * unitScale, s.y * unitScale, s.z * unitScale);
        scene.add(mesh);
    });

    // Player Light
    if (data.playerLight) {
        playerLight.position.set(
            data.playerLight.x * unitScale,
            data.playerLight.y * unitScale,
            data.playerLight.z * unitScale,
        );
    }

    // Camera
    const azimuth = data.camera.azimuth * Math.PI / 180;
    const elevation = data.camera.elevation * Math.PI / 180;
    const distance = 15;
    const focalPoint = new THREE.Vector3(0, 0, 0.55);
    camera.position.x = focalPoint.x + distance * Math.cos(azimuth) * Math.cos(elevation);
    camera.position.y = focalPoint.y + distance * Math.sin(azimuth) * Math.cos(elevation);
    camera.position.z = focalPoint.z + distance * Math.sin(elevation);
    camera.lookAt(focalPoint);

    render();
}
