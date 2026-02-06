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

const scale = 0.1;

function addMesh(geo, mat, x, y, z) {
    const mesh = new THREE.Mesh(geo, mat);
    mesh.position.set(x, y, z);
    scene.add(mesh);
    return mesh;
}

function drawRailings(railings) {
    railings.forEach(r => {
        const baseCoords = r.blockType === 'stairs'
            ? [-4.5, -3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5, 4.5]
            : [-4, -2, 0, 2, 4];

        const getZd = (xd, yd) => {
            if (r.blockType === 'base') return 0.2;
            if (r.blockType === 'bridge') return 1.2;
            if (r.blockType === 'stairs') {
                const bd = r.blockDirection;
                if (bd === 'NW') {
                    if (r.direction === 'NE' || r.direction === 'SW') return -yd - 4.3;
                    if (r.direction === 'NW') return -4 - 4.3;
                    if (r.direction === 'SE') return -(-4) - 4.3;
                } else if (bd === 'NE') {
                    if (r.direction === 'NW' || r.direction === 'SE') return -xd - 4.3;
                    if (r.direction === 'NE') return -4 - 4.3;
                    if (r.direction === 'SW') return -(-4) - 4.3;
                } else if (bd === 'SE') {
                    if (r.direction === 'NE' || r.direction === 'SW') return yd - 4.3;
                    if (r.direction === 'SE') return -4 - 4.3;
                    if (r.direction === 'NW') return -(-4) - 4.3;
                } else if (bd === 'SW') {
                    if (r.direction === 'NW' || r.direction === 'SE') return xd - 4.3;
                    if (r.direction === 'SW') return -4 - 4.3;
                    if (r.direction === 'NE') return -(-4) - 4.3;
                }
            }
            return 0;
        };

        const centers = [];
        baseCoords.forEach(c => {
            if (r.direction === 'SE') centers.push([c, -4]);
            else if (r.direction === 'SW') centers.push([-4, c]);
            else if (r.direction === 'NW') centers.push([c, 4]);
            else if (r.direction === 'NE') centers.push([4, c]);
        });

        const rgeo = new THREE.BoxGeometry(0.3 * 0.01, 0.3 * 0.01, 0.5 * 0.01);
        centers.forEach(([xd, yd]) => {
            addMesh(rgeo, materials.railing, (r.x + xd * 0.1) * scale, (r.y + yd * 0.1) * scale, (r.z + getZd(xd, yd) * 0.1) * scale);
        });
    });
}

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

    // Blocks
    data.blocks.forEach(b => {
        if (b.type === 'base') {
            const h = (b.z + 1) * scale;
            const geo = new THREE.BoxGeometry(scale, scale, h);
            addMesh(geo, materials.base, b.x * scale, b.y * scale, (b.z * 0.5 - 0.5) * scale);
        } else if (b.type === 'bridge') {
            const h = b.z * scale;
            const geo = new THREE.BoxGeometry(scale, scale, h);
            addMesh(geo, materials.base, b.x * scale, b.y * scale, (b.z * 0.5 - 1.0) * scale);

            const bgeo = new THREE.BoxGeometry(scale, scale, 0.01);
            addMesh(bgeo, materials.bridge, b.x * scale, b.y * scale, b.z * scale + 0.005);
        } else if (b.type === 'stairs') {
            const h = b.z * scale;
            const geo = new THREE.BoxGeometry(scale, scale, h);
            addMesh(geo, materials.stairs, b.x * scale, b.y * scale, (b.z * 0.5 - 1.0) * scale);

            for (let i = 0; i < 10; i++) {
                let sw, sd, sh, cx, cy, cz;
                if (b.direction === 'NW') {
                    sw = 10; sd = 1; sh = (1 + i);
                    cx = 0; cy = (4.5 - i); cz = (-9.5 + 0.5 * i);
                } else if (b.direction === 'NE') {
                    sw = 1; sd = 10; sh = (1 + i);
                    cx = (4.5 - i); cy = 0; cz = (-9.5 + 0.5 * i);
                } else if (b.direction === 'SE') {
                    sw = 10; sd = 1; sh = (10 - i);
                    cx = 0; cy = (4.5 - i); cz = (-5.0 - 0.5 * i);
                } else if (b.direction === 'SW') {
                    sw = 1; sd = 10; sh = (10 - i);
                    cx = (4.5 - i); cy = 0; cz = (-5.0 - 0.5 * i);
                }

                const sgeo = new THREE.BoxGeometry(sw * 0.01, sd * 0.01, sh * 0.01);
                addMesh(sgeo, materials.stairs, (b.x + cx * 0.1) * scale, (b.y + cy * 0.1) * scale, (b.z + cz * 0.1) * scale);
            }
        }
    });

    // Railings
    // drawRailings(data.railings);

    // Player
    const p = data.player;
    const isStairs = data.blocks.some(b => b.x === p.x && b.y === p.y && b.type === 'stairs');
    const zStairsFix = isStairs ? -5 * 0.01 : 0;

    const drawSphere = (zOff, rad) => {
        const sgeo = new THREE.SphereGeometry(rad * 0.01, 16, 16);
        addMesh(sgeo, materials.player, p.x * scale, p.y * scale, (p.z + zOff * 0.1) * scale + zStairsFix);
    };
    drawSphere(2.0, 2.2);
    drawSphere(5.5, 1.8);
    drawSphere(8.5, 1.4);

    playerLight.position.set(p.x * scale, p.y * scale, (p.z + 0.5) * scale + zStairsFix);

    // Goal
    const g = data.goal;
    const isAtEnd = p.x === g.x && p.y === g.y && p.z === g.z;
    const goalZ = g.z * scale + (isAtEnd ? 10.5 : 1) * 0.01;
    const goalGeo = new THREE.BoxGeometry(1.6 * 0.01, 1.6 * 0.01, 1.6 * 0.01);
    [0, 30, 60].forEach(rot => {
        const mesh = addMesh(goalGeo, materials.goal, g.x * scale, g.y * scale, goalZ);
        mesh.rotation.z = rot * Math.PI / 180;
    });

    // Focus
    if (data.mode === 'editing') {
        const f = data.focus;
        const fgeo = new THREE.SphereGeometry(0.01, 8, 8);
        const offsets = [
            [-5,-5,-10], [5,-5,-10], [-5,5,-10], [5,5,-10],
            [-5,-5,0], [5,-5,0], [-5,5,0], [5,5,0]
        ];
        offsets.forEach(off => {
            addMesh(fgeo, materials.focus, (f.x + off[0]*0.1) * scale, (f.y + off[1]*0.1) * scale, (f.z + off[2]*0.1) * scale);
        });
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
