import * as THREE from 'three';
import { Elm } from './src/Main.elm';

const app = Elm.Main.init();

let scene, camera, renderer, container;
let isInitialized = false;

// Set Z as up
THREE.Object3D.DEFAULT_UP.set(0, 0, 1);

// Reuse Materials
const materials = {
    base: new THREE.MeshStandardMaterial({ color: 0xffffff, roughness: 1.0 }),
    stairs: new THREE.MeshStandardMaterial({ color: 0xffc8aa, roughness: 1.0 }),
    bridge: new THREE.MeshStandardMaterial({ color: 0xc86464, roughness: 1.0 }),
    railing: new THREE.MeshStandardMaterial({ color: 0xc8c8c8, roughness: 1.0 }),
    player: new THREE.MeshStandardMaterial({ color: 0xffffff, roughness: 0.9, metalness: 0 }),
    goal: new THREE.MeshStandardMaterial({ color: 0x141414, roughness: 0.5, metalness: 0.5 }),
    focus: new THREE.MeshBasicMaterial({ color: 0xffa500, transparent: true, opacity: 0.5 })
};

function initThree() {
    container = document.getElementById('three-container');
    if (!container || container.clientWidth === 0) {
        requestAnimationFrame(initThree);
        return;
    }

    scene = new THREE.Scene();
    scene.background = new THREE.Color(0xadd8e6); // Light Blue

    const aspect = container.clientWidth / container.clientHeight;
    const viewSize = 1.4;
    camera = new THREE.OrthographicCamera(
        -aspect * viewSize / 2, aspect * viewSize / 2,
        viewSize / 2, -viewSize / 2,
        1, 1000
    );
    camera.up.set(0, 0, 1);

    renderer = new THREE.WebGLRenderer({ antialias: true });
    renderer.setPixelRatio(window.devicePixelRatio);
    renderer.setSize(container.clientWidth, container.clientHeight);
    container.appendChild(renderer.domElement);

    const spotLeft = new THREE.PointLight(0xffffff, 50);
    spotLeft.position.set(-2, 3, 5);
    scene.add(spotLeft);

    const fillLeft = new THREE.PointLight(0xffffff, 10);
    fillLeft.position.set(-1, -0.5, 1.5);
    scene.add(fillLeft);

    const fillRight = new THREE.PointLight(0xffffff, 5);
    fillRight.position.set(-0.5, -1, 1.5);
    scene.add(fillRight);

    const fillAbove = new THREE.PointLight(0xffffff, 25);
    fillAbove.position.set(1, 1, 3);
    scene.add(fillAbove);

    const ambientLight = new THREE.AmbientLight(0xffffff, 0.4);
    scene.add(ambientLight);

    isInitialized = true;
}

function render() {
    if (!isInitialized) return;
    renderer.render(scene, camera);
}

window.addEventListener('resize', () => {
    if (!isInitialized) return;
    const aspect = container.clientWidth / container.clientHeight;
    const viewSize = 1.4;
    camera.left = -aspect * viewSize / 2;
    camera.right = aspect * viewSize / 2;
    camera.top = viewSize / 2;
    camera.bottom = -viewSize / 2;
    camera.updateProjectionMatrix();
    renderer.setSize(container.clientWidth, container.clientHeight);
    render();
});

app.ports.renderThreeJS.subscribe(data => {
    if (!isInitialized) {
        initThree();
    }
    updateScene(data);
});

function updateScene(data) {
    if (!isInitialized) return;

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

    const scale = 0.1;

    function addMesh(geo, mat, x, y, z) {
        const mesh = new THREE.Mesh(geo, mat);
        mesh.position.set(x, y, z);
        scene.add(mesh);
        return mesh;
    }

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
    data.railings.forEach(r => {
        const baseCoords = r.blockType === 'stairs'
            ? [-4.5, -3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5, 4.5]
            : [-4, -2, 0, 2, 4];

        const getZd = (xd, yd) => {
            if (r.blockType === 'base') return 0.2;
            if (r.blockType === 'bridge') return 1.2;
            if (r.blockType === 'stairs') {
                if (r.direction === 'SE') return yd - 4.3;
                if (r.direction === 'SW') return xd - 4.3;
                if (r.direction === 'NW') return -4.3 - yd;
                if (r.direction === 'NE') return -4.3 - xd;
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
        const fgeo = new THREE.SphereGeometry(0.005, 8, 8);
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
