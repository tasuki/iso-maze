import { Elm } from './src/Main.elm';

const app = Elm.Main.init();


const scene = new THREE.Scene();
const ambientLight = new THREE.AmbientLight(0xffffff, 0.8);
scene.add(ambientLight);
const directionalLight = new THREE.DirectionalLight(0xffffff, 4);
directionalLight.position.set(2, 3, -6);
scene.add(directionalLight);

app.ports.setStorage.subscribe(function(state) {
    console.log(state);
});
