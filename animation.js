import * as THREE from 'three';

export class PlayerAnimator {
    constructor() {
        this.spheres = [
            { current: new THREE.Vector3(), target: new THREE.Vector3(), velocity: new THREE.Vector3(), mesh: null },
            { current: new THREE.Vector3(), target: new THREE.Vector3(), velocity: new THREE.Vector3(), mesh: null },
            { current: new THREE.Vector3(), target: new THREE.Vector3(), velocity: new THREE.Vector3(), mesh: null }
        ];
        this.initialized = false;
        this.timer = 0;
        this.staggerDelay = 0.05; // seconds
    }

    updateTargets(targets, meshes, light) {
        this.light = light;
        targets.forEach((t, i) => {
            if (i < 3) {
                this.spheres[i].target.copy(t);
                this.spheres[i].mesh = meshes[i];

                if (!this.initialized) {
                    // Initial setup for drop-in
                    this.spheres[i].current.copy(t);
                    this.spheres[i].current.z += 1.0; // Drop from 10 tiles high
                    this.spheres[i].velocity.set(0, 0, 0);
                }
            }
        });
        this.initialized = true;
    }

    update(deltaTime) {
        if (!this.initialized) return;

        // Cap deltaTime to avoid physics explosion
        const dt = Math.min(deltaTime, 0.05);
        this.timer += dt;

        this.spheres.forEach((s, i) => {
            // Staggered start for drop-in (optional, but looks nice)
            if (this.timer < i * this.staggerDelay) return;

            let targetPos = new THREE.Vector3();
            if (i === 0) {
                targetPos.copy(s.target);
            } else {
                // Trail effect: follow the sphere below
                // We calculate the desired offset from the original targets
                const desiredOffset = new THREE.Vector3().subVectors(s.target, this.spheres[i-1].target);
                targetPos.copy(this.spheres[i-1].current).add(desiredOffset);
            }

            // Simple spring-damper physics
            const springK = 250;
            const damping = 20;

            const diff = new THREE.Vector3().subVectors(targetPos, s.current);
            const force = diff.multiplyScalar(springK);
            const friction = new THREE.Vector3().copy(s.velocity).multiplyScalar(-damping);

            const acceleration = force.add(friction);
            s.velocity.add(acceleration.multiplyScalar(dt));
            s.current.add(new THREE.Vector3().copy(s.velocity).multiplyScalar(dt));

            if (s.mesh) {
                s.mesh.position.copy(s.current);
            }
        });

        if (this.light && this.spheres[0]) {
            // Position light relative to the animated spheres
            // It should be roughly at the same relative position as in Elm
            // Sphere 0 is at offset 2.0, Sphere 1 at 5.5, Light at 5.0
            // So light is 3.0 units above sphere 0
            this.light.position.copy(this.spheres[0].current);
            this.light.position.z += 0.03; // 3.0 * unitScale
        }
    }
}
