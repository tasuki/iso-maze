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

    isMoving() {
        if (!this.initialized) return true;

        const velThreshold = 0.01;
        const posThreshold = 0.001;

        for (let i = 0; i < this.spheres.length; i++) {
            const s = this.spheres[i];
            if (s.velocity.length() > velThreshold) return true;

            // Check distance to target
            if (s.current.distanceTo(s.target) > posThreshold) return true;
        }

        return false;
    }

    update(deltaTime) {
        if (!this.initialized) return false;

        // Sub-stepping for stability. We want a fixed-ish dt for the physics simulation.
        // Even if the frame rate drops, we want to simulate in small increments.
        const totalDt = deltaTime / 1000;
        const subSteps = Math.ceil(totalDt / 0.016); // Aim for at least 60Hz simulation
        const dt = totalDt / subSteps;

        for (let step = 0; step < subSteps; step++) {
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
                    const desiredOffset = new THREE.Vector3().subVectors(s.target, this.spheres[i - 1].target);
                    targetPos.copy(this.spheres[i - 1].current).add(desiredOffset);
                }

                // Simple spring-damper physics
                const springK = 400;
                const damping = 30;

                const diff = new THREE.Vector3().subVectors(targetPos, s.current);
                const force = diff.multiplyScalar(springK);
                const friction = new THREE.Vector3().copy(s.velocity).multiplyScalar(-damping);

                const acceleration = force.add(friction);
                s.velocity.add(acceleration.multiplyScalar(dt));
                s.current.add(new THREE.Vector3().copy(s.velocity).multiplyScalar(dt));

                if (s.mesh && step === subSteps - 1) {
                    s.mesh.position.copy(s.current);
                }
            });
        }

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
