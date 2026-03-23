const sdfShader = {
    uniforms: {
        uData: { value: null },
        uCount: { value: 0 },
        uCameraPos: { value: new THREE.Vector3() },
        uCameraForward: { value: new THREE.Vector3() },
        uCameraRight: { value: new THREE.Vector3() },
        uCameraUp: { value: new THREE.Vector3() },
        uTime: { value: 0 }
    },
    vertexShader: `
        varying vec2 vUv;
        void main() {
            vUv = uv;
            gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
        }
    `,
    fragmentShader: `
        precision highp float;
        uniform sampler2D uData;
        uniform int uCount;
        uniform vec3 uCameraPos;
        uniform vec3 uCameraForward;
        uniform vec3 uCameraRight;
        uniform vec3 uCameraUp;
        uniform float uTime;
        varying vec2 vUv;

        out vec4 pc_fragColor;

        float sdBox(vec3 p, vec3 b) {
            vec3 q = abs(p) - b;
            return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0);
        }

        float sdSphere(vec3 p, float s) {
            return length(p) - s;
        }

        vec4 map(vec3 p) {
            float d = 1e10;
            vec3 color = vec3(1.0, 0.0, 1.0); // Default error color

            if (uCount == 0) return vec4(0.0, 0.0, 0.0, 1e10);

            for (int i = 0; i < uCount; i++) {
                if (i >= 1024) break; // Safety

                // Pixel coords in 128x128 texture
                ivec2 uv0 = ivec2(i % 128, i / 128);
                ivec2 uv1 = ivec2((i + 1024) % 128, (i + 1024) / 128);
                ivec2 uv2 = ivec2((i + 2048) % 128, (i + 2048) / 128);

                vec4 data0 = texelFetch(uData, uv0, 0);
                vec4 data1 = texelFetch(uData, uv1, 0);
                vec4 data2 = texelFetch(uData, uv2, 0);

                vec3 pos = data0.xyz;
                float type = data0.w;
                vec3 size = data1.xyz * 0.5;
                float rot = data1.w;
                vec3 col = data2.xyz;

                vec3 q = p - pos;
                if (abs(rot) > 0.001) {
                    float s = sin(rot), c = cos(rot);
                    mat2 m = mat2(c, -s, s, c);
                    q.xy = m * q.xy;
                }

                float d2;
                if (type < 0.5) {
                    d2 = sdBox(q, size);
                } else {
                    d2 = sdSphere(q, size.x);
                }

                if (d2 < d) {
                    d = d2;
                    color = col;
                }
            }
            return vec4(color, d);
        }

        vec3 calcNormal(vec3 p) {
            const float h = 0.001;
            const vec2 k = vec2(1, -1);
            return normalize(k.xyy * map(p + k.xyy * h).w +
                             k.yyx * map(p + k.yyx * h).w +
                             k.yxy * map(p + k.yxy * h).w +
                             k.xxx * map(p + k.xxx * h).w);
        }

        vec3 render_scene(vec2 uv) {
            // Start from camera position
            vec3 ro = uCameraPos;
            // Direction through the pixel on the focus plane
            vec3 rd = normalize(uCameraForward + uCameraRight * uv.x + uCameraUp * uv.y);

            float t = 0.1; // Start a bit away from camera
            vec4 res;
            for (int i = 0; i < 128; i++) {
                vec3 p = ro + rd * t;
                res = map(p);
                if (res.w < 0.001 || t > 200.0) break;
                t += res.w;
            }

            if (t < 200.0) {
                vec3 p = ro + rd * t;
                vec3 normal = calcNormal(p);
                vec3 lightDir = normalize(vec3(0.5, 0.5, 1.0));
                float diff = max(dot(normal, lightDir), 0.0);
                vec3 col = res.xyz * (diff * 0.7 + 0.3);

                // Subtle rim
                float rim = 1.0 - max(dot(normal, -rd), 0.0);
                col += pow(rim, 3.0) * 0.1;

                return col;
            }

            // Background
            float sky = 0.5 + 0.5 * rd.z;
            return mix(vec3(0.1, 0.1, 0.15), vec3(0.2, 0.25, 0.35), sky);
        }

        void main() {
            vec2 uv = vUv * 2.0 - 1.0;
            if (uCount == 0) {
                pc_fragColor = vec4(1.0, 0.0, 0.0, 1.0); // Red if no data
                return;
            }
            vec3 col = render_scene(uv);
            pc_fragColor = vec4(col, 1.0);
        }
    `
};

export default sdfShader;
