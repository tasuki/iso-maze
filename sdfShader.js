export const sdfVertexShader = `
out vec2 vUv;
void main() {
    vUv = uv;
    gl_Position = vec4(position, 1.0);
}
`;

export const sdfFragmentShader = `
precision highp float;
uniform sampler2D uData;
uniform int uCount;
uniform vec3 uBgColor;
uniform vec3 uCameraPosition;
uniform vec3 uCameraRight;
uniform vec3 uCameraUp;
uniform float uViewSize;

in vec2 vUv;
out vec4 fragColor;

vec3 rotateZ(vec3 p, float angle) {
    float s = sin(angle);
    float c = cos(angle);
    return vec3(p.x * c - p.y * s, p.x * s + p.y * c, p.z);
}

float sdBox(vec3 p, vec3 b) {
    vec3 q = abs(p) - b;
    return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0);
}

float sdSphere(vec3 p, float r) {
    return length(p) - r;
}

float map(vec3 p, out vec3 color) {
    float d = 1e10;
    color = vec3(1.0);

    if (uCount == 0) return 1e10;

    for (int i = 0; i < uCount; i++) {
        if (i >= 512) break;
        int baseIdx = i * 3;
        vec4 data0 = texelFetch(uData, ivec2(baseIdx % 128, baseIdx / 128), 0);
        vec4 data1 = texelFetch(uData, ivec2((baseIdx + 1) % 128, (baseIdx + 1) / 128), 0);
        vec4 data2 = texelFetch(uData, ivec2((baseIdx + 2) % 128, (baseIdx + 2) / 128), 0);

        vec3 pos = data0.xyz;
        float type = data0.w;
        vec3 size = data1.xyz;
        float rotZ = data1.w;
        vec3 objCol = data2.xyz;

        float dist = 1e10;
        if (type == 1.0) { // Box
            vec3 q = rotateZ(p - pos, -rotZ);
            dist = sdBox(q, size * 0.5);
        } else if (type == 2.0) { // Sphere
            dist = sdSphere(p - pos, size.x);
        }

        if (dist < d) {
            d = dist;
            color = objCol;
        }
    }
    return d;
}

vec3 calcNormal(vec3 p) {
    vec3 dummy;
    const float h = 0.01;
    const vec2 k = vec2(1, -1);
    return normalize(k.xyy * map(p + k.xyy * h, dummy) +
                     k.yyx * map(p + k.yyx * h, dummy) +
                     k.yxy * map(p + k.yxy * h, dummy) +
                     k.xxx * map(p + k.xxx * h, dummy));
}

void main() {
    vec2 uv = vUv * 2.0 - 1.0;
    vec3 ro = uCameraPosition + (uv.x * uCameraRight + uv.y * uCameraUp) * (uViewSize * 0.5);
    vec3 rd = normalize(cross(uCameraUp, uCameraRight));

    float t = 0.0;
    vec3 color;
    bool hit = false;
    for (int i = 0; i < 256; i++) {
        vec3 p = ro + rd * t;
        float d = map(p, color);
        if (d < 0.01) {
            hit = true;
            break;
        }
        t += d;
        if (t > 2000.0) break;
    }

    if (hit) {
        vec3 p = ro + rd * t;
        vec3 normal = calcNormal(p);

        // 3-point lighting scaled to match original scene
        vec3 keyDir = normalize(vec3(50, 50, 100));
        vec3 fillDir = normalize(vec3(-50, 0, 20));
        vec3 backDir = normalize(vec3(0, -50, -20));

        float key = max(dot(normal, keyDir), 0.0) * 1.5;
        float fill = max(dot(normal, fillDir), 0.0) * 0.5;
        float back = max(dot(normal, backDir), 0.0) * 0.4;

        vec3 finalCol = color * (key + fill + back + 0.3);
        fragColor = vec4(finalCol, 1.0);
    } else {
        fragColor = vec4(uBgColor, 1.0);
    }
}
`;
