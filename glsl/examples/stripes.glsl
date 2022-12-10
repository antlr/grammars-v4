precision highp float;
varying vec3 fNormal;
varying vec3 worldPos;
varying vec3 localPos;
float pulse(float val, float dst) {
	return floor(mod(val * dst, 1.0) + .5);
}
void main() {
	vec3 dir = vec3(0, 1, 0);
	vec3 cpos = localPos;
	vec3 color = vec3(1, pulse(cpos.y, 10.0), 1);
	float diffuse = .5 + dot(fNormal, dir);
	gl_FragColor = vec4(diffuse * color, 1.0);
}
