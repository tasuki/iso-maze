release:
	elm make src/Main.elm --optimize --output=iso-maze.js \
		&& uglifyjs iso-maze.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
		| uglifyjs --mangle --output iso-maze.js

develop: serve
	find . -name '*.elm' | entr elm make src/Main.elm --debug --output=iso-maze.js

serve:
	serve -p 8423 --single &
