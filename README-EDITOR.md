## Level editor

- c reset camera
- + zoom in
- - zoom out

### Move focus

- h NW
- j SW
- k NE
- l SE
- u Down
- i Up

### Act on the focused tile

- space: toggle ground
- s: stairs (press multiple to change direction)
- b: bridge
- a: place snowman
- z: place hat


## Other development notes, eg weird vim things...

Concat maze from Elm to short string:

```
s/.*=\s*"\|++\s*"\|\(\n\|\s\|"\)//g
```

Expand shorthand maze to nicely formatted Elm:

```
let chunk_size = matchstr(getline('.'), 'sz:\zs\d\+,') * 2
s/^\(.*mz:\)/maze = "\1"\r/
execute ":s/x/x /g | :s/\\(.\\{" . chunk_size . "}\\)/    ++ \"\\1\"\r/g"
```

### Emoji

https://emojipedia.org/nature has a good overview

🐜 ant
🐛 bug
🐞 ladybug
🐝 bee
🦋 butterfly
🐌 snail

🐸 frog
🐭 mouse
🐹 hamster
🐥 chick
🐿️ squirrel
🦎 lizard
🦇 bat
🦔 hedgehog

🐰 rabbit
🦆 duck
🐓 rooster
🦥 sloth
🐱 cat
🦉 owl
🦊 fox

🐕 dog
🐒 monkey
🐑 sheep
🦙 llama
🐐 goat
🦘 kangaroo
🐷 pig
🐨 koala
🐼 panda

🐄 cow
🐎 horse
🦓 zebra
🦌 deer
🐂 ox

🦅 eagle
🐊 croco
🐺 wolf
🐯 tiger
🦁 lion
🐻 bear

🐫 camel
🦏 rhino
🦛 hippo
🦒 giraffe
🐘 elephant
🦣 mammoth

🐬 dolphin
🐳 whale
🦈 shark
🦖 t-rex
🐉 dragon
🦄 unicorn
