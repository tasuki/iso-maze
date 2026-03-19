## Level editor

- `e` toggle editor
- `c` reset camera
- `` ` `` twice, show maze analysis

### Move the focus

- `h` NW
- `j` SW
- `k` NE
- `l` SE
- `u` Down
- `i` Up

### Act on the focused tile

- `space`: toggle ground
- `s`: stairs (press multiple to change direction)
- `b`: bridge
- `q`: remove tile
- `a`: place snowman
- `z`: place hat


## Development notes...

### Apache completion POST support

To handle the completion POST requests in production with Apache, you can add this to your `.htaccess` or server config:

```apache
RewriteEngine On
RewriteCond %{REQUEST_METHOD} POST
RewriteRule ^completed/.*$ /completed/index.json [L,T=application/json]
```

This will serve the static `index.json` file for any POST request to `/completed/`.


### Emoji

https://emojipedia.org/nature has a good overview

🐞 🐛 🐜 🐝 🦋 🐌

🐸 🐭 🐹 🐥 🐿️ 🦎

🦔 🐰 🐦 🦆 🐓 🦚

🦢 🦩 🦜 🐧 🦥 🐱

🦊 🐕 🐒 🦉 🦇 🐐

🐑 🐷 🦙 🦘 🐨 🐼

🦌 deer
🐄 cow
🐂 ox
🐎 horse
🦓 zebra
🦬 bison

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
