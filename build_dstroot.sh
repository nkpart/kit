rm -rf dstroot
mkdir -p dstroot/usr/local/bin
mkdir -p dstroot/usr/bin
mkdir -p dstroot/private/etc
install -m 755 dist/build/kit/kit dstroot/usr/local/bin/
