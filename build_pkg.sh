cabal build && 
  rm -rf dstroot && 
  mkdir -p dstroot/usr/local/bin && 
  install -m 755 dist/build/kit/kit dstroot/usr/local/bin/ && 
  /Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker -d etc/kit.pmdoc -o dist/Kit-0.7.7.pkg
