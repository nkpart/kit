cd the-dep
kit publish-local 2> /dev/null
cd ..
cd the-project
rm -rf Kits
kit update 2> /dev/null > /dev/null 

cat Kits/Kit.xcconfig
tree Kits
cd ..
