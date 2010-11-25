cd the-dep
kit publish-local
cd ..
cd the-project
rm -rf Kits
kit update
ls -l Kits/the-dep-1.0/lib
cd ..
