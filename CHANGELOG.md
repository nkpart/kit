0.6.0-pre
---------

* [TODO] Pass the KitSpec file name via '-f'
* KitSpec file is now parsed as YAML. Because it's a superset of JSON,
  this is backwards compatible.

0.5.2
-----

* Source and lib directories are configurable: source-directory, lib-directory 
* Static libs can be linked into a kit. Place them in `lib` (or lib-directory).
* You can pass Xcode flags to the Kits when they are compiled. See
	`kitdeps-xcode-flags`. Contents of this string should be in xcconfig format.

pre-0.5.2
---------

Features and bugs.

