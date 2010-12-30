0.6.2
-----
* Allow 'tagging' of kits as they're published. 
    $ kit publish-local --tag=-HEAD
  Assuming the Kitspec currently specifies version '1.0', this will publish it as '1.0-HEAD'

0.6.1
-----
* [BUG] On error, will now use a failure exit code.

0.6.0
---------
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

