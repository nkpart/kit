0.6.3
-----
* Resource directories. KitSpec field: resources-directory, default "resources". This 
  directory is symlinked to Kits/Resources/<package-name> on an update. It gives a 
  stable path to resources that need to be referenced in a parent project (data models,
  images, etc.)

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

