A kitted project should have an embedded KitProject.xcodeproj

Kit will manage it, adding .m files to be compiled, and exposing a libkit.a for linking with the parent project.

Kit will need to build the list of dependencies (deps of deps, etc.), and stick these into the embedded project.

There'll be a kit server, with an index of kits.

A kit will hold source and headers, and the dependencies.

kit format:

	/Source/Main
		**/*.h -- Included in headers dir
		**/*.m -- linked into libkit.m
	
	KitSpec -- file specifying dependencies, project name, version, etc.
	

kit server

  /kits/functional-kit/0.1
    functional-kit-0.1.tgz
    KitSpec
	
