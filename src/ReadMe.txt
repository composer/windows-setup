To run the main composer.iss script you must create a file named "develop.iss"
in the same directory, defining SetupVersion. For example:

    #define SetupVersion "3.0"

Composer-Setup will be compiled with the following settings:
    output filename:    /Output/setup.exe
    exe version info:   as defined in SetupVersion


To compile a release version, you must create a file named "release.iss" in
the same directory and run it. It must contain:

    #define Release
    #define SetupVersion "3.0" // set the version
    #define SignTool "mysigntool"  // the name of IDE sign tool
    #define SignCmd "path-to-sign.exe params"  // the command line to call the sign tool. The filename will be appended
    #include "composer.iss"
