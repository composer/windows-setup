To run the main composer.iss script you must create a file named "develop.iss"
in the same directory, defining SetupVersion. For example:

    #define SetupVersion "3.0"

Composer-Setup will be compiled with the following settings:
    output filename:    /Output/setup.exe
    exe version info:   as defined in SetupVersion


To compile a release version, you must create a file named "release.iss" in
the same directory and run it. It must contain:

    #define SetupVersion "3.0" // set the version
    #define Release
    #define SignTool "mysigntool"  // the name of IDE sign tool
    #include "composer.iss"

Or you can use the the command-line compiler (see Inno Setup Help). For example:
    iscc /d"SetupVersion=3.0" /d"Release" /d"SignTool=mysigntool" /Smysigntool="..." "path\to\composer.iss"
