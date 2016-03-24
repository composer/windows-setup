You will need to install the latest unicode version of Inno Setup Compiler.
Make sure you include Inno Setup Preprocessor when you run the installer.

To compile the main composer.iss script you must create a file named "develop.iss" in the same directory, defining SetupVersion. For example:

    #define SetupVersion "4.0"

Composer-Setup will be compiled with the following settings:
    output filename:    /Output/setup.exe
    exe version info:   as defined in SetupVersion


To compile a release version, you must create a file named "release.iss" in the same directory and run it. It must contain:

    #define Release
    #define SetupVersion "4.0"
    #define SignTool "uniquename"
    #define SignExe "path\to\signtool.exe" 
    #define SignSha1 "sign /a /t http://timestamp.comodoca.com/authenticode"
    #define SignSha2 "sign /a /as /fd sha256 /td sha256 /tr http://timestamp.comodoca.com/rfc3161"
    #include "composer.iss"

Explanation of SignTool: This is the unique name you give to the IDE SignTool, set from Tools/Options menu.
This IDE value should be "uniquename=$p" (the $p is replaced by the actual parameters).

Note that both the "develop.iss" and "release.iss" files are git-ignored.

Notes
=====

Debugging
---------
A "debug.iss" file is created each time you compile. This concatenates all the include files so that you can debug from a single script (because the Inno IDE only allows one script open at a time). Remember that changes you make will be overwritten next time you compile "composer.iss".


Code Signing
------------
The following instructions relate to Certum CA, who provide cheap open source certificates.

Use Firefox to renew/create a certificate because it allows you to backup and save your complete certificate (ie private and public keys).

Use the Install Online button from the Certum certificate management page (the other options only download the public key) then go to Options/Advanced/Certificates page in Firefox. Backup with a .p12 extension then click on the backup to import the certificate into the Windows Certificate Store - save it as the Current User, otherwise you will have to compile as an admin. Adding it to the certificate store allows you to use the /a option in signtool, which finds the most suitable certificate (rather than specifying a file and password in the command params).

Renewing: unless Certum offer SHA1 and SHA2 certs for a single price, it will be necessary to drop support
for any old OSs that cannot read SHA1.
