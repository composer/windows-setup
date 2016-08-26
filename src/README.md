# Composer-Setup development

## Tools
You will need to [download](http://www.jrsoftware.org/download.php/is-unicode.exe) and install the latest unicode version of Inno Setup Compiler. Make sure you include Inno Setup Preprocessor when you run the installer.

## Compiling
To compile the main `composer.iss` script you must create a file named `develop.iss` in the same directory, defining *SetupVersion*. For example:

```
#define SetupVersion "4.0"
```

Composer-Setup will be compiled with the following settings:

* output filename:    /Output/mysetup.exe
* exe version info:   as defined in SetupVersion

If you want to save the exe to a different directory, or give it a different name, you can use standard Inno Setup syntax:

```
#define SetupVersion "4.0"

[Setup]
OutputDir=myfolder
OutputBaseFilename=mycomposer-setup
```

To compile a release version, you must create a file named `release.iss` in the same directory and run it. It must look like this:

```
#define Release
#define SetupVersion "4.0"
#define SignTool "uniquename"
#define SignExe "path\to\signtool.exe"
#define SignSha1 "sign /a /t http://timestamp.comodoca.com/authenticode"
#define SignSha2 "sign /a /as /fd sha256 /td sha256 /tr http://timestamp.comodoca.com/rfc3161"
#include "composer.iss"
```

The *SignTool* define value is the unique name you give to the IDE SignTool in Inno Setup, set from Tools/Options menu. This value must be `uniquename=$p` (the $p is replaced by the actual parameters).

Note that both the `develop.iss` and `release.iss` files are git-ignored.

## Debugging
Because the Inno IDE only allows one script open at a time, it is only possible to debug the main `composer.iss`. To get round this a `debug.iss` file is created each time you compile `composer.iss`. This concatenates all the include files so that you can debug from this script. Remember that any changes you make here are only temporary to this file.


## Code Signing
The following instructions relate to [Certum CA](https://en.sklep.certum.pl), who provide cheap Open Source Code Signing certificates. Their SHA2 certificate lets you sign code with both SHA256 and SHA1 (for older OSs that do not understand SHA256).

Use Firefox to create your certificate because it allows you to backup and save your complete certificate (ie private and public keys). When your certificate has been activated:

* Go to the Certum certificate management page and click the *Install Online* button (the other options only download the public key).
* Go to *Options/Advanced/Certificates* in Firefox, select your certificate and click *Backup*, saving it with a `.p12` extension.
* Click on the backup file to import the certificate into the Windows Certificate Store - save it as the Current User, otherwise you will have to compile as an admin.

Adding it to the certificate store enables the use of the `/a` option in signtool, which finds the most suitable certificate (rather than specifying a file and password in the command params).
