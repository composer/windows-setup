# Composer-Setup development

## Tools
You will need to [download](http://www.jrsoftware.org/download.php/is-unicode.exe) and install the latest unicode version of Inno Setup Compiler. Make sure you include Inno Setup Preprocessor when you run the installer.

## Compiling
To compile the main `composer.iss` script you must create a file named `develop.iss` in the same directory, defining *SetupVersion*. For example:

```
#define SetupVersion "4.6.0"
```

Composer-Setup will be compiled with the following settings:

* output filename:    /Output/mysetup.exe
* exe version info:   as defined in SetupVersion

If you want to save the exe to a different directory, or give it a different name, you can use standard Inno Setup syntax:

```
#define SetupVersion "4.6.0"

[Setup]
OutputDir=myfolder
OutputBaseFilename=mycomposer-setup
```

To compile a release version, you must create a file named `release.iss` in the same directory and run it. It must look like this:

```
#define Release
#define SetupVersion "4.6.0"
#define SignTool "uniquename"
#define SignExe '"path\to\signtool.exe"''
#define SignSha1 "sign /a /fd sha1 /t http://timestamp.comodoca.com/authenticode"
#define SignSha2 "sign /a /fd sha256 /tr http://time.certum.pl/ /td sha256 /as"
#include "composer.iss"
```

The *SignTool* define value is the unique name you give to the IDE SignTool in Inno Setup, set from Tools/Options menu. Its value must be `uniquename=$p` (the $p is replaced by the actual parameters).

The *SignExe* define value is the full path to `signtool.exe` enclosed in inner doube-quotes and outer single-quotes, as shown.

Note that both the `develop.iss` and `release.iss` files are git-ignored.

## Debugging
Because the Inno IDE only allows one script open at a time, it is only possible to debug the main `composer.iss`. To get round this a `debug.iss` file is created each time you compile `composer.iss`. This concatenates all the include files so that you can debug from this script. Remember that any changes you make here are only temporary to this file.


## Code Signing
The following instructions relate to [Certum CA](https://en.sklep.certum.pl), who provide cheap Open Source Code Signing certificates. Their SHA2 certificate lets you sign code with both SHA256 and SHA1 (for older OSs that do not understand SHA256).

From 2017 Certum can only issue certificates using a card reader with their own proprietary software.
If you purchase their card-reader package, you can install the software from the USB stick, which  also contains helpful instruction manuals. The main interface is the `proCertumCardManager` application. Also included is the `cryptoCertumScanner` utility which lives in the system tray and runs at Start-up. I cannot really see what this does, so it is best to remove it from Start-up.

Use Chrome to create your certificate, from the _Activate Certificates_ page in your account. You will need to download and run a Certum-provided Java plugin from the browser in order to create the key pair and you must have the card-reader inserted as it stores this on the card.

Once the certificate has been activated, go to the _Manage Certificates_ page and select `Save binary` to download the `.cer` file. Open the `proCertumCardManager` and import this file on to the reader, then click to register the certificate in the system (this will put it in the Windows Certificate Store for the Current User, under Personal).

You will need to enter your PIN code for the card-reader several times when compiling.
