# Composer-Setup development

## Tools
You will need to [download](http://www.jrsoftware.org/download.php/is-unicode.exe) and install the latest unicode version of Inno Setup Compiler. Make sure you include Inno Setup Preprocessor when you run the installer.

## Compiling
To compile the main `composer.iss` script you must create a file named `develop.iss` in the same directory, defining *SetupVersion*. For example:

```
#define SetupVersion "4.8.0"
```

Composer-Setup will be compiled with the following settings:

* output filename:    /Output/mysetup.exe
* exe version info:   as defined in SetupVersion

If you want to save the exe to a different directory, or give it a different name, you can use standard Inno Setup syntax:

```
#define SetupVersion "4.8.0"

[Setup]
OutputDir=myfolder
OutputBaseFilename=mycomposer-setup
```

To compile a release version, you must create a file named `release.iss` in the same directory and run it. It must look like this:

```
#define Release
#define SetupVersion "4.8.0"
#define SignTool "uniquename"
#define SignExe '"path\to\signtool.exe"''
#define SignSha1 "sign /a /fd sha1 /t http://timestamp.comodoca.com/authenticode"
#define SignSha2 "sign /a /fd sha256 /tr http://time.certum.pl/ /td sha256 /as"
#include "composer.iss"
```

The *SignTool* define value is the unique name you add to the Inno Setup IDE, using the Tools menu. Click *Configure Sign Tools*, then *Add* to enter the unique name, then enter `$p` as the command (the $p is replaced by the actual parameters).

The *SignExe* define value is the full path to `signtool.exe` enclosed in inner doube-quotes and outer single-quotes, as shown above. This Microsoft tool is in the Windows SDK and can be found using the Visual Studio Developer Command Prompt and typing `where signtool`.

The *Sign*-prefixed defines are used to create the [Setup]: SignTool directive in `build.iss`.

Note that both the `develop.iss` and `release.iss` files are git-ignored.

## Debugging
Because the Inno IDE only allows one script open at a time, it is only possible to debug the main `composer.iss`. To get round this a `debug.iss` file is created each time you compile `composer.iss`. This concatenates all the include files so that you can debug from this script. Remember that any changes you make here are only temporary to this file.


## Code Signing
The following instructions relate to [Certum CA](https://en.sklep.certum.pl), who provide cheap Open Source Code Signing certificates. Their SHA2 certificate lets you sign code with both SHA256 and SHA1 (for older OSs that do not understand SHA256).

From 2017 Certum can only issue certificates using a smart card reader. If you purchase their card-reader package, you can install the software from the USB stick, which  also contains helpful instruction manuals. The main interface is the `proCertumCardManager` application. Also included is the `cryptoCertumScanner` utility which runs at Start-up and is added to the system tray. This does not seem to be needed for simple code signing, so it is best to remove it from Start-up.

Once you have bought the certificate you need to activate it. You need the Java runtime installed on your system and a web browser that allows you to run a `.jnlp` web start file (Chrome, for example). You may also have to set the appropriate level of security from the Java Control Panel item to allow you to run it.

Insert the card-reader, go to the _Activate Certificates_ page in your account and follow the instructions to generate and store a key-pair on the card reader. You then need to verify your email and provide identification documentation.

Once the certificate has been activated, go to the _Manage Certificates_ page and select `Save binary` to download the `.cer` file (note that this works in any browser). Open the `proCertumCardManager`, read the card and choose your profile - probably the Common profile. Click _Import certificate_ to add this file to the reader.

To register the certificate in the Windows Certificate Store, click _Show certificate details_ then _Install Certificate_. Choose Current User for the store location and let the system automatically choose the actual store (which will be Personal). This makes life easy when it comes to invoking `signtool.exe`.

To install an already activated certificate from the card reader, open the `proCertumCardManager`, select the certificate then click to register it, as above.

You will need to enter your PIN code for the card-reader several times when compiling.
