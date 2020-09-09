# Composer-Setup development

## Tools
You will need to [download](http://www.jrsoftware.org/isdl.php) and install the latest version of
Inno Setup Compiler (minimum requirement is version 6.0.2)

## Compiling
For development purposes open and run the main `composer.iss` script. This will create a
`Composer-Setup.dev.exe` file in the `output` directory.

To compile a release version, copy `release.example.iss` to `release.iss` and follow the
instructions for the *SignTool* and *SignExe* defines. Update `version.iss` then run `release.iss`.
This will create a `Composer-Setup.x.x.x.exe` file in the `release` directory.

Note that `release.iss` is git-ignored.

## Versioning
Only the _major_ and _minor_ version parts are used for a release, with a zero _patch_ element. The
current release version is stored in `version.iss`.

## Debugging
Because the Inno IDE only allows one script open at a time, it is only possible to debug the
main `composer.iss`. To get round this a `debug.iss` file is created each time you compile
`composer.iss`. This concatenates all the include files so that you can debug from this script.
Remember that any changes you make here are only temporary to this file.

## Code Signing
The following instructions relate to [Certum CA](https://en.sklep.certum.pl), who provide cheap
Open Source Code Signing certificates. Their SHA2 certificate lets you sign code with both SHA256
and SHA1 (for older OSs that do not understand SHA256). Note that their certificate renewal process
does not work for the Open Source Code Signing product, so you have to buy a new certificate instead.

From 2017 Certum can only issue certificates using a smart card reader. If you purchase their
card-reader package, you can install the software from the USB stick, which  also contains helpful
instruction manuals. The main interface is the `proCertumCardManager` application. Also included is
the `cryptoCertumScanner` utility which runs at Start-up and is added to the system tray. This does
not seem to be needed for simple code signing, so it is best to remove it from Start-up.

Once you have bought the certificate you need to activate it. You need the Java runtime installed
on your system and a web browser that allows you to run a `.jnlp` web start file (Chrome, for
example). You may also have to set the appropriate level of security from the Java Control Panel
item to allow you to run it.

Insert the card-reader, go to the _Activate Certificates_ page in your account and follow the
instructions. It can all be a bit clunky but the general idea is that you:

* Select to store a key-pair on the card reader.
* Download a `CertumCryptoAgent_**.jnlp` file.
* Run the file (you will need to enter your PIN code).

You should eventually get to a page that lets you add various certificate details. Put your name in
the Organization field and fill in the Country, Locality (use your town rather than your address)
and State fields. You will then be sent a verification email and another requesting identification
documentation.

Once the certificate has been fully activated (you will be notified by email when the certificate
has been issued), go to the _Manage Certificates_ page and select `Save binary` to download the
`.cer` file (note that this works in any browser). Open the `proCertumCardManager`, read the card
and choose your profile - probably the Common profile. Then click _Import certificate_ to add this
file to the reader.

To register the certificate in the Windows Certificate Store, click _Show certificate details_ then
_Install Certificate_. Choose Current User for the store location and let the system automatically
choose the actual store (which will be Personal). This makes life easy when it comes to invoking
`signtool.exe`.

To install an already activated certificate from the card reader, open the `proCertumCardManager`,
select the certificate then click to register it, as above.

You will need to enter your PIN code for the card-reader several times when compiling.
