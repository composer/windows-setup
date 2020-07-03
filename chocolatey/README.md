# Chocolatey

A package for Chocolatey: [Composer Setup](https://chocolatey.org/packages/composer)

Releases are built using Chocolatey's [automatic packaging](https://github.com/majkinetor/au)
module. This requires that Chocolatey itself is installed, together with the module (`cinst au`),
and that the release is available on [Github](https://github.com/composer/windows-setup/releases).

From an elevated Powershell, cd to the `chocolatey/composer` directory then run:

```powershell
./update.ps1
```

This will write any changes to the package files. To preview changes instead, run:

```powershell
$au_WhatIf = $true; ./update.ps1

```

If the package is already published on Chocolatey, use:

```powershell
 `$au_Force = $true; ./update.ps1`.
```

Note that the `$au_` variables are persisent and must be set to `$false` to be cleared.

To test the installation and uninstallation of the package, run:

```powershell
Test-Package -Install
Test-Package -Install -Parameters '...'
Test-Package -Uninstall
```

The previous release process can be found in the `chocolatey/local` directory. This can be be used
to test the Composer-Setup binary before it has been officially released on Github.
