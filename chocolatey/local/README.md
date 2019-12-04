# Local package

A local package is one that is created from a release from the `src/release` directory and is
intended to be used for testing/development, ie. before the version is actually released on
Github.

To create a `nupkg` file for testing, you need to have chocolatey itself installed and a release-built `Composer-Setup.x.x.x.exe` in the `src/release` directory.

From Powershell (run it elevated if you want to install the package as well), cd to the `chocolatey/local` directory then run:

```powershell
./release.ps1 -version x.x.x
```

This will write a `release.nuspec` (using the template file to update the version) and run `choco pack` to create the `composer.x.x.x.nupkg` package.

To test that it installs::

```powershell
choco install composer.x.x.x.nupkg
```

Note that both the `release.nuspec` and `composer.x.x.x.nupkg` files are git-ignored.
