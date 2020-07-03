# Local package

A local package is one that is created from either a release build (from the `builds/release` directory) or a
development build (from the `builds/output` directory) and is intended to be used for testing/development,
ie. before the version is actually released on Github.

## Package from release build
To create a `nupkg` file for testing, you need to have chocolatey itself installed and a release-built
`Composer-Setup.x.x.x.exe` in the `builds/release` directory.

From an elevated Powershell, cd to the `chocolatey/local` directory then run:

```powershell
./package.ps1 -Version x.x.x
```

This will write a `composer.nuspec` (using the template file to update the version) and run `choco pack`
to create the `composer.x.x.x.nupkg` package.

To test that it installs:

```powershell
# Note the period character at the end
choco install -y composer --source .

# Uninstall it with
choco uninstall -y composer
```

## Package from development build
Similar to the above, except without the -Version parameter to create the package:

```powershell
./package.ps1
```

Note that both the `composer.nuspec` and `composer.x.x.x.nupkg` files are git-ignored.
