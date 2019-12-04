# Chocolatey

A package for chocolatey: [Composer Setup](https://chocolatey.org/packages/composer)

To create a `nupkg` file for submission, you need to have chocolatey itself installed and a release-built `Composer-Setup.x.x.x.exe` in the `src/release` directory.

From a command prompt (or Git Bash) type `powershell`, cd to the `chocolatey` directory then run:

```powershell
./release.ps1 -version x.x.x
```

This will write a `release.nuspec` (using the template file to update the version) and run `choco pack` to create the `composer.x.x.x.nupkg` package.

Note that both the `release.nuspec` and `composer.x.x.x.nupkg` files are git-ignored.
