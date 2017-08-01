# Chocolatey

A package for chocolatey: [https://chocolatey.org/packages/composer](Composer Setup)

To create a `nupkg` file for submission, you need to have chocolatey itself installed and a release-built `Composer-Setup.x.x.x.exe`.

From a command prompt (or Git Bash) type `powershell`, cd to this directory then run:

```powershell
./release.ps1 -version x.x.x
```

This will write a `release.nuspec` (using the template file) then call `choco pack` with this to create the `composer.x.x.x.nupkg` package.

Note that both the `release.nuspec` and `composer.x.x.x.nupkg` files are git-ignored.
