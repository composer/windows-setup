# Chocolatey

A package for chocolatey: [Composer Setup](https://chocolatey.org/packages/composer)

To create a `nupkg` file for submission, you need to have chocolatey itself installed and a release-built `Composer-Setup.x.x.x.exe`.

From a command prompt (or Git Bash) type `powershell`, cd to _this_ directory then run:

```powershell
./release.ps1 -version x.x.x
```

This will write a `release.nuspec` (using the template file). Next run:

```powershell
choco pack
```

which takes the nuspec and creates the `composer.x.x.x.nupkg` package.

Note that both the `release.nuspec` and `composer.x.x.x.nupkg` files are git-ignored.
