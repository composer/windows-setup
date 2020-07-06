param (
  [string]$version = ''
)

$ErrorActionPreference = 'Stop';
$packageDir = Split-Path $MyInvocation.MyCommand.Definition

$packagefiles = Get-ChildItem $packageDir\*.nupkg
$packagefiles | ForEach-Object { Write-Host "Removing - $_"; Remove-Item $_ -ea 0 }

# Set input and output file paths
$templateFile = (Join-Path $packageDir "composer.template")
$nuspec = (Join-Path $packageDir  "composer.nuspec")

$buildDir = "$PSScriptRoot\..\..\builds"

# Replace values in template and write output file
if ($version -eq '') {
  $filename = "$buildDir\output\Composer-Setup.dev.exe"
  $baseVersion = (Get-Item "$filename").VersionInfo.ProductVersion.Trim()
  $version = "$baseVersion.$([DateTime]::UtcNow.ToString('yyyyMMdd'))"
} else {
  $filename = "$buildDir\release\Composer-Setup.$version.exe"
}

$xml = Get-Content $templateFile -Raw -Encoding UTF8
$text = ($xml -replace "{{Version}}", $version) -replace "{{Filename}}", $filename
[IO.File]::WriteAllText($nuspec, $text)

# Create the package
choco pack $nuspec "--outputdirectory" $packageDir
