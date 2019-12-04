param (
  [string]$version = $(throw "-version parameter is required.")
)

$ErrorActionPreference = 'Stop';
$packageDir = "$(Split-Path -Parent $MyInvocation.MyCommand.Definition)"

# Set input and output file paths
$templateFile = (Join-Path $packageDir "package.template")
$releaseNuspec = (Join-Path $packageDir  "release.nuspec")

# Replace values in template and write output file
$title = 'Composer Setup'
$xml = Get-Content $templateFile -Raw -Encoding UTF8
$text = ($xml -replace "{{Title}}", $title) -replace "{{Version}}", $version
[IO.File]::WriteAllText($releaseNuspec, $text)

# Create the package
choco pack $releaseNuspec "--outputdirectory" $packageDir
