param (  
  [string]$version = $(throw "-version parameter is required.")
)

$ErrorActionPreference = 'Stop';
$packageDir = "$(Split-Path -Parent $MyInvocation.MyCommand.Definition)"

# Set file path to the Composer-Setup...exe release
$releaseDir = (Join-Path $packageDir '..\src\release')
$exeName = "Composer-Setup.$version.exe"
$releaseExe = (Resolve-Path (Join-Path $releaseDir $exeName))

# Set input and output file paths
$templateFile = (Join-Path $packageDir "package.template")
$releaseNuspec = (Join-Path $packageDir  "release.nuspec")

# Replace values in template and write output file
$xml = Get-Content $templateFile -Raw -Encoding UTF8 
$text = ($xml -replace "{{Version}}", $version) -replace "{{ReleaseExe}}", $releaseExe
[IO.File]::WriteAllText($releaseNuspec, $text)

# Create the package
choco pack $releaseNuspec "--outputdirectory" $packageDir
