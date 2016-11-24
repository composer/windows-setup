$ErrorActionPreference = 'Stop';

$toolsDir = "$(Split-Path -Parent $MyInvocation.MyCommand.Definition)"
Import-Module (Join-Path $toolsDir 'common.ps1')
$data = Get-ComposerPackageData($toolsDir)

$packageArgs = @{
  packageName   = $data.packageName
  fileType      = 'EXE'
  file          = $data.fileLocation  
  silentArgs    = '/VERYSILENT /SUPPRESSMSGBOXES'
  validExitCodes= @(0)
}

try {
  Install-ChocolateyInstallPackage @packageArgs
} catch {
  
  if ($env:ChocolateyExitCode -eq '1') {
    Write-Host ""
    Write-Host "*** IMPORTANT ***"
    Write-Host "The installation failed. Your PHP or other settings are incorrect."
    Write-Host "  Use the --notsilent option to run the installer interactively."
    Write-Host ""
  }
}
