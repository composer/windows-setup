$ErrorActionPreference = 'Stop';

$toolsDir = "$(Split-Path -Parent $MyInvocation.MyCommand.Definition)"
$setup = (Get-ChildItem -Path "$toolsDir" -Filter *.exe | Select-Object -First 1).Name

$packageArgs = @{
  packageName   = 'composer'
  fileType      = 'EXE'
  file          = Join-Path $toolsDir $setup
  silentArgs    = '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART'
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
