$ErrorActionPreference = 'Stop';

$packageName= 'Composer-Setup'
$url        = 'https://getcomposer.org/Composer-Setup.exe'

$packageArgs = @{
  packageName   = $packageName
  fileType      = 'EXE'
  url           = $url   
  silentArgs   = '/VERYSILENT /SUPPRESSMSGBOXES'
  validExitCodes= @(0)
}

try {
  Install-ChocolateyPackage @packageArgs
} catch {
  
  if ($env:ChocolateyExitCode -eq '1') {
    Write-Host ""
    Write-Host "The installation failed. Your PHP or other settings are incorrect."
    Write-Host "  Use the --notsilent option to run the installer interactively."
    Write-Host ""
  }
}






















