$ErrorActionPreference = 'Stop'

$toolsPath = Split-Path $MyInvocation.MyCommand.Definition

$fileName = 'Composer-Setup.5.1.0.exe'
$silentArgs = @('/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART')
$pp = Get-PackageParameters

if ($pp.Dev) {
  $silentArgs += '/DEV="{0}"' -f $pp.Dev.Trim("`"'")
}

if ($pp.Php) {
  $silentArgs += '/PHP="{0}"' -f $pp.Php.Trim("`"'")
}

$packageArgs = @{
  packageName    = 'composer'
  fileType       = 'exe'
  file           = Get-Item $toolsPath\$fileName
  checksum       = '80422FEF0EA310C0F565D6028F34010EEEBC96A4EDEC3175156D85F4F0EEEF10'
  checksumType   = 'sha256'
  silentArgs     = $silentArgs -join ' '
  softwareName   = 'composer*'
  validExitCodes = @(0)
}

try {
  Install-ChocolateyInstallPackage @packageArgs
  Get-ChildItem $toolsPath\*.exe | ForEach-Object { Remove-Item $_ -ea 0; if (Test-Path $_) { Set-Content "$_.ignore" '' } }
} catch {

  if ($env:ChocolateyExitCode -eq '1') {
    Write-Host ""
    Write-Host "*** IMPORTANT ***"
    Write-Host "The installation failed. Your PHP or other settings are incorrect."
    Write-Host "  Use the --notsilent option to run the installer interactively."
    Write-Host ""
  }
}
