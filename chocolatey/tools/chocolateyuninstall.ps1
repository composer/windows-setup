$ErrorActionPreference = 'Stop';

$toolsDir = "$(Split-Path -Parent $MyInvocation.MyCommand.Definition)"
Import-Module (Join-Path $toolsDir 'common.ps1')
$data = Get-ComposerPackageData($toolsDir)

$uninstallArgs = @{
  packageName   = $data.packageName
  fileType      = 'EXE'
  file          = $data.uninstaller  
  silentArgs    = '/VERYSILENT'
  validExitCodes= @(0)
} 

if ($data.installed) {  
  Uninstall-ChocolateyPackage @uninstallArgs
}
