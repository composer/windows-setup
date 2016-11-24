function Get-ComposerPackageData($fileDir) {
  $data = @{    
    packageName         = 'Composer-Setup'
    fileLocation        = ''
    installed           = false
    installedVersion    = ''
    uninstaller = ''
  }
  
  $exeName = (Get-ChildItem -Path "$fileDir" -Filter *.exe | Select-Object -First 1).Name
  $data.fileLocation = Join-Path $fileDir $exeName
  
  $appId = "{7315AF68-E777-496A-A6A2-4763A98ED35A}_is1"

  if (Get-ProcessorBits 64) {
    $hklm = "HKLM:\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Uninstall\$appId"
  } else {
    $hklm = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$appId"
  }

  $data.installed = Test-Path $hklm
  
  if ($data.installed) {
    $reg = Get-ItemProperty -Path "$hklm"
    $data.installedVersion = $reg."Inno Setup CodeFile: Version"
    $data.uninstaller = $reg.UninstallString -replace "['`"]","" 
  }
  
  return $data
}
