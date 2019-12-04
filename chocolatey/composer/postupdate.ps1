$ErrorActionPreference = 'Stop'

$root = Split-Path $MyInvocation.MyCommand.Definition

function ConvertEol($file) {
  Write-Host "Writing unix eol - $file"
  $text = [IO.File]::ReadAllText($file) -replace "`r`n", "`n"
  [IO.File]::WriteAllText($file, $text)
}

Write-Host 'Post update actions (non-au)'
Get-ChildItem $root\tools\*.exe | ForEach-Object { Write-Host "Removing - $_"; Remove-Item $_ -ea 0 }
ConvertEol("$root\composer.nuspec")
ConvertEol("$root\legal\VERIFICATION.txt")
ConvertEol("$root\tools\chocolateyinstall.ps1")


