# Params:
#   0       working directory (the selected folder)
#   1...    composer params

cd $args[0]
$success = $?
$cmd = $args[1]

if ($success -eq 'True') {
    if ($cmd -ne '--open--') {
        $x, $params = $args
        Write-Host
        Write-Host Command: composer $params
        Write-Host
        composer.bat $params
    }
}

if ($cmd -eq '--open--') {
    $result = 'y'
} else {
    Write-Host
    $result = Read-Host 'Continue [y] ?'
}

if ($result.ToLower() -eq 'y') {
    Write-Host
    powershell -noexit -nologo
}
