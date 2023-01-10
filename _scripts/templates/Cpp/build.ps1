# Template generated code from trgen <version>
function rmrf([string]$Path) {
    try {
        Remove-Item -Recurse -ErrorAction:Stop $Path
    } catch [System.Management.Automation.ItemNotFoundException] {
        # Ignore
        $Error.Clear()
    }
}

rmrf('build')
New-Item -Path 'build' -ItemType Directory
Set-Location 'build'

$(& cmake .. <cmake_target> ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
if($compile_exit_code -ne 0){
    Write-Host "Failed first cmake call $compile_exit_code."
    exit $compile_exit_code
}

$(& <if(os_win)>cmake --build . --config Release<else>$(MAKE)<endif> ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
if($compile_exit_code -ne 0){
    Write-Host "Failed second cmake call $compile_exit_code."
    exit $compile_exit_code
}
exit $compile_exit_code
