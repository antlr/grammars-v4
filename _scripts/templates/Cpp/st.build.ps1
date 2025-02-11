# Generated from trgen <version>
function rmrf([string]$Path) {
    try {
        Remove-Item -Recurse -ErrorAction:Stop -force $Path
    } catch [System.Management.Automation.ItemNotFoundException] {
        # Ignore
        $Error.Clear()
    }
}

if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

rmrf('build')
New-Item -Path 'build' -ItemType Directory
Set-Location 'build'

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<endif>

echo HOME $HOME

<if(test.IsWindows)>
$(& cmake .. -G "Visual Studio 17 2022" -A x64 -DHOME=$HOME ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
<else>$(& cmake .. -DHOME=$HOME ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
<endif>
if($compile_exit_code -ne 0){
    Write-Host "Failed first cmake call $compile_exit_code."
    exit $compile_exit_code
}

<if(test.IsWindows)>
Select-String -Path .\Test.vcxproj -Pattern "tsx"
$(& cmake --build . --config Release ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
<else>
$make = which make
$(& $make ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
<endif>

if($compile_exit_code -ne 0){
    Write-Host "Failed second cmake call $compile_exit_code."
    exit $compile_exit_code
}

Set-Location '..'
exit $compile_exit_code
