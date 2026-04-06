# Get-Content run.ps1 | Write-Host

<if(test.IsWindows)>.venv\Scripts\Activate.ps1<else>.venv/bin/Activate.ps1<endif>
python Test.py $args
