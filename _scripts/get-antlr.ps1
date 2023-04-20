function Get-StringHash {
    param (
        $String,
        $Algorithm = "SHA256"
    )
    if ($String -is [String]) {
        $ms = [System.IO.MemoryStream]::new()
        $sw = [System.IO.StreamWriter]::new($ms)
        $sw.Write($String)
        $sw.Flush()
        $ms.Position = 0
        return Get-FileHash -InputStream $ms -Algorithm $Algorithm
    }
    else {
        $ms = [System.IO.MemoryStream]::new($String)
        return Get-FileHash -InputStream $ms -Algorithm $Algorithm
    }
}

function Test-Response {
    param (
        [Parameter(ValueFromPipeline)]
        $Response,
        [switch]
        $Hashed = $false,
        [switch]
        $ToString = $false
    )
    if ($Response.StatusCode -ne 200) {
        throw "Invoke-WebRequest failed, ${Response.StatusCode}"
    }
    $ret = $Response.Content
    if ($ToString) {
        $ret = [System.Text.Encoding]::ASCII.GetString($ret)
    }
    if ($Hashed) {
        $expectSha1 = $Response.Headers.'X-Checksum-SHA1'[0]
        $actualSha1 = Get-StringHash $ret -Algorithm SHA1
        if ($expectSha1 -ine $actualSha1.Hash) {
            throw "Hash mismatch"
        }
    }
    return $ret
}

# configs
$mvnRepo = "https://repo1.maven.org/maven2"
$retry = 6
$retryInterval = 10

# download metadata
$resp = Invoke-WebRequest "$mvnRepo/org/antlr/antlr4/maven-metadata.xml" -MaximumRetryCount $retry -RetryIntervalSec $retryInterval | Test-Response -Hashed 
$respSha512 = Invoke-WebRequest "$mvnRepo/org/antlr/antlr4/maven-metadata.xml.sha512" -MaximumRetryCount $retry -RetryIntervalSec $retryInterval | Test-Response -Hashed -ToString
$actualSha512 = Get-StringHash $resp -Algorithm SHA512
if ($respSha512 -ine $actualSha512.Hash) {
    throw "Hash mismatch"
}

# parse metadata
$metadata = [xml]$resp
$versioning = $metadata.metadata.versioning
$latestVersion = $versioning.latest
$versions = $versioning.versions.version

# choose version
$ver = $args[0]
Write-Host "Version desired is $ver"
if (!$ver) {
    throw "Must set a version"
}
if ($ver -eq "latest") {
    $ver = $latestVersion
}
else {
    if ($versions -notcontains $ver ) {
        throw "Version $ver not exist"
    }
}

# download jar itself
$filename = "./antlr4-$ver-complete.jar"
$resp = Invoke-WebRequest "$mvnRepo/org/antlr/antlr4/$ver/antlr4-$ver-complete.jar" -OutFile $filename -PassThru -MaximumRetryCount $retry -RetryIntervalSec $retryInterval | Test-Response -Hashed
$respSha1 = Invoke-WebRequest "$mvnRepo/org/antlr/antlr4/$ver/antlr4-$ver-complete.jar.sha1" -MaximumRetryCount $retry -RetryIntervalSec $retryInterval | Test-Response -Hashed
$fileSha1 = Get-FileHash $filename -Algorithm SHA1
if ($respSha1 -ine $fileSha1.Hash) {
    throw "Hash mismatch"
}

# print jar location
(Resolve-Path $filename).Path