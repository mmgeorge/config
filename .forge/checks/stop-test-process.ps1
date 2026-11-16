# Native Windows termination avoids taskkill's potentially unbounded WMI traversal.
param([int] $TestProcessId)
$ErrorActionPreference = 'Stop'
try {
    $testProcess = [System.Diagnostics.Process]::GetProcessById($TestProcessId)
} catch [System.ArgumentException] {
    exit 0
}
try {
    $testProcess.Kill()
} catch [System.InvalidOperationException] {
    # The process exited between lookup and termination.
} finally {
    $testProcess.Dispose()
}
