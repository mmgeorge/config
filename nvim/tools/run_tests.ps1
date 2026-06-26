# DiffReview test harness: load check + command-open check + every tests/diff_review/*.lua,
# tallied by process exit code (tests cquit on failure, qa! on success), plus the whitespace gate.
# Usage: pwsh -File nvim/tools/run_tests.ps1 [test_name_substring]
param([string]$Filter = "")

$ErrorActionPreference = "Stop"
$repo = Split-Path (Split-Path $PSScriptRoot -Parent) -Parent
Set-Location $repo

# Truncate the diff-review perf log at the start of each run. Profiling is on by default and the
# log balloons across runs (hundreds of MB), which slows the perf-heavy tests into timeouts.
try {
  $cache = (& nvim --headless -u NONE --cmd "set shadafile=NONE" -c "lua io.write(vim.fn.stdpath('cache'))" -c "qa!" 2>$null) -join ""
  if ($cache) {
    $perfLog = Join-Path $cache "diff-review-perf.log"
    if (Test-Path $perfLog) { Clear-Content $perfLog -ErrorAction SilentlyContinue }
  }
} catch {}

$common = @("--headless", "-i", "NONE", "--cmd", "set shadafile=NONE", "-u", "nvim/init.lua")

function Invoke-Nvim {
  param([string[]]$ExtraArgs, [int]$TimeoutSec = 45)
  # Run headless nvim under a hard per-test timeout so a stuck wait loop can never stall the
  # suite. A timed-out test is tree-killed and reported as failure (exit 124), never left to
  # orphan. ProcessStartInfo.ArgumentList quotes each arg correctly (spaces/=) like native splatting.
  $psi = [System.Diagnostics.ProcessStartInfo]::new()
  $psi.FileName = "nvim"
  foreach ($a in ($common + $ExtraArgs)) { [void]$psi.ArgumentList.Add($a) }
  $psi.RedirectStandardError = $true
  $psi.RedirectStandardOutput = $true
  $psi.UseShellExecute = $false
  $proc = [System.Diagnostics.Process]::Start($psi)
  $errTask = $proc.StandardError.ReadToEndAsync()
  [void]$proc.StandardOutput.ReadToEndAsync()  # drain stdout to avoid pipe-buffer deadlock
  if ($proc.WaitForExit($TimeoutSec * 1000)) {
    $code = $proc.ExitCode
    $err = $errTask.Result
  } else {
    try { $proc.Kill($true) } catch {}
    $code = 124
    $err = "TIMEOUT after ${TimeoutSec}s"
  }
  return @{ Code = $code; Err = $err }
}

$pass = 0; $fail = 0; $failures = @()

if ($Filter -eq "") {
  Write-Host "== load check ==" -ForegroundColor Cyan
  $r = Invoke-Nvim @("-c", "lua vim.loader.enable(false); require('diff_review').setup(); require('diff_review.integrations.commit'); require('plugins.diff_review')", "-c", "qa!")
  if ($r.Code -eq 0) { $pass++; Write-Host "PASS load" -ForegroundColor Green }
  else { $fail++; $failures += "load"; Write-Host "FAIL load ($($r.Code)): $($r.Err)" -ForegroundColor Red }

  Write-Host "== command-open check ==" -ForegroundColor Cyan
  $r = Invoke-Nvim @("-c", "lua vim.loader.enable(false)", "-c", "GitStatus", "-c", "lua assert(vim.bo.filetype == 'GitStatus', 'GitStatus buffer did not open')", "-c", "qa!")
  if ($r.Code -eq 0) { $pass++; Write-Host "PASS command-open" -ForegroundColor Green }
  else { $fail++; $failures += "command-open"; Write-Host "FAIL command-open ($($r.Code)): $($r.Err)" -ForegroundColor Red }
}

$tests = Get-ChildItem "nvim/tests/diff_review/*.lua" | Sort-Object Name
foreach ($t in $tests) {
  if ($Filter -ne "" -and $t.BaseName -notlike "*$Filter*") { continue }
  $r = Invoke-Nvim @("-c", "lua vim.loader.enable(false)", "-S", "nvim/tests/diff_review/$($t.Name)")
  if ($r.Code -eq 0) {
    $pass++; Write-Host "PASS $($t.BaseName)" -ForegroundColor Green
  } else {
    $fail++; $failures += $t.BaseName
    Write-Host "FAIL $($t.BaseName) ($($r.Code))" -ForegroundColor Red
    if ($r.Err) { Write-Host ($r.Err.Trim() -split "`n" | Select-Object -First 6 | Out-String).Trim() -ForegroundColor DarkGray }
  }
}

if (Test-Path "nvim.log") { Remove-Item "nvim.log" -Force }

$ws = & git diff --check -- nvim/lua/diff_review nvim/lua/plugins/diff_review.lua nvim/tests/diff_review 2>&1
$wsClean = ($LASTEXITCODE -eq 0)

Write-Host ""
Write-Host "TESTS PASS=$pass FAIL=$fail" -ForegroundColor $(if ($fail -eq 0) { "Green" } else { "Red" })
if ($failures.Count -gt 0) { Write-Host "FAILURES: $($failures -join ', ')" -ForegroundColor Red }
Write-Host "WHITESPACE: $(if ($wsClean) { 'CLEAN' } else { 'DIRTY' })" -ForegroundColor $(if ($wsClean) { "Green" } else { "Red" })
if (-not $wsClean) { Write-Host $ws -ForegroundColor DarkGray }
