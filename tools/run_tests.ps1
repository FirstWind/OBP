param(
  [string]$LazarusDir = "$env:LOCALAPPDATA\\Lazarus"
)

$ErrorActionPreference = "Stop"

$fpc = Join-Path $LazarusDir "fpc\\3.2.2\\bin\\x86_64-win64\\fpc.exe"
if (-not (Test-Path $fpc)) {
  throw "fpc not found: $fpc (run tools/bootstrap.ps1 first)"
}

$fu = @(
  "-Fusrc/domain/entities",
  "-Fusrc/domain/services",
  "-Fusrc/domain/policies",
  "-Fusrc/domain/value_objects",
  "-Fusrc/norms",
  "-Fusrc/infrastructure/config",
  "-Fusrc/infrastructure/db",
  "-Fusrc/audit",
  "-Fusrc/locks",
  "-Fusrc/services",
  "-Fusrc/tests"
)

$tests = @(
  "src/tests/TestDomainBasics.pas",
  "src/tests/TestNormsSchema.pas",
  "src/tests/TestNormsLoader.pas",
  "src/tests/TestScoreService.pas",
  "src/tests/TestScaleScore.pas",
  "src/tests/TestEvaluationService.pas",
  "src/tests/TestEvaluationRaw.pas",
  "src/tests/TestExercisesLoader.pas",
  "src/tests/TestExerciseGrades.pas",
  "src/tests/TestExerciseGradeIntegration.pas",
  "src/tests/TestGoldenRunner.pas",
  "src/tests/TestResultNormalizer.pas",
  "src/tests/TestPolicyDefaults.pas",
  "src/tests/TestParticipationStatusService.pas",
  "src/tests/TestCategoryPolicyService.pas",
  "src/tests/TestInfrastructureCompile.pas",
  "src/tests/TestRepositoriesCompile.pas",
  "src/tests/TestSessionRepositoriesCompile.pas",
  "src/tests/TestDbContextCompile.pas",
  "src/tests/TestPersonServiceCompile.pas",
  "src/tests/TestSessionServiceCompile.pas"
)

foreach ($t in $tests) {
  Write-Host "=== compile $t"
  & $fpc @fu $t | Out-Null
  if ($LASTEXITCODE -ne 0) { throw "compile failed: $t" }

  $exe = [System.IO.Path]::ChangeExtension($t, "exe")
  Write-Host "=== run $exe"
  & $exe | Out-Null
  if ($LASTEXITCODE -ne 0) { throw "test failed: $t" }
}

Write-Host "ALL_OK"
