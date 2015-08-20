@echo off

pushd %~dp0

.nuget\NuGet.exe update -self

.nuget\NuGet.exe install FAKE -ConfigFile .nuget\Nuget.Config -OutputDirectory packages -ExcludeVersion -Version 4.1.0

.nuget\NuGet.exe install xunit.runner.console -ConfigFile .nuget\Nuget.Config -OutputDirectory packages\FAKE -ExcludeVersion -Version 2.0.0
.nuget\NuGet.exe install nunit.runners -ConfigFile .nuget\Nuget.Config -OutputDirectory packages\FAKE -ExcludeVersion -Version 2.6.4

if not exist packages\SourceLink.Fake\tools\SourceLink.fsx (
  .nuget\nuget.exe install SourceLink.Fake -ConfigFile .nuget\Nuget.Config -OutputDirectory packages -ExcludeVersion
)
rem cls

set encoding=utf-8
if "%1"=="" (
	packages\FAKE\tools\FAKE.exe build.fsx RunTests
) else (
	packages\FAKE\tools\FAKE.exe build.fsx %*
)
popd
