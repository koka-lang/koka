; Use 'iexpress /N util\windows-install.sed' from a Windows cmd prompt
; to generate a graphical windows executable installer as 'dist\install-koka-<version>-windows-x64.exe'.

[Strings]
Version=v2.1.7
CpuArch=x64
DisplayLicense=
FinishMessage=Installation of Koka is complete. Type 'koka' in a fresh 'cmd' prompt to run the Koka compiler.
FriendlyName=Koka Install
AppLaunched=cmd /c install.bat --iexpress
PostInstallCmd=<none>
AdminQuietInstCmd=
UserQuietInstCmd=
FILE0="install.bat"

[Version]
Class=IEXPRESS
SEDVersion=3

[Options]
PackagePurpose=InstallApp
ShowInstallProgramWindow=0
HideExtractAnimation=0
UseLongFileName=1
InsideCompressed=0
CAB_FixedSize=0
CAB_ResvCodeSigning=0
RebootMode=N
InstallPrompt=Installing Koka %Version%. Continue?
DisplayLicense=%DisplayLicense%
FinishMessage=%FinishMessage%
TargetName=dist\install-koka-%Version%-windows-%CpuArch%.exe
FriendlyName=%FriendlyName%
AppLaunched=%AppLaunched%
PostInstallCmd=%PostInstallCmd%
AdminQuietInstCmd=%AdminQuietInstCmd%
UserQuietInstCmd=%UserQuietInstCmd%
SourceFiles=SourceFiles

[SourceFiles]
SourceFiles0=util

[SourceFiles0]
%FILE0%=
