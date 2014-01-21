# Microsoft Developer Studio Project File - Name="XeffortLite" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=XeffortLite - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "XeffortLite.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "XeffortLite.mak" CFG="XeffortLite - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "XeffortLite - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "XeffortLite - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""$/Xeffort/Samples/XeffortLite", QGDAAAAA"
# PROP Scc_LocalPath "."
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "XeffortLite - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt /winapp
# ADD F90 /compile_only /define:"XLITE" /include:"Release/" /nologo /warn:nofileopt /winapp
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o /win32 "NUL"
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o /win32 "NUL"
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:windows /machine:I386

!ELSEIF  "$(CFG)" == "XeffortLite - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /debug:full /include:"Debug/" /nologo /warn:nofileopt /winapp
# ADD F90 /compile_only /debug:full /define:"XLITE" /include:"Debug/" /libs:dll /nologo /warn:nofileopt /winapp
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o /win32 "NUL"
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o /win32 "NUL"
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib comdlg32.lib shell32.lib ole32.lib dflogm.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# SUBTRACT LINK32 /verbose

!ENDIF 

# Begin Target

# Name "XeffortLite - Win32 Release"
# Name "XeffortLite - Win32 Debug"
# Begin Source File

SOURCE=.\Main.f90
DEP_F90_MAIN_=\
	".\Debug\xflogm.mod"\
	".\Debug\XFTFILE.MOD"\
	".\Debug\XFTGDI.MOD"\
	".\Resource.fd"\
	
# End Source File
# Begin Source File

SOURCE=.\XFLOGM.F90
# End Source File
# Begin Source File

SOURCE=.\XFTFile.f90
DEP_F90_XFTFI=\
	".\Debug\XSTRINGS.MOD"\
	
NODEP_F90_XFTFI=\
	".\Debug\XFTAPI.mod"\
	".\Debug\XFTTYPES.mod"\
	".\Debug\XFTWND.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\XftGDI.f90
NODEP_F90_XFTGD=\
	".\Debug\XFTAPI.mod"\
	".\Debug\XFTTYPES.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\XeffortLite.rc
# End Source File
# Begin Source File

SOURCE=.\XFTStrings.f90
# End Source File
# End Target
# End Project
