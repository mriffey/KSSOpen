   PROGRAM


  INCLUDE('cwHH.INC'),ONCE
xFiles:TemplateVersion equate('3.25')
StringTheory:TemplateVersion equate('3.38')
!region Notices
! ================================================================================
! Notice : Copyright (C) 2017, Devuna
!          Distributed under the MIT License (https://opensource.org/licenses/MIT)
!
!    This file is part of Devuna-KwikSourceSearch (https://github.com/Devuna/Devuna-KwikSourceSearch)
!
!    Devuna-KwikSourceSearch is free software: you can redistribute it and/or modify
!    it under the terms of the MIT License as published by
!    the Open Source Initiative.
!
!    Devuna-KwikSourceSearch is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    MIT License for more details.
!
!    You should have received a copy of the MIT License
!    along with Devuna-KwikSourceSearch.  If not, see <https://opensource.org/licenses/MIT>.
! ================================================================================
!endregion Notices
   INCLUDE('ctKssKeyCodeName.inc'),ONCE
   INCLUDE('cwsynchc.inc'),ONCE
   INCLUDE('Windows.inc'),ONCE
   INCLUDE('svapi.inc'),ONCE
   INCLUDE('csciviewer.inc'),ONCE
   INCLUDE('cwhh.inc'),ONCE
INCLUDE('BRWEXT.INC'),ONCE
!EQUATES
HANDLE_FLAG_INHERIT              EQUATE(00000001h)
HANDLE_FLAG_PROTECT_FROM_CLOSE   EQUATE(00000002h)

LPBYTE                           EQUATE(LONG)
LPCTSTR                          EQUATE(LONG)
LPDWORD                          EQUATE(LONG)
LPOVERLAPPED                     EQUATE(LONG)
LPSECURITY_ATTRIBUTES            EQUATE(LONG)
LPTSTR                           EQUATE(LONG)
WORD                             EQUATE(SHORT)

BUFSIZE                          EQUATE(10240)   !(16384)

STD_INPUT_HANDLE                 EQUATE(-10)    !  The standard input device. Initially, this is the console input buffer, CONIN$.
STD_OUTPUT_HANDLE                EQUATE(-11)    !  The standard output device. Initially, this is the active console screen buffer, CONOUT$.
STD_ERROR_HANDLE                 EQUATE(-12)    !  The standard error device. Initially, this is the active console screen buffer, CONOUT$.

MAXPATH                          EQUATE(260)
MAXDRIVE                         EQUATE(3)
MAXDIR                          EQUATE(256)
MAXFILE                          EQUATE(256)
MAXEXT                          EQUATE(255)
MAXMRU                           EQUATE(20)

BS_SOLID                         EQUATE(0)
FLOODFILLSURFACE                 EQUATE(1)

!Well known registry keys
HKEY_CLASSES_ROOT                EQUATE(080000000h)
HKEY_CURRENT_USER                EQUATE(080000001h)
HKEY_LOCAL_MACHINE               EQUATE(080000002h)
HKEY_USERS                       EQUATE(080000003h)
HKEY_PERFORMANCE_DATA            EQUATE(080000004h)
HKEY_CURRENT_CONFIG              EQUATE(080000005h)
HKEY_DYN_DATA                    EQUATE(080000006h)

KEY_ALL_ACCESS                   EQUATE(0001F003Fh)
KEY_QUERY_VALUE                  EQUATE(1)
KEY_SET_VALUE                    EQUATE(2)
REG_OPTION_NON_VOLATILE          EQUATE(0)

LANG_USER_DEFAULT                EQUATE(1)
ERROR_FILE_NOT_FOUND             EQUATE(2)
ERROR_BROKEN_PIPE                EQUATE(109) !The pipe has been ended.
ERROR_IO_PENDING                 EQUATE(997)

EVENT:THREAD                     EQUATE(02100)
EVENT:THREADLAST                 EQUATE(02499)
EVENT:PROGRESS                   EQUATE(02500)
EVENT:PROGRESSLAST               EQUATE(02899)
EVENT:SHOWFINDHELP               EQUATE(03000)
EVENT:SHOWREPLACEHELP            EQUATE(03001)
EVENT:PLAYMACRO                  EQUATE(03002)
EVENT:PLAYSELECTIONS             EQUATE(03003)
EVENT:PLAYNEXTSELECTION          EQUATE(03004)
EVENT:MACROPLAYERCLOSED          EQUATE(03005)
EVENT:GOTONEWLINE                EQUATE(03006)

BUTTON:NextFolder                LONG
BUTTON:PreviousFolder            LONG
BUTTON:NextFile                  LONG
BUTTON:PreviousFile              LONG
BUTTON:NextLine                  LONG
BUTTON:PreviousLine              LONG
BUTTON:DeleteLine                LONG
BUTTON:DeleteFile                LONG
BUTTON:DeleteExtension           LONG
BUTTON:DeletePath                LONG
BUTTON:DeleteComments            LONG
BUTTON:DeleteLabels              LONG
BUTTON:DeleteCode                LONG
BUTTON:DeleteData                LONG
BUTTON:FindAndDelete             LONG
BUTTON:DeleteBuiltInClw          LONG !MR 20190101

CtrlShiftHook                    EQUATE(959)

MAX_PROGRESS                     EQUATE(100)

NOTIFY:CheckForUpdate            EQUATE(08100h)

MIN_PANELWIDTH                   EQUATE(10)
MIN_PANELHEIGHT                  EQUATE(48)

FORMAT_MESSAGE_FROM_SYSTEM       EQUATE(01000h)

SplitterOrientation:Vertical     EQUATE(0)
SplitterOrientation:Horizontal   EQUATE(1)

EVENT:FoldMarginClick            EQUATE(EVENT:Last)

COLOR_GRADIENTACTIVECAPTION      EQUATE(27)
COLOR_GRADIENTINACTIVECAPTION    EQUATE(28)
COLOR:GRADIENTACTIVECAPTION      EQUATE(08000001BH)
COLOR:GRADIENTINACTIVECAPTION    EQUATE(08000001CH)

PROCESS_TERMINATE                EQUATE(00001h)
PROCESS_CREATE_THREAD            EQUATE(00002h)
PROCESS_SET_SESSIONID            EQUATE(00004h)
PROCESS_VM_OPERATION             EQUATE(00008h)
!PROCESS_VM_READ                  EQUATE(00010h)
!PROCESS_VM_WRITE                 EQUATE(00020h)
PROCESS_DUP_HANDLE               EQUATE(00040h)
PROCESS_CREATE_PROCESS           EQUATE(00080h)
PROCESS_SET_QUOTA                EQUATE(00100h)
PROCESS_SET_INFORMATION          EQUATE(00200h)
!PROCESS_QUERY_INFORMATION        EQUATE(00400h)

WM_MOUSEWHEEL                    EQUATE(0020Ah)

CommentStyleClarion  EQUATE(0)
CommentStyleCpp      EQUATE(1)
CommentStyleVB       EQUATE(2)
CommentStylePython   EQUATE(3)
CommentStyleSQL      EQUATE(4)

RECT    GROUP,TYPE
left     SIGNED
top      SIGNED
right    SIGNED
bottom   SIGNED
        END

tagMONITORINFO GROUP,TYPE
cbSize            DWORD
rcMonitor         LIKE(RECT)
rcWork            LIKE(RECT)
dwFlags           DWORD
               END
ApplicationEvents             ITEMIZE(EVENT:User+3),PRE(EVENT)
CreateRestorePoint               EQUATE
                              END
LOGBRUSH    GROUP,TYPE
lbStyle       UNSIGNED
lbColor       ULONG
lbHatch       LONG
            END
STARTUPINFOTYPE   GROUP,TYPE
cb                   DWORD
lpReserved           LPTSTR
lpDesktop            LPTSTR
lpTitle              LPTSTR
dwX                  DWORD
dwY                  DWORD
dwXSize              DWORD
dwYSize              DWORD
dwXCountChars        DWORD
dwYCountChars        DWORD
dwFillAttribute      DWORD
dwFlags              DWORD
wShowWindow          WORD
cbReserved2          WORD
lpReserved2          LPBYTE
hStdInput            HANDLE
hStdOutput           HANDLE
hStdError            HANDLE
            END

STARTF_FORCEONFEEDBACK  EQUATE(000000040h)   !  Indicates that the cursor is in feedback mode for two seconds after CreateProcess is called. The Working in Background cursor is displayed (see the Pointers tab in the Mouse control panel utility).
                                             !  If during those two seconds the process makes the first GUI call, the system gives five more seconds to the process. If during those five seconds the process shows a window, the system gives five more seconds to the process to finish drawing the window.
                                             !  The system turns the feedback cursor off after the first call to GetMessage, regardless of whether the process is drawing.
STARTF_FORCEOFFFEEDBACK EQUATE(000000080h)   !  Indicates that the feedback cursor is forced off while the process is starting. The Normal Select cursor is displayed.
STARTF_PREVENTPINNING   EQUATE(000002000h)   !  Indicates that any windows created by the process cannot be pinned on the taskbar.
                                             !  This flag must be combined with STARTF_TITLEISAPPID.
STARTF_RUNFULLSCREEN    EQUATE(000000020h)   !  Indicates that the process should be run in full-screen mode, rather than in windowed mode.
                                             !  This flag is only valid for console applications running on an x86 computer.
STARTF_TITLEISAPPID     EQUATE(000001000h)   !  The lpTitle member contains an AppUserModelID. This identifier controls how the taskbar and Start menu present the application, and enables it to be associated with the correct shortcuts and Jump Lists. Generally, applications will use the SetCurrentProcessExplicitAppUserModelID and GetCurrentProcessExplicitAppUserModelID functions instead of setting this flag. For more information, see Application User Model IDs.
                                             !  If STARTF_PREVENTPINNING is used, application windows cannot be pinned on the taskbar. The use of any AppUserModelID-related window properties by the application overrides this setting for that window only.
                                             !  This flag cannot be used with STARTF_TITLEISLINKNAME.
STARTF_TITLEISLINKNAME  EQUATE(000000800h)   !  The lpTitle member contains the path of the shortcut file (.lnk) that the user invoked to start this process. This is typically set by the shell when a .lnk file pointing to the launched application is invoked. Most applications will not need to set this value.
                                             !  This flag cannot be used with STARTF_TITLEISAPPID.
STARTF_USECOUNTCHARS    EQUATE(000000008h)   !  The dwXCountChars and dwYCountChars members contain additional information.
STARTF_USEFILLATTRIBUTE EQUATE(000000010h)   !  The dwFillAttribute member contains additional information.
STARTF_USEHOTKEY        EQUATE(000000200h)   !  The hStdInput member contains additional information.
                                             !  This flag cannot be used with STARTF_USESTDHANDLES.
STARTF_USEPOSITION      EQUATE(000000004h)   !  The dwX and dwY members contain additional information.
STARTF_USESHOWWINDOW    EQUATE(000000001h)   !  The wShowWindow member contains additional information.
STARTF_USESIZE          EQUATE(000000002h)   !  The dwXSize and dwYSize members contain additional information.
STARTF_USESTDHANDLES    EQUATE(000000100h)   !  The hStdInput, hStdOutput, and hStdError members contain additional information.
                                             !  If this flag is specified when calling one of the process creation functions, the handles must be inheritable and the function's bInheritHandles parameter must be set to TRUE. For more information, see Handle Inheritance.
                                             !  If this flag is specified when calling the GetStartupInfo function, these members are either the handle value specified during process creation or INVALID_HANDLE_VALUE.
                                             !  Handles must be closed with CloseHandle when they are no longer needed.
                                             !  This flag cannot be used with STARTF_USEHOTKEY.

PROCESS_INFORMATION  GROUP,TYPE
hProcess                HANDLE
hThread                 HANDLE
dwProcessId             DWORD
dwThreadId              DWORD
                     END
ResultQueueType      QUEUE,TYPE
Path                   CSTRING(261)
Filename               CSTRING(261)
szExtension            CSTRING(MAXEXT+1)
LineNo                 LONG
ProcedureName          CSTRING(256)
Text                   CSTRING(1025)
SortName               CSTRING(261)
szSection              CSTRING(5)
Position               LONG
DeleteInstance         LONG
FileDate               LONG
FileTime               LONG
                     END
BookmarkQueueType    QUEUE,TYPE
LineNo                  LONG
                     END
STDOUTQueueType      QUEUE,TYPE
Buffer                 STRING(BUFSIZE)
                     END
FindStrOptionsGroupType    GROUP,TYPE
tabNumber                     LONG
bMatchPatternStartOfLine      BOOL
bMatchPatternEndOfLine        BOOL
bUseRegularExpressions        BOOL
bSearchSubdirectories         BOOL
nLevels                       BYTE
nCurrentLevel                 BYTE
bCaseSensitive                BOOL
bExactMatch                   BOOL
bExcludeMatch                 BOOL
bExcludeComments              BOOL
bSearchPressed                BOOL
szPattern                     CSTRING(1025)
szSearchPath                  CSTRING(1025)
szFileMask                    CSTRING(256)
szMatchesFound                CSTRING(256)
ResultQueue                   &ResultQueueType
UndoQueue                     &ResultQueueType
feqSearchProgress             LONG
lPointer                      LONG
bFilenamesOnly                BOOL
bFileListFromFile             BOOL
szFileListFilename            CSTRING(261)
bSearchStringsFromFile        BOOL
szSearchStringFilename        CSTRING(261)
szPropertyFile                CSTRING(33)
szExcludeMask                 CSTRING(256)
szListBoxFormat               CSTRING(256)
FindGroup                     LIKE(FindGrp)
szReplaceWith                 LIKE(FindGrp.What)
                           END
FindTextQueueType             QUEUE,TYPE
szFindText                       CSTRING(256)
                              END

SearchFindOptionsGroupType    GROUP,TYPE
szFindText                       CSTRING(256)
SearchLocation                   BYTE
DeleteCondition                  BYTE
MatchType                        BYTE
MatchCase                        BYTE
szText                           CSTRING(1024)
                              END

enumSearchLocation            ITEMIZE
Search:Path                     EQUATE(1)
Search:Filename                 EQUATE(2)
Search:Extension                EQUATE(3)
Search:Procedure                EQUATE(5)
Search:Text                     EQUATE(6)
                              END

enumDeleteCondition           ITEMIZE,PRE(Delete)
Contains                         EQUATE(0)
DoesNotContain                   EQUATE(1)
                              END
ClarionExtensionsQueueType    QUEUE,TYPE
FileExtension                    CSTRING(256)
                              END
MRUQueueType      QUEUE,TYPE
szValue              CSTRING(MAXPATH*4)
                  END
OVERLAPPED_TYPE                 group,type
Internal                          long
InternalHigh                      long
Offset                            long
OffsetHigh                        long
hEvent                            long
                                end
ReadFileParamsType   GROUP,TYPE
parentThread            LONG
parentPipe              LONG
parentQueue             &STDOUTQueueType
                     END
FileModeQueueType       QUEUE,TYPE
FileMode                   CSTRING(33)
Lexer                      CSTRING(33)
nLexer                     LONG
                        END

FileExtensionQueueType  QUEUE(FileModeQueueType),TYPE
Extension                  CSTRING(33)
                        END
ListFormatQueueType     QUEUE,TYPE
ColumnSequence             BYTE
ColumnName                 CSTRING(32)
ColumnFormat               CSTRING(64)
                        END
MacroSetQueueType  QUEUE,TYPE
MacroSetName         CSTRING(256)
                   END
ProcNameQueueType QUEUE,TYPE
LineNo               LONG
ProcedureName        CSTRING(256)
                  END
SectionQueueType  QUEUE,TYPE
lowLineNo            LONG
highLineNo           LONG
szSection            CSTRING(5)
                  END
OSVERSIONINFO          GROUP,TYPE
dwOSVersionInfoSize      DWORD
dwMajorVersion           DWORD
dwMinorVersion           DWORD
dwBuildNumber            DWORD
dwPlatformId             DWORD
szCSDVersion             BYTE,DIM(128)
                        END
gtRectCW             GROUP,TYPE
X                       SIGNED
Y                       SIGNED
W                       SIGNED
H                       SIGNED
                     END
SM_XVIRTUALSCREEN    EQUATE(76)
SM_YVIRTUALSCREEN    EQUATE(77)
SM_CXVIRTUALSCREEN   EQUATE(78)
SM_CYVIRTUALSCREEN   EQUATE(79)
SM_CMONITORS         EQUATE(80)
SM_SAMEDISPLAYFORMAT EQUATE(81)
_SHFILEOPSTRUCT      GROUP,TYPE
hwnd                    HWND
wFunc                   UNSIGNED !UINT
pFrom                   LONG     !PCZZTSTR
pTo                     LONG     !PCZZTSTR
fFlags                  WORD     !FILEOP_FLAGS
fAnyOperationsAborted   BOOL
hNameMappings           LPVOID
lpszProgressTitle       LONG     !PCTSTR
                     END

! Shell File Operations

FO_MOVE                    EQUATE(00001h)
FO_COPY                    EQUATE(00002h)
FO_DELETE                  EQUATE(00003h)
FO_RENAME                  EQUATE(00004h)

! SHFILEOPSTRUCT.fFlags and IFileOperation::SetOperationFlags() flag values

FOF_MULTIDESTFILES         EQUATE(00001h)
FOF_CONFIRMMOUSE           EQUATE(00002h)
FOF_SILENT                 EQUATE(00004h)  ! don't display progress UI (confirm prompts may be displayed still)
FOF_RENAMEONCOLLISION      EQUATE(00008h)  ! automatically rename the source files to avoid the collisions
FOF_NOCONFIRMATION         EQUATE(00010h)  ! don't display confirmation UI, assume "yes" for cases that can be bypassed, "no" for those that can not
FOF_WANTMAPPINGHANDLE      EQUATE(00020h)  ! Fill in SHFILEOPSTRUCT.hNameMappings
                                           ! Must be freed using SHFreeNameMappings
FOF_ALLOWUNDO              EQUATE(00040h)  ! enable undo including Recycle behavior for IFileOperation::Delete()
FOF_FILESONLY              EQUATE(00080h)  ! only operate on the files (non folders), both files and folders are assumed without this
FOF_SIMPLEPROGRESS         EQUATE(00100h)  ! means don't show names of files
FOF_NOCONFIRMMKDIR         EQUATE(00200h)  ! don't dispplay confirmatino UI before making any needed directories, assume "Yes" in these cases
FOF_NOERRORUI              EQUATE(00400h)  ! don't put up error UI, other UI may be displayed, progress, confirmations
FOF_NOCOPYSECURITYATTRIBS  EQUATE(00800h)  ! dont copy file security attributes (ACLs)
FOF_NORECURSION            EQUATE(01000h)  ! don't recurse into directories for operations that would recurse
FOF_NO_CONNECTED_ELEMENTS  EQUATE(02000h)  ! don't operate on connected elements ("xxx_files" folders that go with .htm files)
FOF_WANTNUKEWARNING        EQUATE(04000h)  ! during delete operation, warn if nuking instead of recycling (partially overrides FOF_NOCONFIRMATION)
FOF_NORECURSEREPARSE       EQUATE(08000h)  ! deprecated; the operations engine always does the right thing on FolderLink objects (symlinks, reparse points, folder shortcuts)
FOF_NO_UI                  EQUATE(00614h)  !(FOF_SILENT | FOF_NOCONFIRMATION | FOF_NOERRORUI | FOF_NOCONFIRMMKDIR) ! don't display any UI at all
CtrlShiftBar   EQUATE(988)

   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('CSIDL.EQU'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE
   INCLUDE('SPECIALFOLDER.INC'),ONCE
   INCLUDE('ABFUZZY.INC'),ONCE
   INCLUDE('UltimateDebug.INC'),ONCE 
   INCLUDE('xfiles.inc'),ONCE
   INCLUDE('StringTheory.Inc'),ONCE
!    Include('WinEvent.Inc'),Once
   INCLUDE('ABEIP.INC')
   INCLUDE('KCRQEIP.INC')
   INCLUDE('ABEIP.INC')
   INCLUDE('KCRQEIP.INC')
   INCLUDE('ABEIP.INC')
   INCLUDE('KCRQEIP.INC')
   INCLUDE('ABEIP.INC')
   INCLUDE('KCRQEIP.INC')

   MAP
     MODULE('KSSOpen-Main.CLW')
Main                   PROCEDURE   !
     END
     MODULE('KSSOpen-CreateChildProcess.CLW')
CreateChildProcess     PROCEDURE(*CSTRING szCmdLine)   !Create a child process that uses the previously created pipes for STDIN and STDOUT.
     END
     MODULE('KSSOpen-ReadFromPipe.CLW')
ReadFromPipe           FUNCTION(LONG feqSearchProgress, *BYTE bCancelFlag),LONG   !Read output from the child process's pipe for STDOUT
ReadFile               PROCEDURE(STRING ReadFileParams)   !Run this on it's own thread cause it block until input is available
     END
     MODULE('KSSOpen-FillResultQueue.CLW')
FillResultQueue        PROCEDURE(*ResultQueueType ResultQueue, LONG feqSearchProgress, *CSTRING szFixedFileMask, *CSTRING szFixedExcludeMask, *BYTE bCancelFlag, LONG progressCalls, *BOOL  bFilenamesOnly, *BOOL bFileListFromFile)   !Process STDOUT_Queue and store lines in ResultQueue
BrowseQueues           PROCEDURE(*ProcNameQueueType ProcNameQueue,*SectionQueueType SectionQueue)   !
     END
     MODULE('KSSOpen-BuildFileList.CLW')
BuildFileList          PROCEDURE(LONG pCurrentLevel, LONG pMaxLevel, *CSTRING pCurrentPath, *CSTRING pFileMask, *FILE:queue pFileQueue)   !
     END
     MODULE('KSSOpen-UserOptions.CLW')
UserOptions            FUNCTION(LONG MaxStyleIndex, *CSTRING szInstallProgram),BOOL   !User Options
     END
     MODULE('KSSOpen-Helpers.CLW')
ColourRGB              FUNCTION(LONG r, LONG g, LONG b),LONG   !
srcGetColorString      FUNCTION(LONG lColor),STRING   !
DoubleQuote            FUNCTION(*CSTRING InputString, *CSTRING OutputString, LONG bufferSize, *CSTRING quoteChar),LONG,PROC   !
srcGetRGBColorString   FUNCTION(LONG lColor),STRING   !
URLEncode              PROCEDURE(*CSTRING source, *CSTRING destination)   !
ReplaceTabs            PROCEDURE(*STRING szText)   !
ReplaceChr             PROCEDURE(*CSTRING szText, STRING strReplace, STRING strWIth)   !
ColourBrightness       FUNCTION(LONG colour),BYTE   !
winGetKeyAssignment    PROCEDURE(STRING strAssignmentText, *LONG lKeyCode)   !
SilentlyRemoveDirectory PROCEDURE(*CSTRING szDirPath)   !
     END
     MODULE('KSSOpen-FindStr.CLW')
FindStr                PROCEDURE(STRING strOptionsGroup)   !
     END
     MODULE('KSSOpen-GetFindDeleteOptions.CLW')
GetFindDeleteOptions   FUNCTION(*SearchFindOptionsGroupType SearchFindOptions),BOOL   !
     END
     MODULE('KSSOpen-winGetSearchParameters.CLW')
winGetSearchParameters FUNCTION(*FindStrOptionsGroupType SearchOptions),BOOL   !
     END
     MODULE('KSSOpen-ValidateSearchPath.CLW')
ValidateSearchPath     FUNCTION(*CSTRING szSearchPath),BOOL   !
     END
     MODULE('KSSOpen-SelectSendToCommand.CLW')
SelectSendToCommand    FUNCTION(),BOOL   !
     END
     MODULE('KSSOpen-MatchWithoutComment.CLW')
MatchWithoutComment    FUNCTION(*CSTRING szText, *CSTRING szPattern, BYTE bMatchMode, * CSTRING szExtension),LONG   !
GetCommentStyle        FUNCTION(*CSTRING szExtension),LONG   !
     END
     MODULE('KSSOpen-SaveResults.CLW')
SaveResults            FUNCTION(FindStrOptionsGroupType FindStrOptions, *CSTRING szSendToFilename),BOOL   !
     END
     MODULE('KSSOpen-MRUContextMenu.CLW')
MRUContextMenu         PROCEDURE( MRUQueueType MRUQueue, LONG feqControl, STRING strDefault)   !
     END
     MODULE('KSSOpen-GetRunningCopyCount.CLW')
GetRunningCopyCount    FUNCTION(),LONG   !
ApplicationIsRunning   FUNCTION(),LONG   !
     END
     MODULE('KSSOpen-LoadLexerProperties.CLW')
LoadLexerProperties    FUNCTION(csciControl sciControl, STRING lexer),LONG,PROC   !
GetPropertyFileLexer   FUNCTION(*CSTRING szPropertyFile),STRING   !
     END
     MODULE('KSSOpen-LoadFileExtensionQueue.CLW')
LoadFileExtensionQueue FUNCTION(FileModeQueueType FileModeQueue),STRING   !
GetLexerNumber         FUNCTION(*CSTRING szLexer),LONG   !
     END
     MODULE('KSSOpen-PropertyEditor.CLW')
PropertyEditor         FUNCTION(*CSTRING szPropertyFile, LONG MaxStyleIndex),BOOL,PROC   !Edit special properties files
     END
     MODULE('KSSOpen-ListBoxFormatter.CLW')
ListBoxFormatter       FUNCTION(ListFormatQueueType pListFormatQueue),STRING   !
     END
     MODULE('KSSOpen-RestorePointIO.CLW')
CreateRestorePoint     FUNCTION(FindStrOptionsGroupType pFindStrOptions, *CSTRING szFilename),LONG,PROC   !
LoadRestorePoint       FUNCTION(FindStrOptionsGroupType pFindStrOptions, <*CSTRING szRestorePointFile>),LONG,PROC   !
     END
     MODULE('KSSOpen-winShowMatchSummary.CLW')
winShowMatchSummary    FUNCTION(ResultQueueType ResultQueue, ResultQueueType UndoQueue, BOOL bCaseSensitive, BOOL bRegularExpression, *CSTRING szPattern),STRING   !
     END
     MODULE('KSSOpen-MacroPlayer.CLW')
MacroPlayer            PROCEDURE   !
winGetMacroSetName     PROCEDURE(*CSTRING MacroSetName)   !
winLoadMacroSet        FUNCTION(MacroSetQueueType MacroSetQueue),LONG   !
     END
     MODULE('KSSOpen-SavePatternToFile.CLW')
SavePatternToFile      FUNCTION(*CSTRING szPattern),STRING   !
     END
     MODULE('KSSOpen-ResultQueueHasLocations.CLW')
ResultQueueHasLocations FUNCTION(*ResultQueueType ResultQueue),BOOL   !
     END
     MODULE('KSSOpen-SaveSearchOptions.CLW')
SaveSearchOptions      FUNCTION(*FindStrOptionsGroupType SearchOptions),LONG,PROC   !
LoadSearchOptions      FUNCTION(*FindStrOptionsGroupType SearchOptions),LONG,PROC   !
     END
     MODULE('KSSOpen-CorrectForOffScreen.CLW')
CorrectForOffScreen    PROCEDURE(*WINDOW xChild)   !
     END
     MODULE('KSSOpen-winReplaceInResults.CLW')
winReplaceInResults    FUNCTION(*FindGrp FindGroup, *CSTRING szReplaceWith, *ResultQueueType ResultQueue),LONG   !
     END
     MODULE('KSSOpen-RestorePointTimer.CLW')
RestorePointTimer      PROCEDURE   !
     END
     INCLUDE('CWUTIL.INC'),ONCE
     !INCLUDE('CLIB.CLW'),ONCE
     INCLUDE('kcrapifnc.inc'),ONCE
      MODULE('psapi')
         kcr_EnumProcesses(*DWORD pProcessIds, DWORD cb, *DWORD pBytesReturned),BOOL,PASCAL,RAW,PROC,NAME('EnumProcesses')
         kcr_EnumProcessModules(HANDLE hProcess, *HMODULE hModule, DWORD cb, *DWORD lpcbNeeded),BOOL,PASCAL,RAW,PROC,NAME('EnumProcessModules')
         kcr_GetModuleBaseName(HANDLE hProcess, HMODULE hModule, *CSTRING BaseName, DWORD nSize),DWORD,RAW,PASCAL,PROC,NAME('GetModuleBaseNameA')
      END
     MODULE('Shell32')
        kcr_SHFileOperation(*_SHFILEOPSTRUCT pSHFILEOPSTRUCT),LONG,PASCAL,RAW,PROC,NAME('SHFileOperationA')
     END

DebugABCGlobalInformation_kss PROCEDURE()                                                ! DEBUG Prototype
DebugABCGlobalVariables_kss PROCEDURE()                                                  ! DEBUG Prototype

       MyOKToEndSessionHandler(long pLogoff),long,pascal
       MyEndSessionHandler(long pLogoff),pascal
   END

  include('StringTheory.Inc'),ONCE
!To Do List
!VB         - done
!Python     - done
!Perl
!Ruby
!PHPScript
!CSS        - done
!SQL
!MSSQL
!MySQL
!PowerShell
!Batch
!VBScript   - done
!
 INCLUDE('VersionMe.clw'),ONCE  ! Used by VersionMe.exe to inject a version # into the code for screens/reports 
! 
!glo:szVersion        CSTRING('')!('2017.08.05.0<0>{19}')
glo:DisableSlashP    LONG
glo:szNewVersion     CSTRING(32)
glo:szNoDownloadVersion CSTRING(32)
glo:szNewFeatures    CSTRING(4096)
glo:szInstallProgram CSTRING(260)
glo:szNull           CSTRING(2)
glo:bInstallOnExit   BOOL
RegisteredTo         CSTRING(29)
cs                   CriticalSection
CurrentSearch        LONG
g_hChildStd_IN_Rd    HANDLE,THREAD
g_hChildStd_IN_Wr    HANDLE,THREAD
g_hChildStd_OUT_Rd   HANDLE,THREAD
g_hChildStd_OUT_Wr   HANDLE,THREAD
piProcInfo           LIKE(PROCESS_INFORMATION),THREAD
CancelFlag           BYTE,THREAD
glo:findstrCommandLine CSTRING(261)
glo:SplitterOrientation BYTE(1)
glo:SplitX           LONG
glo:SplitY           LONG
glo:PromptForEditor  BYTE
glo:bAllExtensions   BYTE
glo:szEditorCommand  CSTRING(261)
glo:NewSearchAction  BYTE
glo:bUseAssociation  BYTE(0)
glo:ResultListFontName CSTRING(32)
glo:ResultListFontSize BYTE
glo:ResultListForeColor LONG
glo:ResultListFontStyle LONG
glo:ApplicationColor LONG
glo:ToolbarColor     LONG
glo:BookmarkBack     LONG
glo:SelectedBack     LONG
glo:ViewerStyles     LIKE(COLORGROUPTYPE)
glo:Zoom             LONG
glo:szClarionHelpFile CSTRING(260)
glo:szDefaultPropertyFile CSTRING(65)
glo:bHotSpotsEnabled BOOL
glo:bAutoSave        BOOL
glo:bShowAutoSaveWarning BOOL
glo:bHideResultsPanel BYTE
glo:bHideEditPanel   BYTE
glo:sqlProperties    CSTRING(33)
glo:bSearchNewTabPressed BYTE
glo:nDefaultSearchButton BYTE(2)
glo:nDeleteWarningCount LONG
glo:bDontShowSubdirectoryWarning BOOL
glo:SyncPathWithPattern BOOL
glo:PlusKey          LONG(06Bh)
glo:OldPlusKey       LONG(06Bh)
glo:MinusKey         LONG(06Dh)
glo:OldMinusKey      LONG(06Dh)
szRoot               CSTRING(49)
glo:MainWindow       &WINDOW
glo:AllowMultipleInstances BYTE(FALSE)
glo:AutoSizeResultColumns BYTE(FALSE)
glo:RestorePointTimerInterval LONG
glo:RestorePointChecked BYTE(0)
glo:RestorePointFolder CSTRING(261)
glo:RestorePointTimerThread LONG
MacroQueue           QUEUE,PRE(MacroQueue)
feqButton              LONG
szField1               CSTRING(261)
szField2               CSTRING(261)
szField3               CSTRING(261)
mark                   BOOL
                     END
EditorQueue          QUEUE(MRUQueueType),PRE(EditorQueue)
                     END
FileExtensionQueue   QUEUE(FileExtensionQueueType),PRE(fxq)
                     END
ClarionExtensionsQueue QUEUE(ClarionExtensionsQueueType),PRE(CEQ)
                     END
ThreadQueue          QUEUE,PRE(ThreadQ)
ID                     LONG
ThreadNo               LONG
TargetProcessHandle    LONG
tabNumber              LONG
lClock                 LONG
                     END
STDOUT_Queue         QUEUE(STDOUTQueueType),PRE(SQ),THREAD
                     END
SilentRunning        BYTE(0)                               ! Set true when application is running in 'silent mode'

!region File Declaration
!endregion

AsciiFilename        CSTRING(261),THREAD
AsciiFile            FILE,DRIVER('ASCII'),NAME(AsciiFilename),CREATE,PRE(ASCII),THREAD
                        RECORD
Buffer                     STRING(2048)
                        END
                     END
SyncOptionsFilename     CSTRING(261),THREAD
SyncOptions             FILE,DRIVER('TOPSPEED'),NAME(SyncOptionsFilename),CREATE,PRE(SYNC),THREAD
PatternKey                 KEY(SYNC:szPattern),NOCASE
                           RECORD
bMatchPatternStartOfLine      BOOL
bMatchPatternEndOfLine        BOOL
bUseRegularExpressions        BOOL
bSearchSubdirectories         BOOL
nLevels                       BYTE
bCaseSensitive                BOOL
bExactMatch                   BOOL
bExcludeMatch                 BOOL
bExcludeComments              BOOL
szPattern                     CSTRING(1025)
szSearchPath                  CSTRING(1025)
szFileMask                    CSTRING(256)
bFilenamesOnly                BOOL
bFileListFromFile             BOOL
szFileListFilename            CSTRING(261)
bSearchStringsFromFile        BOOL
szSearchStringFilename        CSTRING(261)
szExcludeMask                 CSTRING(256)
                           END
                        END
CLSkel_TplVersion    CSTRING('v17.09.06.1, Released 2017-09-06')
udb_Settings                GROUP,PRE(udb_Settings)
DebugOff                            BOOL(FALSE)
DebugPrefix                         STRING(20)
SaveToFile                          BOOL(FALSE)
ASCIIFileName                       STRING(100)
DebugNoCR                           BYTE(FALSE)
LineWrap                            BYTE(TRUE)
ModuleName                          STRING(100)
AppName                             STRING(100)
Modified                            STRING(26)
                            END
UD                   UltimateDebug
WE::MustClose       long
WE::CantCloseNow    long

FuzzyMatcher         FuzzyClass                            ! Global fuzzy matcher
GlobalErrorStatus    ErrorStatusClass,THREAD
GlobalErrors         ErrorClass                            ! Global error manager
INIMgr               INIClass                              ! Global non-volatile storage manager
svSpecialFolder        SpecialFolder
GlobalRequest        BYTE(0),THREAD                        ! Set when a browse calls a form, to let it know action to perform
GlobalResponse       BYTE(0),THREAD                        ! Set to the response from the form
VCRRequest           LONG(0),THREAD                        ! Set to the request from the VCR buttons

!Dictionary           CLASS,THREAD
!Construct              PROCEDURE
!Destruct               PROCEDURE
!                     END


  CODE
  GlobalErrors.Init(GlobalErrorStatus)
  FuzzyMatcher.Init                                        ! Initilaize the browse 'fuzzy matcher'
  FuzzyMatcher.SetOption(MatchOption:NoCase, 1)            ! Configure case matching
  FuzzyMatcher.SetOption(MatchOption:WordOnly, 0)          ! Configure 'word only' matching
  svSpecialFolder.CreateDirIn(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS' )
  INIMgr.Init(svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS') & '\' & 'kss.INI', NVD_INI)
!  DctInit()
    udb_Settings.DebugOff      =  0
    udb_Settings.DebugPrefix   =  '!'
    udb_Settings.SaveToFile    =  0
    udb_Settings.ASCIIFileName =  'DebugLog.txt'
    udb_Settings.DebugNoCR     =  1
    udb_Settings.LineWrap      =  0
    UD.INIT('global',udb_settings)
  SYSTEM{PROP:Icon} = '~kss.ico'
  SyncOptionsFilename = svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS\pattern.dat')
  INIMgr.Fetch('Global','AllowMultipleInstances',glo:AllowMultipleInstances)
  !IF glo:AllowMultipleInstances = FALSE
  !   OPEN(SyncOptions)
  !ELSE
     OPEN(SyncOptions,ReadWrite+DenyNone)
  !END
  IF ERRORCODE()
     CREATE(SyncOptions)
  !   IF glo:AllowMultipleInstances = FALSE
  !      OPEN(SyncOptions)
  !   ELSE
        OPEN(SyncOptions,ReadWrite+DenyNone)
  !   END
     IF ERRORCODE()
        IF ApplicationIsRunning()
           INIMgr.Kill                                              ! Destroy INI manager
           FuzzyMatcher.Kill                                        ! Destroy fuzzy matcher
           RETURN
        ELSE
           MESSAGE(ERROR() & ' encountered opening SyncOptions','Unexpected Error',ICON:HAND)
        END
     END
  END
    !ds_SetOKToEndSessionHandler(address(MyOKToEndSessionHandler))
    !ds_SetEndSessionHandler(address(MyEndSessionHandler))
  Main
  INIMgr.Update
  CLOSE(SyncOptions)
  IF glo:bInstallOnExit = TRUE
     kcr_ShellExecute(0,0,glo:szInstallProgram,0,glo:szNull,1)
  END
  INIMgr.Kill                                              ! Destroy INI manager
  FuzzyMatcher.Kill                                        ! Destroy fuzzy matcher
    
! ------ winevent -------------------------------------------------------------------
MyOKToEndSessionHandler procedure(long pLogoff)
OKToEndSession    long(TRUE)
! Setting the return value OKToEndSession = FALSE
! will tell windows not to shutdown / logoff now.
! If parameter pLogoff = TRUE if the user is logging off.

  code
  return(OKToEndSession)

! ------ winevent -------------------------------------------------------------------
MyEndSessionHandler procedure(long pLogoff)
! If parameter pLogoff = TRUE if the user is logging off.

  code
 
!BOE: DEBUG Global
DebugABCGlobalInformation_kss PROCEDURE()
                     
  CODE
    
  
  RETURN

DebugABCGlobalVariables_kss PROCEDURE()
  CODE

  
  RETURN
!EOE: DEBUG Global
