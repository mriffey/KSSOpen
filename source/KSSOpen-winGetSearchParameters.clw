

   MEMBER('KSSOpen.clw')                                       ! This is a MEMBER module

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

   INCLUDE('ABRESIZE.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
winGetSearchParameters PROCEDURE (*FindStrOptionsGroupType SearchOptions)

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
oHH           &tagHTMLHelp
ResultCode           BYTE(Level:Cancel)                    ! 
PatternQueue         QUEUE(MRUQueueType),PRE(PatternQueue) ! 
                     END                                   ! 
SearchPathQueue      QUEUE(MRUQueueType),PRE(SearchPathQueue) ! 
                     END                                   ! 
FileMaskQueue        QUEUE(MRUQueueType),PRE(FileMaskQueue) ! 
                     END                                   ! 
ExcludeMaskQueue     QUEUE(MRUQueueType),PRE(ExcludeMaskQueue) ! 
                     END                                   ! 
thisNewSearchAction     LIKE(glo:NewSearchAction)
szSearchPath            CSTRING(1025)
szFileListFilename      CSTRING(260)
szSearchStringFilename  CSTRING(260)

BUTTON:LOAD             EQUATE(1)
BUTTON:SEARCH           EQUATE(2)
BUTTON:SEARCHNEW        EQUATE(3)
!bFilenamesOnly       BOOL
Window               WINDOW('Search for...'),AT(,,320,170),FONT('Segoe UI',10),RESIZE,GRAY,HLP('SearchFor.htm'), |
  SYSTEM
                       PROMPT('&File'),AT(5,5),USE(?szSearchStringFilename:Prompt),HIDE,TRN
                       ENTRY(@S255),AT(35,5,262,10),USE(SearchOptions.szSearchStringFilename,,?szSearchStringFilename), |
  HIDE
                       BUTTON('...'),AT(170,5,14,10),USE(?cmdLookupSearchStringFilename),HIDE,TIP('Lookup Sear' & |
  'ch String Filename')
                       PROMPT('Fi&nd'),AT(5,5),USE(?szPattern:Prompt),TRN
                       COMBO(@s255),AT(35,5,262,10),USE(SearchOptions.szPattern,,?szPattern),VSCROLL,ALRT(MouseRight), |
  COLOR(COLOR:White),DROP(10),FORMAT('80L(2)|MS(1024)@s255@'),FROM(PatternQueue),TIP('Specified ' & |
  'text to be searched for.')
                       CHECK(' &Get Find Text from Specified File'),AT(35,17),USE(SearchOptions.bSearchStringsFromFile, |
  ,?bSearchStringsFromFile),TIP('Get search strings from the specified file')
                       CHECK(' &Case Sensitive'),AT(35,35),USE(SearchOptions.bCaseSensitive,,?bCaseSensitive),TIP('Specifies ' & |
  'that the search is to be case-sensitive.'),TRN
                       CHECK(' Exclude Commen&t Lines'),AT(170,35),USE(SearchOptions.bExcludeComments,,?bExcludeComments), |
  TIP('Prints only lines that do not contain a match inside a comment.'),TRN
                       CHECK(' Matc&h Pattern at Start of Line'),AT(35,45),USE(SearchOptions.bMatchPatternStartOfLine, |
  ,?bMatchPatternStartOfLine),TIP('Matches the pattern if at the beginning of a line.'),TRN
                       CHECK(' Match Pattern at &End of Line'),AT(170,45),USE(SearchOptions.bMatchPatternEndOfLine, |
  ,?bMatchPatternEndOfLine),TIP('Matches the pattern if at the end of a line.'),TRN
                       CHECK(' Exact &Match'),AT(35,55),USE(SearchOptions.bExactMatch,,?bExactMatch),TIP('Finds line' & |
  's that match exactly.'),TRN
                       CHECK(' E&xclude Matching Lines'),AT(170,55),USE(SearchOptions.bExcludeMatch,,?bExcludeMatch), |
  TIP('Prints only lines that do not contain a match.'),TRN
                       CHECK(' Use &Regular Expressions'),AT(35,65),USE(SearchOptions.bUseRegularExpressions,,?bUseRegularExpressions), |
  TIP('Uses Find strings as regular expressions.'),TRN
                       BUTTON,AT(143,65,12,10),USE(?cmdRegExHelp),ICON('help.ico'),FLAT
                       CHECK(' Filenames &Only'),AT(170,65),USE(SearchOptions.bFilenamesOnly,,?bFilenamesOnly),TIP('Prints onl' & |
  'y the names of files that do contain a match.'),TRN
                       CHECK(' Re&ad File List from Specified File'),AT(35,80),USE(SearchOptions.bFileListFromFile, |
  ,?bFileListFromFile),TIP('Get the list of files to search from the specified file'),TRN
                       PROMPT('F&ile'),AT(5,92),USE(?szFileListFilename:Prompt),HIDE,TRN
                       ENTRY(@S255),AT(35,92,262,10),USE(SearchOptions.szFileListFilename,,?szFileListFilename),HIDE
                       BUTTON('...'),AT(301,92,14,10),USE(?cmdLookupFileListFilename),HIDE,TIP('Lookup File Li' & |
  'st Filename')
                       PROMPT('&Paths'),AT(5,92),USE(?szSearchPath:Prompt),TRN
                       COMBO(@S255),AT(35,92,245,10),USE(SearchOptions.szSearchPath,,?szSearchPath),VSCROLL,ALRT(MouseRight), |
  COLOR(COLOR:White),DROP(10),FORMAT('80L(2)|MS(1024)@s255@'),FROM(SearchPathQueue),MSG('Search Path'), |
  TIP('Search Path Starting Folder')
                       BUTTON('...'),AT(284,92,14,10),USE(?cmdSelectFolder),TIP('Select New Search Path')
                       BUTTON('+'),AT(301,92,14,10),USE(?cmdAddFolder),TIP('Append to Existing Search Path')
                       CHECK(' Search S&ubdirectories'),AT(35,107),USE(SearchOptions.bSearchSubdirectories,,?bSearchSubdirectories), |
  TIP('Searches for matching files in the current directory and all subdirectories.'),TRN
                       PROMPT('Le&vels:'),AT(139,107),USE(?nLevels:Prompt),TRN
                       SPIN(@n3b),AT(165,107,25,10),USE(SearchOptions.nLevels,,?nLevels),RIGHT,COLOR(COLOR:White), |
  MSG('Subdirectory Levels to Search'),RANGE(0,255),TIP('Specify the number of subdirec' & |
  'tory levels to search.  If this value is blank then all subdirectories will be searched.')
                       PROMPT('&Files'),AT(5,122),USE(?szFileMask:Prompt),TRN
                       COMBO(@s255),AT(35,122,280,10),USE(SearchOptions.szFileMask,,?szFileMask),VSCROLL,ALRT(MouseRight), |
  COLOR(COLOR:White),DROP(10),FORMAT('80L(2)|MS(1024)@s255@'),FROM(FileMaskQueue),TIP('Specifies ' & |
  'a file or files to search.<0DH,0AH>The elements in the string may be delimited by ve' & |
  'rtical bars (|) ,  semicolons (;), or spaces.')
                       PROMPT('E&xclude'),AT(5,137),USE(?szExcludeMask:Prompt),TRN
                       COMBO(@s255),AT(35,137,280,10),USE(SearchOptions.szExcludeMask,,?szExcludeMask),VSCROLL,ALRT(MouseRight), |
  COLOR(COLOR:White),DROP(10),FORMAT('80L(2)|MS(1024)@s255@'),FROM(ExcludeMaskQueue),TIP('Specifies ' & |
  'a file or files to be excluded from the search.<0DH,0AH>The elements in the string m' & |
  'ay be delimited by vertical bars (|) ,  semicolons (;), or spaces.')
                       BUTTON('&Load Saved Results'),AT(35,152,70,14),USE(?cmdLoadSavedResults)
                       BUTTON('&Search'),AT(140,152,70,14),USE(?cmdSearch),DEFAULT
                       BUTTON('Search Ne&w Tab'),AT(245,152,70,14),USE(?cmdSearchNew)
                       CHECK(' Include files with binary content'),AT(170,80),USE(glo:DisableSlashP),TIP('Disables /' & |
  'P in FINDSTR.'),VALUE('1','0')
                     END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeNewSelection       PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop
  RETURN(ResultCode)

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
SetSearchPathTip  ROUTINE
   DATA
tipText     CSTRING(1024)
i           LONG
j           LONG

   CODE
      tipText = ''
      i = 1
      j = INSTRING(';',SearchOptions.szSearchPath,1,i)
      LOOP WHILE j
         tipText = tipText & SearchOptions.szSearchPath[i : j-1] & '<13,10>'
         IF j < LEN(SearchOptions.szSearchPath)
            i = j+1
            j = INSTRING(';',SearchOptions.szSearchPath,1,i)
         ELSE
            i = LEN(SearchOptions.szSearchPath)
            BREAK
         END
      END
      IF i < LEN(SearchOptions.szSearchPath)
         tipText = tipText & SearchOptions.szSearchPath[i : LEN(SearchOptions.szSearchPath)]
      END
     ?szSearchPath{PROP:Tip} = tipText

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

i                    LONG,AUTO
j                    LONG,AUTO
n                    LONG,AUTO
p                    LONG,AUTO
resultQueue          QUEUE(ff_:queue),PRE(RQ)
                     END
szDirPath            CSTRING(261)


dwProcess            DWORD
cbNeeded             DWORD
cProcesses           DWORD
lCopyCount           LONG
szProcessName        CSTRING(MAX_PATH)
hProcess             HANDLE
hMod                 HMODULE
thisProcessId        DWORD
hwnd                 HWND
dwPID                DWORD
szWindowCaption      CSTRING('Kwik Source Search')
RestoreSession       BOOL(FALSE)
SessionRestored      BOOL(FALSE)
  CODE
    
  GlobalErrors.SetProcedureName('winGetSearchParameters')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?szSearchStringFilename:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  IF glo:RestorePointChecked = FALSE
     DIRECTORY(resultQueue,svSpecialFolder.GetDir(SV:CSIDL_APPDATA) & '\Devuna\KSS\RestorePoint\*.*',ff_:DIRECTORY)
  
     !look for restore points
     LOOP i = RECORDS(resultQueue) TO 1 BY -1
        GET(resultQueue,i)
        CASE resultQueue.name
          OF '.' OROF '..'
             DELETE(resultQueue)
        ELSE
          IF NUMERIC(resultQueue.name)
             dwProcess = resultQueue.name
             szProcessName = '<<unknown>'
             hProcess = kcr_OpenProcess(BOR(PROCESS_QUERY_INFORMATION,PROCESS_VM_READ),FALSE,dwProcess)
             ! Get the process name.
             IF hProcess <> 0
                IF kcr_EnumProcessModules( hProcess, hMod, SIZE(hMod), cbNeeded)
                   kcr_GetModuleBaseName( hProcess, hMod, szProcessName, SIZE(szProcessName))
                   IF UPPER(szProcessName) = 'KSS.EXE'
                      DELETE(resultQueue)
                   END
                END
                kcr_CloseHandle(hProcess)
             ELSE
             END
          ELSE
             DELETE(resultQueue)
          END
       END
     END
  
     IF RECORDS(resultQueue) > 0
        CASE MESSAGE('Would you like to recover a System Restore Point?','System Restore Point Detected',ICON:Hand,BUTTON:YES+BUTTON:NO,BUTTON:YES)
          OF BUTTON:YES
             RestoreSession = TRUE
          OF BUTTON:NO
             RestoreSession = FALSE
        END
     END
  
     !delete restore points
     LOOP i = 1 TO RECORDS(resultQueue)
          GET(resultQueue,i)
          IF RestoreSession = TRUE
             glo:RestorePointFolder = svSpecialFolder.GetDir(SV:CSIDL_APPDATA) & '\Devuna\KSS\RestorePoint\' & CLIP(resultQueue.name)
             RestoreSession = FALSE
             SessionRestored = TRUE
          ELSE
             szDirPath = svSpecialFolder.GetDir(SV:CSIDL_APPDATA) & '\Devuna\KSS\RestorePoint\' & CLIP(resultQueue.name)
             SilentlyRemoveDirectory(szDirPath)
          END
     END
     glo:RestorePointChecked = TRUE
  END
  SELF.Open(Window)                                        ! Open window
  CASE glo:nDefaultSearchButton
    OF BUTTON:LOAD
       ?cmdLoadSavedResults{PROP:Default} = TRUE
    OF BUTTON:SEARCH
       ?cmdSearch{PROP:Default} = TRUE
    OF BUTTON:SEARCHNEW
       ?cmdSearchNew{PROP:Default} = TRUE
  ELSE
       ?cmdSearch{PROP:Default} = TRUE
  END
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  ?szPattern{PROP:LineHeight} = 10
  ?szSearchPath{PROP:LineHeight} = 10
  ?szFileMask{PROP:LineHeight} = 10
  ?szExcludeMask{PROP:LineHeight} = 10
  Do DefineListboxStyle
  Alert(AltKeyPressed)  ! WinEvent : These keys cause a program to crash on Windows 7 and Windows 10.
  Alert(F10Key)         !
  Alert(CtrlF10)        !
  Alert(ShiftF10)       !
  Alert(CtrlShiftF10)   !
  Alert(AltSpace)       !
!  WinAlertMouseZoom()
!  WinAlert(WE::WM_QueryEndSession,,Return1+PostUser)
  Window{PROP:MinHeight} = Window{PROP:Height}            ! Restrict the minimum window height
  Window{PROP:MaxHeight} = Window{PROP:Height}            ! Restrict the maximum window height
  Window{PROP:MinWidth}  = Window{PROP:Width}             ! Restrict the minimum window width
  Window{Prop:Alrt,255} = CtrlShiftP
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  INIMgr.Fetch('winGetSearchParameters',Window)            ! Restore window settings from non-volatile store
  Resizer.Resize                                           ! Reset required after window size altered by INI manager
  CorrectForOffscreen(Window)
  
  INIMgr.FetchQueue('Pattern Queue','Pattern',PatternQueue,PatternQueue.szValue)
  
  j = RECORDS(PatternQueue)
  LOOP i = 1 TO j
     GET(PatternQueue,i)
     ReplaceChr(PatternQueue.szValue,'§','''')
     PUT(PatternQueue)
  END
  
  INIMgr.FetchQueue('SearchPath Queue','SearchPath',SearchPathQueue,SearchPathQueue.szValue)
  INIMgr.FetchQueue('FileMask Queue','FileMask',FileMaskQueue,FileMaskQueue.szValue)
  INIMgr.FetchQueue('ExcludeMask Queue','ExcludeMask',ExcludeMaskQueue,ExcludeMaskQueue.szValue)
  thisNewSearchAction = glo:NewSearchAction
  SELF.SetAlerts()
  IF SearchOPtions.bSearchStringsFromFile = TRUE
     HIDE(?szPattern:Prompt,?szPattern)
     UNHIDE(?szSearchStringFilename:Prompt,?cmdLookupSearchStringFilename)
  ELSE
     HIDE(?szSearchStringFilename:Prompt,?cmdLookupSearchStringFilename)
     UNHIDE(?szPattern:Prompt,?szPattern)
  END
  
  IF SearchOptions.bSearchSubdirectories = FALSE
     DISABLE(?nLevels)
  END
  
  IF SearchOptions.bFileListFromFile = TRUE
     HIDE(?szSearchPath:Prompt,?szFileMask)
     UNHIDE(?szFileListFilename:Prompt,?cmdLookupFileListFilename)
  END
  oHH &= NEW tagHTMLHelp
  oHH.Init( 'kss.chm' )
  oHH.SetTopic('SearchFor.htm')
  DO SetSearchPathTip
  IF SessionRestored = TRUE
     ResultCode = Level:Program
     SELF.Response = RequestCompleted
     ReturnValue = Level:User
  END
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

i                    LONG,AUTO
j                    LONG,AUTO
n                    LONG,AUTO
p                    LONG,AUTO
tempValue            LIKE(PatternQueue.szValue)
quoteChar            CSTRING('''')
  CODE
!  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  !FindStrOptionsGroup = SearchOptions
  IF SELF.Opened
    INIMgr.Update('winGetSearchParameters',Window)         ! Save window data to non-volatile store
  END
  !INIMgr.Update('Search Options','szPattern',SearchOptions.szPattern)
  
  j = RECORDS(PatternQueue)
  LOOP i = 1 TO j
      GET(PatternQueue,i)
      ReplaceChr(PatternQueue.szValue,'''','§')
  !      n = LEN(PatternQueue.szValue)
  !      LOOP p = 1 TO n
  !        IF PatternQueue.szValue[p] = ''''
  !           PatternQueue.szValue[p] = CHR(VAL('''') + 128)
  !        END
  !      END
      PUT(PatternQueue)
  END
  
  INIMgr.UpdateQueue('Pattern Queue','Pattern',PatternQueue,PatternQueue.szValue)
  INIMgr.UpdateQueue('SearchPath Queue','SearchPath',SearchPathQueue,SearchPathQueue.szValue)
  INIMgr.UpdateQueue('FileMask Queue','Filemask',FileMaskQueue,FileMaskQueue.szValue)
  INIMgr.UpdateQueue('ExcludeMask Queue','ExcludeMask',ExcludeMaskQueue,ExcludeMaskQueue.szValue)
  GlobalErrors.SetProcedureName
  IF ~oHH &= NULL
    oHH.Kill()
    DISPOSE( oHH )
  END
  RETURN ReturnValue


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF Window{Prop:AcceptAll} THEN RETURN.
  PARENT.Reset(Force)
  IF SearchOptions.bExcludeComments
     SearchOptions.bExcludeMatch = FALSE
     DISABLE(?bExcludeMatch)
     DISPLAY(?bExcludeMatch)
  ELSE
     ENABLE(?bExcludeMatch)
  END
  IF SearchOptions.bExcludeMatch
     SearchOptions.bExcludeComments = FALSE
     DISABLE(?bExcludeComments)
     DISPLAY(?bExcludeComments)
  ELSE
     ENABLE(?bExcludeComments)
  END
  IF SearchOptions.bUseRegularExpressions = TRUE
     ENABLE(?cmdRegExHelp)
     SearchOptions.bMatchPatternStartOfLine = FALSE
     DISABLE(?bMatchPatternStartOfLine)
     DISPLAY(?bMatchPatternStartOfLine)
     SearchOptions.bMatchPatternEndOfLine = FALSE
     DISABLE(?bMatchPatternEndOfLine)
     DISPLAY(?bMatchPatternEndOfLine)
     SearchOptions.bExactMatch = FALSE
     DISABLE(?bExactMatch)
     DISPLAY(?bExactMatch)
  ELSE
     DISABLE(?cmdRegExHelp)
     ENABLE(?bMatchPatternStartOfLine)
     ENABLE(?bMatchPatternEndOfLine)
     ENABLE(?bExactMatch)
     !IF SearchOptions.bCaseSensitive           = TRUE |
     IF SearchOptions.bMatchPatternStartOfLine = TRUE |
     OR SearchOptions.bMatchPatternEndOfLine   = TRUE |
     OR SearchOptions.bExactMatch              = TRUE
        DISABLE(?bUseRegularExpressions)
     ELSE
        ENABLE(?bUseRegularExpressions)
     END
  END


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

szSelectedFolder     CSTRING(MAX_PATH)
szPath               CSTRING(MAX_PATH)
i                    LONG
j                    LONG
Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?szSearchStringFilename
      UPDATE()
      IF SearchOptions.szSearchStringFilename <> ''
         IF NOT EXISTS(SearchOptions.szSearchStringFilename)
            MESSAGE('Specified File not found!','Validation Error',ICON:Hand)
            SELECT(?szSearchStringFilename)
         END
      END
    OF ?cmdLookupSearchStringFilename
      ThisWindow.Update()
      szSearchStringFilename = SearchOptions.szSearchStringFilename
      IF FILEDIALOG('Select Search Text Filename',szSearchStringFilename,'*.*',FILE:KeepDir+FILE:LongName)
         SearchOptions.szSearchStringFilename = szSearchStringFilename
         DISPLAY(?szSearchStringFilename)
         POST(EVENT:Accepted,?szSearchStringFilename)
      END
    OF ?szPattern
      UPDATE()
      IF SearchOptions.szPattern <> ''
         j = RECORDS(PatternQueue)
         LOOP i = 1 TO j
            GET(PatternQueue,i)
            IF PatternQueue.szValue = SearchOptions.szPattern
               BREAK
            END
         END
         IF i > j
            IF j = MAXMRU
               GET(PatternQueue,j)
               DELETE(PatternQueue)
            END
            PatternQueue.szValue = SearchOptions.szPattern
            ADD(PatternQueue,1)
         ELSE
      !      IF glo:SyncPathWithPattern = TRUE
      !         IF LoadSearchOptions(SearchOptions) = NoError
      !            DISPLAY()
      !            POST(EVENT:Accepted,?szSearchPath,,1)
      !            POST(EVENT:Accepted,?szFileMask,,1)
      !            POST(EVENT:Accepted,?szExcludeMask,,1)
      !         END
      !      END
      !      DELETE(PatternQueue)
      !      PatternQueue.szValue = SearchOptions.szPattern
      !      IF glo:SyncPathWithPattern = TRUE
      !         SaveSearchOptions(SearchOptions)
      !      END
      !      ADD(PatternQueue,1)
      !      POST(EVENT:Accepted,?szSearchPath,,1)
         END
      END
    OF ?bSearchStringsFromFile
       IF SearchOptions.bSearchStringsFromFile = TRUE
          HIDE(?szPattern:Prompt,?szPattern)
          UNHIDE(?szSearchStringFilename:Prompt,?cmdLookupSearchStringFilename)
          SELECT(?szSearchStringFilename)
       ELSE
          HIDE(?szSearchStringFilename:Prompt,?cmdLookupSearchStringFilename)
          UNHIDE(?szPattern:Prompt,?szPattern)
          SELECT(?szPattern)
       END
    OF ?bCaseSensitive
      ThisWindow.Reset()
    OF ?bExcludeComments
      ThisWindow.Reset()
    OF ?bMatchPatternStartOfLine
      ThisWindow.Reset()
    OF ?bMatchPatternEndOfLine
      ThisWindow.Reset()
    OF ?bExactMatch
      ThisWindow.Reset()
    OF ?bExcludeMatch
      ThisWindow.Reset()
    OF ?bUseRegularExpressions
      ThisWindow.Reset()
    OF ?cmdRegExHelp
      ThisWindow.Update()
      oHH.ShowTopic('UsingRegularExpressions.htm')
    OF ?bFileListFromFile
      IF SearchOptions.bFileListFromFile = TRUE
         HIDE(?szSearchPath:Prompt,?szFileMask)
         UNHIDE(?szFileListFilename:Prompt,?cmdLookupFileListFilename)
      ELSE
         HIDE(?szFileListFilename:Prompt,?cmdLookupFileListFilename)
         UNHIDE(?szSearchPath:Prompt,?szFileMask)
      END
    OF ?szFileListFilename
      UPDATE()
      IF SearchOptions.szFileListFilename <> ''
         IF NOT EXISTS(SearchOptions.szFileListFilename)
            MESSAGE('Specified File not found!','Validation Error',ICON:Hand)
            SELECT(?szFileListFilename)
         END
      END
    OF ?cmdLookupFileListFilename
      ThisWindow.Update()
      szFileListFilename = SearchOptions.szFileListFilename
      IF FILEDIALOG('Select File List Filename',szFileListFilename,'*.*',FILE:KeepDir+FILE:LongName)
         SearchOptions.szFileListFilename = szFileListFilename
         DISPLAY(?szFileListFilename)
         POST(EVENT:Accepted,?szFileListFilename)
      END
    OF ?szSearchPath
      UPDATE()
      IF SearchOptions.szSearchPath <> ''
         !fix up search path by removing trailing '\'
         szSearchPath = ''
         i = 1
         j = INSTRING(';',SearchOptions.szSearchPath,1,i)
         LOOP WHILE j > 0
            IF j > i
               szPath = CLIP(LEFT(SearchOptions.szSearchPath[i : j-1]))
               IF szPath[LEN(szPath)] = '\'
                  szPath[LEN(szPath)] = '<0>'
               END
               IF szPath <> ''
                  szSearchPath = szSearchPath & szPath & ';'
               END
            END
            i = j + 1
            j = INSTRING(';',SearchOptions.szSearchPath,1,i)
         END
         j = LEN(SearchOptions.szSearchPath)
         szPath = CLIP(LEFT(SearchOptions.szSearchPath[i : j]))
         IF szPath[LEN(szPath)] = '\'
            szPath[LEN(szPath)] = '<0>'
         END
         IF szPath <> ''
            SearchOptions.szSearchPath = szSearchPath & szPath
         ELSE
            SearchOptions.szSearchPath = szSearchPath
         END
      
         IF ValidateSearchPath(SearchOptions.szSearchPath)
            SELECT(?szSearchPath)
         ELSE
            j = RECORDS(SearchPathQueue)
            LOOP i = 1 TO j
               GET(SearchPathQueue,i)
               IF SearchPathQueue.szValue = SearchOptions.szSearchPath
                  BREAK
               END
            END
            IF i > j
               IF j = MAXMRU
                  GET(SearchPathQueue,j)
                  DELETE(SearchPathQueue)
               END
               SearchPathQueue.szValue = SearchOptions.szSearchPath
               ADD(SearchPathQueue,1)
            ELSE
               DELETE(SearchPathQueue)
               SearchPathQueue.szValue = SearchOptions.szSearchPath
               ADD(SearchPathQueue,1)
            END
         END
      END
      DO SetSearchPathTip
    OF ?cmdSelectFolder
      ThisWindow.Update()
      szSelectedFolder = SearchOptions.szSearchPath
      IF FILEDIALOG('Select Search Folder',szSelectedFolder,'*.*',FILE:KeepDir+FILE:LongName+FILE:Directory)
         IF szSelectedFolder[LEN(szSelectedFolder)] = '\'
            szSelectedFolder[LEN(szSelectedFolder)] = '<0>'
         END
         SearchOptions.szSearchPath = szSelectedFolder
         DISPLAY(?szSearchPath)
         POST(EVENT:Accepted,?szSearchPath)
      END
    OF ?cmdAddFolder
      ThisWindow.Update()
      szSelectedFolder = SearchOptions.szSearchPath
      IF FILEDIALOG('Select Search Folder',szSelectedFolder,'*.*',FILE:KeepDir+FILE:LongName+FILE:Directory)
         IF szSelectedFolder[LEN(szSelectedFolder)] = '\'
            szSelectedFolder[LEN(szSelectedFolder)] = '<0>'
         END
         IF SearchOptions.szSearchPath = ''
            SearchOptions.szSearchPath = szSelectedFolder
         ELSE
            SearchOptions.szSearchPath = SearchOptions.szSearchPath & ';' & szSelectedFolder
         END
         DISPLAY(?szSearchPath)
         POST(EVENT:Accepted,?szSearchPath)
      END
    OF ?bSearchSubdirectories
      IF SearchOptions.bSearchSubdirectories = TRUE
         ENABLE(?nLevels:Prompt,?nLevels)
      ELSE
         DISABLE(?nLevels:Prompt,?nLevels)
      END
    OF ?szFileMask
      UPDATE()
      IF SearchOptions.szFileMask <> ''
         j = RECORDS(FileMaskQueue)
         LOOP i = 1 TO j
            GET(FileMaskQueue,i)
            IF FileMaskQueue.szValue = SearchOptions.szFileMask
               BREAK
            END
         END
         IF i > j
            IF j = MAXMRU
               GET(FileMaskQueue,j)
               DELETE(FileMaskQueue)
            END
            FileMaskQueue.szValue = SearchOptions.szFileMask
            ADD(FileMaskQueue,1)
         ELSE
            DELETE(FileMaskQueue)
            FileMaskQueue.szValue = SearchOptions.szFileMask
            ADD(FileMaskQueue,1)
         END
      END
    OF ?szExcludeMask
      UPDATE()
      IF SearchOptions.szExcludeMask <> ''
         j = RECORDS(ExcludeMaskQueue)
         LOOP i = 1 TO j
            GET(ExcludeMaskQueue,i)
            IF ExcludeMaskQueue.szValue = SearchOptions.szExcludeMask
               BREAK
            END
         END
         IF i > j
            IF j = MAXMRU
               GET(ExcludeMaskQueue,j)
               DELETE(ExcludeMaskQueue)
            END
            ExcludeMaskQueue.szValue = SearchOptions.szExcludeMask
            ADD(ExcludeMaskQueue,1)
         ELSE
            DELETE(ExcludeMaskQueue)
            ExcludeMaskQueue.szValue = SearchOptions.szExcludeMask
            ADD(ExcludeMaskQueue,1)
         END
      END
    OF ?cmdLoadSavedResults
      ThisWindow.Update()
      glo:NewSearchAction = thisNewSearchAction
      ResultCode = Level:User
      glo:nDefaultSearchButton = BUTTON:LOAD
      POST(EVENT:CloseWindow)
    OF ?cmdSearch
      ThisWindow.Update()
      IF SearchOptions.szPattern = ''
         SELECT(?szPattern)
      ELSIF SearchOptions.szSearchPath = ''
         SELECT(?szSearchPath)
      ELSIF SearchOptions.szFileMask = ''
         SELECT(?szFileMask)
      ELSE
         IF ValidateSearchPath(SearchOptions.szSearchPath)
            SELECT(?szSearchPath)
         ELSE
            IF glo:SyncPathWithPattern = TRUE
               SaveSearchOptions(SearchOptions)
            END
            glo:NewSearchAction = FALSE   !thisNewSearchAction
            ResultCode = Level:Benign
            glo:nDefaultSearchButton = BUTTON:SEARCH
            POST(EVENT:CloseWindow)
         END
      END
    OF ?cmdSearchNew
      ThisWindow.Update()
      IF SearchOptions.szPattern = ''
         SELECT(?szPattern)
      ELSIF SearchOptions.szSearchPath = ''
         SELECT(?szSearchPath)
      ELSIF SearchOptions.szFileMask = ''
         SELECT(?szFileMask)
      ELSE
         IF ValidateSearchPath(SearchOptions.szSearchPath)
            SELECT(?szSearchPath)
         ELSE
            IF glo:SyncPathWithPattern = TRUE
               SaveSearchOptions(SearchOptions)
            END
            glo:NewSearchAction = TRUE   !thisNewSearchAction
            ResultCode = Level:Benign
            glo:nDefaultSearchButton = BUTTON:SEARCHNEW
            glo:bSearchNewTabPressed = TRUE
            POST(EVENT:CloseWindow)
         END
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeEvent()
!  If event() = event:VisibleOnDesktop !or event() = event:moved
!    ds_VisibleOnDesktop()
!  end
     IF KEYCODE()=CtrlShiftP AND EVENT() = Event:PreAlertKey
       CYCLE
     END
     IF KEYCODE()=CtrlShiftP  
    
       CYCLE
     END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeFieldEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all field specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeFieldEvent()
  CASE FIELD()
  OF ?szPattern
    CASE EVENT()
    OF EVENT:AlertKey
       CASE KEYCODE()
         OF MouseRight
            MRUContextMenu(PatternQueue,?szPattern,'')
       END
    END
  OF ?szSearchPath
    CASE EVENT()
    OF EVENT:AlertKey
       CASE KEYCODE()
         OF MouseRight
            MRUContextMenu(SearchPathQueue,?szSearchPath,'')
       END
    END
  OF ?szFileMask
    CASE EVENT()
    OF EVENT:AlertKey
       CASE KEYCODE()
         OF MouseRight
            MRUContextMenu(FileMaskQueue,?szFileMask,'')
       END
    END
  OF ?szExcludeMask
    CASE EVENT()
    OF EVENT:AlertKey
       CASE KEYCODE()
         OF MouseRight
            MRUContextMenu(ExcludeMaskQueue,?szExcludeMask,'')
       END
    END
  END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeNewSelection PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all NewSelection events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeNewSelection()
    CASE FIELD()
    OF ?szPattern
      GET(PatternQueue,CHOICE(?szPattern))
      IF glo:SyncPathWithPattern = TRUE
         IF LoadSearchOptions(SearchOptions) = NoError
            DISPLAY()
            POST(EVENT:Accepted,?szSearchPath,,1)
            POST(EVENT:Accepted,?szFileMask,,1)
            POST(EVENT:Accepted,?szExcludeMask,,1)
         END
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE EVENT()
    OF EVENT:CloseDown
      if WE::CantCloseNow
        WE::MustClose = 1
        cycle
      else
        self.CancelAction = cancel:cancel
        self.response = requestcancelled
      end
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:OpenWindow
!        post(event:visibleondesktop)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window
  SELF.SetStrategy(?szSearchStringFilename:Prompt, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?szSearchStringFilename:Prompt
  SELF.SetStrategy(?szSearchStringFilename, Resize:FixLeft+Resize:FixTop, Resize:ConstantRight+Resize:LockHeight) ! Override strategy for ?szSearchStringFilename
  SELF.SetStrategy(?cmdLookupSearchStringFilename, Resize:FixRight+Resize:FixTop, Resize:LockSize) ! Override strategy for ?cmdLookupSearchStringFilename
  SELF.SetStrategy(?szPattern:Prompt, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?szPattern:Prompt
  SELF.SetStrategy(?szPattern, Resize:FixLeft+Resize:FixTop, Resize:ConstantRight+Resize:LockHeight) ! Override strategy for ?szPattern
  SELF.SetStrategy(?bSearchStringsFromFile, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?bSearchStringsFromFile
  SELF.SetStrategy(?bCaseSensitive, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?bCaseSensitive
  SELF.SetStrategy(?bExcludeComments, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?bExcludeComments
  SELF.SetStrategy(?bMatchPatternStartOfLine, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?bMatchPatternStartOfLine
  SELF.SetStrategy(?bMatchPatternEndOfLine, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?bMatchPatternEndOfLine
  SELF.SetStrategy(?bExactMatch, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?bExactMatch
  SELF.SetStrategy(?bExcludeMatch, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?bExcludeMatch
  SELF.SetStrategy(?bUseRegularExpressions, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?bUseRegularExpressions
  SELF.SetStrategy(?cmdRegExHelp, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?cmdRegExHelp
  SELF.SetStrategy(?bFilenamesOnly, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?bFilenamesOnly
  SELF.SetStrategy(?bFileListFromFile, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?bFileListFromFile
  SELF.SetStrategy(?szFileListFilename:Prompt, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?szFileListFilename:Prompt
  SELF.SetStrategy(?szFileListFilename, Resize:FixLeft+Resize:FixTop, Resize:ConstantRight+Resize:LockHeight) ! Override strategy for ?szFileListFilename
  SELF.SetStrategy(?cmdLookupFileListFilename, Resize:FixRight+Resize:FixTop, Resize:LockSize) ! Override strategy for ?cmdLookupFileListFilename
  SELF.SetStrategy(?szSearchPath:Prompt, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?szSearchPath:Prompt
  SELF.SetStrategy(?szSearchPath, Resize:FixLeft+Resize:FixTop, Resize:ConstantRight+Resize:LockHeight) ! Override strategy for ?szSearchPath
  SELF.SetStrategy(?cmdSelectFolder, Resize:FixRight+Resize:FixTop, Resize:LockSize) ! Override strategy for ?cmdSelectFolder
  SELF.SetStrategy(?cmdAddFolder, Resize:FixRight+Resize:FixTop, Resize:LockSize) ! Override strategy for ?cmdAddFolder
  SELF.SetStrategy(?bSearchSubdirectories, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?bSearchSubdirectories
  SELF.SetStrategy(?nLevels:Prompt, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?nLevels:Prompt
  SELF.SetStrategy(?nLevels, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?nLevels
  SELF.SetStrategy(?szFileMask:Prompt, Resize:FixLeft+Resize:FixTop, Resize:LockSize) ! Override strategy for ?szFileMask:Prompt
  SELF.SetStrategy(?szFileMask, Resize:FixLeft+Resize:FixTop, Resize:ConstantRight+Resize:LockHeight) ! Override strategy for ?szFileMask
  SELF.SetStrategy(?szExcludeMask:Prompt, Resize:FixLeft+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?szExcludeMask:Prompt
  SELF.SetStrategy(?szExcludeMask, Resize:FixLeft+Resize:FixTop, Resize:ConstantRight+Resize:LockHeight) ! Override strategy for ?szExcludeMask
  SELF.SetStrategy(?cmdLoadSavedResults, Resize:FixLeft+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?cmdLoadSavedResults
  SELF.SetStrategy(?cmdSearch, Resize:FixXCenter+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?cmdSearch
  SELF.SetStrategy(?cmdSearchNew, Resize:FixRight+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?cmdSearchNew

