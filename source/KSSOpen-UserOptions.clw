

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

   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

!!! <summary>
!!! Generated from procedure template - Window
!!! User Options
!!! </summary>
UserOptions PROCEDURE (LONG MaxStyleIndex, *CSTRING szInstallProgram)

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
STYLE:NORMAL      EQUATE(0)
STYLE:BOLD        EQUATE(1)
STYLE:ITALIC      EQUATE(2)
STYLE:BOLDITALIC  EQUATE(3)
oHH           &tagHTMLHelp
keyCodeName       ctKssKeyCodeName

PropertyFileQueue          QUEUE
FileName                      CSTRING(MAXFILE)
FilenameStyle                 LONG
FilePath                      CSTRING(MAXPATH)
IsReadOnly                    BOOL
                           END
bReadOnly                  BOOL
szSearchFolder             CSTRING(MAXPATH)
loc:SelectedBack           LONG
loc:sSelectedBackColor     STRING(30)
loc:BookmarkBack           LONG
loc:sBookmarkBackColor     STRING(30)
loc:ApplicationColor       LONG
loc:sApplicationColor      STRING(30)
loc:ToolbarColor           LONG
loc:sToolbarColor          STRING(30)
loc:PlusKey                LONG
loc:MinusKey               LONG
szFontDescription          CSTRING(64)                           !
szFontName                 CSTRING(32)                           !
nFontSize                  BYTE                                  !
lForeColor                 LONG
nFontStyle                 LONG
DefaultErrors              GROUP
Number                        USHORT(1)
                              USHORT(Msg:ConfirmDelete)
                              BYTE(Level:User)
                              PSTRING('Confirm Delete')
                              PSTRING('Are you sure you want to delete the %File properties file?')
                           END
thisEditCommand      LIKE(glo:szEditorCommand)             ! 
bPropertiesChanged   BOOL                                  ! 
loc:SplitterOrientation BYTE                               ! 
loc:szClarionHelpFile CSTRING(260)                         ! 
loc:szVersion        CSTRING(23)                           ! 
loc:nDeleteWarningCount LONG                               ! 
loc:PlusKeyString    CSTRING(81)                           ! 
loc:PlusKeyTipString CSTRING(128)                          ! 
loc:MinusKeyString   CSTRING(81)                           ! 
loc:MinusKeyTipString CSTRING(128)                         ! 
Window WINDOW('Options'),AT(,,530,320),GRAY,SYSTEM,HLP('UserOptions.htm'), |
      FONT('Segoe UI',10),Tiled,DOUBLE
    PANEL,AT(5,5,520,310),USE(?Panel1),BEVEL(1)
    PROMPT('S&end to Command'),AT(10,10),USE(?thisEditCommand:Prompt)
    COMBO(@s255),AT(80,10,328,10),USE(thisEditCommand),VSCROLL,DROP(10), |
        FROM(EditorQueue),ALRT(DeleteKey), ALRT(MouseRight),FORMAT('1020L(2)|M@s255@')
    BUTTON('...'),AT(413,10,11,10),USE(?LookupSendToCommand)
    CHECK(' &Prompt for Command'),AT(428,10,92),USE(glo:PromptForEditor)
    PROMPT('Clarion &Help File'),AT(10,25),USE(?loc:szClarionHelpFile:Prompt)
    ENTRY(@s255),AT(80,25,422,10),USE(loc:szClarionHelpFile),SKIP,COLOR(COLOR:BTNFACE), |
        READONLY
    BUTTON('...'),AT(505,24,11,10),USE(?LookupFile)
    CHECK(' Identify &Location in all files'),AT(17,49,137),USE(glo:bAllExtensions)
    GROUP('Identify Location in these File Extensions'),AT(10,39,150,271), |
        USE(?ClarionExtensions:Group),BOXED
      LIST,AT(17,62,136,225),USE(?List),VSCROLL,CENTER,TIP('Filenames containing' & |
          ' the extensions in this list will receive Location Identification pro' & |
          'cessing.  This special processing may incur significant processing ov' & |
          'erhead.'),FROM(ClarionExtensionsQueue),FORMAT('80L(2)|M@s20@'), |
          ALRT(MouseLeft2)
      BUTTON('Insert'),AT(17,292,40,12),USE(?Insert),KEY(InsertKey)
      BUTTON('Edit'),AT(59,292,40,12),USE(?Change),KEY(CtrlEnter)
      BUTTON('Delete'),AT(102,292,40,12),USE(?Delete),KEY(DeleteKey)
    END
    GROUP('Properties'),AT(165,39,355,271),USE(?EditorStylesGroup),BOXED
      LIST,AT(170,62,102,225),USE(?lbPropertyFiles),FROM(PropertyFileQueue), |
          FORMAT('125L(2)Y@s255@'),ALRT(MouseLeft2)
      BUTTON('Edit'),AT(188,292,40,12),USE(?cmdPropertyEditor),TIP('Edit the hig' & |
          'hlighted property file.')
      PROMPT('&Confirm deletions'),AT(282,48,60,10),USE(?DeleteWarningThresholdPrompt), |
          TRN
      LIST,AT(357,48,38,10),USE(loc:nDeleteWarningCount),DROP(3),FROM('Never|#0|' & |
          'Always|#1|When|#2')
      STRING('removing at least'),AT(398,48,,10),USE(?RemovingAtLeast),HIDE
      SPIN(@N10~ rows~),AT(460,48,55,10),USE(loc:nDeleteWarningCount,, |
           ?loc:nDeleteWarningCount:2),HIDE,RIGHT(2),TIP('Show a warning message' & |
          ' when the number of records to be deleted  is greater than or equal t' & |
          'o this number.<0DH,0AH>Leave blank to completely disable the warning ' & |
          'message.'),RANGE(2,9999)
      BOX,AT(282,62,11,10),USE(?loc:sResultListColorBox),COLOR(0B99D7FH), |
          FILL(COLOR:Black),ROUND,LINEWIDTH(1)
      PROMPT('&Result List'),AT(297,62),USE(?Font:Prompt),TRN
      ENTRY(@s63),AT(357,62,145,10),USE(szFontDescription),DISABLE
      BUTTON('...'),AT(505,62,11,10),USE(?cmdFontDialog)
      BOX,AT(282,77,11,10),USE(?loc:sApplicationColorBox),COLOR(0B99D7FH), |
          FILL(0EAD1B9H),ROUND,LINEWIDTH(1)
      PROMPT('&Application'),AT(297,77),USE(?loc:ApplicationColor:Prompt),TRN
      ENTRY(@s30),AT(357,77,145,10),USE(loc:sApplicationColor),SKIP,DISABLE,LEFT, |
          COLOR(COLOR:White)
      BUTTON('...'),AT(505,77,11,10),USE(?ApplicationColorButton),TIP('Select Ap' & |
          'plication Color')
      BOX,AT(282,92,11,10),USE(?loc:sToolbarColorBox),COLOR(0B99D7FH),FILL(0F2E4D7H), |
          ROUND,LINEWIDTH(1)
      PROMPT('&Toolbar'),AT(297,92),USE(?loc:ToolbarColor:Prompt),TRN
      ENTRY(@s30),AT(357,92,145,10),USE(loc:sToolbarColor),SKIP,DISABLE,LEFT, |
          COLOR(COLOR:White)
      BUTTON('...'),AT(505,92,11,10),USE(?ToolbarColorButton),TIP('Select Toolba' & |
          'r Color')
      BOX,AT(282,107,11,10),USE(?loc:sSelectedBackColorBox),COLOR(0B99D7FH), |
          FILL(COLOR:Red),ROUND,LINEWIDTH(1)
      PROMPT('&Selected'),AT(297,107),USE(?loc:SelectedBackColor:Prompt),TRN
      ENTRY(@s30),AT(357,107,145,10),USE(loc:sSelectedBackColor),SKIP,DISABLE,LEFT, |
          COLOR(COLOR:White)
      BUTTON('...'),AT(505,107,11,10),USE(?SelectedBackColorButton),TIP('Select ' & |
          'Selected Text Background Color')
      BOX,AT(282,122,11,10),USE(?loc:sBookmarkBackColorBox),COLOR(0B99D7FH), |
          FILL(COLOR:Maroon),ROUND,LINEWIDTH(1)
      PROMPT('Boo&kmark'),AT(297,122),USE(?loc:BookmarkBackColor:Prompt),TRN
      ENTRY(@s30),AT(357,122,145,10),USE(loc:sBookmarkBackColor),SKIP,DISABLE,LEFT, |
          COLOR(COLOR:White)
      BUTTON('...'),AT(505,122,11,10),USE(?BookmarkBackColorButton),TIP('Select ' & |
          'Bookmark Background Color')
      BUTTON,AT(170,292,14,12),USE(?cmdDefault),ICON('check-black.ico'), |
          TIP('Set the default property file')
      BUTTON('Delete'),AT(232,292,40,12),USE(?cmdDeletePropertyFile),TIP('Delete' & |
          ' the highlighted property file.')
      BUTTON('Official Devuna Github'),AT(282,292,,12),USE(?btnGetSource), |
          FONT(,,,FONT:regular)
      BUTTON('Github - This Fork'),AT(386,292,,12),USE(?btnGithubThisFork)
    END
    CHECK(' &Link Paths and Files with Find Text'),AT(282,137),USE(glo:SyncPathWithPattern) |
        ,TIP('Recall Paths and Files when you select prior Find text from the dr' & |
        'op list.')
    CHECK(' Allow &Multiple Instances'),AT(282,147),USE(glo:AllowMultipleInstances)
    CHECK(' Auto Size Result &Columns'),AT(407,147),USE(glo:AutoSizeResultColumns)
    PROMPT('Restore &Point Timer'),AT(282,171),USE(?glo:RestorePointTimerInterval:Prompt) |
        
    SPIN(@n13),AT(358,171,60,10),USE(glo:RestorePointTimerInterval),RIGHT(1), |
        TIP('Set to 0 to disable Restore Point Feature'),STEP(1)
    STRING('minutes'),AT(423,171),USE(?Minutes:String)
    GROUP('Application Function Keys'),AT(282,182,234,38),USE(?FunctionKeyBox),BOXED, |
        TRN
      STRING(@s80),AT(286,192,215,10),USE(loc:MinusKeyString),TRN,RIGHT
      REGION,AT(286,192,215,10),USE(?loc:MinusKeyStringRegion),IMM
      BUTTON('...'),AT(503,192,11,10),USE(?cmdGetMinusKey)
      STRING(@s80),AT(286,204,215,10),USE(loc:PlusKeyString),TRN,RIGHT
      REGION,AT(286,204,215,10),USE(?loc:PlusKeyStringRegion),IMM
      BUTTON('...'),AT(503,204,11,10),USE(?cmdGetPlusKey)
    END
    GROUP,AT(282,220,234,68),USE(?CopyrightBox),BOXED,TRN
      STRING('Kwik Source Search - Hand Code Open Source Edition'),AT(290,226,213), |
          USE(?ProductName),TRN,CENTER,FONT(,,,FONT:bold)
      STRING(@s31),AT(336,234,127),USE(glo:szVersion),TRN,CENTER
      STRING('© Copyright  2011-2017'),AT(336,242,127),USE(?Copyright),TRN,CENTER
      STRING('Devuna Inc.'),AT(336,251,127),USE(?CompanyName),TRN,CENTER
      REGION,AT(359,260,90,10),USE(?rgnWebAddress),IMM
      STRING('http://www.devuna.com'),AT(336,260,127),USE(?strWebAddress),TRN,CENTER, |
          FONT(,10,COLOR:Blue,FONT:regular)
      STRING(''),AT(329,269,141),USE(?strRegisteredTo),TRN,HIDE,CENTER
      STRING('Devuna Inc.'),AT(336,251,127,10),USE(?CompanyName:2),TRN,CENTER
    END
    BUTTON('&Apply'),AT(476,292,40,12),USE(?cmdApply)
    BUTTON('Cancel'),AT(276,251),USE(?Cancel),HIDE
    CHECK(' Include files with binary content'),AT(282,158,142,10),USE(glo:DisableSlashP) |
        ,VALUE('1','0'),TIP('Disable /p in FINDSTR')
  END

QEIP1:SaveEntry      GROUP,PRE(QEIP1)
FileExtension          LIKE(CEQ:FileExtension)
                     END
QEIP1:Fields         FieldPairsClass
QEIP1:PopupString    STRING(20)
QEIP1:PopupMgr       PopupClass
QEIP1:EditList       QUEUE(EditQueue),PRE(QEIP1)
                     END
QEIP1:EM             CLASS(EIPManager)
TabAction              BYTE
EnterAction            BYTE
ArrowAction            BYTE
FocusLossAction        BYTE
CurrentChoice          LONG,PRIVATE
AddControl             PROCEDURE(<EditClass EC>,UNSIGNED Column,BYTE AutoFree = 0)
ClearColumn            PROCEDURE,DERIVED
Init                   PROCEDURE,BYTE,DERIVED,PROC
InitControls           PROCEDURE,DERIVED
Kill                   PROCEDURE,PROC,BYTE,DERIVED
Next                   PROCEDURE,PROTECTED
GetEdit                PROCEDURE,BYTE,DERIVED,PROTECTED
PrimeRecord            PROCEDURE(BYTE SuppressClear = 0)
ResetColumn            PROCEDURE,DERIVED,PROTECTED
Run                    PROCEDURE(BYTE Req),BYTE
TakeAction             PROCEDURE(UNSIGNED Action),DERIVED
TakeCompleted          PROCEDURE(BYTE Force),DERIVED   ! Note this does -not- override the WindowManager variant
TakeEvent              PROCEDURE,DERIVED,BYTE,PROC
TakeFieldEvent         PROCEDURE,DERIVED,BYTE,PROC
TakeFocusLoss          PROCEDURE,DERIVED
TakeNewSelection       PROCEDURE,DERIVED,BYTE,PROC
                     END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
QEIP1::FileExtension CLASS(EditEntryClass)
CreateControl          PROCEDURE(),DERIVED                      ! Method added to host embed code
TakeAccepted           PROCEDURE(BYTE Action),BYTE,DERIVED      ! Method added to host embed code
                     END
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
FileLookup3          SelectFileClass
FileLookup4          SelectFileClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop
  RETURN(bPropertiesChanged)

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
  !------------------------------------
  !Style for ?lbPropertyFiles
  !------------------------------------
  ?lbPropertyFiles{PROPSTYLE:FontName, 1}      = 'Segoe UI'
  ?lbPropertyFiles{PROPSTYLE:FontSize, 1}      = 10
  ?lbPropertyFiles{PROPSTYLE:FontStyle, 1}     = 700
  ?lbPropertyFiles{PROPSTYLE:TextColor, 1}     = -1
  ?lbPropertyFiles{PROPSTYLE:BackColor, 1}     = -1
  ?lbPropertyFiles{PROPSTYLE:TextSelected, 1}  = -1
  ?lbPropertyFiles{PROPSTYLE:BackSelected, 1}  = -1
  !------------------------------------
  ?lbPropertyFiles{PROPSTYLE:FontName, 2}      = 'Segoe UI'
  ?lbPropertyFiles{PROPSTYLE:FontSize, 2}      = 10
  ?lbPropertyFiles{PROPSTYLE:FontStyle, 2}     = 4496
  ?lbPropertyFiles{PROPSTYLE:TextColor, 2}     = -1
  ?lbPropertyFiles{PROPSTYLE:BackColor, 2}     = -1
  ?lbPropertyFiles{PROPSTYLE:TextSelected, 2}  = -1
  ?lbPropertyFiles{PROPSTYLE:BackSelected, 2}  = -1
  !------------------------------------
  ?lbPropertyFiles{PROPSTYLE:FontName, 3}      = 'Segoe UI'
  ?lbPropertyFiles{PROPSTYLE:FontSize, 3}      = 10
  ?lbPropertyFiles{PROPSTYLE:FontStyle, 3}     = 4796
  ?lbPropertyFiles{PROPSTYLE:TextColor, 3}     = -1
  ?lbPropertyFiles{PROPSTYLE:BackColor, 3}     = -1
  ?lbPropertyFiles{PROPSTYLE:TextSelected, 3}  = -1
  ?lbPropertyFiles{PROPSTYLE:BackSelected, 3}  = -1
  !------------------------------------
!---------------------------------------------------------------------------
AddEditCommand    ROUTINE
   DATA
i     LONG
j     LONG

   CODE
      UPDATE()
      IF thisEditCommand <> ''
         j = RECORDS(EditorQueue)
         LOOP i = 1 TO j
            GET(EditorQueue,i)
            IF EditorQueue.szValue = thisEditCommand
               BREAK
            END
         END
         IF i > j
            IF j = MAXMRU
               GET(EditorQueue,j)
               DELETE(EditorQueue)
            END
            EditorQueue.szValue = thisEditCommand
            ADD(EditorQueue,+EditorQueue.szValue)
         END
      END
FillPropertyFileQueue  ROUTINE
   DATA

   CODE
      FREE(PropertyFileQueue)
      !get user properties
      szSearchFolder = svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS\')
      bReadOnly = FALSE
      DO GetFilenames

      !get system properties
      szSearchFolder = LONGPATH() & '\'
      bReadOnly = TRUE
      DO GetFileNames

      GET(PropertyFileQueue,1)
GetFilenames   ROUTINE
   DATA
i              LONG
j              LONG
cc             LONG
szPath         CSTRING(MAXPATH+1)
szDrive        CSTRING(MAXDRIVE+1)
szDir          CSTRING(MAXDIR+1)
szName         CSTRING(MAXFILE+1)
szExtension    CSTRING(MAXEXT+1)
FileModes      QUEUE(FILE:queue),PRE(fm)
               END
   CODE
      DIRECTORY(FileModes,szSearchFolder & '*.properties',0)
      j = RECORDS(FileModes)
      IF j > 0
         LOOP i = 1 TO j
            GET(FileModes,i)
            szPath = szSearchFolder & CLIP(FileModes.Name)
            cc = kcr_fnSplit(szPath, szDrive, szDir, szName, szExtension)
            PropertyFileQueue.FileName = LOWER(szName)
            GET(PropertyFileQueue,+PropertyFileQueue.FileName)
            IF ERRORCODE()
               PropertyFileQueue.FileName = LOWER(szName)
               PropertyFileQueue.FilePath = szPath
               PropertyFileQueue.IsReadOnly = bReadOnly
               IF PropertyFileQueue.FileName = glo:szDefaultPropertyFile
                  PropertyFileQueue.FilenameStyle = STYLE:BOLD + CHOOSE(PropertyFileQueue.IsReadOnly=0,STYLE:ITALIC,0)
               ELSE
                  PropertyFileQueue.FilenameStyle = STYLE:NORMAL + CHOOSE(PropertyFileQueue.IsReadOnly=0,STYLE:ITALIC,0)
               END
               ADD(PropertyFileQueue,+PropertyFileQueue.FileName)
            END
         END
      END
GetFontDescription   ROUTINE
   IF szFontName
      szFontDescription = CLIP(szFontName) & ','
   END

   IF nFontSize
      szFontDescription = szFontDescription & nFontSize & ','
   END

   IF BAND(nFontStyle,0FFFh) >= FONT:bold
      szFontDescription = szFontDescription & 'Bold,'
   ELSIF BAND(nFontStyle,0FFFh) >= FONT:regular
      szFontDescription = szFontDescription & 'Regular,'
   ELSE
      szFontDescription = szFontDescription & 'Thin,'
   END

   IF BAND(nFontStyle,FONT:italic) = FONT:italic
      szFontDescription = szFontDescription & 'Italic,'
   END

   IF BAND(nFontStyle,FONT:underline) = FONT:underline
      szFontDescription = szFontDescription & 'Underline,'
   END

   IF szFontDescription[LEN(szFontDescription)] = ','
      szFontDescription[LEN(szFontDescription)] = '<0>'
   END
   DISPLAY(?szFontDescription)
   EXIT

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
    
  GlobalErrors.SetProcedureName('UserOptions')
  GlobalErrors.AddErrors(DefaultErrors)
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Panel1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  DO FillPropertyFileQueue
  
  loc:MinusKeyString = 'Previous Result Line assigned to ' & CLIP(KeyCodeName.ToName(glo:MinusKey))
  loc:MinusKeyTipString = 'The ''Jump to Previous Line in Results List'' function is assigned to the ' & CLIP(KeyCodeName.ToName(glo:MinusKey))
  
  loc:PlusKeyString = 'Next Result Line assigned to ' & CLIP(KeyCodeName.ToName(glo:PlusKey))
  loc:PlusKeyTipString  = 'The ''Jump to Next Line in Results List'' function is assigned to the ' & CLIP(KeyCodeName.ToName(glo:PlusKey))
  
  glo:OldMinusKey = glo:MinusKey
  glo:OldPlusKey = glo:PlusKey
  loc:MinusKey = glo:MinusKey
  loc:PlusKey  = glo:PlusKey
  SELF.Open(Window)                                        ! Open window
  ?loc:MinusKeyStringRegion{PROP:Tip} = loc:MinusKeyTipString
  ?loc:PlusKeyStringRegion{PROP:Tip} = loc:PlusKeyTipString
  ?lbPropertyFiles{PROP:Selected} = 1
  GET(PropertyFileQueue,1)
  CASE PropertyFileQueue.FilenameStyle
    OF STYLE:NORMAL
       ?cmdDefault{PROP:Disable} = FALSE
    OF STYLE:BOLD
       ?cmdDefault{PROP:Disable} = TRUE
    OF STYLE:ITALIC
       ?cmdDefault{PROP:Disable} = FALSE
    OF STYLE:BOLDITALIC
       ?cmdDefault{PROP:Disable} = TRUE
  END
  ?cmdDeletePropertyFile{PROP:Disable} = PropertyFileQueue.IsReadOnly
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  ?thisEditCommand{PROP:LineHeight} = 10
  ?List{PROP:LineHeight} = 10
  ?lbPropertyFiles{PROP:LineHeight} = 10
  ?loc:nDeleteWarningCount{PROP:LineHeight} = 10
  !IF RegisteredTo = UNREGISTERED_COPY
  !   UNHIDE(?cmdRegisterNow)
  !ELSE
     !?cmdRegisterNow{PROP:Text} = 'Check for Update'
     !UNHIDE(?cmdRegisterNow)
  !END
  Do DefineListboxStyle
  Alert(AltKeyPressed)  ! WinEvent : These keys cause a program to crash on Windows 7 and Windows 10.
  Alert(F10Key)         !
  Alert(CtrlF10)        !
  Alert(ShiftF10)       !
  Alert(CtrlShiftF10)   !
  Alert(AltSpace)       !
!  WinAlertMouseZoom()
!  WinAlert(WE::WM_QueryEndSession,,Return1+PostUser)
  Window{Prop:Alrt,255} = CtrlShiftP
  INIMgr.Fetch('UserOptions',Window)                       ! Restore window settings from non-volatile store
  CorrectForOffscreen(Window)
  
  thisEditCommand = glo:szEditorCommand
  
  szFontName  = glo:ResultListFontName
  nFontSize   = glo:ResultListFontSize
  lForeColor  = glo:ResultListForeColor
  nFontStyle  = glo:ResultListFontStyle
  DO GetFontDescription
  
  CASE glo:ApplicationColor
    OF COLOR:GRADIENTACTIVECAPTION
       loc:ApplicationColor = kcr_GetSysColor(COLOR_GRADIENTACTIVECAPTION)
    OF COLOR:GRADIENTINACTIVECAPTION
       loc:ApplicationColor = kcr_GetSysColor(COLOR_GRADIENTINACTIVECAPTION)
  ELSE
       loc:ApplicationColor = glo:ApplicationColor
  END
  CASE glo:ToolbarColor
    OF COLOR:GRADIENTACTIVECAPTION
       loc:ToolbarColor = kcr_GetSysColor(COLOR_GRADIENTACTIVECAPTION)
    OF COLOR:GRADIENTINACTIVECAPTION
       loc:ToolbarColor = kcr_GetSysColor(COLOR_GRADIENTINACTIVECAPTION)
  ELSE
       loc:ToolbarColor = glo:ToolbarColor
  END
  loc:SelectedBack = glo:SelectedBack
  loc:BookmarkBack = glo:BookmarkBack
  loc:SplitterOrientation = glo:SplitterOrientation
  loc:szVersion    = 'Version ' & glo:szVersion
  loc:szClarionHelpFile = glo:szClarionHelpFile
  loc:nDeleteWarningCount = glo:nDeleteWarningCount
  ?loc:nDeleteWarningCount{PROP:From} = 'Never|#0|Always|#1|When|#' & CHOOSE(loc:nDeleteWarningCount > 1,loc:nDeleteWarningCount,2)
  FileLookup3.Init
  FileLookup3.ClearOnCancel = True
  FileLookup3.Flags=BOR(FileLookup3.Flags,FILE:LongName)   ! Allow long filenames
  FileLookup3.SetMask('Helpl Files','*.chm')               ! Set the file mask
  FileLookup3.DefaultFile='ClarionHelp.chm'
  FileLookup3.WindowTitle='Select Clarion Help File...'
  FileLookup4.Init
  FileLookup4.ClearOnCancel = True
  FileLookup4.Flags=BOR(FileLookup4.Flags,FILE:LongName)   ! Allow long filenames
  FileLookup4.SetMask('Program Files','*.exe')             ! Set the file mask
  FileLookup4.AddMask('All Files','*.*')                   ! Add additional masks
  FileLookup4.WindowTitle='Select Send To Target...'
  FileLookup3.Flags=BOR(FileLookup3.Flags,FILE:KeepDir)    ! Return to start directory
  FileLookup4.Flags=BOR(FileLookup4.Flags,FILE:KeepDir)    ! Return to start directory
  SELF.SetAlerts()
  QEIP1:Fields.Init()
  QEIP1:Fields.AddPair(QEIP1:SaveEntry.FileExtension,ClarionExtensionsQueue.FileExtension)
  QEIP1:PopupMgr.Init()
  QEIP1:PopupMgr.AddItemMimic('Add',?Insert)
  QEIP1:PopupMgr.AddItemMimic('Edit',?Change)
  QEIP1:PopupMgr.AddItemMimic('Delete',?Delete)
  GlobalErrors.AddErrors(QEIP:Errors)
  ?List{Prop:Alrt,QEIP:MouseLeft2Index} = MouseLeft2
  ?List{Prop:Alrt,QEIP:MouseRightIndex} = MouseRight
  oHH &= NEW tagHTMLHelp
  oHH.Init( 'kss.chm' )
  oHH.SetTopic('UserOptions.htm')
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
!  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.Opened
    INIMgr.Update('UserOptions',Window)                    ! Save window data to non-volatile store
  END
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
  !Color Options
  !======================================================================================
  ?loc:sApplicationColorBox{PROP:Fill} = loc:ApplicationColor
  loc:sApplicationColor = srcGetColorString(loc:ApplicationColor)
  
  ?loc:sToolbarColorBox{PROP:Fill} = loc:ToolbarColor
  loc:sToolbarColor = srcGetColorString(loc:ToolbarColor)
  
  ?loc:sResultListColorBox{PROP:Fill} = lForeColor
  DO GetFontDescription
  
  ?loc:sSelectedBackColorBox{PROP:Fill} = loc:SelectedBack
  loc:sSelectedBackColor = srcGetColorString(loc:SelectedBack)
  
  ?loc:sBookmarkBackColorBox{PROP:Fill} = loc:BookmarkBack
  loc:sBookmarkBackColor = srcGetColorString(loc:BookmarkBack)
  PARENT.Reset(Force)
  IF RECORDS(ClarionExtensionsQueue)
     ?Change{PROP:Disable} = FALSE
     ?Delete{PROP:Disable} = FALSE
  ELSE
     ?Change{PROP:Disable} = TRUE
     ?Delete{PROP:Disable} = TRUE
  END
  IF RECORDS(EditorQueue) > 1
     ENABLE(?glo:PromptForEditor)
  ELSE
     glo:PromptForEditor = FALSE
     DISABLE(?glo:PromptForEditor)
     DISPLAY(?glo:PromptForEditor)
  END
  
  IF glo:bAllExtensions = TRUE
     DISABLE(?ClarionExtensions:Group)
  ELSE
     ENABLE(?ClarionExtensions:Group)
  END
  
  IF loc:nDeleteWarningCount < 2
     HIDE(?RemovingAtLeast,?loc:nDeleteWarningCount:2)
  ELSE
     UNHIDE(?RemovingAtLeast,?loc:nDeleteWarningCount:2)
  END
  
  GET(PropertyFileQueue,CHOICE(?lbPropertyFiles))
  CASE PropertyFileQueue.FilenameStyle
    OF STYLE:NORMAL
       ?cmdDefault{PROP:Disable} = FALSE
    OF STYLE:BOLD
       ?cmdDefault{PROP:Disable} = TRUE
    OF STYLE:ITALIC
       ?cmdDefault{PROP:Disable} = FALSE
    OF STYLE:BOLDITALIC
       ?cmdDefault{PROP:Disable} = TRUE
  END
  ?cmdDeletePropertyFile{PROP:Disable} = PropertyFileQueue.IsReadOnly


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

szURL                CSTRING(256)
szNull               CSTRING('<0>')
i                    LONG
j                    LONG
lExpiryDate          LONG
saveColor            LONG
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
    OF ?thisEditCommand
      DO AddEditCommand
    OF ?LookupSendToCommand
      ThisWindow.Update()
      thisEditCommand = FileLookup4.Ask(1)
      DISPLAY
    OF ?LookupFile
      ThisWindow.Update()
      loc:szClarionHelpFile = FileLookup3.Ask(1)
      DISPLAY
    OF ?glo:bAllExtensions
      SELF.Reset()
    OF ?Insert
      ThisWindow.Update()
      LOOP
        ThisWindow.VCRRequest = VCR:None
        IF KEYCODE() = MouseRightUp
          SETKEYCODE(0)
        END
        ReturnValue = QEIP1:EM.Run(InsertRecord)
        CASE ThisWindow.VCRRequest
          OF VCR:Forward
             IF POINTER(ClarionExtensionsQueue) < RECORDS(ClarionExtensionsQueue)
                ?List{PROP:Selected} = POINTER(ClarionExtensionsQueue)
             ELSE
                ThisWindow.VCRRequest = VCR:None
             END
          OF VCR:Backward
             IF POINTER(ClarionExtensionsQueue) > 1
                ?List{PROP:Selected} = POINTER(ClarionExtensionsQueue)
             ELSE
                ThisWindow.VCRRequest = VCR:None
             END
        END
      UNTIL ThisWindow.VCRRequest = VCR:None
      SELECT(?List)
      ThisWindow.Reset()
    OF ?Change
      ThisWindow.Update()
      LOOP
        ThisWindow.VCRRequest = VCR:None
        IF KEYCODE() = MouseRightUp
          SETKEYCODE(0)
        END
        ReturnValue = QEIP1:EM.Run(ChangeRecord)
        CASE ThisWindow.VCRRequest
          OF VCR:Forward
             IF POINTER(ClarionExtensionsQueue) < RECORDS(ClarionExtensionsQueue)
                GET(ClarionExtensionsQueue,POINTER(ClarionExtensionsQueue)+1)
                ?List{PROP:Selected} = POINTER(ClarionExtensionsQueue)
             ELSE
                ThisWindow.VCRRequest = VCR:None
             END
          OF VCR:Backward
             IF POINTER(ClarionExtensionsQueue) > 1
                GET(ClarionExtensionsQueue,POINTER(ClarionExtensionsQueue)-1)
                ?List{PROP:Selected} = POINTER(ClarionExtensionsQueue)
             ELSE
                ThisWindow.VCRRequest = VCR:None
             END
        END
      UNTIL ThisWindow.VCRRequest = VCR:None
      SELECT(?List)
      ThisWindow.Reset()
    OF ?Delete
      ThisWindow.Update()
      ReturnValue = QEIP1:EM.Run(DeleteRecord)
      ThisWindow.Reset()
    OF ?cmdPropertyEditor
      ThisWindow.Update()
      GET(PropertyFileQueue,CHOICE(?lbPropertyFiles))
      IF bPropertiesChanged = FALSE
         bPropertiesChanged = PropertyEditor(PropertyFileQueue.FilePath,MaxStyleIndex)
      ELSE
         PropertyEditor(PropertyFileQueue.FilePath,MaxStyleIndex)
      END
      DO FillPropertyFileQueue
      ThisWindow.Reset
    OF ?loc:nDeleteWarningCount
      ThisWindow.Reset()
    OF ?cmdFontDialog
      ThisWindow.Update()
      IF FONTDIALOG('Choose Font',szFontName,nFontSize,lForeColor,nFontStyle,0)
         IF szFontName  <> glo:ResultListFontName  |
         OR nFontSize   <> glo:ResultListFontSize  |
         OR lForeColor  <> glo:ResultListForeColor |
         OR nFontStyle  <> glo:ResultListFontStyle
            bPropertiesChanged = TRUE
         END
         ThisWindow.Reset()
      END
    OF ?ApplicationColorButton
      ThisWindow.Update()
      saveColor = loc:ApplicationColor
      COLORDIALOG('Application Color',loc:ApplicationColor)
      IF saveColor <> loc:ApplicationColor
         bPropertiesChanged = TRUE
      END
      ThisWindow.Reset()
    OF ?ToolbarColorButton
      ThisWindow.Update()
      saveColor = loc:ToolbarColor
      COLORDIALOG('Toolbar Color',loc:ToolbarColor)
      IF saveColor <> loc:ToolbarColor
         bPropertiesChanged = TRUE
      END
      ThisWindow.Reset()
    OF ?SelectedBackColorButton
      ThisWindow.Update()
      saveColor = loc:SelectedBack
      COLORDIALOG('Selected Color',loc:SelectedBack)
      IF saveColor <> loc:SelectedBack
         bPropertiesChanged = TRUE
      END
      ThisWindow.Reset()
    OF ?BookmarkBackColorButton
      ThisWindow.Update()
      saveColor = loc:BookmarkBack
      COLORDIALOG('Bookmark Color',loc:BookmarkBack)
      IF saveColor <> loc:BookmarkBack
         bPropertiesChanged = TRUE
      END
      ThisWindow.Reset()
    OF ?cmdDefault
      ThisWindow.Update()
      i = CHOICE(?lbPropertyFiles)
      LOOP j = 1 TO RECORDS(PropertyFileQueue)
         GET(PropertyFileQueue,j)
         IF j = i
            PropertyFileQueue.FilenameStyle = STYLE:BOLD + CHOOSE(PropertyFileQueue.IsReadOnly=0,STYLE:ITALIC,0)
            glo:szDefaultPropertyFile = PropertyFileQueue.FileName
         ELSE
            PropertyFileQueue.FilenameStyle = STYLE:NORMAL + CHOOSE(PropertyFileQueue.IsReadOnly=0,STYLE:ITALIC,0)
         END
         PUT(PropertyFileQueue)
      END
      GET(PropertyFileQueue,i)
      CASE PropertyFileQueue.FilenameStyle
        OF STYLE:NORMAL
           ?cmdDefault{PROP:Disable} = FALSE
        OF STYLE:BOLD
           ?cmdDefault{PROP:Disable} = TRUE
        OF STYLE:ITALIC
           ?cmdDefault{PROP:Disable} = FALSE
        OF STYLE:BOLDITALIC
           ?cmdDefault{PROP:Disable} = TRUE
      END
    OF ?cmdDeletePropertyFile
      ThisWindow.Update()
      IF GlobalErrors.ThrowFile(Msg:ConfirmDelete,PropertyFileQueue.FileName) = Level:Benign
         REMOVE(PropertyFileQueue.FilePath)
         DO FillPropertyFileQueue
         ThisWindow.Reset
      END
    OF ?btnGetSource
      ThisWindow.Update()
            szURL = 'https://github.com/Devuna?tab=repositories'
            szNull = ''
            kcr_ShellExecute(window{prop:handle},0,szURL,0,szNull,1)
    OF ?btnGithubThisFork
      ThisWindow.Update()
            szURL = 'https://github.com/mriffey/KSSOpen'
            szNull = ''
            kcr_ShellExecute(window{prop:handle},0,szURL,0,szNull,1)
    OF ?cmdGetMinusKey
      ThisWindow.Update()
      WinGetKeyAssignment('Jump to Previous Line in Results List',loc:MinusKey)
      loc:MinusKeyString = 'Previous Result Line assigned to ' & CLIP(KeyCodeName.ToName(loc:MinusKey))
      loc:MinusKeyTipString = 'The ''Jump to Previous Line in Results List'' function is assigned to the ' & CLIP(KeyCodeName.ToName(loc:MinusKey))
      ?loc:MinusKeyStringRegion{PROP:Tip} = loc:MinusKeyTipString
    OF ?cmdGetPlusKey
      ThisWindow.Update()
      WinGetKeyAssignment('Jump to Next Line in Results List',loc:PlusKey)
      loc:PlusKeyString = 'Next Result Line assigned to ' & CLIP(KeyCodeName.ToName(loc:PlusKey))
      loc:PlusKeyTipString  = 'The ''Jump to Next Line in Results List'' function is assigned to the ' & CLIP(KeyCodeName.ToName(loc:PlusKey))
      ?loc:PlusKeyStringRegion{PROP:Tip} = loc:PlusKeyTipString
    OF ?rgnWebAddress
      szURL = ?strWebAddress{PROP:Text}
      szNull = ''
      kcr_ShellExecute(window{prop:handle},0,szURL,0,szNull,1)
    OF ?cmdApply
      ThisWindow.Update()
      glo:szClarionHelpFile   = loc:szClarionHelpFile
      glo:szEditorCommand     = thisEditCommand
      glo:nDeleteWarningCount = loc:nDeleteWarningCount
      glo:SelectedBack        = loc:SelectedBack
      glo:BookmarkBack        = loc:BookmarkBack
      glo:SplitterOrientation = loc:SplitterOrientation
      
      
      IF loc:ApplicationColor = kcr_GetSysColor(COLOR_GRADIENTACTIVECAPTION)
         glo:ApplicationColor = COLOR:GRADIENTACTIVECAPTION
      ELSIF loc:ApplicationColor = kcr_GetSysColor(COLOR_GRADIENTINACTIVECAPTION)
         glo:ApplicationColor = COLOR:GRADIENTINACTIVECAPTION
      ELSE
         glo:ApplicationColor   = loc:ApplicationColor
      END
      
      IF loc:ToolbarColor = kcr_GetSysColor(COLOR_GRADIENTACTIVECAPTION)
         glo:ToolbarColor = COLOR:GRADIENTACTIVECAPTION
      ELSIF loc:ToolbarColor = kcr_GetSysColor(COLOR_GRADIENTINACTIVECAPTION)
         glo:ToolbarColor = COLOR:GRADIENTINACTIVECAPTION
      ELSE
         glo:ToolbarColor   = loc:ToolbarColor
      END
      
      INIMgr.Update('Global','DeleteWarningCount',glo:nDeleteWarningCount)
      INIMgr.Update('Global','ApplicationColor',glo:ApplicationColor)
      INIMgr.Update('Global','ToolbarColor',glo:ToolbarColor)
      INIMgr.Update('Global','SelectedBack',glo:SelectedBack)
      INIMgr.Update('Global','BookmarkBack',glo:BookmarkBack)
      INIMgr.Update('Global','DisableSlashP',glo:DisableSlashP) ! mr 20180912 
      
      glo:ResultListFontName  = szFontName
      glo:ResultListFontSize  = nFontSize
      glo:ResultListForeColor = lForeColor
      glo:ResultListFontStyle = nFontStyle
      INIMgr.Update('Global','ResultListFontName',glo:ResultListFontName)
      INIMgr.Update('Global','ResultListFontSize',glo:ResultListFontSize)
      INIMgr.Update('Global','ResultListForeColor',glo:ResultListForeColor)
      INIMgr.Update('Global','ResultListFontStyle',glo:ResultListFontStyle)
      
      IF loc:MinusKey <> glo:MinusKey OR loc:PlusKey <> glo:PlusKey
         bPropertiesChanged = TRUE
      END
      
      glo:OldMinusKey = glo:MinusKey
      glo:MinusKey = loc:MinusKey
      INIMgr.Update('Global','ResultListMinusKey',glo:MinusKey)
      
      glo:OldPlusKey = glo:PlusKey
      glo:PlusKey  = loc:PlusKey
      INIMgr.Update('Global','ResultListPlusKey',glo:PlusKey)
      
      INIMgr.UpdateQueue('Editor Queue','EditCommand',EditorQueue,EditorQueue.szValue)
      INIMgr.UpdateQueue('ClarionExtensions Queue','ClarionExtension',ClarionExtensionsQueue,ClarionExtensionsQueue.FileExtension)
      POST(EVENT:CloseWindow)
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
  OF ?thisEditCommand
    CASE EVENT()
    OF EVENT:AlertKey
      CASE KEYCODE()
        OF DeleteKey
           FORWARDKEY(?thisEditCommand)
        OF MouseRight
           MRUContextMenu(EditorQueue, ?thisEditCommand, '<<Use Windows Default>')
           SELF.Reset()
      END
    END
  OF ?List
    CASE EVENT()
    OF EVENT:AlertKey
      CASE KEYCODE()
        OF MouseLeft2
           IF ~?Change{PROP:Disable} AND RECORDS(ClarionExtensionsQueue)
              POST(EVENT:Accepted,?Change)
           END
        OF MouseRight
           QEIP1:PopupString = QEIP1:PopupMgr.Ask()
      END
    END
  OF ?lbPropertyFiles
    CASE EVENT()
    OF EVENT:AlertKey
      CASE KEYCODE()
        OF MouseLeft2
           POST(EVENT:Accepted,?cmdPropertyEditor)
      END
    END
  OF ?rgnWebAddress
    CASE EVENT()
    OF EVENT:MouseIn
      ?strWebAddress{PROP:FontStyle} = FONT:regular+FONT:underline
      SETCURSOR('~harrow.cur')
    OF EVENT:MouseOut
      ?strWebAddress{PROP:FontStyle} = FONT:regular
      SETCURSOR()
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
    OF ?lbPropertyFiles
      GET(PropertyFileQueue,CHOICE(?lbPropertyFiles))
      CASE PropertyFileQueue.FilenameStyle
        OF STYLE:NORMAL
           ?cmdDefault{PROP:Disable} = FALSE
        OF STYLE:BOLD
           ?cmdDefault{PROP:Disable} = TRUE
        OF STYLE:ITALIC
           ?cmdDefault{PROP:Disable} = FALSE
        OF STYLE:BOLDITALIC
           ?cmdDefault{PROP:Disable} = TRUE
      END
      ?cmdDeletePropertyFile{PROP:Disable} = PropertyFileQueue.IsReadOnly
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

nCode  SIGNED
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

QEIP1:EM.AddControl            PROCEDURE(<EditClass E>,UNSIGNED Column,BYTE AutoFree)
  CODE
  PARENT.AddControl(E,Column,AutoFree)
  RETURN

QEIP1:EM.ClearColumn           PROCEDURE
  CODE
  IF KEYCODE() <> EscKey
     IF SELF.LastColumn
        UPDATE
        GET(SELF.EQ,SELF.Column)
        PUT(ClarionExtensionsQueue)
        ASSERT(~ERRORCODE())
     END
  END
  PARENT.ClearColumn()
  RETURN

QEIP1:EM.Init                  PROCEDURE
RetVal BYTE(RequestCancelled)
AtEnd  BYTE,AUTO
  CODE
  SELF.DeleteAction = EIPAction:Always
  SELF.TabAction = EIPAction:Always+EIPAction:Remain
  SELF.EnterAction = EIPAction:Always
  SELF.FocusLossAction = EIPAction:Never
  SELF.ArrowAction = EIPAction:Always+EIPAction:Remain+EIPAction:RetainColumn
  SELF.Insert = EIPAction:Append
  SELF.Arrow &= SELF.ArrowAction
  SELF.Enter &= SELF.EnterAction
  SELF.EQ &= QEIP1:EditList
  SELF.Errors &= NULL
  SELF.Fields &= QEIP1:Fields
  SELF.FocusLoss &= SELF.FocusLossAction
  SELF.ListControl = ?List
  SELF.Tab &= SELF.TabAction
  SELF.VCRRequest &= ThisWindow.VCRRequest
  SELF.CurrentChoice = CHOICE(?List)
  IF ~SELF.CurrentChoice
     SELF.CurrentChoice = 1
     ?List{PROP:Selected} = 1
  END
  GET(ClarionExtensionsQueue,SELF.CurrentChoice)
  SELF.AddControl(QEIP1::FileExtension,1,0)
  CASE SELF.Req
  OF InsertRecord
    IF RECORDS(ClarionExtensionsQueue)
      AtEnd = CHOOSE(SELF.CurrentChoice = RECORDS(ClarionExtensionsQueue))
      CASE(SELF.Insert)
        OF EIPAction:Before
           !Default
        OF EIPAction:Append
           SELF.CurrentChoice = RECORDS(ClarionExtensionsQueue)+1
      ELSE
           SELF.CurrentChoice += 1
      END
    ELSE
      SELF.CurrentChoice = 1
    END
    SELF.PrimeRecord()
    ADD(ClarionExtensionsQueue,SELF.CurrentChoice)
    ASSERT(~ERRORCODE())
    DISPLAY(?List)
    SELECT(?List,SELF.CurrentChoice)
    SELF.Column = 1
  OF DeleteRecord
    RetVal = CHOOSE(GlobalErrors.Throw(Msg:ConfirmDelete) = Level:Benign,RequestCompleted,RequestCancelled)
    IF RetVal = RequestCompleted
       DELETE(ClarionExtensionsQueue)
    END
    SELF.Response = RetVal
    RETURN Level:Fatal
  OF ChangeRecord
    QEIP1:SaveEntry = ClarionExtensionsQueue
    IF KEYCODE() = MouseLeft2
      SELF.Column = ?List{PROPLIST:MouseUpField}
    END
  ELSE
    ASSERT(0)
  END
  GET(ClarionExtensionsQueue,SELF.CurrentChoice)
  SELF.Fields.AssignRightToLeft()
  ?List{PROP:Alrt,QEIP:MouseLeft2Index} = 0 ! Prevent alert short-stopping double click
  RetVal = PARENT.Init()
  RETURN(RetVal)

QEIP1:EM.InitControls          PROCEDURE
  CODE
  SELF.EQ.Field = 1
  PARENT.InitControls()
  RETURN

QEIP1:EM.Kill                  PROCEDURE
ReturnValue BYTE,AUTO
I           LONG,AUTO
J           LONG,AUTO
  CODE
  ReturnValue = PARENT.Kill()
  !Now dispose of any edit classes we created
  J = RECORDS(QEIP1:EditList)
  LOOP I = 1 TO J
    GET(QEIP1:EditList,I)
    IF ~QEIP1:EditList.Control &= NULL AND QEIP1:EditList.FreeUp = TRUE
       DISPOSE(QEIP1:EditList.Control)
    END
  END
  !and free up the edit queue
  FREE(QEIP1:EditList)
  RETURN(ReturnValue)

QEIP1:EM.Next                  PROCEDURE
  CODE
  PARENT.Next()
  RETURN

QEIP1:EM.GetEdit               PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  ReturnValue = PARENT.GetEdit()
  RETURN(ReturnValue)

QEIP1:EM.PrimeRecord           PROCEDURE(BYTE SC)
  CODE
  IF ~SC
     CLEAR(ClarionExtensionsQueue)
  END
  RETURN

QEIP1:EM.ResetColumn           PROCEDURE
  CODE
  PARENT.ResetColumn()
  RETURN

QEIP1:EM.Run                   PROCEDURE(BYTE Req)
ReturnValue BYTE,AUTO
  CODE
  DISABLE(?Insert,?Delete)
  ReturnValue = PARENT.Run(Req)
  ENABLE(?Insert,?Delete)
  RETURN(ReturnValue)

QEIP1:EM.TakeAction            PROCEDURE(UNSIGNED Action)
  CODE
  PARENT.TakeAction(Action)

QEIP1:EM.TakeCompleted         PROCEDURE(BYTE Force)
SaveAns UNSIGNED,AUTO
  CODE
      IF Force = BUTTON:Yes
         IF INSTRING('*',ClarionExtensionsQueue.FileExtension,1)  |
         OR INSTRING('?',ClarionExtensionsQueue.FileExtension,1)
            MESSAGE('File Extension may not contain wild cards (*,?)','Data Validation Error',ICON:HAND)
            Force = BUTTON:Cancel
         ELSIF ClarionExtensionsQueue.FileExtension[1] <> '.'
            ClarionExtensionsQueue.FileExtension = '.' & ClarionExtensionsQueue.FileExtension
         END
      END
  SELF.Again = 0
  SELF.ClearColumn
  SaveAns = CHOOSE(Force = 0,Button:Yes,Force)
  IF SELF.Fields.Equal()
      SaveAns = Button:No
  ELSE
     IF ~Force
        SaveAns = GlobalErrors.Message(Msg:SaveRecord,Button:Yes+Button:No+Button:Cancel,Button:Yes)
     END
  END
  Force = 0
  SELF.Response = RequestCancelled
  CASE SaveAns
    OF Button:Cancel
       SELF.Again = 1
    OF Button:No
       IF SELF.Req = InsertRecord
          DELETE(ClarionExtensionsQueue)
          IF SELF.CurrentChoice AND SELF.Insert <> EIPAction:Before
             SELF.CurrentChoice -= 1
          END
       ELSE
          SELF.Fields.AssignLeftToRight
          PUT(ClarionExtensionsQueue)
       END
    OF Button:Yes
       SELF.Response = RequestCompleted
  END
  PARENT.TakeCompleted(Force)
  IF SELF.Response = RequestCompleted
     SORT(ClarionExtensionsQueue,ClarionExtensionsQueue.FileExtension)
  END
  RETURN

QEIP1:EM.TakeEvent             PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  ReturnValue = PARENT.TakeEvent()
  RETURN(ReturnValue)

QEIP1:EM.TakeFieldEvent        PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  ReturnValue = PARENT.TakeFieldEvent()
  RETURN(ReturnValue)

QEIP1:EM.TakeFocusLoss         PROCEDURE
  CODE
  PARENT.TakeFocusLoss()
  RETURN

QEIP1:EM.TakeNewSelection      PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  IF FIELD() = ?List
    IF CHOICE(?List) = SELF.CurrentChoice
      ReturnValue = PARENT.TakeNewSelection()
    ELSE                                  ! Focus change to different record
      SELF.TakeFocusLoss
      IF SELF.Again
        SELECT(?List,SELF.CurrentChoice)
        ReturnValue = Level:Benign
      ELSE
        SELF.CurrentChoice = CHOICE(?List)
        SELF.Response = RequestCancelled           ! Avoid cursor following 'new' record
        ReturnValue = Level:Fatal
      END
    END
  END
  RETURN(ReturnValue)

QEIP1::FileExtension.CreateControl    PROCEDURE
   CODE
      PARENT.CreateControl()
      SELF.Feq{PROP:Upr} = TRUE
      RETURN

QEIP1::FileExtension.TakeAccepted    PROCEDURE(BYTE Action)
ReturnValue BYTE
   CODE
      ReturnValue = PARENT.TakeAccepted(Action)
      !CASE Action
      !  OF EditAction:Complete
      !     IF INSTRING('*',ClarionExtensionsQueue.FileExtension,1)  |
      !     OR INSTRING('?',ClarionExtensionsQueue.FileExtension,1)
      !        MESSAGE('File Extension may not contain wild cards (*,?)','Data Validation Error',ICON:HAND)
      !        ReturnValue = EditAction:None
      !     END
      !END
      RETURN(ReturnValue)

