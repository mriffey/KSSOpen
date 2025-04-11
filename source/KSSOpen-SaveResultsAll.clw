

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
!!! </summary>
SaveResultsAll PROCEDURE (tqSearchQueue pSearchQueue, *CSTRING szSendToFilename)

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
tqSearchQueue             QUEUE,TYPE
tabNumber                  LONG
bMatchPatternStartOfLine   BOOL
bMatchPatternEndOfLine     BOOL
bUseRegularExpressions     BOOL
bSearchSubdirectories      BOOL
nLevels                    BYTE
nCurrentLevel              BYTE
bCaseSensitive             BOOL
bExactMatch                BOOL
bExcludeMatch              BOOL
bExcludeComments           BOOL
!bIncludeBinary             BOOL
bSearchPressed             BOOL
szPattern                  CSTRING(1025)
szSearchPath               CSTRING(1025)
szFileMask                 CSTRING(256)
szMatchesFound             CSTRING(256)
ResultQueue                &ResultQueueType
UndoQueue                  &ResultQueueType
feqSearchProgress          LONG
lPointer                   LONG
bFilenamesOnly             BOOL
bFileListFromFile          BOOL
szFileListFilename         CSTRING(261)
bSearchStringsFromFile     BOOL
szSearchStringFilename     CSTRING(261)
szPropertyFile             CSTRING(33)
szExcludeMask              CSTRING(256)
szListBoxFormat            CSTRING(256)
FindGroup                  LIKE(FindGrp)
szReplaceWith              LIKE(FindGrp.What)
                        END
                        
oHH           &tagHTMLHelp
SaveToClipboard               EQUATE(0)
SaveToTextFile                EQUATE(1)
SaveToRestorePoint            EQUATE(2)

CommaDelimited                EQUATE(0)
TabDelimited                  EQUATE(1)
ColumnDelimited               EQUATE(2)

FullPathFormat                EQUATE(0)
FolderFilenameFormat          EQUATE(1)
FolderBasenameExtensionFormat EQUATE(2)
FilenameFormat                EQUATE(3)
BasenameExtensionFormat       EQUATE(4)

crlf                          EQUATE('<13,10>')

DosBufferSize        EQUATE(512000)
DosFilename          CSTRING(261),STATIC
DosFile              FILE,DRIVER('DOS'),NAME(DosFilename),CREATE,PRE(DOS)
                        RECORD
Buffer                     STRING(DosBufferSize)
                        END
                     END
ResultQueue          &ResultQueueType
SaveTo               BYTE                                  ! 
bRetVal              BOOL                                  ! 
szTextFilename       CSTRING(261)                          ! 
ColumnDelimiter      BYTE                                  ! 
FormatOption         BYTE                                  ! 
bQuoteStrings        BYTE                                  ! 
bSendToAfterSave     BOOL                                  ! 
bSaveFilename        BYTE                                  ! 
bSaveLineNumber      BYTE                                  ! 
bSaveLocation        BYTE                                  ! 
bSaveText            BYTE                                  ! 
FormattedFilename    CSTRING(MAXPATH)
Window WINDOW('Save All Session Results To A Reloadable Result File'), |
      AT(,,470,168),CENTER,GRAY,SYSTEM,HLP('SaveResults.htm'),FONT('Segoe UI',10)
    PANEL,AT(5,5,460,48),USE(?PANEL1),BEVEL(1)
    PROMPT('Save to Filename'),AT(10,22),USE(?Filename:Prompt)
    ENTRY(@s255),AT(72,22,354,10),USE(szTextFilename)
    BUTTON('...'),AT(431,21,14,11),USE(?LookupFile:2)
    CHECK(' Send To command after save'),AT(247,58),USE(bSendToAfterSave)
    BUTTON('&Save'),AT(371,56,45,14),USE(?cmdSave)
    BUTTON('&Cancel'),AT(420,56,45,14),USE(?cmdCancel)
  END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
arMaxWidth          LONG,DIM(7)
NumberOfColumns     BYTE
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
FileLookup2          SelectFileClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop
  RETURN(bRetVal)

HandleSave  ROUTINE
   DATA
i                 LONG
j                 LONG
nTextLength       LONG
Quote             CSTRING('"')
Delimiter         CSTRING(',')
szClipboardText   &CSTRING
ExtaCharacters    BYTE
TotalColumnWidth  LONG
szLineNo          CSTRING(11)
pBuffer           LONG
buffSizeNeeded    LONG
quotedText        &CSTRING

   CODE


      CreateRestorePointAll(pSearchQueue,szTextFilename)

      !ASSERT(0,eqDBG & 'DISPOSE szClipboardText [' & ADDRESS(szClipboardText) & ']')
      DISPOSE(szClipboardText)
      szClipboardText &= NULL

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
    
  GlobalErrors.SetProcedureName('SaveResults')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?PANEL1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  ResultQueue &= FindStrOptions.ResultQueue
  SELF.Open(Window)                                        ! Open window
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
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
  INIMgr.Fetch('SaveResults',Window)                       ! Restore window settings from non-volatile store
  CorrectForOffscreen(Window)
  
  INIMgr.Fetch('SaveResults','SaveTo',SaveTo)
  INIMgr.Fetch('SaveResults','szTextFilename',szTextFilename)
  INIMgr.Fetch('SaveResults','ColumnDelimiter',ColumnDelimiter)
  INIMgr.Fetch('SaveResults','FormatOption',FormatOption)
  INIMgr.Fetch('SaveResults','bQuoteStrings',bQuoteStrings)
  INIMgr.Fetch('SaveResults','bSendToAfterSave',bSendToAfterSave)
  
  bSaveFilename = TRUE
  bSaveLineNumber = TRUE
  bSaveLocation = TRUE
  bSaveText = TRUE
  
  INIMgr.Fetch('SaveResults','bSaveFilename',bSaveFilename)
  INIMgr.Fetch('SaveResults','bSaveLineNumber',bSaveLineNumber)
  INIMgr.Fetch('SaveResults','bSaveLocation',bSaveLocation)
  INIMgr.Fetch('SaveResults','bSaveText',bSaveText)
  FileLookup2.Init
  FileLookup2.ClearOnCancel = True
  FileLookup2.Flags=BOR(FileLookup2.Flags,FILE:LongName)   ! Allow long filenames
  FileLookup2.Flags=BOR(FileLookup2.Flags,FILE:Save)       ! Allow save Dialog
  FileLookup2.SetMask('All Files','*.*')                   ! Set the file mask
  FileLookup2.DefaultFile='KSS_Results.txt'
  FileLookup2.WindowTitle='Save as ...'
  FileLookup2.Flags=BOR(FileLookup2.Flags,FILE:KeepDir)    ! Return to current folder
  SELF.SetAlerts()
  oHH &= NEW tagHTMLHelp
  oHH.Init( 'kss.chm' )
  oHH.SetTopic('SaveResults.htm')
  !POST(EVENT:Accepted,?SaveTo)
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
!  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.Opened
    INIMgr.Update('SaveResults',Window)                    ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  IF ~oHH &= NULL
    oHH.Kill()
    DISPOSE( oHH )
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

szDrive              CSTRING(MAXDRIVE+1)
szDir                CSTRING(MAXDIR+1)
szName               CSTRING(MAXFILE+1)
szExtension          CSTRING(MAXEXT+1)
cc                   LONG
Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE ACCEPTED()
    OF szTextFilename      
           bSaveFilename = TRUE
           bSaveLineNumber = TRUE
           bSaveLocation = TRUE
           bSaveText = TRUE
           bSendToAfterSave = FALSE
           FormatOption = FolderBasenameExtensionFormat
           IF szTextFilename = ''
              INIMgr.Fetch('SaveResults','szTextFilename',szTextFilename)
           END
           IF szTextFilename <> ''
              IF UPPER(szTextFilename[LEN(szTextFilename)-3 : LEN(szTextFilename)]) = '.TXT'
                 szTextFilename = szTextFilename[1 : LEN(szTextFilename)-3] & 'rrl'
                 DISPLAY(?szTextFilename)
              END
           END
           ENABLE(?Filename:Prompt,?LookupFile:2)
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?LookupFile:2
      ThisWindow.Update()
      CASE SaveTo
        OF SaveToRestorePoint
           FileLookup2.SetMask('Re-loadable Result List Files','*.RRL')                   ! Set the file mask
           IF szTextFilename = ''
              szTextFilename = svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS') & '\KSS_Results.rrl'
           END
           FileLookup2.DefaultFile = szTextFilename
      ELSE
           FileLookup2.SetMask('All Files','*.*')                   ! Set the file mask
           IF szTextFilename = ''
              szTextFilename = svSpecialFolder.GetDir(SV:CSIDL_PERSONAL) & '\KSS_Results.txt'
           END
           FileLookup2.DefaultFile = szTextFilename
      END
      szTextFilename = FileLookup2.Ask(1)
      DISPLAY
    OF ?cmdSave
      ThisWindow.Update()
      INIMgr.Update('SaveResults','SaveTo',SaveTo)
      INIMgr.Update('SaveResults','szTextFilename',szTextFilename)
      INIMgr.Update('SaveResults','ColumnDelimiter',ColumnDelimiter)
      INIMgr.Update('SaveResults','FormatOption',FormatOption)
      INIMgr.Update('SaveResults','bQuoteStrings',bQuoteStrings)
      INIMgr.Update('SaveResults','bSendToAfterSave',bSendToAfterSave)
      INIMgr.Update('SaveResults','bSaveFilename',bSaveFilename)
      INIMgr.Update('SaveResults','bSaveLineNumber',bSaveLineNumber)
      INIMgr.Update('SaveResults','bSaveLocation',bSaveLocation)
      INIMgr.Update('SaveResults','bSaveText',bSaveText)
      IF szTextFilename = ''
         SELECT(?szTextFilename)
      ELSE
         IF SaveTo = SaveToRestorePoint
            cc = kcr_fnSplit(szTextFilename, szDrive, szDir, szName, szExtension)
            IF UPPER(szExtension) <> '.RRL'
               szTextFilename = szTextFilename & '.rrl'
            END
         END
         DO HandleSave
         POST(EVENT:CloseWindow)
      END
    OF ?cmdCancel
      ThisWindow.Update()
      bRetVal = FALSE
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

