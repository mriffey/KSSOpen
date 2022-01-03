

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
   INCLUDE('csciviewer.inc'),ONCE

!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
winReplaceInResults PROCEDURE (*FindGrp FindGroup, *CSTRING szReplaceWith, *ResultQueueType ResultQueue)  !,LONG

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
nReplacements        LONG                                  ! 
ProcessQueue         QUEUE,PRE()                           ! 
Replacements         LONG                                  ! 
ReplacementsStyle    LONG                                  ! 
Filename             CSTRING(261)                          ! 
FilenameStyle        LONG                                  ! 
Modified             BYTE                                  ! 
                     END                                   ! 
listBoxFormat        CSTRING(256)
Window               WINDOW('Replace in Results'),AT(,,244,108),FONT('Segoe UI',10),RESIZE,ALRT(CtrlEnd),ALRT(CtrlHome), |
  ALRT(CtrlPgDn),ALRT(CtrlPgUp),ALRT(DownKey),ALRT(PgDnKey),ALRT(PgUpKey),ALRT(UpKey),CENTER, |
  GRAY,SYSTEM
                       TEXT,AT(1,1,242,106),USE(?sciControl:Region)
                       LIST,AT(4,4,236,70),USE(?ProcessedFileList),VSCROLL,FORMAT('60R(2)|MY~Replacements~@n13' & |
  '@140L(2)|MY~FIlename~S(256)@s255@'),FROM(ProcessQueue)
                       STRING('Black - File processed, changes saved.'),AT(4,76,236,10),USE(?String1),TRN
                       STRING('Gray  - File processed, no changes.'),AT(4,86,236,10),USE(?String2),FONT(,,COLOR:Gray), |
  TRN
                       STRING('Red   - File modified after the creation of the result list, not processed.'),AT(4, |
  96,236,10),USE(?String3),FONT(,,COLOR:Red),TRN
                     END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
SciControl           CLASS(CSciViewer)                     ! Scintilla using ?sciControl:Region
OpenFile               PROCEDURE(*CSTRING szFileName),BOOL,PROC,DERIVED
SaveFile               PROCEDURE(*CSTRING szFilename),BOOL,PROC ! New method added to this class instance
                     END

Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
Resize                 PROCEDURE(),BYTE,PROC,DERIVED
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop
  RETURN(nReplacements)

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
  !------------------------------------
  !Style for ?ProcessedFileList
  !------------------------------------
  ?ProcessedFileList{PROPSTYLE:FontName, 1}      = 'Segoe UI'
  ?ProcessedFileList{PROPSTYLE:FontSize, 1}      = 10
  ?ProcessedFileList{PROPSTYLE:FontStyle, 1}     = 400
  ?ProcessedFileList{PROPSTYLE:TextColor, 1}     = 255
  ?ProcessedFileList{PROPSTYLE:BackColor, 1}     = -1
  ?ProcessedFileList{PROPSTYLE:TextSelected, 1}  = -2147483643
  ?ProcessedFileList{PROPSTYLE:BackSelected, 1}  = 255
  !------------------------------------
  ?ProcessedFileList{PROPSTYLE:FontName, 2}      = 'Segoe UI'
  ?ProcessedFileList{PROPSTYLE:FontSize, 2}      = 10
  ?ProcessedFileList{PROPSTYLE:FontStyle, 2}     = 400
  ?ProcessedFileList{PROPSTYLE:TextColor, 2}     = 8421504
  ?ProcessedFileList{PROPSTYLE:BackColor, 2}     = -1
  ?ProcessedFileList{PROPSTYLE:TextSelected, 2}  = -2147483643
  ?ProcessedFileList{PROPSTYLE:BackSelected, 2}  = 8421504
  !------------------------------------
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
    
  GlobalErrors.SetProcedureName('winReplaceInResults')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?sciControl:Region
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.Open(Window)                                        ! Open window
  Window{PROP:Hide} = TRUE
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  ?ProcessedFileList{PROP:LineHeight} = 10
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
  HIDE(?sciControl:Region)
  ReturnValue = SciControl.Init(Window, ?sciControl:Region, 1002)
  SciControl.SetContextMenuEvent(EVENT:USER)
  IF ReturnValue = Level:Benign
     ThisWindow.AddItem(SciControl.WindowComponent)
  END
  Resizer.Init(AppStrategy:Spread,Resize:SetMinSize)       ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  INIMgr.Fetch('winReplaceInResults',Window)               ! Restore window settings from non-volatile store
  CorrectForOffscreen(Window)
  listBoxFormat = ?ProcessedFileList{PROP:Format}
  INIMgr.Fetch('winReplaceInResults','ListBoxFormat',listBoxFormat)
  ?ProcessedFileList{PROP:Format} = listBoxFormat
  Resizer.Resize                                           ! Reset required after window size altered by INI manager
  SELF.SetAlerts()
  oHH &= NEW tagHTMLHelp
  oHH.Init( 'kss.chm' )
  POST(EVENT:User+1)
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
!  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.Opened
    INIMgr.Update('winReplaceInResults',Window)            ! Save window data to non-volatile store
  END
  IF SELF.Opened
    listBoxFormat = ?ProcessedFileList{PROP:Format}
    INIMgr.Update('winReplaceInResults','ListBoxFormat',listBoxFormat)
  END
  GlobalErrors.SetProcedureName
  IF ~oHH &= NULL
    oHH.Kill()
    DISPOSE( oHH )
  END
  SciControl.Kill()
  SETKEYCODE(0)
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

ResultQueuePtr       LONG
i                    LONG
j                    LONG
n                    LONG
lFoundNext           LONG
lFoundPosition       LONG
cchReplace           LONG
currentFile          CSTRING(MAX_PATH+1)
eolPosition          LONG
stxPosition          LONG
bLineModified        BOOL
bFileModified        BOOL
QueueFilePointer     LONG

thisFile             CSTRING(MAX_PATH+1)
fileQueue            QUEUE(FILE:queue),PRE(fq)
                     END
szText               CSTRING(1024)
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
    OF EVENT:AlertKey
       CASE KEYCODE()
         OF UpKey
            FORWARDKEY(?ProcessedFileList)
         OF DownKey
            FORWARDKEY(?ProcessedFileList)
         OF PgUpKey
            FORWARDKEY(?ProcessedFileList)
         OF PgDnKey
            FORWARDKEY(?ProcessedFileList)
         OF CtrlPgUp
            FORWARDKEY(?ProcessedFileList)
         OF CtrlPgDn
            FORWARDKEY(?ProcessedFileList)
         OF CtrlHome
            FORWARDKEY(?ProcessedFileList)
         OF CtrlEnd
            FORWARDKEY(?ProcessedFileList)
       END
    OF EVENT:OpenWindow
!        post(event:visibleondesktop)
    OF EVENT:User+1
      !Do Search and replace code here
      FREE(ProcessQueue)
      ResultQueuePtr = POINTER(ResultQueue)
      j = RECORDS(ResultQueue)
      LOOP i = 1 TO j
         GET(ResultQueue,i)
  
         IF thisFile <> ResultQueue.SortName
            IF RECORDS(ProcessQueue)
               IF ProcessQueue.Replacements = 0 AND ProcessQueue.Modified = FALSE
                  ProcessQueue.FilenameStyle = 2
                  ProcessQueue.ReplacementsStyle = 2
               END
               PUT(ProcessQueue)
            END
            thisFile = ResultQueue.SortName
            DIRECTORY(fileQueue, thisFile, ff_:Normal)
            IF ResultQueue.FileDate = fileQueue.date AND ResultQueue.FileTime = fileQueue.time
               !file has not changed ... continue
               ProcessQueue.Filename = ResultQueue.Path & ResultQueue.Filename & ResultQueue.szExtension
               ProcessQueue.FilenameStyle = 0
               ProcessQueue.Replacements = 0
               ProcessQueue.ReplacementsStyle = 0
               ProcessQueue.Modified = FALSE
               ADD(ProcessQueue)
            ELSE
               ProcessQueue.Filename = ResultQueue.Path & ResultQueue.Filename & ResultQueue.szExtension
               ProcessQueue.FilenameStyle = 1
               ProcessQueue.Replacements = 0
               ProcessQueue.ReplacementsStyle = 1
               ProcessQueue.Modified = TRUE
               ADD(ProcessQueue)
  
               !skip to next file
               LOOP WHILE ResultQueue.SortName = thisFile
                  i += 1
                  GET(ResultQueue,i)
                  IF ERRORCODE()
                     BREAK
                  END
               END
               i -= 1
               CYCLE
  
            END
         END
  
  
         CASE currentFile
           OF ''
              IF SciControl.OpenFile(ResultQueue.SortName)
                 currentFile = ResultQueue.SortName
                 QueueFilePointer = POINTER(ResultQueue)
                 bFileModified = FALSE
                 bLineModified = FALSE
              ELSE
                 currentFile = ''
              END
  
           OF ResultQueue.SortName
              !process next match
  
         ELSE
            IF bFileModified
               SciControl.SaveFile(currentFile)
               bFileModified = FALSE
               DIRECTORY(fileQueue, currentFile, ff_:Normal)
               LOOP n = QueueFilePointer TO i - 1
                  GET(ResultQueue,n)
                  ResultQueue.FileDate = fileQueue.Date
                  ResultQueue.FileTime = fileQueue.Time
                  PUT(ResultQueue)
               END
               GET(ResultQueue,i)
            END
            IF SciControl.OpenFile(ResultQueue.SortName)
               currentFile = ResultQueue.SortName
               QueueFilePointer = POINTER(ResultQueue)
               bFileModified = FALSE
               bLineModified = FALSE
            ELSE
               currentFile = ''
            END
         END
  
         IF currentFile
            !set the search target to be the entire line
            stxPosition = SciControl.PositionFromLine(ResultQueue.LineNo - 1)
            eolPosition = SciControl.PositionFromLine(ResultQueue.LineNo) - 1
  
            SciControl.SetTargetStart(stxPosition)
            SciControl.SetTargetEnd(eolPosition)
  
            LOOP
               lFoundNext = SciControl.SearchInTarget(LEN(FindGroup.What),FindGroup.What)
               IF lFoundNext = -1
                  BREAK
               ELSIF lFoundNext = INVALID_POSITION
                  BREAK
               ELSE
                  bFileModified = TRUE
                  bLineModified = TRUE
                  lFoundPosition = lFoundNext
                  SciControl.SetSel(lFoundPosition + LEN(FindGroup.What), lFoundPosition)
                  SciControl.TargetFromSelection()
                  cchReplace = SciControl.ReplaceTarget(LEN(szReplaceWith),szReplaceWith)
                  ProcessQueue.Replacements += 1
                  nReplacements += 1
                  SciControl.SetAnchor(lFoundPosition + (LEN(szReplaceWith) + 1))
                  SciControl.SetCurrentPos(SciControl.GetAnchor())
  
                  eolPosition = SciControl.PositionFromLine(ResultQueue.LineNo) - 1
                  IF SciControl.GetCurrentPos() >= eolPosition
                     !we are finished with this line
                     BREAK
                  ELSE
                     SciControl.SetTargetStart(SciControl.GetCurrentPos())
                     SciControl.SetTargetEnd(eolPosition)
                  END
               END
            END
            SciControl.GotoPos(stxPosition)
            IF bLineModified
               bLineModified = FALSE
               SciControl.GetCurLine(SIZE(ResultQueue.Text),ResultQueue.Text)
               PUT(ResultQueue)
            END
         ELSE
            !failed to open the file
         END
      END
  
      !save last file processed
      IF ProcessQueue.Replacements = 0 AND ProcessQueue.Modified = FALSE
         ProcessQueue.FilenameStyle = 2
         ProcessQueue.ReplacementsStyle = 2
      END
      PUT(ProcessQueue)
  
      IF currentFile AND bFileModified
         SciControl.SaveFile(currentFile)
         bFileModified = FALSE
         DIRECTORY(fileQueue, currentFile, ff_:Normal)
         LOOP n = QueueFilePointer TO i - 1
            GET(ResultQueue,n)
            ResultQueue.FileDate = fileQueue.Date
            ResultQueue.FileTime = fileQueue.Time
            PUT(ResultQUeue)
         END
      END
  
      GET(ResultQueue,ResultQueuePtr)
  
      IF RECORDS(ProcessQueue) > 0
         GET(ProcessQueue,1)
         ?ProcessedFileList{PROP:Selected} = 1
         SciControl.SetHide(TRUE)
         Window{PROP:Hide} = FALSE
      ELSE
         POST(EVENT:CloseWindow)
      END
    ELSE
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


SciControl.OpenFile PROCEDURE(*CSTRING szFileName)

ReturnValue          BOOL,AUTO

  CODE
  ReturnValue = PARENT.OpenFile(szFileName)
  SELF.SetLexerType(ResultQueue.SortName)
  SELF.SetScrollWidthTracking(TRUE)
  SELF.SetXCaretPolicy(CARET_SLOP,20)
  SELF.SetYCaretPolicy(BOR(CARET_STRICT,BOR(CARET_SLOP,CARET_EVEN)),1)
  SELF.SetVisiblePolicy(VISIBLE_SLOP,0)
  SELF.SetEOLMode(SC_EOL_CRLF)
  RETURN ReturnValue

SciControl.SaveFile PROCEDURE(*CSTRING szFilename)

! =======================================================================================
! CSciViewer.SaveFile
! purpose:  get contents into Scintilla Control buffer and Save to a file
! inputs :  *CSTRING szFilename - Name of file to save
! outputs:  Scintilla Control is loaded with file contents
! returns:  BYTE
!           Level:Benign to indicate success
!           Level:Notify to indicate failure
! =======================================================================================
ReturnValue         BOOL,AUTO

lFileSize           LONG(0)

szExtension         CSTRING(33)
szMsgText           CSTRING(240)
szMsg_NoCanDo       CSTRING('Not possible.')

szFileExtension     CSTRING(33)

szAAFileName        CSTRING(256),STATIC
A_A                 FILE,DRIVER('DOS'),NAME(szAAFileName),PRE(AA),CREATE
Record                RECORD
Bytes                    STRING(65535)
                    END
                  END

  CODE
!SCI_GETTEXT(int length, char *text)
! This returns length-1 characters of text from the start of the document plus one terminating 0 character.
! To collect all the text in a document, use SCI_GETLENGTH to get the number of characters in the document (nLen),
! allocate a character buffer of length nLen+1 bytes, then call SCI_GETTEXT(nLen+1, char *text).
!If the text argument is 0 then the length that should be allocated to store the entire document is returned.
!If you then save the text, you should use SCI_SETSAVEPOINT to mark the text as unmodified.
  IF szFilename <> ''
     lFileSize = SELF.GetTextLength() + 1                   ! returns length with null terminator
     IF ~SELF.szTextBuffer &= NULL                          ! If we already have a buffer allocated
        !ASSERT(0,eqDBG & 'DISPOSE szTextBuffer [' & ADDRESS(SELF.szTextBuffer) & ']')
        DISPOSE(SELF.szTextBuffer)                          !   dispose buffer
        SELF.szTextBuffer &= NULL                           !   clear reference
     END
     SELF.szTextBuffer &= NEW(CSTRING(lFileSize))           ! Create a buffer to hold the file
     !ASSERT(0,eqDBG & 'NEW szTextBuffer [' & ADDRESS(SELF.szTextBuffer) & ']')
?      ASSERT(~SELF.szTextBuffer &= NULL)
     lFileSize = SELF.GetText(lFileSize,SELF.szTextBuffer)  ! Returns length without null terminator

     szAAFileName = szFileName
     SELF.szFileName &= szAAFileName

     LOOP
        CREATE(A_A)
        IF ~ERRORCODE()
           IF ~SELF.szTextBuffer &= NULL
              DO SaveFile
              SELF.SetSavePoint()
              ReturnValue = TRUE
              BREAK
           ELSE
              SELF.ErrorMgr.ThrowMessage(CSciViewerMsg:BufferAllocationError, lFileSize+1)
              ReturnValue = FALSE
              BREAK
           END
        ELSIF ERRORCODE() = NoAccessErr  !Access Denied
           IF SELF.ErrorMgr.ThrowFile(CSciViewerMsg:AccessDenied, CLIP(szAAFileName)) = Level:Benign
              IF FILEDIALOG('Save as ...',SELF.szFileName,'All Files (*.*)|*.*', FILE:Save + FILE:KeepDir + FILE:LongName)
                 CYCLE
              ELSE
                 SELF.SetSavePoint()
                 ReturnValue = FALSE
                 BREAK
              END
           ELSE
              SELF.SetSavePoint()
              ReturnValue = FALSE
              BREAK
           END
        ELSE
           SELF.ErrorMgr.ThrowFile(CSciViewerMsg:OpenFailed, CLIP(szAAFileName))
           ReturnValue = FALSE
           BREAK
        END
     END
 ELSE
    ReturnValue = FALSE
 END
 RETURN(ReturnValue)
! Exit Procedure

! Procedure Routines
!-------------------------------------------
SaveFile    ROUTINE
!-------------------------------------------
DATA

lRecSize            LONG(0)                 ! Note new variables to keep track of bytes read
lBytesWritten       LONG(0)                 ! from file and Bytes written to buffer.
lBytes2Write        LONG(0)
lBytePtr            LONG(0)

CODE                                      ! Enter Routine

  OPEN(A_A,WriteOnly+DenyAll)
  IF ~ERRORCODE()
     lBytesWritten = 0                                         ! We haven't written any bytes yet
     lFileSize  = SIZE(SELF.szTextBuffer) - 1
     lRecSize   = SIZE(AA:Bytes)

     !ShowWaitCursor = TRUE
     SETCURSOR(CURSOR:Wait)

     LOOP
       IF (lBytesWritten + lrecSize >= lFileSize)                       ! If we have written up to or past the file size
          lBytes2Write = lFileSize - lBytesWritten                       ! Bytes to write to file is the last "partial" chunk
       ELSE
          lBytes2Write = lRecSize                                     ! Else the Byte to write is the full record
       END

       LOOP lBytePtr = 1 TO lBytes2Write                              ! Loop through the record
          A_A:Bytes[lBytePtr] = SELF.szTextBuffer[(lBytesWritten + lBytePtr)]   ! Storing the bytes
       END
       APPEND(A_A,lBytes2Write)

       lBytesWritten = lBytesWritten + lBytes2Write                        ! Increment the byte written
       IF (lBytesWritten >= lFileSize)                                  ! Break if we are up to the file size
          BREAK
       END
     END
     CLOSE(A_A)

     !ShowWaitCursor = FALSE
     SETCURSOR()
  ELSE
     SELF.ErrorMgr.ThrowFile(CSciViewerMsg:OpenFailed, CLIP(szAAFileName))
  END
EXIT
!Exit Routine



Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window
  SELF.SetStrategy(?ProcessedFileList, Resize:FixLeft+Resize:FixTop, Resize:ConstantRight+Resize:ConstantBottom) ! Override strategy for ?ProcessedFileList
  SELF.SetStrategy(?String1, Resize:FixLeft+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?String1
  SELF.SetStrategy(?String2, Resize:FixLeft+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?String2
  SELF.SetStrategy(?String3, Resize:FixLeft+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?String3
  SELF.RemoveControl(?sciControl:Region)                   ! Remove ?sciControl:Region from the resizer, it will not be moved or sized


Resizer.Resize PROCEDURE

ReturnValue          BYTE,AUTO

ColumnWidth          LONG

  CODE
  ColumnWidth = ?ProcessedFileList{PROPLIST:width,1}
  ReturnValue = PARENT.Resize()
  ?ProcessedFileList{PROPLIST:width,2} = ?ProcessedFileList{PROP:Width} - ColumnWidth
  RETURN ReturnValue

