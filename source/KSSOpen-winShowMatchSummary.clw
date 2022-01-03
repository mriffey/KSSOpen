

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
winShowMatchSummary PROCEDURE (ResultQueueType ResultQueue, ResultQueueType UndoQueue, BOOL bCaseSensitive, BOOL bRegularExpression, *CSTRING szPattern)

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
thisSort       CLASS(SortHeaderClassType) !Declare SortHeader Class
QueueResorted     PROCEDURE(STRING pString),VIRTUAL
               END
oHH           &tagHTMLHelp
szFormat       CSTRING(256)
strReturn            STRING(260)                           ! 
AllResultsQueue      ResultQueueType                       ! 
SummaryQueue         QUEUE,PRE(sq)                         ! 
Path                 CSTRING(261)                          ! 
Filename             CSTRING(261)                          ! 
Filetime             CSTRING(21)                           ! 
MatchCount           LONG                                  ! 
DataCount            LONG                                  ! 
CodeCount            LONG                                  ! 
CommentCount         LONG                                  ! 
                     END                                   ! 
FileTime             CSTRING(256)
Window               WINDOW('Match Summary'),AT(,,460,255),FONT('Segoe UI',10),RESIZE,GRAY,HLP('MatchSummary.htm'), |
  SYSTEM
                       PANEL,AT(5,5,450,245),USE(?PANEL1),BEVEL(1)
                       STRING('There were 500 matches found in 250 files.'),AT(10,10),USE(?MatchingFileCountString), |
  TRN
                       STRING('Click on the column header to sort by that column.'),AT(282,10),USE(?Instructions), |
  TRN
                       LIST,AT(10,20,440,225),USE(?MatchSummaryList),VSCROLL,ALRT(MouseLeft2),FORMAT('125L(2)|M~' & |
  'Path~@s255@80L(2)|M~File Name~@s255@80L(2)|M~File Time~@s20@[36R(2)|M~Total~C(0)@n13' & |
  '@36R(2)|M~Data~C(0)@n13@36R(2)|M~Code~C(0)@n13@36R(2)|M~Comment~C(0)@n13@]|~Matches~'), |
  FROM(SummaryQueue)
                     END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
SetAlerts              PROCEDURE(),DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop
  RETURN(strReturn)

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

i              LONG
j              LONG
p              LONG
totalMatches   LONG
bMatchMode     BYTE
thisFilename   CSTRING(MAX_PATH)
thisText       LIKE(ResultQueue.Text)
  CODE
    
  GlobalErrors.SetProcedureName('winShowMatchSummary')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?PANEL1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  p = POINTER(ResultQueue)
  j = RECORDS(ResultQueue)
  LOOP i = 1 TO j
     GET(ResultQueue,i)
     AllResultsQueue = ResultQueue
     ADD(AllResultsQueue,AllResultsQueue.SortName)
  END
  GET(ResultQueue,p)
  
  p = POINTER(UndoQueue)
  j = RECORDS(UndoQueue)
  LOOP i = 1 TO j
     GET(UndoQueue,i)
     AllResultsQueue = UndoQueue
     ADD(AllResultsQueue,AllResultsQueue.SortName)
  END
  GET(UndoQueue,p)
  
  bMatchMode = Match:Simple
  IF NOT bCaseSensitive
     bMatchMode = BOR(bMatchMode,Match:NoCase)
  END
  IF bRegularExpression
     bMatchMode = BOR(bMatchMode,Match:Regular)
  END
  
  totalMatches = 0
  j = RECORDS(AllResultsQueue)
  LOOP i = 1 TO j
     GET(AllResultsQueue,i)
     thisText = CLIP(LEFT(AllResultsQueue.Text))
     SummaryQueue.Path     = UPPER(AllResultsQueue.Path)
     SummaryQueue.FileName = UPPER(AllResultsQueue.Filename & AllResultsQueue.szExtension)
     IF thisFilename = SummaryQueue.Path & SummaryQueue.FileName
       SummaryQueue.MatchCount += 1
       IF AllResultsQueue.szSection = 'DATA'
          SummaryQueue.DataCount += 1
       ELSIF AllResultsQueue.szSection = 'CODE'
          SummaryQueue.CodeCount += 1
       ELSE
       END
       IF NOT MatchWithoutComment(thisText,szPattern,bMatchMode,AllResultsQueue.szExtension)
          SummaryQueue.CommentCount += 1
       end
       PUT(SummaryQueue)
     ELSE
       totalMatches += SummaryQueue.MatchCount
       SummaryQueue.MatchCount = 1
       IF AllResultsQueue.szSection = 'DATA'
          SummaryQueue.DataCount = 1
          SummaryQueue.CodeCount = 0
       ELSIF AllResultsQueue.szSection = 'CODE'
          SummaryQueue.DataCount = 0
          SummaryQueue.CodeCount = 1
       ELSE
          SummaryQueue.DataCount = 0
          SummaryQueue.CodeCount = 0
       END
       IF NOT MatchWithoutComment(thisText,szPattern,bMatchMode,AllResultsQueue.szExtension)
          SummaryQueue.CommentCount = 1
       ELSE
          SummaryQueue.CommentCount = 0
       END
       thisFilename = SummaryQueue.Path & SummaryQueue.FileName
       SummaryQueue.FileTime = FORMAT(GetFileDate(thisFilename),@D010) & ' ' & FORMAT(GetFileTime(thisFilename),@T04)
       ADD(SummaryQueue)
     END
  END
  totalMatches += SummaryQueue.MatchCount
  SELF.Open(Window)                                        ! Open window
  ?MatchingFileCountString{PROP:Text} = 'There were ' & totalMatches & ' matches found in ' & RECORDS(SummaryQueue) & ' files.'
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  ?MatchSummaryList{PROP:LineHeight} = 10
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
  Resizer.Init(AppStrategy:Spread,Resize:SetMinSize)       ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  INIMgr.Fetch('winShowMatchSummary',Window)               ! Restore window settings from non-volatile store
  CorrectForOffscreen(Window)
  szFormat = ?MatchSummaryList{PROP:Format}
  INIMgr.Fetch('winShowMatchSummary','MatchSummaryListFormat',szFormat)
  IF INSTRING('File Time',szFormat,1)
     ?MatchSummaryList{PROP:Format} = szFormat
  END
  Resizer.Resize                                           ! Reset required after window size altered by INI manager
  SELF.SetAlerts()
  oHH &= NEW tagHTMLHelp
  oHH.Init( 'kss.chm' )
  oHH.SetTopic('MatchSummary.htm')
  thisSort.Init(SummaryQueue,?MatchSummaryList)
  thisSort.MultipleColumns = False
  thisSort.NoCase = False
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
!  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  thisSort.ClearSort()
  thisSort.Kill()
  IF SELF.Opened
    INIMgr.Update('winShowMatchSummary',Window)            ! Save window data to non-volatile store
  END
  szFormat = ?MatchSummaryList{PROP:Format}
  INIMgr.Update('winShowMatchSummary','MatchSummaryListFormat',szFormat)
  GlobalErrors.SetProcedureName
  IF ~oHH &= NULL
    oHH.Kill()
    DISPOSE( oHH )
  END
  RETURN ReturnValue


ThisWindow.SetAlerts PROCEDURE

  CODE
  PARENT.SetAlerts
  thisSort.SetAlerts()


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
  IF thisSort.TakeEvents()
     RETURN Level:Notify
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

MouseDownRow      LONG
MouseDownColumn   LONG
MouseDownZone     LONG
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
  OF ?MatchSummaryList
    CASE EVENT()
    OF EVENT:AlertKey
       CASE KEYCODE()
       OF MouseLeft2
          MouseDownZone = ?MatchSummaryList{PROPLIST:MouseDownZone}
          IF MouseDownZone = LISTZONE:field
             MouseDownRow = ?MatchSummaryList{PROPLIST:MouseDownRow}
             GET(SummaryQueue,MouseDownRow)
             strReturn = SummaryQueue.Path & SummaryQueue.Filename
             POST(EVENT:CloseWindow)
          END
       END
    OF EVENT:PreAlertKey
      !       CASE KEYCODE()
      !       OF MouseLeft
      !          MouseDownZone = ?MatchSummaryList{PROPLIST:MouseDownZone}
      !          IF MouseDownZone = LISTZONE:Header AND MouseDownZone <> LISTZONE:GroupHeader
      !             MouseDownColumn = ?MatchSummaryList{PROPLIST:MouseDownField}
      !             EXECUTE MouseDownColumn
      !                SORT(SummaryQueue,SummaryQueue.Path)
      !                SORT(SummaryQueue,SummaryQueue.Filename)
      !                SORT(SummaryQueue,-SummaryQueue.MatchCount)
      !                SORT(SummaryQueue,-SummaryQueue.DataCount)
      !                SORT(SummaryQueue,-SummaryQueue.CodeCount)
      !                SORT(SummaryQueue,-SummaryQueue.CommentCount)
      !             END
      !             CYCLE
      !          END
      !       END
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
  SELF.SetStrategy(?MatchSummaryList, Resize:FixLeft+Resize:FixTop, Resize:ConstantRight+Resize:ConstantBottom) ! Override strategy for ?MatchSummaryList
  SELF.SetStrategy(?PANEL1, Resize:FixLeft+Resize:FixTop, Resize:ConstantRight+Resize:ConstantBottom) ! Override strategy for ?PANEL1
  SELF.SetStrategy(?MatchingFileCountString, Resize:FixLeft+Resize:FixTop, Resize:LockHeight) ! Override strategy for ?MatchingFileCountString
  SELF.SetStrategy(?Instructions, Resize:FixRight+Resize:FixTop, Resize:LockSize) ! Override strategy for ?Instructions

thisSort.QueueResorted     PROCEDURE(STRING pString)  !,VIRTUAL
   CODE
      DISPLAY(?MatchSummaryList)
