

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
MacroPlayer PROCEDURE 

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
MacroSetQueue  QUEUE,PRE(msq)
MacroSetName      CSTRING(256)
               END
AutoAdvance          BOOL                                  ! 
loc:MacroQueue       QUEUE,PRE(MQ)                         ! 
feqButton            LONG                                  ! 
feqButtonIcon        LONG                                  ! 
MacroName            CSTRING(21)                           ! 
MacroNameStyle       LONG                                  ! 
szField1             CSTRING(261)                          ! 
szField1Style        LONG                                  ! 
szField2             CSTRING(261)                          ! 
szField2Style        LONG                                  ! 
szField3             CSTRING(261)                          ! 
mark                 BYTE                                  ! 
                     END                                   ! 
MarkCount            LONG
Window               WINDOW('Macro Player'),AT(,,273,176),FONT('Segoe UI',10),RESIZE,GRAY,IMM,HLP('MacroPlayer.htm'), |
  SYSTEM,TOOLBOX
                       LIST,AT(10,10,253,132),USE(?MacroList),VSCROLL,ALRT(MouseLeft2),FORMAT('10L(2)I@p_pB@70' & |
  'L(2)|Y~Action~@s20@96L(2)|Y~Parameter 1~L(1)S(1080)@s255@96L(2)|Y~Parameter 2~S(1080)@s255@'), |
  FROM(loc:MacroQueue)
                       CHECK(' Advance to next action after playing'),AT(10,145),USE(AutoAdvance)
                       BUTTON,AT(10,157,18,14),USE(?cmdMoveUp),ICON('abuprow.ico'),TIP('Move Action Up')
                       BUTTON,AT(30,157,18,14),USE(?cmdMoveDown),ICON('abdnrow.ico'),TIP('Move Action Down')
                       BUTTON,AT(50,157,18,14),USE(?cmdDelete),ICON('delete16.ico'),TIP('Delete Action')
                       BUTTON('&Load'),AT(75,157,45),USE(?cmdLoad),TIP('Load a Saved Macro Set')
                       BUTTON('&Save'),AT(125,157,45),USE(?cmdSave),TIP('Save Actions to a Macro Set')
                       BUTTON('&Play'),AT(175,157,45,14),USE(?cmdPlay),DEFAULT,TIP('Perform the Highlighted Action')
                       BUTTON,AT(225,157,18,14),USE(?cmdNextMacro),ICON('NextMacro.ico'),TIP('Move to Next Act' & |
  'ion in List')
                       BUTTON,AT(245,157,18,14),USE(?cmdPreviousMacro),ICON('PreviousMacro.ico'),TIP('Move to Pr' & |
  'evious Action in List')
                     END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
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

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
  !------------------------------------
  !Style for ?MacroList
  !------------------------------------
  ?MacroList{PROPSTYLE:FontName, 1}      = 'Segoe UI Semibold'
  ?MacroList{PROPSTYLE:FontSize, 1}      = 10
  ?MacroList{PROPSTYLE:FontStyle, 1}     = 600
  ?MacroList{PROPSTYLE:TextColor, 1}     = -1
  ?MacroList{PROPSTYLE:BackColor, 1}     = 8454143
  ?MacroList{PROPSTYLE:TextSelected, 1}  = 8454143
  ?MacroList{PROPSTYLE:BackSelected, 1}  = 12632256
  !------------------------------------
  ?MacroList{PROP:IconList,1} = '~check-green.ico'
!---------------------------------------------------------------------------
FillLocalQueue    ROUTINE
   DATA
i     LONG
j     LONG

   CODE
      FREE(loc:MacroQueue)
      j = RECORDS(MacroQueue)
      LOOP i = 1 TO j
         GET(MacroQueue,i)
         DO FormatQueueRecord
         ADD(loc:MacroQueue)
      END
   EXIT

FormatQueueRecord ROUTINE
   CASE MacroQueue.feqButton
     OF BUTTON:NextFolder
        loc:MacroQueue.MacroName = 'Next Folder'
     OF BUTTON:PreviousFolder
        loc:MacroQueue.MacroName = 'Previous Folder'
     OF BUTTON:NextFile
        loc:MacroQueue.MacroName = 'Next File'
     OF BUTTON:PreviousFile
        loc:MacroQueue.MacroName = 'Previous File'
     OF BUTTON:DeleteLine
        loc:MacroQueue.MacroName = 'Delete Line'
     OF BUTTON:DeleteFile
        loc:MacroQueue.MacroName = 'Delete File'
     OF BUTTON:DeleteExtension
        loc:MacroQueue.MacroName = 'Delete Extension'
     OF BUTTON:DeletePath
        loc:MacroQueue.MacroName = 'Delete Path'
     OF BUTTON:DeleteComments
        loc:MacroQueue.MacroName = 'Delete Comments'
     OF BUTTON:DeleteLabels
        loc:MacroQueue.MacroName = 'Delete Labels'
     OF BUTTON:DeleteCode
        loc:MacroQueue.MacroName = 'Delete Code'
     OF BUTTON:DeleteData
        loc:MacroQueue.MacroName = 'Delete Data'
     OF BUTTON:FindAndDelete
        loc:MacroQueue.MacroName = 'Find and Delete'
   END
   loc:MacroQueue.feqButton = MacroQueue.feqButton
   CASE loc:MacroQueue.feqButton
     OF BUTTON:DELETEFILE
        loc:MacroQueue.szField1  = MacroQueue.szField1 & MacroQueue.szField2
        loc:MacroQueue.szField2  = MacroQueue.szField3
   ELSE
        loc:MacroQueue.szField1  = MacroQueue.szField1
        loc:MacroQueue.szField2  = MacroQueue.szField2
   END
   loc:MacroQueue.mark      = FALSE
   loc:MacroQueue.MacroNameStyle = 0
   loc:MacroQueue.szField1Style  = 0
   loc:MacroQueue.szField2Style  = 0
   EXIT

SaveMacroQueue ROUTINE
   DATA
i        LONG
j        LONG
n        LONG
p        LONG
   CODE
      FREE(MacroQueue)
      j = POINTER(loc:MacroQueue)
      LOOP i = 1 TO RECORDS(loc:MacroQueue)
         GET(loc:MacroQueue,i)
         MacroQueue.feqButton = loc:MacroQueue.feqButton
         CASE loc:MacroQueue.feqButton
           OF BUTTON:DeleteFile
              n = LEN(loc:MacroQueue.szField1)
              LOOP p = n to 1 BY -1
                 IF loc:MacroQueue.szField1[p] = '.'
                    BREAK
                 END
              END
              IF p > 0
                 MacroQueue.szField1  = loc:MacroQueue.szField1[1 : p-1]
                 MacroQueue.szField2  = loc:MacroQueue.szField1[p : n]
                 MacroQueue.szField3  = loc:MacroQueue.szField2
              ELSE
                 MacroQueue.szField1  = loc:MacroQueue.szField1
                 MacroQueue.szField2  = ''
                 MacroQueue.szField3  = loc:MacroQueue.szField2
              END
         ELSE
            MacroQueue.szField1  = loc:MacroQueue.szField1
            MacroQueue.szField2  = loc:MacroQueue.szField2
            MacroQueue.szField3  = ''
         END
         MacroQueue.mark      = loc:MacroQueue.mark
         ADD(MacroQueue)
         loc:MacroQueue.feqButtonIcon = loc:MacroQueue.mark
         PUT(loc:MacroQueue)
      END
      GET(loc:MacroQueue,j)
      ?MacroList{PROP:Selected} = j
   EXIT

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

i        LONG
j        LONG
  CODE
    
  GlobalErrors.SetProcedureName('MacroPlayer')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?MacroList
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  DO FillLocalQueue
  SELF.Open(Window,glo:MainWindow)
  OMIT('***')
  SELF.Open(Window)                                        ! Open window
  !***
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  ?MacroList{PROP:LineHeight} = 10
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
  Window{PROP:MinWidth} = Window{PROP:Width}               ! Restrict the minimum window width to the 'design time' width
  Window{PROP:MinHeight} = 93                              ! Restrict the minimum window height
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  ?MacroList{PROP:FontName}   = glo:ResultListFontName
  ?MacroList{PROP:FontSize}   = glo:ResultListFontSize
  ?MacroList{PROP:FontColor}  = glo:ResultListForeColor
  ?MacroList{PROP:FontStyle}  = glo:ResultListFontStyle
  ?MacroList{PROP:LineHeight} = glo:ResultListFontSize
  ?MacroList{PROPLIST:Width,1} = ?MacroList{PROP:LineHeight} * 1.45
  INIMgr.Fetch('MacroPlayer',Window)                       ! Restore window settings from non-volatile store
  INIMgr.Fetch('MacroPlayer','AutoAdvance',AutoAdvance)
  INIMgr.FetchQueue('MacroSets','MacroSet',MacroSetQueue,MacroSetQueue.MacroSetName)
  Resizer.Resize                                           ! Reset required after window size altered by INI manager
  SELF.SetAlerts()
  oHH &= NEW tagHTMLHelp
  oHH.Init( 'kss.chm' )
  oHH.SetTopic('MacroPlayer.htm')
  GET(loc:MacroQueue,1)
  ?MacroList{PROP:Selected} = 1
  POST(EVENT:NewSelection,?MacroList)
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
!  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.Opened
    INIMgr.Update('MacroPlayer',Window)                    ! Save window data to non-volatile store
  END
  INIMgr.Update('MacroPlayer','AutoAdvance',AutoAdvance)
  INIMgr.UpdateQueue('MacroSets','MacroSet',MacroSetQueue,MacroSetQueue.MacroSetName)
  GlobalErrors.SetProcedureName
  IF ~oHH &= NULL
    oHH.Kill()
    DISPOSE( oHH )
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

i                 LONG
retVal            LONG
thisMacroSetName  CSTRING(256)
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
    OF ?cmdMoveUp
      ThisWindow.Update()
      GET(loc:MacroQueue,CHOICE(?MacroList))
      i = POINTER(loc:MacroQueue)
      DELETE(loc:MacroQueue)
      ADD(loc:MacroQueue,i-1)
      DO SaveMacroQueue
      ?MacroList{PROP:Selected} = i-1
      POST(EVENT:NewSelection,?MacroList)
    OF ?cmdMoveDown
      ThisWindow.Update()
      GET(loc:MacroQueue,CHOICE(?MacroList))
      i = POINTER(loc:MacroQueue)
      DELETE(loc:MacroQueue)
      ADD(loc:MacroQueue,i+1)
      DO SaveMacroQueue
      ?MacroList{PROP:Selected} = i+1
      POST(EVENT:NewSelection,?MacroList)
    OF ?cmdDelete
      ThisWindow.Update()
      RetVal = CHOOSE(GlobalErrors.Throw(Msg:ConfirmDelete) = Level:Benign,RequestCompleted,RequestCancelled)
      IF RetVal = RequestCompleted
         GET(loc:MacroQueue,CHOICE(?MacroList))
         DELETE(loc:MacroQueue)
         DO SaveMacroQueue
         IF CHOICE(?MacroList) = 0
            GET(loc:MacroQueue,RECORDS(loc:MacroQueue))
            ?MacroList{PROP:Selected} = POINTER(loc:MacroQueue)
         END
         POST(EVENT:NewSelection,?MacroList)
      END
    OF ?cmdLoad
      ThisWindow.Update()
      RetVal = winLoadMacroSet(MacroSetQueue)
      IF RetVal > 0
         GET(MacroSetQueue,RetVal)
         FREE(loc:MacroQueue)
         INIMgr.FetchQueue(MacroSetQueue.MacroSetName,'Macro',loc:MacroQueue,loc:MacroQueue.feqButton,loc:MacroQueue.szField1,loc:MacroQueue.szField2,loc:MacroQueue.szField3)
         DO SaveMacroQueue
         POST(EVENT:NewSelection,?MacroList)
      END
    OF ?cmdSave
      ThisWindow.Update()
      winGetMacroSetName(thisMacroSetName)
      IF ThisMacroSetName <> ''
         MacroSetQueue.MacroSetName = thisMacroSetName
         ADD(MacroSetQueue)
         INIMgr.UpdateQueue('MacroSets','MacroSet',MacroSetQueue,MacroSetQueue.MacroSetName)
      
         INIMgr.UpdateQueue(ThisMacroSetName,'Macro',loc:MacroQueue,loc:MacroQueue.feqButton,loc:MacroQueue.szField1,loc:MacroQueue.szField2,loc:MacroQueue.szField3)
         MESSAGE('Macro Set ' & thisMacroSetName & ' saved.','Save Macro Set',ICON:Asterisk)
      END
    OF ?cmdPlay
      ThisWindow.Update()
      GET(MacroQueue,CHOICE(?MacroList))
      POST(EVENT:PLAYMACRO,,1)
      
      GET(loc:MacroQueue,CHOICE(?MacroList))
      loc:MacroQueue.feqButtonIcon = 1
      PUT(loc:MacroQueue)
      
      IF AutoAdvance = TRUE
         IF CHOICE(?MacroList) < RECORDS(loc:MacroQueue)
            ?MacroList{PROP:Selected} = CHOICE(?MacroList)+1
            POST(EVENT:NewSelection,?MacroList)
         END
      END
    OF ?cmdNextMacro
      ThisWindow.Update()
      IF CHOICE(?MacroList) < RECORDS(loc:MacroQueue)
         ?MacroList{PROP:Selected} = CHOICE(?MacroList)+1
         POST(EVENT:NewSelection,?MacroList)
      END
    OF ?cmdPreviousMacro
      ThisWindow.Update()
      IF CHOICE(?MacroList) > 1
         ?MacroList{PROP:Selected} = CHOICE(?MacroList)-1
         POST(EVENT:NewSelection,?MacroList)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

i      LONG
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
  OF ?MacroList
    CASE EVENT()
    OF EVENT:AlertKey
      CASE KEYCODE()
        OF MouseLeft2
           POST(EVENT:Accepted,?cmdPlay)
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
    OF ?MacroList
      GET(loc:MacroQueue,CHOICE(?MacroList))
      !      IF loc:MacroQueue.mark = TRUE
      !         ?cmdMark{PROP:Icon} = '~checkno.ico'
      !      ELSE
      !         ?cmdMark{PROP:Icon} = '~checkyes.ico'
      !      END
      !      DISPLAY(?cmdMark)
      
      IF RECORDS(loc:MacroQueue) = 0
         DISABLE(?cmdDelete)
      !         DISABLE(?cmdMark)
         DISABLE(?cmdSave)
         DISABLE(?cmdPlay)
      !         DISABLE(?cmdPlaySelections)
      ELSE
         ENABLE(?cmdDelete)
      !         ENABLE(?cmdMark)
      !         ENABLE(?cmdSave)
         ENABLE(?cmdPlay)
      !         IF MarkCount > 0
      !            ENABLE(?cmdPlaySelections)
      !         ELSE
      !            DISABLE(?cmdPlaySelections)
      !         END
      END
      
      IF RECORDS(loc:MacroQueue) < 2
         DISABLE(?cmdMoveUp)
         DISABLE(?cmdMoveDown)
         DISABLE(?cmdNextMacro)
         DISABLE(?cmdPreviousMacro)
      ELSE
         CASE ?MacroList{PROP:Selected}
           OF 1
              DISABLE(?cmdMoveUp)
              ENABLE(?cmdMoveDown)
              DISABLE(?cmdPreviousMacro)
              ENABLE(?cmdNextMacro)
           OF RECORDS(loc:MacroQueue)
              ENABLE(?cmdMoveUp)
              DISABLE(?cmdMoveDown)
              ENABLE(?cmdPreviousMacro)
              DISABLE(?cmdNextMacro)
         ELSE
              ENABLE(?cmdMoveUp)
              ENABLE(?cmdMoveDown)
              ENABLE(?cmdPreviousMacro)
              ENABLE(?cmdNextMacro)
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
    OF EVENT:CloseWindow
      DO SaveMacroQueue
      POST(EVENT:MACROPLAYERCLOSED,,1)
    OF EVENT:DoResize
      ?MacroList{PROPLIST:Width,3} = (?MacroList{PROP:Width} - 60)/2
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
  SELF.SetStrategy(?MacroList, Resize:FixLeft+Resize:FixTop, Resize:ConstantRight+Resize:ConstantBottom) ! Override strategy for ?MacroList
  SELF.SetStrategy(?cmdMoveUp, Resize:FixLeft+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?cmdMoveUp
  SELF.SetStrategy(?cmdMoveDown, Resize:FixLeft+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?cmdMoveDown
  SELF.SetStrategy(?cmdDelete, Resize:FixLeft+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?cmdDelete
  SELF.SetStrategy(?cmdPlay, Resize:FixRight+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?cmdPlay
  SELF.SetStrategy(?AutoAdvance, Resize:FixLeft+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?AutoAdvance
  SELF.SetStrategy(?cmdLoad, Resize:FixRight+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?cmdLoad
  SELF.SetStrategy(?cmdSave, Resize:FixRight+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?cmdSave
  SELF.SetStrategy(?cmdNextMacro, Resize:FixRight+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?cmdNextMacro
  SELF.SetStrategy(?cmdPreviousMacro, Resize:FixRight+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?cmdPreviousMacro

!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
winGetMacroSetName PROCEDURE (*CSTRING MacroSetName)

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
loc:MacroSetName     CSTRING(256)                          ! 
Window               WINDOW('Macro Set Name ...'),AT(,,262,45),FONT('Segoe UI',10),GRAY,SYSTEM
                       BUTTON('&OK'),AT(157,25,45,14),USE(?cmdOK),DEFAULT
                       BUTTON('&Cancel'),AT(207,25,45,14),USE(?cmdCancel)
                       PROMPT('Macro Set Name'),AT(10,10),USE(?loc:MacroSetName:Prompt)
                       ENTRY(@s255),AT(72,10,180,10),USE(loc:MacroSetName)
                     END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

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
    
  GlobalErrors.SetProcedureName('winGetMacroSetName')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?cmdOK
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  loc:MacroSetName = MacroSetName
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
  INIMgr.Fetch('winGetMacroSetName',Window)                ! Restore window settings from non-volatile store
  CorrectForOffscreen(Window)
  SELF.SetAlerts()
  oHH &= NEW tagHTMLHelp
  oHH.Init( 'kss.chm' )
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
!  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.Opened
    INIMgr.Update('winGetMacroSetName',Window)             ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  IF ~oHH &= NULL
    oHH.Kill()
    DISPOSE( oHH )
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

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
    OF ?cmdOK
      ThisWindow.Update()
      MacroSetName = loc:MacroSetName
      POST(EVENT:CloseWindow)
    OF ?cmdCancel
      ThisWindow.Update()
      MacroSetName = ''
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

!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
winLoadMacroSet PROCEDURE (MacroSetQueueType MacroSetQueue)

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
RetVal               LONG                                  ! 
Window               WINDOW('Select Macro Set  ...'),AT(0,0,256,90),FONT('Segoe UI',10),RESIZE,GRAY,SYSTEM
                       LIST,AT(10,7,237,60),USE(?MacroSetList),FORMAT('60L(2)~Macro Set Name~@S255@'),FROM(MacroSetQueue)
                       BUTTON('&OK'),AT(154,71,45,14),USE(?OkButton),DEFAULT
                       BUTTON('&Cancel'),AT(202,71,45,14),USE(?CancelButton)
                     END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop
  RETURN(RetVal)

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
    
  GlobalErrors.SetProcedureName('winLoadMacroSet')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?MacroSetList
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.Open(Window)                                        ! Open window
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  ?MacroSetList{PROP:LineHeight} = 10
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
  INIMgr.Fetch('winLoadMacroSet',Window)                   ! Restore window settings from non-volatile store
  Resizer.Resize                                           ! Reset required after window size altered by INI manager
  CorrectForOffscreen(Window)
  SELF.SetAlerts()
  oHH &= NEW tagHTMLHelp
  oHH.Init( 'kss.chm' )
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
!  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.Opened
    INIMgr.Update('winLoadMacroSet',Window)                ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  IF ~oHH &= NULL
    oHH.Kill()
    DISPOSE( oHH )
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

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
    OF ?OkButton
      ThisWindow.Update()
      RetVal = CHOICE(?MacroSetList)
      POST(EVENT:CloseWindow)
    OF ?CancelButton
      ThisWindow.Update()
      RetVal = 0
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


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window
  SELF.SetStrategy(?MacroSetList, Resize:FixLeft+Resize:FixTop, Resize:ConstantRight+Resize:ConstantBottom) ! Override strategy for ?MacroSetList
  SELF.SetStrategy(?OkButton, Resize:FixRight+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?OkButton
  SELF.SetStrategy(?CancelButton, Resize:FixRight+Resize:FixBottom, Resize:LockSize) ! Override strategy for ?CancelButton

