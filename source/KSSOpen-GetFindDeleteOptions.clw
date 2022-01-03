

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
   INCLUDE('ABWINDOW.INC'),ONCE

!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
GetFindDeleteOptions PROCEDURE (*SearchFindOptionsGroupType SearchFindOptions)

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
thisSearchFindOptions GROUP(SearchFindOptionsGroupType),PRE() ! 
                     END                                   ! 
FindTextQueue        FindTextQueueType                     ! 
cc                   BYTE(LEVEL:User)                      ! Completion Code
Window               WINDOW('Find and Delete Options'),AT(,,290,208),FONT('Segoe UI',10,,,CHARSET:DEFAULT),DOUBLE, |
  ICON('kss.ico'),GRAY,HLP('FindAndDeleteOptions.htm'),SYSTEM,IMM
                       PROMPT('Find'),AT(5,5),USE(?FindText:Prompt),TRN
                       COMBO(@S255),AT(30,5,255,10),USE(thisSearchFindOptions.szFindText,,?FindText),DROP(20),FROM(FindTextQueue)
                       OPTION('Match In'),AT(5,20,280,25),USE(thisSearchFindOptions.SearchLocation,,?SearchLocation), |
  BOXED
                         RADIO('Path'),AT(11,30),USE(?SearchLocation:Radio1),TRN,VALUE('1')
                         RADIO('Filename'),AT(55,30),USE(?SearchLocation:Radio2),TRN,VALUE('2')
                         RADIO('Extension'),AT(115,30),USE(?SearchLocation:Radio2:2),TRN,VALUE('3')
                         RADIO('Location'),AT(175,30),USE(?SearchLocation:Radio3),TRN,VALUE('5')
                         RADIO('Text'),AT(233,30),USE(?SearchLocation:Radio4),FONT(,,,FONT:bold),TRN,VALUE('6')
                       END
                       OPTION('Delete Condition'),AT(5,50,280,25),USE(thisSearchFindOptions.DeleteCondition,,?DeleteCondition), |
  BOXED
                         RADIO('Delete rows when they match'),AT(11,60),USE(?DeleteCondition:Radio1),TRN,VALUE('0')
                         RADIO('Delete rows when they do NOT match'),AT(132,60),USE(?DeleteCondition:Radio2),TRN,VALUE('1')
                       END
                       OPTION('Match Type'),AT(5,80,280,25),USE(thisSearchFindOptions.MatchType,,?MatchType),BOXED
                         RADIO('Simple'),AT(11,90),USE(?MatchType:Radio1),TIP('Searches for an occurrence of the' & |
  ' find parameter in the selected result column.'),TRN,VALUE('0')
                         RADIO('Regular Expression'),AT(87,90,76),USE(?MatchType:Radio2),TIP('A regular expressi' & |
  'on match where the find parameter contains the regular expression.'),TRN,VALUE('2')
                       END
                       BUTTON,AT(169,90,12,10),USE(?cmdRegExHelp),ICON('help.ico'),FLAT,TIP('Regular Expression Help')
                       CHECK(' Case Sensitive'),AT(198,90),USE(thisSearchFindOptions.MatchCase,,?MatchCase),TRN
                       BOX,AT(5,108,279,76),USE(?TextBox),COLOR(COLOR:Black),LINEWIDTH(1)
                       TEXT,AT(7,109,276,74),USE(thisSearchFindOptions.szText,,?szText),VSCROLL,READONLY,SKIP
                       BUTTON('Delete &Now'),AT(222,189,60,14),USE(?cmdDeleteNow),DEFAULT
                       BUTTON('Delete &Selected'),AT(157,189,60,14),USE(?cmdDeleteSelected)
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
  RETURN(cc)

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
    
  GlobalErrors.SetProcedureName('GetFindDeleteOptions')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?FindText:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  thisSearchFindOptions = SearchFindOptions
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.Open(Window)                                        ! Open window
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  ?FindText{PROP:LineHeight} = 10
    IF thisSearchFindOptions.MatchType = Match:Regular
       ENABLE(?cmdRegExHelp)
    ELSE
       DISABLE(?cmdRegExHelp)
    END
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
  INIMgr.Fetch('GetFindDeleteOptions',Window)              ! Restore window settings from non-volatile store
  CorrectForOffscreen(Window)
  INIMgr.FetchQueue('FindText Queue','FindText',FindTextQueue,FindTextQueue.szFindText)
  SELF.SetAlerts()
  oHH &= NEW tagHTMLHelp
  oHH.Init( 'kss.chm' )
  oHH.SetTopic('FindAndDeleteOptions.htm')
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
!  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.Opened
    INIMgr.Update('GetFindDeleteOptions',Window)           ! Save window data to non-volatile store
  END
  INIMgr.UpdateQueue('FindText Queue','FindText',FindTextQueue,FindTextQueue.szFindText)
  GlobalErrors.SetProcedureName
  IF ~oHH &= NULL
    oHH.Kill()
    DISPOSE( oHH )
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
i      LONG
j      LONG
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?FindText
      j = RECORDS(FindTextQueue)
      LOOP i = 1 TO j
         GET(FindTextQueue,i)
         IF FindTextQueue.szFindText = thisSearchFindOptions.szFindText
            BREAK
         END
      END
      IF i > j
         IF j = MAXMRU
            GET(FindTextQueue,j)
            DELETE(FindTextQueue)
         END
         FindTextQueue.szFindText = thisSearchFindOptions.szFindText
         ADD(FindTextQueue,1)
      ELSE
         DELETE(FindTextQueue)
         FindTextQueue.szFindText = thisSearchFindOptions.szFindText
         ADD(FindTextQueue,1)
      END
    OF ?MatchType
      IF thisSearchFindOptions.MatchType = Match:Simple
         DISABLE(?cmdRegExHelp)
      ELSE
         ENABLE(?cmdRegExHelp)
      END
    OF ?cmdRegExHelp
      ThisWindow.Update()
      oHH.ShowTopic('RegularExpressionOperators.htm')
    OF ?cmdDeleteNow
      ThisWindow.Update()
      SearchFindOptions = thisSearchFindOptions
      cc = LEVEL:Benign
      POST(EVENT:CloseWindow)
    OF ?cmdDeleteSelected
      ThisWindow.Update()
      i = ?szText{PROP:SelStart}
      j = ?szText{PROP:SelEnd}
      IF j > i
         thisSearchFindOptions.szFindText = thisSearchFindOptions.szText[i : j]
         j = RECORDS(FindTextQueue)
         LOOP i = 1 TO j
            GET(FindTextQueue,i)
            IF FindTextQueue.szFindText = thisSearchFindOptions.szFindText
               BREAK
            END
         END
         IF i > j
            IF j = MAXMRU
               GET(FindTextQueue,j)
               DELETE(FindTextQueue)
            END
            FindTextQueue.szFindText = thisSearchFindOptions.szFindText
            ADD(FindTextQueue,1)
         ELSE
            DELETE(FindTextQueue)
            FindTextQueue.szFindText = thisSearchFindOptions.szFindText
            ADD(FindTextQueue,1)
         END
         SearchFindOptions = thisSearchFindOptions
         cc = LEVEL:Benign
         POST(EVENT:CloseWindow)
      ELSE
         MESSAGE('This button is intended to make it easy to search|on the selected (highlighted) text from the current line.','No Text Selected',ICON:Asterisk)
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

