

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
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
CorrectForOffScreen  PROCEDURE  (*WINDOW xChild)           ! Declare Procedure
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
!CorrectPosition     PROCEDURE(*WINDOW xChild, *gtRectCW ChildLoc, *gtRectCW MainLoc)
! Just alter ChildLoc.X and .Y  (leave .W & .H alone)
oHH           &tagHTMLHelp
cMonitors      LONG
MainLoc        LIKE(gtRectCW)
ChildLoc       LIKE(gtRectCW)
CenterOffset   SIGNED
Moved          BOOL

hMonitor                HANDLE
ptFrom                  LIKE(POINT)
rcMain                  LIKE(RECT)
mi                      LIKE(tagMONITORINFO)

MONITOR_DEFAULTTOPRIMARY   EQUATE(00000001h)
 

  CODE
    
  cMonitors = kcr_GetSystemMetrics(SM_CMONITORS)
    
!  !ud.debug('CorrectForOffScreen: API details- Monitors = ' & cMonitors & ' X dimension=' & kcr_GetSystemMetrics(SM_CXSCREEN) & ' ydimension=' & kcr_GetSystemMetrics(SM_CYSCREEN) & ' WinDPI=' & oITWin.GetScreenDPI())
! ! IF cMonitors > 1
! 
     GETPOSITION(glo:MainWindow,MainLoc.X,MainLoc.Y,MainLoc.W,MainLoc.H)
     GETPOSITION(xChild,ChildLoc.X,ChildLoc.Y,ChildLoc.W,ChildLoc.H)
     UD.debug('CorrectForOffScreen: MainLoc(' & MainLoc.X & ',' & MainLoc.Y & ',' & MainLoc.W & ',' & MainLoc.H & ')' )
     UD.debug('CorrectForOffScreen: ChildLoc(' & ChildLoc.X & ',' & ChildLoc.Y & ',' & ChildLoc.W & ',' & ChildLoc.H & ')')

     Moved = CHOOSE( ChildLoc.X + ChildLoc.W > MainLoc.X + MainLoc.W)
     IF Moved
        ChildLoc.X = MainLoc.X + MainLoc.W - ChildLoc.W
     END
     IF ChildLoc.X < MainLoc.X
        ChildLoc.X = MainLoc.X
        Moved = TRUE
     END
     IF Moved
        CenterOffset = CHOOSE( ChildLoc.W >= MainLoc.W, 0, (MainLoc.W - ChildLoc.W) / 2)
        ChildLoc.X = MainLoc.X + CenterOffset
     END


     Moved = CHOOSE(ChildLoc.Y + ChildLoc.H > MainLoc.Y + MainLoc.H)
     IF Moved
        ChildLoc.Y = MainLoc.Y + MainLoc.H - ChildLoc.H
     END
     IF ChildLoc.Y < MainLoc.Y
        ChildLoc.Y = MainLoc.Y
        Moved = TRUE
     END
     IF Moved
        CenterOffset = CHOOSE( ChildLoc.H >= MainLoc.H, 0, (MainLoc.H - ChildLoc.H) / 2)
        ChildLoc.Y = MainLoc.Y + CenterOffset
     END

     SETPOSITION(xChild,ChildLoc.X,ChildLoc.Y,ChildLoc.W,ChildLoc.H)
     UD.debug('CorrectForOffScreen: SetPosition(xChild,' & ChildLoc.X & ',' & ChildLoc.Y & ',' & ChildLoc.W & ',' & ChildLoc.H & ')')

 ! END
  RETURN
