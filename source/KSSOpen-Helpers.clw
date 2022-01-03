

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
ColourRGB            PROCEDURE  (LONG r, LONG g, LONG b)   ! Declare Procedure
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

  CODE
    
    RETURN(BOR(r,BOR(BSHIFT(g,8),BSHIFT(b,16))))
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
srcGetColorString    PROCEDURE  (LONG lColor)              ! Declare Procedure
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
sReturn             CSTRING(31)

  CODE
    
     CASE lColor
       OF COLOR:GRADIENTACTIVECAPTION
          lColor = kcr_GetSysColor(COLOR_GRADIENTACTIVECAPTION)
       OF COLOR:GRADIENTINACTIVECAPTION
          lColor = kcr_GetSysColor(COLOR_GRADIENTINACTIVECAPTION)
     END

     CASE lColor
       OF COLOR:NONE
          sReturn = 'COLOR:NONE'
       OF COLOR:SCROLLBAR
          sReturn = 'COLOR:SCROLLBAR'
       OF COLOR:BACKGROUND
          sReturn = 'COLOR:BACKGROUND'
       OF COLOR:ACTIVECAPTION
          sReturn = 'COLOR:ACTIVECAPTION'
       OF COLOR:INACTIVECAPTION
          sReturn = 'COLOR:INACTIVECAPTION'
       OF COLOR:MENU
          sReturn = 'COLOR:MENU'
       OF COLOR:WINDOW
          sReturn = 'COLOR:WINDOW'
       OF COLOR:WINDOWFRAME
          sReturn = 'COLOR:WINDOWFRAME'
       OF COLOR:MENUTEXT
          sReturn = 'COLOR:MENUTEXT'
       OF COLOR:WINDOWTEXT
          sReturn = 'COLOR:WINDOWTEXT'
       OF COLOR:CAPTIONTEXT
          sReturn = 'COLOR:CAPTIONTEXT'
       OF COLOR:ACTIVEBORDER
          sReturn = 'COLOR:ACTIVEBORDER'
       OF COLOR:INACTIVEBORDER
          sReturn = 'COLOR:INACTIVEBORDER'
       OF COLOR:APPWORKSPACE
          sReturn = 'COLOR:APPWORKSPACE'
       OF COLOR:HIGHLIGHT
          sReturn = 'COLOR:HIGHLIGHT'
       OF COLOR:HIGHLIGHTTEXT
          sReturn = 'COLOR:HIGHLIGHTTEXT'
       OF COLOR:BTNFACE
          sReturn = 'COLOR:BTNFACE'
       OF COLOR:BTNSHADOW
          sReturn = 'COLOR:BTNSHADOW'
       OF COLOR:GRAYTEXT
          sReturn = 'COLOR:GRAYTEXT'
       OF COLOR:BTNTEXT
          sReturn = 'COLOR:BTNTEXT'
       OF COLOR:INACTIVECAPTIONTEXT
          sReturn = 'COLOR:INACTIVECAPTIONTEXT'
       OF COLOR:BTNHIGHLIGHT
          sReturn = 'COLOR:BTNHIGHLIGHT'
       OF COLOR:Black
          sReturn = 'COLOR:Black'
       OF COLOR:Maroon
          sReturn = 'COLOR:Maroon'
       OF COLOR:Green
          sReturn = 'COLOR:Green'
       OF COLOR:Olive
          sReturn = 'COLOR:Olive'
       OF COLOR:Navy
          sReturn = 'COLOR:Navy'
       OF COLOR:Purple
          sReturn = 'COLOR:Purple'
       OF COLOR:Teal
          sReturn = 'COLOR:Teal'
       OF COLOR:Gray
          sReturn = 'COLOR:Gray'
       OF COLOR:Silver
          sReturn = 'COLOR:Silver'
       OF COLOR:Red
          sReturn = 'COLOR:Red'
       OF COLOR:Lime
          sReturn = 'COLOR:Lime'
       OF COLOR:Yellow
          sReturn = 'COLOR:Yellow'
       OF COLOR:Blue
          sReturn = 'COLOR:Blue'
       OF COLOR:Fuschia
          sReturn = 'COLOR:Fuschia'
       OF COLOR:Aqua
          sReturn = 'COLOR:Aqua'
       OF COLOR:White
          sReturn = 'COLOR:White'
     ELSE
       !kcr_ltoa(lColor,sReturn,16)
       !sReturn = '0' & UPPER(sReturn) & 'H'

       kcr_ltoa(lColor,sReturn,16)
       sReturn = ALL('0',6-LEN(sReturn)) & sReturn
       sReturn = '#' & sReturn[5 : 6] & sReturn[3 : 4] & sReturn[1 : 2]
       sReturn = UPPER(sReturn)
     END
  RETURN(sReturn)
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
DoubleQuote          PROCEDURE  (*CSTRING InputString, *CSTRING OutputString, LONG bufferSize, *CSTRING quoteChar) ! Declare Procedure
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
i                   LONG
j                   LONG
p                   LONG
quoteCount          LONG
returnString        &CSTRING

  CODE
    
      j = LEN(InputString)
      LOOP i = 1 TO j
         IF InputString[i] = quoteChar[1]
            quoteCount += 1
         END
      END
      IF bufferSize = 0
         RETURN j + quoteCount
      ELSIF bufferSize < j + quoteCount
         RETURN -1
      ELSE
         IF quoteCount = 0
            OutputString = InputString
            RETURN LEN(OutputString)
         ELSE
            p = 0
            LOOP i = 1 TO j
               p += 1
               OutputString[p] = InputString[i]
               IF InputString[i] = quoteChar[1]
                  p += 1
                  OutputString[p] = InputString[i]
               END
            END
            RETURN LEN(OutputString)
         END
      END
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
srcGetRGBColorString PROCEDURE  (LONG lColor)              ! Declare Procedure
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
sReturn             CSTRING(31)

  CODE
    
      CASE lColor
        OF COLOR:GRADIENTACTIVECAPTION
           lColor = kcr_GetSysColor(COLOR_GRADIENTACTIVECAPTION)
        OF COLOR:GRADIENTINACTIVECAPTION
           lColor = kcr_GetSysColor(COLOR_GRADIENTINACTIVECAPTION)
      END
      !sReturn = srcGetColorString(lColor)
      !IF sReturn[1] <> 'C'
         kcr_ltoa(lColor,sReturn,16)
         sReturn = ALL('0',6-LEN(sReturn)) & sReturn
         sReturn = '#' & sReturn[5 : 6] & sReturn[3 : 4] & sReturn[1 : 2]
      !END
      RETURN UPPER(sReturn)
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
URLEncode            PROCEDURE  (*CSTRING source, *CSTRING destination) ! Declare Procedure
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
WordCharacters CSTRING('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
i              LONG
j              LONG
n              LONG
d              ULONG
h              STRING(30)

  CODE
    
      n = 1
      j = LEN(source)
      LOOP i = 1 TO j
         IF INSTRING(source[i],WordCharacters)
            destination[n] = source[i]
            n += 1
         ELSE
            d = VAL(source[i])
            LOOP UNTIL (~d)
               h = SUB('0123456789ABCDEF',1+d % 16,1) & CLIP(h)
               d = INT(d / 16)
            END
            destination[n] = '%'
            n += 1
            destination[n] = h[1]
            n += 1
            destination[n] = h[2]
            n += 1
         END
      END
      destination[n] = '<0>'
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
ReplaceTabs          PROCEDURE  (*STRING szText)           ! Declare Procedure
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
i              LONG
j              LONG

  CODE
    
      j = LEN(CLIP(szText))
      LOOP i = 1 TO j
        IF szText[i] = '<9>'
           szText[i] = ' '
        END
      END
      RETURN
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
ReplaceChr           PROCEDURE  (*CSTRING szText, STRING strReplace, STRING strWIth) ! Declare Procedure
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
i              LONG
j              LONG

  CODE
    
      j = LEN(CLIP(szText))
      LOOP i = 1 TO j
        IF szText[i] = strReplace   !'<9>'
           szText[i] = strWith      !' '
        END
      END
      RETURN
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
ColourBrightness     PROCEDURE  (LONG colour) !,BYTE       ! Declare Procedure
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
rgb            LONG
r              LONG
g              LONG
b              LONG
brightness     BYTE

  CODE
    
      CASE colour
        OF COLOR:GRADIENTACTIVECAPTION
           rgb = kcr_GetSysColor(COLOR_GRADIENTACTIVECAPTION)
        OF COLOR:GRADIENTINACTIVECAPTION
           rgb = kcr_GetSysColor(COLOR_GRADIENTINACTIVECAPTION)
      ELSE
           rgb = colour
      END

      r = BAND(rgb,0000000FFh)
      g = BSHIFT(BAND(rgb,00000FF00h),-8)
      b = BSHIFT(BAND(rgb,000FF0000h),-16)
      brightness  = ((r * 299) + (g * 587) + (b * 114)) / 1000

      RETURN brightness
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
winGetKeyAssignment  PROCEDURE  (STRING strAssignmentText, *LONG lKeyCode) ! Declare Procedure
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
keyCodeName       ctKssKeyCodeName
szFunctionText    CSTRING(81)
szAssignmentText  CSTRING(81)
newKeyCode        LONG
ReservedKeyQueue  QUEUE,PRE()
nCode                LONG
                  END
window WINDOW('Application Function Key Assignment'),AT(,,335,82),GRAY, |
            FONT('Segoe UI',10,,,CHARSET:DEFAULT),TIMER(10),DOUBLE
        STRING(@S80),AT(5,10,325),USE(szFunctionText),CENTER
        STRING(@S80),AT(5,20,325),USE(szAssignmentText),CENTER
        STRING('Press Any Key to select it for assignment to the application function.'), |
                AT(47,40,240),USE(?PressAnyKeyString),CENTER
        BUTTON('Assign'),AT(120,60,45,14),USE(?cmdAssign),DISABLE
        BUTTON('Cancel'),AT(169,60,45,14),USE(?cmdCancel)
    END
oHH           &tagHTMLHelp

  CODE
    
  ReservedKeyQueue.nCode = AltDelete
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlC
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlDelete
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlE
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlEnd
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlF
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlF4
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlHome
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlP
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlPgDn
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlPgUp
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlR
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlS
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlShiftF2
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlShiftHook
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlShiftM
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlShiftS
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlSHiftT
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlT
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlW
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = CtrlZ
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = DeleteKey
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = DownKey
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = EnterKey
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = EscKey
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = F12Key
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = F2Key
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = F3Key
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = F5Key
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = glo:MinusKey
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = PgDnKey
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = PgUpKey
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = glo:PlusKey
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)
  ReservedKeyQueue.nCode = UpKey
  ADD(ReservedKeyQueue,+ReservedKeyQueue.nCode)

  newKeyCode = lKeyCode
  szFunctionText = 'The ' & strAssignmentText & ' function'
  szAssignmentText = 'is assigned to the ' & keyCodeName.ToName(newKeyCode)
  OPEN(Window)
  ACCEPT
    CASE EVENT()
      OF EVENT:Timer
         IF KEYCODE() > MouseCenter2 AND KEYCODE() <> EnterKey
            ReservedKeyQueue.nCode = KEYCODE()
            GET(ReservedKeyQueue,+ReservedKeyQueue.nCode)
            IF ~ERRORCODE() AND KEYCODE() <> lKeyCode
               szAssignmentText = keyCodeName.ToName() & ' is assigned to another function - please try a different key'
               ?szAssignmentText{PROP:FontColor} = COLOR:White
               ?szAssignmentText{PROP:Color} = COLOR:Red
               DISABLE(?cmdAssign)
            ELSE
               IF BAND(KEYCODE(),0FFh)
                  newKeyCode = KEYCODE()
                  szAssignmentText = CHOOSE(newKeyCode = lKeyCode,'is','will be') & ' assigned to the ' & keyCodeName.ToName(newKeyCode)
                  ?szAssignmentText{PROP:FontColor} = COLOR:None
                  ?szAssignmentText{PROP:Color} = COLOR:None
                  IF newKeyCode = lKeyCode
                     DISABLE(?cmdAssign)
                  ELSE
                     ENABLE(?cmdAssign)
                     SELECT(?cmdAssign)
                  END
               END
            END
            DISPLAY(?szAssignmentText)
         END
      OF EVENT:Accepted
         CASE ACCEPTED()
           OF ?cmdAssign
              lKeyCode = newKeyCode
              POST(EVENT:CloseWindow)
           OF ?cmdCancel
              POST(EVENT:CloseWindow)
         END
      OF EVENT:CloseWindow
         BREAK
    END
  END
  CLOSE(Window)
  RETURN
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
SilentlyRemoveDirectory PROCEDURE  (*CSTRING szDirPath)    ! Declare Procedure
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
czztFrom      CSTRING(262)
czztTo        CSTRING('<0,0>')
file_op       GROUP(_SHFILEOPSTRUCT),PRE(FO)
              END
oHH           &tagHTMLHelp

  CODE
    
  file_op.hwnd = 0
  file_op.wFunc = FO_DELETE
  czztFrom = szDirPath & '<0,0>'
  file_op.pFrom = ADDRESS(czztFrom)
  file_op.pTo   = ADDRESS(czztTo)
  file_op.fFlags = FOF_NO_UI
  file_op.fAnyOperationsAborted = FALSE
  file_op.hNameMappings = 0
  file_op.lpszProgressTitle = ADDRESS(czztTo)
  kcr_SHFileOperation(file_op)
