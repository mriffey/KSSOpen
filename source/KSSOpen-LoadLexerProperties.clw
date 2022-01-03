

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
LoadLexerProperties  PROCEDURE  (csciControl sciControl, STRING lexer) ! Declare Procedure
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
returnCode           LONG(Level:Fatal)

szAAFileName         CSTRING(260),STATIC
A_A                  FILE,DRIVER('ASCII'),NAME(szAAFileName),PRE(AA)
Record                  RECORD
Buffer                     STRING(1024)
                        END
                     END
szBuffer             CSTRING(1025)

keywordQueue         QUEUE,PRE(kwq)
keywords                CSTRING(1025)
                     END
szKeywords           &CSTRING

styleNumber          LONG
thisToken            CSTRING(33)
oHH           &tagHTMLHelp

  CODE
    
  szAAFilename = svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS') & '\' & lexer & '.properties'
  OPEN(A_A,ReadOnly+DenyWrite)
  IF ERRORCODE()
     szAAFilename = lexer & '.properties'
     OPEN(A_A,ReadOnly+DenyWrite)
  END
  IF ~ERRORCODE()
     SET(A_A)
     NEXT(A_A)
     LOOP UNTIL ERRORCODE()
        szBuffer = CLIP(LEFT(A_A.Buffer))
        IF szBuffer <> '' AND szBuffer[1] = '['
           CASE UPPER(szBuffer[1 : 7])
             OF '[FILEPA'
                DO ProcessFilePatterns
             OF '[OPTION'
                DO ProcessOptions
             OF '[KEYWOR'
                DO ProcessKeywords
             OF '[STYLES'
                DO ProcessStyles
           ELSE
                NEXT(A_A)
           END
        ELSE
           NEXT(A_A)
        END
     END
     CLOSE(A_A)
     returnCode = Level:Benign
  ELSE
     MESSAGE('Unexpected error opening ' & szAAFilename & '|' & ERROR() |
     & '|DEVS: If you get this message frequently, copy ' & szAAFileName & 'from your bin folder to ' & svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS') & '\','Load Lexer Properties',ICON:HAND)
  END
  RETURN returnCode
ProcessFilePatterns  ROUTINE
   NEXT(A_A)
   LOOP UNTIL ERRORCODE()
        szBuffer = CLIP(LEFT(A_A.Buffer))
        IF szBuffer <> '' AND szBuffer[1] <> '!'
           IF szBuffer[1] = '['
              BREAK
           ELSE
              !pick out the file extensions for this lexer
           END
        END
        NEXT(A_A)
   END
   EXIT

ProcessOptions ROUTINE
   DATA
szOption       CSTRING(256)
szOptionValue  CSTRING(256)
p              LONG
n              LONG

   CODE
      NEXT(A_A)
      LOOP UNTIL ERRORCODE()
           szBuffer = CLIP(LEFT(A_A.Buffer))
           IF szBuffer <> '' AND szBuffer[1] <> '!'
              IF szBuffer[1] = '['
                 BREAK
              ELSE
                 n = INSTRING(';',szBuffer)
                 IF n = 0
                    n = LEN(szBuffer)
                 ELSE
                    n -= 1
                 END
                 p = INSTRING('=',szBuffer)
                 IF p
                    !set properties for this lexer
                    szOption = LOWER(CLIP(szBuffer[1 : p-1]))
                    szOptionValue = szBuffer[p+1 : n]
                    CASE szOption
                    OF 'word.characters'
                       sciControl.SetWordChars(szOptionValue)
                    ELSE
                       sciControl.SetProperty(szOption,szOptionValue)
                    END
                 END
              END
           END
           NEXT(A_A)
      END
   EXIT

ProcessKeywords      ROUTINE
   DATA
i              LONG
j              LONG
bytesNeeded    LONG
setNumber      LONG

   CODE
   !pick out set number
   setNumber = szBuffer[10 : LEN(szBuffer)-1]
   NEXT(A_A)
   FREE(keywordQueue)
   LOOP UNTIL ERRORCODE()
        szBuffer = CLIP(LEFT(A_A.Buffer))
        IF szBuffer <> '' AND szBuffer[1] <> '!'
           IF szBuffer[1] = '['
              BREAK
           ELSE
              !add to queue
              keywordQueue.keywords = szBuffer
              ADD(keywordQueue)
           END
        END
        NEXT(A_A)
   END
   !determine length needed for cstring
   bytesNeeded = 0
   j = RECORDS(keywordQueue)
   IF j > 0
      LOOP i = 1 TO j
         GET(keywordQueue,i)
         bytesNeeded += LEN(keywordQueue.keywords)
      END
      bytesNeeded += j+1

      !allocate cstring
      szKeywords &= NEW CSTRING(bytesNeeded)
      LOOP i = 1 TO j
         GET(keywordQueue,i)
         szKeywords = szKeywords & keywordQueue.keywords & ' '
      END
      szKeywords[LEN(szKeywords)] = '<0>'

      !assign to keyword list
      sciControl.SetKeyWords(setNumber,szKeywords)
      DISPOSE(szKeywords)
      szKeywords &= NULL
   END
   EXIT

ProcessStyles        ROUTINE
   DATA
pDelimiter     LONG
pStart         LONG
firstStyle     BOOL(TRUE)

   CODE
   NEXT(A_A)
   LOOP UNTIL ERRORCODE()
        szBuffer = CLIP(LEFT(A_A.Buffer))
        IF szBuffer <> '' AND szBuffer[1] <> '!'
           IF szBuffer[1] = '['
              BREAK
           ELSE
              pDelimiter = INSTRING('=',szBuffer)
              IF pDelimiter
                 styleNumber = szBuffer[6 : pDelimiter-1]
                 LOOP WHILE pDelimiter < LEN(szBuffer)
                      pStart = pDelimiter+1
                      pDelimiter = INSTRING(',',szBuffer,,pStart)
                      IF pDelimiter
                         thisToken = szBuffer[pStart : pDelimiter-1]
                         DO ProcessToken
                      ELSE
                         thisToken = szBuffer[pStart : LEN(szBuffer)]
                         DO ProcessToken
                         BREAK
                      END
                 END
                 IF firstStyle
                    IF styleNumber = 32
                       SciControl.StyleClearAll()
                    END
                    firstStyle = FALSE
                 END
              ELSE
                 BREAK
              END
              !style0=font:Times New Roman,fontsize:11,fore:#000000
              !process the style
           END
        END
        NEXT(A_A)
   END
   EXIT

ProcessToken   ROUTINE
   DATA
nSize    LONG
szFont   CSTRING(33)
lRed     LONG
lGreen   LONG
lBlue    LONG
lColor   LONG
caseOpt  LONG

   CODE
!font:,size:,bold,italic,underline,fore:,back:,eolfilled,case:,hide,hotSpot
   CASE UPPER(thisToken[1 : 4])
     OF 'FONT'
        szFont = thisToken[6 : LEN(thisToken)]
        sciControl.StyleSetFont(styleNumber,szFont)
     OF 'SIZE'
        nSize = thisToken[6 : LEN(thisToken)]
        sciControl.StyleSetSize(styleNumber,nSize)
     OF 'BOLD'
        sciControl.StyleSetBold(styleNumber,TRUE)
     OF 'ITAL'
        sciControl.StyleSetItalic(styleNumber,TRUE)
     OF 'FORE'
        lRed   = EVALUATE('0' & thisToken[7  :  8] & 'h')
        lGreen = EVALUATE('0' & thisToken[9  : 10] & 'h')
        lblue  = EVALUATE('0' & thisToken[11 : 12] & 'h')
        sciControl.StyleSetFore(styleNumber,colourRGB(lRed,lGreen,lBlue))
     OF 'BACK'
        lRed   = EVALUATE('0' & thisToken[7  :  8] & 'h')
        lGreen = EVALUATE('0' & thisToken[9  : 10] & 'h')
        lblue  = EVALUATE('0' & thisToken[11 : 12] & 'h')
        sciControl.StyleSetBack(styleNumber,colourRGB(lRed,lGreen,lBlue))
     OF 'EOLF'
        sciControl.StyleSetEOLFilled(styleNumber,TRUE)
     OF 'CASE'
        caseOpt = thisToken[6 : LEN(thisToken)]
        IF INRANGE(caseOpt,0,2)
           sciControl.StyleSetCase(styleNumber,caseOpt)
        END
     OF 'HIDE'
        sciControl.StyleSetVisible(styleNumber,FALSE)
     OF 'HOTS'
        !sciControl.StyleSetHotSpot(styleNumber,TRUE)
        sciControl.StyleSetHotSpot(styleNumber,glo:bHotSpotsEnabled)
   END
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
GetPropertyFileLexer PROCEDURE  (*CSTRING szPropertyFile) !,STRING ! Declare Procedure
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
szAAFileName         CSTRING(260),STATIC
A_A                  FILE,DRIVER('ASCII'),NAME(szAAFileName),PRE(AA)
Record                  RECORD
Buffer                     STRING(1024)
                        END
                     END
szBuffer             CSTRING(1025)
szLexer              CSTRING(65)
oHH           &tagHTMLHelp

  CODE
    
  szLexer = 'text'
  szAAFilename = svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS') & '\' & szPropertyFile & '.properties'
  OPEN(A_A,ReadOnly+DenyWrite)
  IF ERRORCODE()
     szAAFilename = szPropertyFile & '.properties'
     OPEN(A_A,ReadOnly+DenyWrite)
  END
  IF ~ERRORCODE()
     SET(A_A)
     NEXT(A_A)
     LOOP UNTIL ERRORCODE()
        szBuffer = CLIP(LEFT(A_A.Buffer))
        IF UPPER(szBuffer[1 : 6]) = 'LEXER='
           szLexer = szBuffer[7 : LEN(szBuffer)]
           BREAK
        ELSE
           NEXT(A_A)
        END
     END
     CLOSE(A_A)
  ELSE
     CASE ERRORCODE()
       OF NoFileErr
          glo:szDefaultPropertyFile = 'text'
     ELSE
          MESSAGE('Unexpected error (' & ERRORCODE() & ') opening ' & szAAFilename & '|' & ERROR() | 
                  & '|DEVS: If you get this message frequently, copy ' & szAAFileName & 'from your bin folder to ' & svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS') & '\','Load Lexer Properties',ICON:HAND)
     END
  END
  RETURN szLexer
