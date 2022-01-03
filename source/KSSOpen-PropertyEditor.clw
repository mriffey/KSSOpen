

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
!!! Edit special properties files
!!! </summary>
PropertyEditor PROCEDURE (*CSTRING szPropertyFile, LONG MaxStyleIndex)

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
RECORDSIZE     EQUATE(80)

filequeueIndx  LONG
filequeueMax   LONG
filequeue      QUEUE
record            CSTRING(SIZE(AsciiFile.Buffer))
               END
szBuffer       CSTRING(SIZE(AsciiFile.Buffer)+1)

DefaultErrors  GROUP
Number            USHORT(1)
                  USHORT(Msg:InsertIllegal)
                  BYTE(Level:Notify)
                  PSTRING('Invalid Request')
                  PSTRING('%message already exists')
               END

szDrive        CSTRING(MAXDRIVE+1)
szDir          CSTRING(MAXDIR+1)
szName         CSTRING(MAXFILE+1)
szExtension    CSTRING(MAXEXT+1)
cc             LONG

popupMenu      PopupClass
thisReq        BYTE
bDirty         BOOL(FALSE)
oHH           &tagHTMLHelp
bPropertiesChanged   BOOL                                  ! 
szDescription        CSTRING(256)                          ! 
szLexer              CSTRING(32)                           ! 
szFilePatterns       CSTRING(2048)                         ! 
FilePatternQueue     QUEUE,PRE(fpq)                        ! 
Pattern              CSTRING(21)                           ! 
                     END                                   ! 
ListBoxQueue         QUEUE,PRE(lbq)                        ! 
Value                CSTRING(33)                           ! 
                     END                                   ! 
qOptions             QUEUE,PRE(opt)                        ! 
szOption             CSTRING(256)                          ! 
szOptionTip          CSTRING(256)                          ! 
szValue              CSTRING(256)                          ! 
IsBool               BOOL                                  ! 
                     END                                   ! 
qKeywords            QUEUE,PRE(kwd)                        ! 
szDescription        CSTRING(256)                          ! 
szKeywordNumber      CSTRING(16)                           ! 
szKeywords           CSTRING(2048)                         ! 
                     END                                   ! 
qStyles              QUEUE,PRE(sty)                        ! 
szDescription        CSTRING(256)                          ! 
szStyleNumber        CSTRING(16)                           ! 
szFontName           CSTRING(32)                           ! 
nFontSize            BYTE                                  ! 
nFontStyle           LONG                                  ! 
bBold                BOOL                                  ! 
bEolFilled           BOOL                                  ! 
bHide                BOOL                                  ! 
bHotSpot             BOOL                                  ! 
bItalic              BOOL                                  ! 
bUnderline           BOOL                                  ! 
nCaseOpt             BYTE                                  ! 
lForeColor           LONG                                  ! 
lBackColor           LONG                                  ! 
szSort               CSTRING(16)                           ! 
                     END                                   ! 
szFontDescription    CSTRING(64)                           ! 
szForeColor          CSTRING(31)                           ! 
szBackColor          CSTRING(31)                           ! 
defaultFont    CSTRING(32)
defaultSize    BYTE
defaultStyle   LONG
defaultFore    LONG
defaultBack    LONG
defaultCase    BYTE
defaultHide    BOOL
defaultHot     BOOL
defaultEOLF    BOOL
nCurrentStyle  BYTE
lastComment    CSTRING(256)

saveFilePatterns  LIKE(szFilePatterns)
saveKeywordSet    GROUP,PRE(savkwd)
szDescription        CSTRING(256)                          !
szKeywordNumber      CSTRING(16)                           !
szKeywords           CSTRING(2048)                         !
                  END
saveStyle         GROUP,PRE(savsty)
szDescription        CSTRING(256)                          !
szStyleNumber        CSTRING(16)                           !
szFontName           CSTRING(32)                           !
nFontSize            BYTE                                  !
nFontStyle           LONG                                  !
bBold                BOOL                                  !
bEolFilled           BOOL                                  !
bHide                BOOL                                  !
bHotSpot             BOOL                                  !
bItalic              BOOL                                  !
bUnderline           BOOL                                  !
nCaseOpt             BYTE                                  !
lForeColor           LONG                                  !
lBackColor           LONG                                  !
szSort               CSTRING(16)                           !
                  END
Window               WINDOW('Property Editor'),AT(,,400,149),FONT('Segoe UI',10),HSCROLL,GRAY,HLP('PropertyEditor.htm'), |
  SYSTEM
                       SHEET,AT(5,5,390,120),USE(?Sheet),IMM
                         TAB('General'),USE(?TAB1)
                           PROMPT('Description'),AT(10,20),USE(?szDescription:Prompt),FONT(,,00E16941h,FONT:bold),TRN
                           ENTRY(@s255),AT(55,20,330,10),USE(szDescription),FONT(,,,FONT:regular)
                           PROMPT('File Patterns'),AT(10,30),USE(?FilePatterns:Prompt),FONT(,,00E16941h,FONT:bold),TRN
                           TEXT,AT(10,40,379,20),USE(szFilePatterns),VSCROLL,ALRT(MouseRight),TIP('not case sensit' & |
  'ive, order of extentions is ignored')
                           LIST,AT(10,40,88,80),USE(?lbFilePatterns),VSCROLL,ALRT(CtrlEnter),ALRT(DeleteKey),ALRT(InsertKey), |
  ALRT(MouseRight),FORMAT('80L(2)|M@s20@'),FROM(FilePatternQueue),HIDE
                         END
                         TAB('Options'),USE(?TAB2)
                           LIST,AT(10,22,379,97),USE(?lbOptions),VSCROLL,ALRT(MouseLeft2),FORMAT('100L(2)|MPS(500)' & |
  '@s255@250L(2)|MS(500)@s255@'),FROM(qOptions)
                           TEXT,AT(5,128,286,19),USE(opt:szOptionTip),FONT(,8),FLAT,READONLY,SKIP,TRN
                         END
                         TAB('Keywords'),USE(?TAB3)
                           LIST,AT(10,22,145,97),USE(?lbKeywords),VSCROLL,ALRT(MouseRight),FORMAT('140L(2)|MS(1024)@s255@'), |
  FROM(qKeywords)
                           TEXT,AT(160,22,230,97),USE(kwd:szKeywords),VSCROLL,ALRT(MouseRight)
                           LIST,AT(160,22,230,97),USE(?lbKeywordValues),VSCROLL,ALRT(CtrlEnter),ALRT(DeleteKey),ALRT(InsertKey), |
  ALRT(MouseRight),FORMAT('230L(2)|M@s32@'),FROM(ListBoxQueue),HIDE
                         END
                         TAB('Styles'),USE(?TAB4)
                           LIST,AT(10,22,145,97),USE(?lbStyles),VSCROLL,ALRT(MouseRight),FORMAT('140L(2)|MS(1024)@s255@'), |
  FROM(qStyles)
                           BOX,AT(159,21,231,98),USE(?StyleGroup),COLOR(00A0A0A0h),LINEWIDTH(1)
                           PROMPT('Font'),AT(181,25),USE(?Font:Prompt),TRN
                           ENTRY(@s63),AT(221,25,150,10),USE(szFontDescription),DISABLE
                           BUTTON('...'),AT(375,25,11,10),USE(?cmdFontDialog)
                           BOX,AT(166,40,11,10),USE(?ForeColorBox),COLOR(00B99D7Fh),FILL(COLOR:Black),LINEWIDTH(1),ROUND
                           PROMPT('Fore Color'),AT(181,40),USE(?ForeColor:Prompt)
                           ENTRY(@s30),AT(221,40,150,10),USE(szForeColor),LEFT,DISABLE
                           BUTTON('...'),AT(375,40,11,10),USE(?cmdSelectForeColor),TIP('Select Fore Color')
                           BOX,AT(166,56,11,10),USE(?BackColorBox),COLOR(00B99D7Fh),FILL(COLOR:White),LINEWIDTH(1),ROUND
                           PROMPT('Back Color'),AT(181,56),USE(?BackColor:Prompt)
                           ENTRY(@s30),AT(221,56,150,10),USE(szBackColor),LEFT,DISABLE
                           BUTTON('...'),AT(375,56,11,10),USE(?cmdSelectBackColor),TIP('Select Back Color')
                           CHECK(' Hide'),AT(221,72,40,10),USE(sty:bHide),TRN
                           CHECK(' Hot Spot'),AT(270,72,50,10),USE(sty:bHotSpot),TRN
                           CHECK(' EOL Filled'),AT(331,72,55,10),USE(sty:bEolFilled),TRN
                           PROMPT('Case'),AT(181,87),USE(?nCaseOpt:Prompt)
                           OPTION,AT(221,85,166,14),USE(sty:nCaseOpt)
                             RADIO(' Mixed'),AT(221,87),USE(?STY:NCASEOPT:Mixed),TRN,VALUE('1')
                             RADIO(' Upper'),AT(270,87),USE(?STY:NCASEOPT:Upper),TRN,VALUE('2')
                             RADIO(' Lower'),AT(331,87),USE(?STY:NCASEOPT:Lower),TRN,VALUE('3')
                           END
                           BUTTON('...'),AT(375,87,11,10),USE(?cmdSelectCase)
                           STRING('Legend'),AT(166,103),USE(?STRING5),FONT(,8),TRN
                           STRING('bold'),AT(198,103),USE(?STRING1),FONT(,8,,FONT:bold),TRN
                           STRING('= overrides default'),AT(215,103),USE(?STRING2),FONT(,8),TRN
                           STRING('italic'),AT(278,103),USE(?STRING3),FONT(,8,,FONT:regular+FONT:italic),TRN
                           STRING('= uses default'),AT(295,103),USE(?STRING4),FONT(,8),TRN
                         END
                       END
                       BUTTON('&Save'),AT(298,130,45,14),USE(?cmdSave),DEFAULT
                       BUTTON('Cancel'),AT(348,130,45,14),USE(?cmdCancel)
                       BUTTON('&Insert'),AT(54,130,30,12),USE(?InsertFilePattern),KEY(InsertKey),HIDE
                       BUTTON('&Edit'),AT(88,130,30,12),USE(?ChangeFilePattern),KEY(CtrlEnter),HIDE
                       BUTTON('&Delete'),AT(121,130,30,12),USE(?DeleteFilePattern),KEY(DeleteKey),HIDE
                       BUTTON('&Edit'),AT(159,130,30,12),USE(?ChangeOption),KEY(CtrlEnter),HIDE
                       BUTTON('&Insert'),AT(196,130,30,12),USE(?InsertKeyword),KEY(InsertKey),HIDE
                       BUTTON('&Edit'),AT(229,130,30,12),USE(?ChangeKeyword),KEY(CtrlEnter),HIDE
                       BUTTON('&Delete'),AT(262,130,30,12),USE(?DeleteKeyword),KEY(DeleteKey),HIDE
                     END

QEIP2:SaveEntry      GROUP,PRE(QEIP2)
Value                  LIKE(lbq:Value)
                     END
QEIP2:Fields         FieldPairsClass
QEIP2:PopupString    STRING(20)
QEIP2:PopupMgr       PopupClass
QEIP2:EditList       QUEUE(EditQueue),PRE(QEIP2)
                     END
QEIP2:EM             CLASS(EIPManager)
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

QEIP2::lbq:Value     CLASS(EditEntryClass)
CreateControl          PROCEDURE(),DERIVED                      ! Method added to host embed code
Init                   PROCEDURE(UNSIGNED FieldNumber,UNSIGNED ListBox,*? UseVar),DERIVED ! Method added to host embed code
Kill                   PROCEDURE(),DERIVED                      ! Method added to host embed code
SetAlerts              PROCEDURE(),DERIVED                      ! Method added to host embed code
SetReadOnly            PROCEDURE(BYTE State),DERIVED            ! Method added to host embed code
TakeAccepted           PROCEDURE(BYTE Action),BYTE,DERIVED      ! Method added to host embed code
TakeEvent              PROCEDURE(UNSIGNED Event),BYTE,DERIVED   ! Method added to host embed code
                     END
QEIP3:SaveEntry      GROUP,PRE(QEIP3)
szOption               LIKE(opt:szOption)
szValue                LIKE(opt:szValue)
                     END
QEIP3:Fields         FieldPairsClass
QEIP3:PopupString    STRING(20)
QEIP3:PopupMgr       PopupClass
QEIP3:EditList       QUEUE(EditQueue),PRE(QEIP3)
                     END
QEIP3:EM             CLASS(EIPManager)
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

QEIP3::opt:szValue   CLASS(EditEntryClass)
CreateControl          PROCEDURE(),DERIVED                      ! Method added to host embed code
Init                   PROCEDURE(UNSIGNED FieldNumber,UNSIGNED ListBox,*? UseVar),DERIVED ! Method added to host embed code
Kill                   PROCEDURE(),DERIVED                      ! Method added to host embed code
SetAlerts              PROCEDURE(),DERIVED                      ! Method added to host embed code
SetReadOnly            PROCEDURE(BYTE State),DERIVED            ! Method added to host embed code
TakeAccepted           PROCEDURE(BYTE Action),BYTE,DERIVED      ! Method added to host embed code
TakeEvent              PROCEDURE(UNSIGNED Event),BYTE,DERIVED   ! Method added to host embed code
                     END
QEIP4:SaveEntry      GROUP,PRE(QEIP4)
Pattern                LIKE(fpq:Pattern)
                     END
QEIP4:Fields         FieldPairsClass
QEIP4:PopupString    STRING(20)
QEIP4:PopupMgr       PopupClass
QEIP4:EditList       QUEUE(EditQueue),PRE(QEIP4)
                     END
QEIP4:EM             CLASS(EIPManager)
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
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeNewSelection       PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop
  RETURN(bPropertiesChanged)

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
LoadPropertyFile     ROUTINE
   cc = kcr_fnSplit(szPropertyFile, szDrive, szDir, szName, szExtension)

   AsciiFilename = szPropertyFile
   OPEN(AsciiFile,ReadOnly+DenyNone)
   IF ERRORCODE()
      !error opening file
   ELSE
      SET(AsciiFile)
      NEXT(AsciiFile)
      LOOP UNTIL ERRORCODE()

         filequeue.record = CLIP(AsciiFile.Buffer)
         ADD(filequeue)

         szBuffer = CLIP(LEFT(AsciiFile.Buffer))
         IF szBuffer = ''
            !skip
         ELSIF szBuffer[1] = '!'
            szBuffer[1] = ' '
            IF szDescription = ''
               szDescription = CLIP(LEFT(szBuffer))
            END
            LastComment = CLIP(LEFT(szBuffer))
         ELSE
            !process
            IF UPPER(szBuffer[1 : 6]) = 'LEXER='
               szLexer = LOWER(szBuffer[7 : LEN(szBuffer)])
            ELSIF UPPER(szBuffer[1 : 14]) = '[FILEPATTERNS]'
               DO LoadFilePatterns
               CYCLE
            ELSIF UPPER(szBuffer[1 : 9]) = '[OPTIONS]'
               DO LoadOptions
               CYCLE
            ELSIF UPPER(szBuffer[1 : 9])  = '[KEYWORDS'
               DO LoadKeywords
               CYCLE
            ELSIF UPPER(szBuffer[1 : 8])  = '[STYLES]'
               DO LoadStyles
               CYCLE
            END
         END
         NEXT(AsciiFile)
      END
      CLOSE(AsciiFile)
   END
LoadFilePatterns     ROUTINE
   NEXT(AsciiFile)
   LOOP UNTIL ERRORCODE()
      szBuffer = CLIP(LEFT(AsciiFile.Buffer))
      IF szBuffer = ''
         !skip
      ELSIF szBuffer[1] = '!'
         szBuffer[1] = ' '
         LastComment = CLIP(LEFT(szBuffer))
      ELSIF szBuffer[1] = '['
         szFilePatterns = CLIP(LEFT(szFilePatterns))
         BREAK
      ELSE
         szFilePatterns = szFilePatterns & ' ' & szBuffer
      END
      filequeue.record = CLIP(AsciiFile.Buffer)
      ADD(filequeue)
      NEXT(AsciiFile)
   END
LoadOptions    ROUTINE
   DATA
i     LONG
j     LONG
p     LONG

   CODE
      NEXT(AsciiFile)
      LOOP UNTIL ERRORCODE()
         szBuffer = CLIP(LEFT(AsciiFile.Buffer))
         IF szBuffer = ''
            !skip
         ELSIF szBuffer[1] = '!'
            lastComment = CLIP(LEFT(szBuffer[2 : LEN(szBuffer)]))
            !skip
         ELSIF szBuffer[1] = '['
            BREAK
         ELSE
            j = INSTRING(';',szBuffer)
            IF j = 0
               j = LEN(szBuffer)
            ELSE
               j -= 1
            END
            i = INSTRING('=',szBuffer)
            IF i
               qOptions.szOption = szBuffer[1 : i-1]
               qOptions.szOptionTip = ''
               IF j < LEN(szBuffer)
                  qOptions.szOptionTip = szBuffer[j+2 : LEN(szBuffer)]
                  p = INSTRING('||',qOptions.szOptionTip,1,1)
                  LOOP WHILE p > 0
                     qOptions.szOptionTip[p] = '<13>'
                     qOptions.szOptionTip[p+1] = '<10>'
                     p = INSTRING('||',qOptions.szOptionTip,1,p+2)
                  END
               END
               qOptions.szValue = szBuffer[i+1 : j]
               IF qOptions.szOption = 'asp.default.language'
                  CASE qOptions.szValue
                    OF '1'
                       qOptions.szValue = 'JavaScript'
                    OF '2'
                       qOptions.szValue = 'VBScript'
                    OF '3'
                       qOptions.szValue = 'Python'
                  END
                  qOptions.IsBool = FALSE
               ELSE
                  CASE qOptions.szValue
                    OF '0'
                       qOptions.szValue = 'false'
                       qOptions.IsBool = TRUE
                    OF '1'
                       qOptions.szValue = 'true'
                       qOptions.IsBool = TRUE
                  ELSE
                       qOptions.IsBool = FALSE
                  END
               END
               ADD(qOptions)
            END
         END
         filequeue.record = CLIP(AsciiFile.Buffer)
         ADD(filequeue)
         NEXT(AsciiFile)
      END
   EXIT
LoadKeywords         ROUTINE
   szBuffer = CLIP(LEFT(AsciiFile.Buffer))
   qKeywords.szKeywordNumber = UPPER(szBuffer[2 : LEN(szBuffer)-1])
   GET(qKeywords,+qKeywords.szKeywordNumber)
   IF ERRORCODE()
      qKeywords.szKeywordNumber = UPPER(szBuffer[2 : LEN(szBuffer)-1])
      qKeywords.szDescription = lastComment
      qKeywords.szKeywords = ''
      ADD(qKeywords,+qKeywords.szDescription)
      !ADD(qKeywords,+qKeywords.szKeywordNumber)
   END
   NEXT(AsciiFile)
   LOOP UNTIL ERRORCODE()
      szBuffer = CLIP(LEFT(AsciiFile.Buffer))
      IF szBuffer = ''
         !skip
      ELSIF szBuffer[1] = '!'
         szBuffer[1] = ' '
         LastComment = CLIP(LEFT(szBuffer))
      ELSIF szBuffer[1] = '['
         qKeywords.szKeywords = CLIP(LEFT(qKeywords.szKeywords))
         PUT(qKeywords)
         BREAK
      ELSE
         qKeywords.szKeywords = qKeywords.szKeywords & ' ' & szBuffer
      END
      filequeue.record = CLIP(AsciiFile.Buffer)
      ADD(filequeue)
      NEXT(AsciiFile)
   END
LoadStyles           ROUTINE
   DATA
i           LONG
j           LONG
r           BYTE
g           BYTE
b           BYTE
qAttribute  QUEUE,PRE(att)
attribute      CSTRING(32)
            END
bDefault    BOOL

   CODE
      NEXT(AsciiFile)
      LOOP UNTIL ERRORCODE()
         szBuffer = CLIP(LEFT(AsciiFile.Buffer))
         IF szBuffer = ''
            !skip
         ELSIF szBuffer[1] = '!'
            szBuffer[1] = ' '
            LastComment = CLIP(LEFT(szBuffer))
         ELSIF szBuffer[1] = '['
            BREAK
         ELSE
            !Style#=font:,size:,bold,italic,underline,fore:,back:,eolfilled,case:,hide,hotSpot
            IF UPPER(szBuffer[1 : 5]) = 'STYLE'
               qStyles.szSort = UPPER(lastComment)
               j = INSTRING('=',szBuffer)
               IF qStyles.szSort = ''
                  IF j < 9
                     qStyles.szSort = 'STYLE' & ALL('0',9-j) & UPPER(szBuffer[6 : j-1])
                  ELSE
                     qStyles.szSort = UPPER(szBuffer[1 : j-1])
                  END
               END
               GET(qStyles,+qStyles.szSort)

               CLEAR(qStyles)
               IF j < 9
                  qStyles.szStyleNumber = 'STYLE' & ALL('0',9-j) & UPPER(szBuffer[6 : j-1])
               ELSE
                  qStyles.szStyleNumber = UPPER(szBuffer[1 : j-1])

               END
               qStyles.szDescription = lastComment
               qStyles.szFontName = ''
               qStyles.nFontStyle = FONT:regular
               qStyles.lForeColor = COLOR:NONE
               qStyles.lBackColor = COLOR:NONE
               qStyles.szSort = UPPER(lastComment)
               IF qStyles.szSort = ''
                  j = INSTRING('=',szBuffer)
                  IF j < 9
                     qStyles.szSort = 'STYLE' & ALL('0',9-j) & UPPER(szBuffer[6 : j-1])
                  ELSE
                     qStyles.szSort = UPPER(szBuffer[1 : j-1])
                  END
               END
               ADD(qStyles,+qStyles.szSort,qStyles.szStyleNumber)

               IF qStyles.szStyleNumber = 'STYLE032'
                  bDefault = TRUE
                  qStyles.nCaseOpt = 1
               ELSE
                  bDefault = FALSE
               END

               FREE(qAttribute)
               j += 1
               i = INSTRING(',',szBuffer,,j)
               LOOP WHILE i > 0
                  qAttribute.attribute = szBuffer[j : i-1]
                  ADD(qAttribute)
                  j = i+1
                  i = INSTRING(',',szBuffer,,j)
               END
               qAttribute.attribute = szBuffer[j : LEN(szBuffer)]
               ADD(qAttribute)

               j = RECORDS(qAttribute)
               LOOP i = 1 TO j
                  GET(qAttribute,i)
                  CASE UPPER(qAttribute.attribute[1 : 4])
                    OF 'FONT'
                       qStyles.szFontName = qAttribute.attribute[6 : LEN(qAttribute.attribute)]
                    OF 'SIZE'
                       qStyles.nFontSize = qAttribute.attribute[6 : LEN(qAttribute.attribute)]
                    OF 'BOLD'
                       qStyles.bBold = TRUE
                       qStyles.nFontStyle = BXOR(qStyles.nFontStyle,FONT:regular)
                       qStyles.nFontStyle = BOR(qStyles.nFontStyle,FONT:bold)
                    OF 'ITAL'
                       qStyles.bItalic = TRUE
                       qStyles.nFontStyle = BOR(qStyles.nFontStyle,FONT:italic)
                    OF 'UNDE'
                       qStyles.bUnderline = TRUE
                       qStyles.nFontStyle = BOR(qStyles.nFontStyle,FONT:underline)
                    OF 'FORE' !fore:#rrggbb
                       r = EVALUATE('0' & qAttribute.attribute[7  :  8] & 'h')
                       g = EVALUATE('0' & qAttribute.attribute[9  : 10] & 'h')
                       b = EVALUATE('0' & qAttribute.attribute[11 : 12] & 'h')
                       qStyles.lForeColor = ColourRGB(r,g,b)
                    OF 'BACK'
                       r = EVALUATE('0' & qAttribute.attribute[7  :  8] & 'h')
                       g = EVALUATE('0' & qAttribute.attribute[9  : 10] & 'h')
                       b = EVALUATE('0' & qAttribute.attribute[11 : 12] & 'h')
                       qStyles.lBackColor = ColourRGB(r,g,b)
                    OF 'EOLF'
                       qStyles.bEolFilled = TRUE
                    OF 'HIDE'
                       qStyles.bHide = TRUE
                    OF 'CASE'
                       qStyles.nCaseOpt = qAttribute.attribute[6 : LEN(qAttribute.attribute)]
                       qStyles.nCaseOpt += 1
                    OF 'HOTS'
                       qStyles.bHotSpot = TRUE
                  END
               END
               !look for default style
               IF bDefault
                  defaultFont  = qStyles.szFontName
                  defaultSize  = qStyles.nFontSize
                  defaultStyle = qStyles.nFontStyle
                  defaultFore  = qStyles.lForeColor
                  defaultBack  = qStyles.lBackColor
                  defaultCase  = qStyles.nCaseOPt
                  defaultHide  = qStyles.bHide
                  defaultHot   = qStyles.bHotSpot
                  defaultEOLF  = qStyles.bEolFilled
               END
               PUT(qStyles)
            END
         END
         filequeue.record = CLIP(AsciiFile.Buffer)
         ADD(filequeue)
         NEXT(AsciiFile)
      END
SavePropertyFile     ROUTINE
   ThisWindow.Update()

   AsciiFilename = svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS') & '\' & szName & szExtension
   IF FILEDIALOG('Save As ...',AsciiFilename,'KSS properties files|*.properties',BOR(BOR(FILE:Save,FILE:KeepDir),FILE:LongName))

      CREATE(AsciiFile)
      OPEN(AsciiFile,ReadWrite+DenyAll)

      filequeueMax = RECORDS(filequeue)
      LOOP filequeueIndx = 1 TO filequeueMax
         GET(filequeue,filequeueIndx)
         szBuffer = CLIP(LEFT(filequeue.record))
         IF szBuffer = ''
            AsciiFile.Buffer = filequeue.record
            ADD(AsciiFile)
         ELSIF szBuffer[1] = '!'
            IF filequeueIndx = 1
               AsciiFile.Buffer = '! ' & szDescription
               ADD(AsciiFile)
            ELSE
               AsciiFile.Buffer = filequeue.record
               ADD(AsciiFile)
            END
         ELSIF UPPER(szBuffer[1 : 6]) = 'LEXER='
            AsciiFile.Buffer = 'Lexer=' & szLexer
            ADD(AsciiFile)
         ELSIF UPPER(szBuffer[1 : 14]) = '[FILEPATTERNS]'
            AsciiFile.Buffer = filequeue.record
            ADD(AsciiFile)
            DO SaveFilePatterns
            DO SkipToNextSection
         ELSIF UPPER(szBuffer[1 : 9]) = '[OPTIONS]'
            AsciiFile.Buffer = filequeue.record
            ADD(AsciiFile)
            DO SaveOptions
            DO SkipToNextSection
            filequeueIndx += 1
         ELSIF UPPER(szBuffer[1 : 9])  = '[KEYWORDS'
            DO SaveKeywords
            DO SkipToStyles
            filequeueIndx += 1
         ELSIF UPPER(szBuffer[1 : 8])  = '[STYLES]'
            AsciiFile.Buffer = filequeue.record
            ADD(AsciiFile)
            DO SaveStyles
            BREAK
         ELSE
            AsciiFile.Buffer = filequeue.record
            ADD(AsciiFile)
         END
      END   !loop

      CLOSE(AsciiFile)

   END   !if filedialog
SaveFilePatterns  ROUTINE
   DATA
p     LONG

   CODE
      IF ?szFilePatterns{PROP:Hide} = TRUE
         DO LoadFilePatternsFromFilePatternQueue
      END
      LOOP WHILE LEN(szFilePatterns) > RECORDSIZE
         LOOP p = RECORDSIZE TO 1 BY -1
           IF szFilePatterns[p] = ' '
              AsciiFile.Buffer = szFilePatterns[1 : p-1]
              ADD(AsciiFile)
              szFilePatterns = szFilePatterns[p+1 : LEN(szFilePatterns)]
              BREAK
           END
         END
         IF p = 0
            BREAK
         END
      END
      AsciiFile.Buffer = szFilePatterns
      ADD(AsciiFile)
   EXIT
SaveOptions    ROUTINE
   DATA
n     LONG
p     LONG

   CODE
      LOOP n = 1 TO RECORDS(qOptions)
         GET(qOptions,n)
         IF qOptions.szOption = 'asp.default.language'
            CASE qOptions.szValue
              OF 'JavaScript'
                 AsciiFile.Buffer = qOptions.szOption & '=1'
              OF 'VBScript'
                 AsciiFile.Buffer = qOptions.szOption & '=2'
              OF 'Python'
                 AsciiFile.Buffer = qOptions.szOption & '=3'
            END
         ELSE
            CASE qOptions.szValue
              OF 'false'
                 AsciiFile.Buffer = qOptions.szOption & '=0'
              OF 'true'
                 AsciiFile.Buffer = qOptions.szOption & '=1'
            ELSE
                 AsciiFile.Buffer = qOptions.szOption & '=' & qOptions.szValue
            END
         END
         IF qOptions.szOptionTip <> ''
            p = INSTRING('<13,10>',qOptions.szOptionTip,1,1)
            LOOP WHILE p > 0
               qOptions.szOptionTip[p] = '|'
               qOptions.szOptionTip[p+1] = '|'
               p = INSTRING('<13,10>',qOptions.szOptionTip,1,p+2)
            END
            AsciiFile.Buffer = CLIP(AsciiFile.Buffer) & ';' & qOptions.szOptionTip
         END
         ADD(AsciiFile)
      END
      AsciiFile.Buffer = ''
      ADD(AsciiFile)
   EXIT
SaveKeywords      ROUTINE
   DATA
n     LONG
P     LONG
   CODE
      IF ?kwd:szKeywords{PROP:Hide} = TRUE
         DO LoadKeywordsFromListBoxQueue
      END
      !add the keyword groups
      SORT(qKeywords,qKeywords.szKeywordNumber)
      LOOP n = 1 TO RECORDS(qKeywords)
         GET(qKeyWords,n)
         AsciiFile.Buffer = '! ' & qKeywords.szDescription
         ADD(AsciiFile)
         AsciiFile.Buffer = '[' & qKeywords.szKeywordNumber & ']'
         ADD(AsciiFile)

         LOOP WHILE LEN(qKeywords.szKeywords) > recordSize
            LOOP p = recordSize TO 1 BY -1
              IF qKeywords.szKeywords[p] = ' '
                 AsciiFile.Buffer = qKeywords.szKeywords[1 : p-1]
                 ADD(AsciiFile)
                 qKeywords.szKeywords = qKeywords.szKeywords[p+1 : LEN(qKeywords.szKeywords)]
                 BREAK
              END
            END
            IF p = 0
               BREAK
            END
         END
         AsciiFile.Buffer = qKeywords.szKeywords
         ADD(AsciiFile)
         AsciiFile.Buffer = ''
         ADD(AsciiFile)
      END
   EXIT
SaveStyles     ROUTINE
   DATA
n           LONG
tempBuffer  CSTRING(SIZE(AsciiFile.Buffer)+1)

   CODE
      !write out any comments before styles
      filequeueIndx += 1
      GET(filequeue,filequeueIndx)
      szBuffer = CLIP(LEFT(filequeue.record))
      LOOP WHILE szBuffer = '' OR szBuffer[1] = '!'
         IF szBuffer[1] = '!'
            GET(filequeue,filequeueIndx+1)
            tempBuffer = UPPER(CLIP(LEFT(filequeue.record)))
            GET(filequeue,filequeueIndx)
            IF tempBuffer[1 : 5] = 'STYLE'
               BREAK
            END
         END
         AsciiFile.Buffer = filequeue.record
         ADD(AsciiFile)
         filequeueIndx += 1
         GET(filequeue,filequeueIndx)
         szBuffer = CLIP(LEFT(filequeue.record))
      END

      !add the styles
      !Style#=font:,size:,bold,italic,underline,fore:,back:,eolfilled,case:,hide,hotSpot
      !get default style
      SORT(qStyles,+qStyles.szStyleNumber)

      qStyles.szStyleNumber = 'STYLE032'
      GET(qStyles,+qStyles.szStyleNumber)
      IF NOT ERRORCODE()
         DO AddStyleRecord
      END

      LOOP n = 1 TO RECORDS(qStyles)
         GET(qStyles,n)
         IF qStyles.szStyleNumber = 'STYLE032'
            CYCLE
         ELSE
            DO AddStyleRecord
         END
      END   !styles loop
   EXIT
SkipToNextSection ROUTINE
   LOOP WHILE filequeueIndx < filequeueMax
      GET(fileQueue,filequeueIndx+1)
      szBuffer = CLIP(LEFT(filequeue.record))
      IF szBuffer[1] = '['
         filequeueIndx -= 1
         BREAK
      ELSE
         filequeueIndx += 1
      END
   END
SkipToStyles      ROUTINE
   LOOP WHILE filequeueIndx < filequeueMax
      GET(fileQueue,filequeueIndx+1)
      szBuffer = CLIP(LEFT(filequeue.record))
      IF UPPER(szBuffer[1 : 8])  = '[STYLES]'
         filequeueIndx -= 1
         BREAK
      ELSE
         filequeueIndx += 1
      END
   END
AddStyleRecord    ROUTINE
   DATA
szBuffer    CSTRING(SIZE(AsciiFile.Buffer)+1)

   CODE
      AsciiFile.Buffer = '! ' & qStyles.szDescription
      ADD(AsciiFile)
      szBuffer = qStyles.szStyleNumber & '='
      IF qStyles.szFontName <> ''
         szBuffer = szBuffer & 'font:' & qStyles.szFontName & ','
      END
      IF qStyles.nFontSize > 0
         szBuffer = szBuffer & 'size:' & qStyles.nFontSize & ','
      END
      IF qStyles.bBold = TRUE
         szBuffer = szBuffer & 'bold' & ','
      END
      IF qStyles.bItalic = TRUE
         szBuffer = szBuffer & 'italic' & ','
      END
      IF qStyles.bUnderline = TRUE
         szBuffer = szBuffer & 'underline' & ','
      END
      IF qStyles.lForeColor <> COLOR:NONE
         szBuffer = szBuffer & 'fore:' & srcGetRGBColorString(qStyles.lForeColor) & ','
      END
      IF qStyles.lBackColor <> COLOR:NONE
         szBuffer = szBuffer & 'back:' & srcGetRGBColorString(qStyles.lBackColor) & ','
      END
      IF qStyles.bEolFilled = TRUE
         szBuffer = szBuffer & 'eolfilled' & ','
      END
      IF qStyles.nCaseOpt > 0
         szBuffer = szBuffer & 'case:' & qStyles.nCaseOpt-1 & ','
      END
      IF qStyles.bHide = TRUE
         szBuffer = szBuffer & 'hide' & ','
      END
      IF qStyles.bHotSpot = TRUE
         szBuffer = szBuffer & 'hotspot' & ','
      END
      IF szBuffer[LEN(szBuffer)] = ','
         szBuffer[LEN(szBuffer)] = '<0>'
      END
      AsciiFile.Buffer = szBuffer
      ADD(AsciiFile)
   EXIT
FillFilePatternQueue     ROUTINE
   DATA
i     LONG
j     LONG

   CODE
      FREE(FilePatternQueue)
      j = 1
      i = INSTRING(' ',szFilePatterns,,j)
      LOOP WHILE i > 0
         FilePatternQueue.Pattern =szFilePatterns[j : i-1]
         ADD(FilePatternQueue,+FilePatternQueue.Pattern)
         j = i+1
         i = INSTRING(' ',szFilePatterns,,j)
      END
      FilePatternQueue.Pattern = szFilePatterns[j : LEN(szFilePatterns)]
      ADD(FilePatternQueue,+FilePatternQueue.Pattern)

      IF RECORDS(FilePatternQueue)
         ?ChangeFilePattern{PROP:Disable} = FALSE
         ?DeleteFilePattern{PROP:Disable} = FALSE
      ELSE
         ?ChangeFilePattern{PROP:Disable} = TRUE
         ?DeleteFilePattern{PROP:Disable} = TRUE
      END
      GET(FilePatternQueue,1)
      ?lbFilePatterns{PROP:Selected} = 1
   EXIT
LoadFilePatternsFromFilePatternQueue     ROUTINE
   DATA
i     LONG
j     LONG

   CODE
      szFilePatterns = ''
      SORT(FilePatternQueue,+FilePatternQueue.Pattern)
      j = RECORDS(FilePatternQueue)
      LOOP i = 1 TO j
         GET(FilePatternQueue,i)
         szFilePatterns = szFilePatterns & FilePatternQueue.Pattern & ' '
      END
      szFilePatterns[LEN(szFilePatterns)] = '<0>'
   EXIT
FillListBoxQueue     ROUTINE
   DATA
i     LONG
j     LONG

   CODE
      FREE(ListBoxQueue)

      GET(qKeywords,CHOICE(?lbKeywords))
      j = 1
      i = INSTRING(' ',qKeywords.szKeywords,,j)
      LOOP WHILE i > 0
         ListBoxQueue.Value = qKeywords.szKeywords[j : i-1]
         ADD(ListBoxQueue,+ListBoxQueue.Value)
         j = i+1
         i = INSTRING(' ',qKeywords.szKeywords,,j)
      END
      ListBoxQueue.Value = qKeywords.szKeywords[j : LEN(qKeywords.szKeywords)]
      ADD(ListBoxQueue,+ListBoxQueue.Value)

      IF RECORDS(ListBoxQueue)
         ?ChangeKeyword{PROP:Disable} = FALSE
         ?DeleteKeyword{PROP:Disable} = FALSE
      ELSE
         ?ChangeKeyword{PROP:Disable} = TRUE
         ?DeleteKeyword{PROP:Disable} = TRUE
      END
      GET(ListBoxQueue,1)
      ?lbKeywordValues{PROP:Selected} = 1
   EXIT
LoadKeywordsFromListBoxQueue     ROUTINE
   DATA
i     LONG
j     LONG

   CODE
      qKeywords.szKeywords = ''
      SORT(ListBoxQueue,+ListBoxQueue.Value)
      j = RECORDS(ListBoxQueue)
      LOOP i = 1 TO j
         GET(ListBoxQueue,i)
         qKeywords.szKeywords = qKeywords.szKeywords & ListBoxQueue.Value & ' '
      END
      qKeywords.szKeywords[LEN(qKeywords.szKeywords)] = '<0>'
      PUT(qKeywords)
   EXIT
CheckForChanges   ROUTINE
   IF bDirty = FALSE
      IF saveFilePatterns <> '' AND saveFilePatterns <> szFilePatterns
         bDirty = TRUE
      END

      IF bDirty = FALSE AND saveKeywordSet.szKeywordNumber <> ''
         IF saveKeywordSet.szDescription <> qKeywords.szDescription     |
         OR saveKeywordSet.szKeywordNumber <> qKeywords.szKeywordNumber |
         OR saveKeywordSet.szKeywords <> qKeywords.szKeywords
            bDirty = TRUE
         END
      END

      IF bDirty = FALSE AND saveStyle.szStyleNumber <> ''
         IF saveStyle.szDescription <> qStyles.szDescription   |
         OR saveStyle.szStyleNumber <> qStyles.szStyleNumber   |
         OR saveStyle.szFontName    <> qStyles.szFontName      |
         OR saveStyle.nFontSize     <> qStyles.nFontSize       |
         OR saveStyle.nFontStyle    <> qStyles.nFontStyle      |
         OR saveStyle.bBold         <> qStyles.bBold           |
         OR saveStyle.bEolFilled    <> qStyles.bEolFilled      |
         OR saveStyle.bHide         <> qStyles.bHide           |
         OR saveStyle.bHotSpot      <> qStyles.bHotSpot        |
         OR saveStyle.bItalic       <> qStyles.bItalic         |
         OR saveStyle.bUnderline    <> qStyles.bUnderline      |
         OR saveStyle.nCaseOpt      <> qStyles.nCaseOpt        |
         OR saveStyle.lForeColor    <> qStyles.lForeColor      |
         OR saveStyle.lBackColor    <> qStyles.lBackColor
            bDirty = TRUE
         END
      END
   END

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
    
  GlobalErrors.SetProcedureName('PropertyEditor')
  GlobalErrors.AddErrors(DefaultErrors)
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  popupMenu.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?szDescription:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  DO LoadPropertyFile
  SELF.Open(Window)                                        ! Open window
  Window{PROP:Text} = 'Property Editor [' & szName & szExtension & ']'
  Window{PROP:Pixels} = TRUE
  ?StyleGroup{PROP:Ypos} = (?StyleGroup{PROP:Ypos} + 1)
  Window{PROP:Pixels} = FALSE
  ?Sheet{PROP:TabSheetStyle} = TabStyle:BlackAndWhite
  ?Sheet{PROP:NoTheme} = True
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  ?lbFilePatterns{PROP:LineHeight} = 10
  ?lbOptions{PROP:LineHeight} = 10
  ?lbKeywords{PROP:LineHeight} = 10
  ?lbKeywordValues{PROP:LineHeight} = 10
  ?lbStyles{PROP:LineHeight} = 10
  GET(qOptions,1)
  ?lbOptions{PROP:Selected} = 1
  POST(EVENT:NewSelection,?lbOptions)
  GET(qKeywords,1)
  ?lbKeywords{PROP:Selected} = 1
  GET(qStyles,1)
  ?lbStyles{PROP:Selected} = 1
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
  INIMgr.Fetch('PropertyEditor',Window)                    ! Restore window settings from non-volatile store
  CorrectForOffscreen(Window)
  SELF.SetAlerts()
  QEIP2:Fields.Init()
  QEIP2:Fields.AddPair(QEIP2:SaveEntry.Value,ListBoxQueue.Value)
  QEIP2:PopupMgr.Init()
  QEIP2:PopupMgr.AddItemMimic('Add',?InsertKeyword)
  QEIP2:PopupMgr.AddItemMimic('Edit',?ChangeKeyword)
  QEIP2:PopupMgr.AddItemMimic('Delete',?DeleteKeyword)
  GlobalErrors.AddErrors(QEIP:Errors)
  ?lbKeywordValues{Prop:Alrt,QEIP:MouseLeft2Index} = MouseLeft2
  ?lbKeywordValues{Prop:Alrt,QEIP:MouseRightIndex} = MouseRight
  QEIP3:Fields.Init()
  QEIP3:Fields.AddPair(QEIP3:SaveEntry.szOption,qOptions.szOption)
  QEIP3:Fields.AddPair(QEIP3:SaveEntry.szValue,qOptions.szValue)
  QEIP3:PopupMgr.Init()
  QEIP3:PopupMgr.AddItemMimic('Edit',?ChangeOption)
  GlobalErrors.AddErrors(QEIP:Errors)
  ?lbOptions{Prop:Alrt,QEIP:MouseLeft2Index} = MouseLeft2
  ?lbOptions{Prop:Alrt,QEIP:MouseRightIndex} = MouseRight
  QEIP4:Fields.Init()
  QEIP4:Fields.AddPair(QEIP4:SaveEntry.Pattern,FilePatternQueue.Pattern)
  QEIP4:PopupMgr.Init()
  QEIP4:PopupMgr.AddItemMimic('Add',?InsertFilePattern)
  QEIP4:PopupMgr.AddItemMimic('Edit',?ChangeFilePattern)
  QEIP4:PopupMgr.AddItemMimic('Delete',?DeleteFilePattern)
  GlobalErrors.AddErrors(QEIP:Errors)
  ?lbFilePatterns{Prop:Alrt,QEIP:MouseLeft2Index} = MouseLeft2
  ?lbFilePatterns{Prop:Alrt,QEIP:MouseRightIndex} = MouseRight
  QEIP2:PopupMgr.AddItem('-')
  QEIP2:PopupMgr.AddItemEvent('View as Text',EVENT:USER)
  QEIP4:PopupMgr.AddItem('-')
  QEIP4:PopupMgr.AddItemEvent('View as Text',EVENT:USER+1)
  oHH &= NEW tagHTMLHelp
  oHH.Init( 'kss.chm' )
  oHH.SetTopic('PropertyEditor.htm')
  POST(EVENT:NewSelection,?lbKeywords)
  POST(EVENT:NewSelection,?lbStyles)
  POST(EVENT:NewSelection,?Sheet)
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
!  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.Opened
    INIMgr.Update('PropertyEditor',Window)                 ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  IF ~oHH &= NULL
    oHH.Kill()
    DISPOSE( oHH )
  END
  popupMenu.Kill()
  RETURN ReturnValue


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF Window{Prop:AcceptAll} THEN RETURN.
  !Viewer Options
  !======================================================================================
  !style##
  
  nCurrentStyle = qStyles.szStyleNumber[6 : LEN(qStyles.szStyleNumber)]
  
  IF qStyles.szFontName
     szFontDescription = CLIP(qStyles.szFontName) & ','
     ?Font:Prompt{PROP:FontStyle} = FONT:bold
  ELSIF defaultFont
     szFontDescription = defaultFont & ','
     ?Font:Prompt{PROP:FontStyle} = FONT:italic
  ELSE
     ?Font:Prompt{PROP:FontStyle} = FONT:italic
     szFontDescription = ''
  END
  
  IF qStyles.nFontSize
     szFontDescription = szFontDescription & qStyles.nFontSize & ','
     ?Font:Prompt{PROP:FontStyle} = BOR(?Font:Prompt{PROP:FontStyle},FONT:bold)
  ELSIF defaultSize
     szFontDescription = szFontDescription & defaultSize & ','
     ?Font:Prompt{PROP:FontStyle} = BOR(?Font:Prompt{PROP:FontStyle},FONT:italic)
  END
  
  IF qStyles.nFontStyle = defaultStyle
     IF nCurrentStyle <> 32
        ?Font:Prompt{PROP:FontStyle} = FONT:italic
     END
  END
  IF BAND(qStyles.nFontStyle,0FFFh) >= FONT:bold
     szFontDescription = szFontDescription & 'Bold,'
     qStyles.bBold = TRUE
     ?Font:Prompt{PROP:FontStyle} = BOR(?Font:Prompt{PROP:FontStyle},FONT:bold)
  ELSIF BAND(qStyles.nFontStyle,0FFFh) >= FONT:regular
     szFontDescription = szFontDescription & 'Regular,'
     qStyles.bBold = FALSE
  ELSE
     szFontDescription = szFontDescription & 'Thin,'
     qStyles.bBold = FALSE
     ?Font:Prompt{PROP:FontStyle} = BOR(?Font:Prompt{PROP:FontStyle},FONT:bold)
  END
  IF BAND(qStyles.nFontStyle,FONT:italic) = FONT:italic
     szFontDescription = szFontDescription & 'Italic,'
     qStyles.bItalic = TRUE
     ?Font:Prompt{PROP:FontStyle} = BOR(?Font:Prompt{PROP:FontStyle},FONT:bold)
  ELSE
     qStyles.bItalic = FALSE
  END
  IF BAND(qStyles.nFontStyle,FONT:underline) = FONT:underline
     szFontDescription = szFontDescription & 'Underline,'
     qStyles.bUnderline = TRUE
     ?Font:Prompt{PROP:FontStyle} = BOR(?Font:Prompt{PROP:FontStyle},FONT:bold)
  ELSE
     qStyles.bUnderline = FALSE
  END
  
  IF szFontDescription[LEN(szFontDescription)] = ','
     szFontDescription[LEN(szFontDescription)] = '<0>'
  END
  
  ?ForeColor:Prompt{PROP:FontStyle} = FONT:bold
  IF qStyles.lForeColor <> COLOR:NONE
     ?ForeColorBox{PROP:Fill} = qStyles.lForeColor
     szForeColor = srcGetColorString(qStyles.lForeColor)
     !IF NOT INSTRING(':',szForeColor)
     !   szForeColor = srcGetRGBColorString(qStyles.lForeColor)
     !END
  ELSIF defaultFore <>  COLOR:NONE
     ?ForeColor:Prompt{PROP:FontStyle} = FONT:italic
     ?ForeColorBox{PROP:Fill} = defaultFore
     szForeColor = srcGetColorString(defaultFore)
     !IF NOT INSTRING(':',szForeColor)
     !   szForeColor = srcGetRGBColorString(defaultFore)
     !END
  ELSE
     ?ForeColor:Prompt{PROP:FontStyle} = FONT:italic
     ?ForeColorBox{PROP:Fill} = COLOR:NONE
     szForeColor = srcGetColorString(COLOR:NONE)
  END
  
  ?BackColor:Prompt{PROP:FontStyle} = FONT:bold
  IF qStyles.lBackColor <> COLOR:NONE
     ?BackColorBox{PROP:Fill} = qStyles.lBackColor
     szBackColor = srcGetColorString(qStyles.lBackColor)
     !IF NOT INSTRING(':',szBackColor)
     !   szBackColor = srcGetRGBColorString(qStyles.lBackColor)
     !END
  ELSIF defaultBack <>  COLOR:NONE
     ?BackColor:Prompt{PROP:FontStyle} = FONT:italic
     ?BackColorBox{PROP:Fill} = defaultBack
     szBackColor = srcGetColorString(defaultBack)
     !IF NOT INSTRING(':',szBackColor)
     !   szBackColor = srcGetRGBColorString(defaultBack)
     !END
  ELSE
     ?BackColor:Prompt{PROP:FontStyle} = FONT:italic
     ?BackColorBox{PROP:Fill} = COLOR:NONE
     szBackColor = srcGetColorString(COLOR:NONE)
     !szBackColor = srcGetRGBColorString(COLOR:NONE)
  END
  
  PUT(qStyles)
  
  IF qStyles.bHide = 0
     qStyles.bHide = defaultHide
     IF nCurrentStyle <> 32
        ?sty:bHide{PROP:FontStyle} = FONT:italic
     ELSE
        ?sty:bHide{PROP:FontStyle} = FONT:bold
     END
  ELSE
     ?sty:bHide{PROP:FontStyle} = FONT:bold
  END
  
  IF qStyles.bHotSpot = 0
     qStyles.bHotSpot = defaultHot
     IF nCurrentStyle <> 32
        ?sty:bHotSpot{PROP:FontStyle} = FONT:italic
     ELSE
        ?sty:bHotSpot{PROP:FontStyle} = FONT:bold
     END
  ELSE
     ?sty:bHotSpot{PROP:FontStyle} = FONT:bold
  END
  
  IF qStyles.bEolFilled = 0
     qStyles.bEolFilled = defaultEOLF
     IF nCurrentStyle <> 32
        ?sty:bEolFilled{PROP:FontStyle} = FONT:italic
     ELSE
        ?sty:bEolFilled{PROP:FontStyle} = FONT:bold
     END
  ELSE
     ?sty:bEolFilled{PROP:FontStyle} = FONT:bold
  END
  
  IF qStyles.nCaseOpt = 0
     !qStyles.nCaseOpt = defaultCase
     ?nCaseOpt:Prompt{PROP:FontStyle} = FONT:italic
     ?STY:NCASEOPT:Mixed{PROP:FontStyle} = CHOOSE(defaultCase = 1,FONT:italic,FONT:regular)
     ?STY:NCASEOPT:Upper{PROP:FontStyle} = CHOOSE(defaultCase = 2,FONT:italic,FONT:regular)
     ?STY:NCASEOPT:Lower{PROP:FontStyle} = CHOOSE(defaultCase = 3,FONT:italic,FONT:regular)
  ELSE
     ?nCaseOpt:Prompt{PROP:FontStyle} = FONT:bold
     ?STY:NCASEOPT:Mixed{PROP:FontStyle} = CHOOSE(qStyles.nCaseOpt = 1,FONT:bold,FONT:regular)
     ?STY:NCASEOPT:Upper{PROP:FontStyle} = CHOOSE(qStyles.nCaseOpt = 2,FONT:bold,FONT:regular)
     ?STY:NCASEOPT:Lower{PROP:FontStyle} = CHOOSE(qStyles.nCaseOpt = 3,FONT:bold,FONT:regular)
  END
  PARENT.Reset(Force)
  IF RECORDS(ListBoxQueue)
     ?ChangeKeyword{PROP:Disable} = FALSE
     ?DeleteKeyword{PROP:Disable} = FALSE
  ELSE
     ?ChangeKeyword{PROP:Disable} = TRUE
     ?DeleteKeyword{PROP:Disable} = TRUE
  END
  IF RECORDS(qOptions)
     ?ChangeOption{PROP:Disable} = FALSE
  ELSE
     ?ChangeOption{PROP:Disable} = TRUE
  END
  IF RECORDS(FilePatternQueue)
     ?ChangeFilePattern{PROP:Disable} = FALSE
     ?DeleteFilePattern{PROP:Disable} = FALSE
  ELSE
     ?ChangeFilePattern{PROP:Disable} = TRUE
     ?DeleteFilePattern{PROP:Disable} = TRUE
  END


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
    OF ?szFilePatterns
      DO CheckForChanges
    OF ?kwd:szKeywords
      SELF.Update()
      PUT(qKeywords)
    OF ?cmdFontDialog
      ThisWindow.Update()
      CASE POPUP('Default|Select Font')
      OF 1
         qStyles.szFontName = ''
         qStyles.nFontSize  = 0
         qStyles.nFontStyle = FONT:regular
         SELF.Reset()
      OF 2
         IF FONTDIALOG('Choose Font',qStyles.szFontName,qStyles.nFontSize,qStyles.lForeColor,qStyles.nFontStyle,0)
            IF nCurrentStyle = 32
               defaultFont = qStyles.szFontName
               defaultSize = qStyles.nFontSize
               defaultFore = qStyles.lForeColor
               defaultStyle = qStyles.nFontStyle
            END
            SELF.Reset()
         END
      END
    OF ?cmdSelectForeColor
      ThisWindow.Update()
      CASE POPUP('Default|Select Color')
      OF 1
         qStyles.lForeColor = COLOR:NONE
         IF nCurrentStyle = 32
            defaultFore = qStyles.lForeColor
         END
         SELF.Reset()
      OF 2
         IF COLORDIALOG('Fore Color',qStyles.lForeColor)
            IF nCurrentStyle = 32
               defaultFore = qStyles.lForeColor
            END
            SELF.Reset()
         END
      END
    OF ?cmdSelectBackColor
      ThisWindow.Update()
      CASE POPUP('Default|Select Color')
      OF 1
         qStyles.lBackColor = COLOR:NONE
         IF nCurrentStyle = 32
            defaultBack = qStyles.lBackColor
         END
         SELF.Reset()
      OF 2
         IF COLORDIALOG('Back Color',qStyles.lBackColor)
            IF nCurrentStyle = 32
               defaultBack = qStyles.lBackColor
            END
            SELF.Reset()
         END
      END
    OF ?sty:bHide
      IF nCurrentStyle = 32
         defaultHide = sty:bHide
      END
      SELF.Reset()
    OF ?sty:bHotSpot
      IF nCurrentStyle = 32
         defaultHot = sty:bHotSpot
      END
      SELF.Reset()
    OF ?sty:bEolFilled
      IF nCurrentStyle = 32
         defaultEOLF = sty:bEolFilled
      END
      SELF.Reset()
    OF ?sty:nCaseOpt
      IF nCurrentStyle = 32
         defaultCase = sty:nCaseOpt
      END
      SELF.Reset()
    OF ?cmdSelectCase
      ThisWindow.Update()
      CASE POPUP('Default|Mixed|Upper|Lower')
        OF 1
           sty:nCaseOpt = 0
        OF 2
           sty:nCaseOpt = 1
        OF 3
           sty:nCaseOpt = 2
        OF 4
           sty:nCaseOpt = 3
      END
      SELF.Reset()
    OF ?cmdSave
      ThisWindow.Update()
      DO CheckForChanges
      IF bDirty = TRUE
         DO SavePropertyFile
         bPropertiesChanged = TRUE
      END
      POST(EVENT:CloseWindow)
    OF ?cmdCancel
      ThisWindow.Update()
      bPropertiesChanged = FALSE
      POST(EVENT:CloseWindow)
    OF ?InsertFilePattern
      ThisWindow.Update()
      LOOP
        ThisWindow.VCRRequest = VCR:None
        IF KEYCODE() = MouseRightUp
          SETKEYCODE(0)
        END
        ReturnValue = QEIP4:EM.Run(InsertRecord)
        CASE ThisWindow.VCRRequest
          OF VCR:Forward
             IF POINTER(FilePatternQueue) < RECORDS(FilePatternQueue)
                ?lbFilePatterns{PROP:Selected} = POINTER(FilePatternQueue)
             ELSE
                ThisWindow.VCRRequest = VCR:None
             END
          OF VCR:Backward
             IF POINTER(FilePatternQueue) > 1
                ?lbFilePatterns{PROP:Selected} = POINTER(FilePatternQueue)
             ELSE
                ThisWindow.VCRRequest = VCR:None
             END
        END
      UNTIL ThisWindow.VCRRequest = VCR:None
      SELECT(?lbFilePatterns)
      ThisWindow.Reset()
    OF ?ChangeFilePattern
      ThisWindow.Update()
      LOOP
        ThisWindow.VCRRequest = VCR:None
        IF KEYCODE() = MouseRightUp
          SETKEYCODE(0)
        END
        ReturnValue = QEIP4:EM.Run(ChangeRecord)
        CASE ThisWindow.VCRRequest
          OF VCR:Forward
             IF POINTER(FilePatternQueue) < RECORDS(FilePatternQueue)
                GET(FilePatternQueue,POINTER(FilePatternQueue)+1)
                ?lbFilePatterns{PROP:Selected} = POINTER(FilePatternQueue)
             ELSE
                ThisWindow.VCRRequest = VCR:None
             END
          OF VCR:Backward
             IF POINTER(FilePatternQueue) > 1
                GET(FilePatternQueue,POINTER(FilePatternQueue)-1)
                ?lbFilePatterns{PROP:Selected} = POINTER(FilePatternQueue)
             ELSE
                ThisWindow.VCRRequest = VCR:None
             END
        END
      UNTIL ThisWindow.VCRRequest = VCR:None
      SELECT(?lbFilePatterns)
      ThisWindow.Reset()
    OF ?DeleteFilePattern
      ThisWindow.Update()
      ReturnValue = QEIP4:EM.Run(DeleteRecord)
      ThisWindow.Reset()
    OF ?ChangeOption
      ThisWindow.Update()
      LOOP
        ThisWindow.VCRRequest = VCR:None
        IF KEYCODE() = MouseRightUp
          SETKEYCODE(0)
        END
        ReturnValue = QEIP3:EM.Run(ChangeRecord)
        CASE ThisWindow.VCRRequest
          OF VCR:Forward
             IF POINTER(qOptions) < RECORDS(qOptions)
                GET(qOptions,POINTER(qOptions)+1)
                ?lbOptions{PROP:Selected} = POINTER(qOptions)
             ELSE
                ThisWindow.VCRRequest = VCR:None
             END
          OF VCR:Backward
             IF POINTER(qOptions) > 1
                GET(qOptions,POINTER(qOptions)-1)
                ?lbOptions{PROP:Selected} = POINTER(qOptions)
             ELSE
                ThisWindow.VCRRequest = VCR:None
             END
        END
      UNTIL ThisWindow.VCRRequest = VCR:None
      SELECT(?lbOptions)
      ThisWindow.Reset()
    OF ?InsertKeyword
      ThisWindow.Update()
      LOOP
        ThisWindow.VCRRequest = VCR:None
        IF KEYCODE() = MouseRightUp
          SETKEYCODE(0)
        END
        ReturnValue = QEIP2:EM.Run(InsertRecord)
        CASE ThisWindow.VCRRequest
          OF VCR:Forward
             IF POINTER(ListBoxQueue) < RECORDS(ListBoxQueue)
                ?lbKeywordValues{PROP:Selected} = POINTER(ListBoxQueue)
             ELSE
                ThisWindow.VCRRequest = VCR:None
             END
          OF VCR:Backward
             IF POINTER(ListBoxQueue) > 1
                ?lbKeywordValues{PROP:Selected} = POINTER(ListBoxQueue)
             ELSE
                ThisWindow.VCRRequest = VCR:None
             END
        END
      UNTIL ThisWindow.VCRRequest = VCR:None
      SELECT(?lbKeywordValues)
      ThisWindow.Reset()
    OF ?ChangeKeyword
      ThisWindow.Update()
      LOOP
        ThisWindow.VCRRequest = VCR:None
        IF KEYCODE() = MouseRightUp
          SETKEYCODE(0)
        END
        ReturnValue = QEIP2:EM.Run(ChangeRecord)
        CASE ThisWindow.VCRRequest
          OF VCR:Forward
             IF POINTER(ListBoxQueue) < RECORDS(ListBoxQueue)
                GET(ListBoxQueue,POINTER(ListBoxQueue)+1)
                ?lbKeywordValues{PROP:Selected} = POINTER(ListBoxQueue)
             ELSE
                ThisWindow.VCRRequest = VCR:None
             END
          OF VCR:Backward
             IF POINTER(ListBoxQueue) > 1
                GET(ListBoxQueue,POINTER(ListBoxQueue)-1)
                ?lbKeywordValues{PROP:Selected} = POINTER(ListBoxQueue)
             ELSE
                ThisWindow.VCRRequest = VCR:None
             END
        END
      UNTIL ThisWindow.VCRRequest = VCR:None
      SELECT(?lbKeywordValues)
      ThisWindow.Reset()
    OF ?DeleteKeyword
      ThisWindow.Update()
      ReturnValue = QEIP2:EM.Run(DeleteRecord)
      ThisWindow.Reset()
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
    CASE EVENT()
      OF EVENT:User
         DO LoadKeywordsFromListBoxQueue
         HIDE(?lbKeywordValues)
         UNHIDE(?kwd:szKeywords)
         DISPLAY(?kwd:szKeywords)
      OF EVENT:User+1
         DO LoadFilePatternsFromFilePatternQueue
         HIDE(?lbFilePatterns)
         UNHIDE(?szFilePatterns)
         DISPLAY(?szFilePatterns)
    END
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
Indx   LONG
  CODE
  LOOP                                                     ! This method receives all field specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  CASE FIELD()
  OF ?Sheet
    CASE EVENT()
    OF EVENT:TabChanging
      (?Sheet{PROP:ChoiceFeq}){PROP:FontColor} = COLOR:Black !00E16941h
      (?Sheet{PROP:ChoiceFeq}){PROP:FontStyle} = FONT:regular
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
  CASE FIELD()
  OF ?szFilePatterns
    CASE EVENT()
    OF EVENT:AlertKey
      CASE KEYCODE()
        OF MouseRight
           IF POPUP('View as List')
               SELF.Update()
               HIDE(?szFilePatterns)
               DO FillFilePatternQueue
               UNHIDE(?lbFilePatterns)
           END
      END
    END
  OF ?lbFilePatterns
    CASE EVENT()
    OF EVENT:AlertKey
      CASE KEYCODE()
        OF MouseLeft2
           IF ~?ChangeFilePattern{PROP:Disable} AND RECORDS(FilePatternQueue)
              POST(EVENT:Accepted,?ChangeFilePattern)
           END
        OF MouseRight
           QEIP4:PopupString = QEIP4:PopupMgr.Ask()
      END
      CASE KEYCODE()
        OF InsertKey
           POST(EVENT:Accepted,?InsertFilePattern)
        OF DeleteKey
           POST(EVENT:Accepted,?DeleteFilePattern)
        OF CtrlEnter
           POST(EVENT:Accepted,?ChangeFilePattern)
      END
    END
  OF ?lbOptions
    CASE EVENT()
    OF EVENT:AlertKey
      CASE KEYCODE()
        OF MouseLeft2
           IF ~?ChangeOption{PROP:Disable} AND RECORDS(qOptions)
              POST(EVENT:Accepted,?ChangeOption)
           END
        OF MouseRight
           QEIP3:PopupString = QEIP3:PopupMgr.Ask()
      END
    END
  OF ?kwd:szKeywords
    CASE EVENT()
    OF EVENT:AlertKey
      CASE KEYCODE()
        OF MouseRight
           IF POPUP('View as List')
               SELF.Update()
               !PUT(qKeywords)
               HIDE(?kwd:szKeywords)
               DO FillListBoxQueue
               UNHIDE(?lbKeywordValues)
           END
      END
    END
  OF ?lbKeywordValues
    CASE EVENT()
    OF EVENT:AlertKey
      CASE KEYCODE()
        OF MouseLeft2
           IF ~?ChangeKeyword{PROP:Disable} AND RECORDS(ListBoxQueue)
              POST(EVENT:Accepted,?ChangeKeyword)
           END
        OF MouseRight
           QEIP2:PopupString = QEIP2:PopupMgr.Ask()
      END
      CASE KEYCODE()
        OF InsertKey
           POST(EVENT:Accepted,?InsertKeyword)
        OF DeleteKey
           POST(EVENT:Accepted,?DeleteKeyword)
        OF CtrlEnter
           POST(EVENT:Accepted,?ChangeKeyword)
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
    CASE FIELD()
    OF ?lbKeywords
      IF ?kwd:szKeywords{PROP:Hide} = TRUE
         DO LoadKeywordsFromListBoxQueue
      END
    END
  ReturnValue = PARENT.TakeNewSelection()
    CASE FIELD()
    OF ?Sheet
      (?Sheet{PROP:ChoiceFeq}){PROP:FontColor} = 00E16941h
      (?Sheet{PROP:ChoiceFeq}){PROP:FontStyle} = FONT:Bold
    OF ?lbOptions
      GET(qOptions,CHOICE(?lbOptions))
      DISPLAY(?opt:szOptionTip)
    OF ?lbKeywords
      DO CheckForChanges
      GET(qKeywords,CHOICE(?lbKeywords))
      saveKeywordSet = qKeywords
      IF ?kwd:szKeywords{PROP:Hide} = TRUE
         DO FillListBoxQueue
         DISPLAY(?lbKeywordValues)
      ELSE
         DISPLAY(?kwd:szKeywords)
      END
    OF ?lbStyles
      DO CheckForChanges
      GET(qStyles,CHOICE(?lbStyles))
      saveStyle = qStyles
      SELF.Reset()
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE FIELD()
    OF ?szFilePatterns
      saveFilePatterns = szFilePatterns
    END
  ReturnValue = PARENT.TakeSelected()
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
       !IF glo:ShowPropertyEditorInfo = TRUE
       !   PropertyEditorInfo()
       !END
!        post(event:visibleondesktop)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

QEIP2:EM.AddControl            PROCEDURE(<EditClass E>,UNSIGNED Column,BYTE AutoFree)
  CODE
  PARENT.AddControl(E,Column,AutoFree)
  RETURN

QEIP2:EM.ClearColumn           PROCEDURE
  CODE
  IF KEYCODE() <> EscKey
     IF SELF.LastColumn
        UPDATE
        GET(SELF.EQ,SELF.Column)
        PUT(ListBoxQueue)
        ASSERT(~ERRORCODE())
     END
  END
  PARENT.ClearColumn()
  RETURN

QEIP2:EM.Init                  PROCEDURE
RetVal BYTE(RequestCancelled)
AtEnd  BYTE,AUTO
  CODE
  SELF.TabAction = EIPAction:Always
  SELF.EnterAction = EIPAction:Always
  SELF.ArrowAction = EIPAction:Default+EIPAction:Remain+EIPAction:RetainColumn
  SELF.Arrow &= SELF.ArrowAction
  SELF.Enter &= SELF.EnterAction
  SELF.EQ &= QEIP2:EditList
  SELF.Errors &= NULL
  SELF.Fields &= QEIP2:Fields
  SELF.FocusLoss &= SELF.FocusLossAction
  SELF.ListControl = ?lbKeywordValues
  SELF.Tab &= SELF.TabAction
  SELF.VCRRequest &= ThisWindow.VCRRequest
  SELF.AddControl(QEIP2::lbq:Value,1,0)
  SELF.CurrentChoice = CHOICE(?lbKeywordValues)
  IF ~SELF.CurrentChoice
     SELF.CurrentChoice = 1
     ?lbKeywordValues{PROP:Selected} = 1
  END
  GET(ListBoxQueue,SELF.CurrentChoice)
  CASE SELF.Req
  OF InsertRecord
    IF RECORDS(ListBoxQueue)
      AtEnd = CHOOSE(SELF.CurrentChoice = RECORDS(ListBoxQueue))
      CASE(SELF.Insert)
        OF EIPAction:Before
           !Default
        OF EIPAction:Append
           SELF.CurrentChoice = RECORDS(ListBoxQueue)+1
      ELSE
           SELF.CurrentChoice += 1
      END
    ELSE
      SELF.CurrentChoice = 1
    END
    SELF.PrimeRecord()
    ADD(ListBoxQueue,SELF.CurrentChoice)
    ASSERT(~ERRORCODE())
    DISPLAY(?lbKeywordValues)
    SELECT(?lbKeywordValues,SELF.CurrentChoice)
    SELF.Column = 1
  OF DeleteRecord
    RetVal = CHOOSE(GlobalErrors.Throw(Msg:ConfirmDelete) = Level:Benign,RequestCompleted,RequestCancelled)
    IF RetVal = RequestCompleted
       DELETE(ListBoxQueue)
    END
    SELF.Response = RetVal
    RETURN Level:Fatal
  OF ChangeRecord
    QEIP2:SaveEntry = ListBoxQueue
    IF KEYCODE() = MouseLeft2
      SELF.Column = ?lbKeywordValues{PROPLIST:MouseUpField}
    END
  ELSE
    ASSERT(0)
  END
  GET(ListBoxQueue,SELF.CurrentChoice)
  SELF.Fields.AssignRightToLeft()
  ?lbKeywordValues{PROP:Alrt,QEIP:MouseLeft2Index} = 0 ! Prevent alert short-stopping double click
  RetVal = PARENT.Init()
  RETURN(RetVal)

QEIP2:EM.InitControls          PROCEDURE
  CODE
  SELF.EQ.Field = 1
  PARENT.InitControls()
  RETURN

QEIP2:EM.Kill                  PROCEDURE
ReturnValue BYTE,AUTO
I           LONG,AUTO
J           LONG,AUTO
  CODE
  ReturnValue = PARENT.Kill()
  !Now dispose of any edit classes we created
  J = RECORDS(QEIP2:EditList)
  LOOP I = 1 TO J
    GET(QEIP2:EditList,I)
    IF ~QEIP2:EditList.Control &= NULL AND QEIP2:EditList.FreeUp = TRUE
       DISPOSE(QEIP2:EditList.Control)
    END
  END
  !and free up the edit queue
  FREE(QEIP2:EditList)
  RETURN(ReturnValue)

QEIP2:EM.Next                  PROCEDURE
  CODE
  PARENT.Next()
  RETURN

QEIP2:EM.GetEdit               PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  ReturnValue = PARENT.GetEdit()
  RETURN(ReturnValue)

QEIP2:EM.PrimeRecord           PROCEDURE(BYTE SC)
  CODE
  IF ~SC
     CLEAR(ListBoxQueue)
  END
  CLEAR(ListBoxQueue.lbq:Value)
  RETURN

QEIP2:EM.ResetColumn           PROCEDURE
  CODE
  PARENT.ResetColumn()
  RETURN

QEIP2:EM.Run                   PROCEDURE(BYTE Req)
ReturnValue BYTE,AUTO
  CODE
  thisReq = Req
  ReturnValue = PARENT.Run(Req)
  RETURN(ReturnValue)

QEIP2:EM.TakeAction            PROCEDURE(UNSIGNED Action)
  CODE
  PARENT.TakeAction(Action)

QEIP2:EM.TakeCompleted         PROCEDURE(BYTE Force)
SaveAns UNSIGNED,AUTO
ptr         LONG
thisValue   LIKE(ListBoxQueue.Value)
  CODE
  IF Force <> Button:No
     IF SELF.Req = InsertRecord
        ptr = POINTER(ListBoxQueue)
        thisValue = ListBoxQueue.Value
        GET(ListBoxQueue,ListBoxQueue.Value)
        IF ERRORCODE()
           GET(ListBoxQueue,ptr)
           ListBoxQueue.Value = thisValue
        ELSE
           GET(ListBoxQueue,ptr)
           ListBoxQueue.Value = thisValue
           GlobalErrors.ThrowMessage(Msg:InsertIllegal,'Keyword')
           Force = Button:No
        END
     ELSIF SELF.Req = ChangeRecord
        IF NOT SELF.Fields.Equal()
           ptr = POINTER(ListBoxQueue)
           thisValue = ListBoxQueue.Value
           GET(ListBoxQueue,ListBoxQueue.Value)
           IF ERRORCODE()
              GET(ListBoxQueue,ptr)
              ListBoxQueue.Value = thisValue
           ELSE
              GET(ListBoxQueue,ptr)
              GlobalErrors.ThrowMessage(Msg:InsertIllegal,'Keyword')
              Force = Button:No
           END
        END
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
          DELETE(ListBoxQueue)
          IF SELF.CurrentChoice AND SELF.Insert <> EIPAction:Before
             SELF.CurrentChoice -= 1
          END
       ELSE
          SELF.Fields.AssignLeftToRight
          PUT(ListBoxQueue)
       END
    OF Button:Yes
       SELF.Response = RequestCompleted
  END
  PARENT.TakeCompleted(Force)
  thisValue = ListBoxQueue.Value
  SORT(ListBoxQueue,+ListBoxQueue.Value)
  ListBoxQueue.Value = thisValue
  GET(ListBoxQueue,ListBoxQueue.Value)
  ?lbKeywordValues{PROP:Selected} = POINTER(ListBoxQueue)
  RETURN

QEIP2:EM.TakeEvent             PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  ReturnValue = PARENT.TakeEvent()
  RETURN(ReturnValue)

QEIP2:EM.TakeFieldEvent        PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  ReturnValue = PARENT.TakeFieldEvent()
  RETURN(ReturnValue)

QEIP2:EM.TakeFocusLoss         PROCEDURE
  CODE
  PARENT.TakeFocusLoss()
  RETURN

QEIP2:EM.TakeNewSelection      PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  IF FIELD() = ?lbKeywordValues
    IF CHOICE(?lbKeywordValues) = SELF.CurrentChoice
      ReturnValue = PARENT.TakeNewSelection()
    ELSE                                  ! Focus change to different record
      SELF.TakeFocusLoss
      IF SELF.Again
        SELECT(?lbKeywordValues,SELF.CurrentChoice)
        ReturnValue = Level:Benign
      ELSE
        SELF.CurrentChoice = CHOICE(?lbKeywordValues)
        SELF.Response = RequestCancelled           ! Avoid cursor following 'new' record
        ReturnValue = Level:Fatal
      END
    END
  END
  RETURN(ReturnValue)



QEIP2::lbq:Value.CreateControl    PROCEDURE
  CODE
  PARENT.CreateControl()
  IF szLexer = 'clarion'
     SELF.Feq{PROP:Upr} = TRUE
  END
  SELF.Feq{PROP:Alrt,255} = DeleteKey
  RETURN

QEIP2::lbq:Value.Init    PROCEDURE(UNSIGNED FieldNumber,UNSIGNED ListBox,*? UseVar)
  CODE
  PARENT.Init(FieldNumber,ListBox,UseVar)
  RETURN

QEIP2::lbq:Value.Kill    PROCEDURE
  CODE
  PARENT.Kill()
  RETURN

QEIP2::lbq:Value.SetAlerts    PROCEDURE
  CODE
  PARENT.SetAlerts()
  RETURN

QEIP2::lbq:Value.SetReadOnly    PROCEDURE(BYTE State)
  CODE
  PARENT.SetReadOnly(State)
  RETURN

QEIP2::lbq:Value.TakeAccepted    PROCEDURE(BYTE Action)
ReturnValue BYTE
  CODE
  ReturnValue = PARENT.TakeAccepted(Action)
  RETURN(ReturnValue)

QEIP2::lbq:Value.TakeEvent    PROCEDURE(UNSIGNED Event)
ReturnValue BYTE
  CODE
  ReturnValue = PARENT.TakeEvent(Event)
  RETURN(ReturnValue)

QEIP3:EM.AddControl            PROCEDURE(<EditClass E>,UNSIGNED Column,BYTE AutoFree)
  CODE
  PARENT.AddControl(E,Column,AutoFree)
  RETURN

QEIP3:EM.ClearColumn           PROCEDURE
  CODE
  IF KEYCODE() <> EscKey
     IF SELF.LastColumn
        UPDATE
        GET(SELF.EQ,SELF.Column)
        PUT(qOptions)
        ASSERT(~ERRORCODE())
     END
  END
  PARENT.ClearColumn()
  RETURN

QEIP3:EM.Init                  PROCEDURE
RetVal BYTE(RequestCancelled)
AtEnd  BYTE,AUTO
  CODE
  SELF.ArrowAction = EIPAction:Default+EIPAction:Remain+EIPAction:RetainColumn
  SELF.Arrow &= SELF.ArrowAction
  SELF.Enter &= SELF.EnterAction
  SELF.EQ &= QEIP3:EditList
  SELF.Errors &= NULL
  SELF.Fields &= QEIP3:Fields
  SELF.FocusLoss &= SELF.FocusLossAction
  SELF.ListControl = ?lbOptions
  SELF.Tab &= SELF.TabAction
  SELF.VCRRequest &= ThisWindow.VCRRequest
  SELF.AddControl(,1,0)
  SELF.AddControl(QEIP3::opt:szValue,3,0)
  SELF.CurrentChoice = CHOICE(?lbOptions)
  IF ~SELF.CurrentChoice
     SELF.CurrentChoice = 1
     ?lbOptions{PROP:Selected} = 1
  END
  GET(qOptions,SELF.CurrentChoice)
  CASE SELF.Req
  OF InsertRecord
    IF RECORDS(qOptions)
      AtEnd = CHOOSE(SELF.CurrentChoice = RECORDS(qOptions))
      CASE(SELF.Insert)
        OF EIPAction:Before
           !Default
        OF EIPAction:Append
           SELF.CurrentChoice = RECORDS(qOptions)+1
      ELSE
           SELF.CurrentChoice += 1
      END
    ELSE
      SELF.CurrentChoice = 1
    END
    SELF.PrimeRecord()
    ADD(qOptions,SELF.CurrentChoice)
    ASSERT(~ERRORCODE())
    DISPLAY(?lbOptions)
    SELECT(?lbOptions,SELF.CurrentChoice)
    SELF.Column = 1
  OF DeleteRecord
    RetVal = CHOOSE(GlobalErrors.Throw(Msg:ConfirmDelete) = Level:Benign,RequestCompleted,RequestCancelled)
    IF RetVal = RequestCompleted
       DELETE(qOptions)
    END
    SELF.Response = RetVal
    RETURN Level:Fatal
  OF ChangeRecord
    QEIP3:SaveEntry = qOptions
    IF KEYCODE() = MouseLeft2
      SELF.Column = ?lbOptions{PROPLIST:MouseUpField}
    END
  ELSE
    ASSERT(0)
  END
  GET(qOptions,SELF.CurrentChoice)
  SELF.Fields.AssignRightToLeft()
  ?lbOptions{PROP:Alrt,QEIP:MouseLeft2Index} = 0 ! Prevent alert short-stopping double click
  RetVal = PARENT.Init()
  RETURN(RetVal)

QEIP3:EM.InitControls          PROCEDURE
  CODE
  SELF.EQ.Field = 1
  PARENT.InitControls()
  RETURN

QEIP3:EM.Kill                  PROCEDURE
ReturnValue BYTE,AUTO
I           LONG,AUTO
J           LONG,AUTO
  CODE
  ReturnValue = PARENT.Kill()
  !Now dispose of any edit classes we created
  J = RECORDS(QEIP3:EditList)
  LOOP I = 1 TO J
    GET(QEIP3:EditList,I)
    IF ~QEIP3:EditList.Control &= NULL AND QEIP3:EditList.FreeUp = TRUE
       DISPOSE(QEIP3:EditList.Control)
    END
  END
  !and free up the edit queue
  FREE(QEIP3:EditList)
  RETURN(ReturnValue)

QEIP3:EM.Next                  PROCEDURE
  CODE
  PARENT.Next()
  RETURN

QEIP3:EM.GetEdit               PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  ReturnValue = PARENT.GetEdit()
  RETURN(ReturnValue)

QEIP3:EM.PrimeRecord           PROCEDURE(BYTE SC)
  CODE
  IF ~SC
     CLEAR(qOptions)
  END
  CLEAR(qOptions.opt:szOption)
  CLEAR(qOptions.opt:szValue)
  RETURN

QEIP3:EM.ResetColumn           PROCEDURE
  CODE
  PARENT.ResetColumn()
  RETURN

QEIP3:EM.Run                   PROCEDURE(BYTE Req)
ReturnValue BYTE,AUTO
  CODE
  ReturnValue = PARENT.Run(Req)
  RETURN(ReturnValue)

QEIP3:EM.TakeAction            PROCEDURE(UNSIGNED Action)
  CODE
  PARENT.TakeAction(Action)

QEIP3:EM.TakeCompleted         PROCEDURE(BYTE Force)
SaveAns UNSIGNED,AUTO
  CODE
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
          DELETE(qOptions)
          IF SELF.CurrentChoice AND SELF.Insert <> EIPAction:Before
             SELF.CurrentChoice -= 1
          END
       ELSE
          SELF.Fields.AssignLeftToRight
          PUT(qOptions)
       END
    OF Button:Yes
       SELF.Response = RequestCompleted
  END
  PARENT.TakeCompleted(Force)
  RETURN

QEIP3:EM.TakeEvent             PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  ReturnValue = PARENT.TakeEvent()
  RETURN(ReturnValue)

QEIP3:EM.TakeFieldEvent        PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  ReturnValue = PARENT.TakeFieldEvent()
  RETURN(ReturnValue)

QEIP3:EM.TakeFocusLoss         PROCEDURE
  CODE
  PARENT.TakeFocusLoss()
  RETURN

QEIP3:EM.TakeNewSelection      PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  IF FIELD() = ?lbOptions
    IF CHOICE(?lbOptions) = SELF.CurrentChoice
      ReturnValue = PARENT.TakeNewSelection()
    ELSE                                  ! Focus change to different record
      SELF.TakeFocusLoss
      IF SELF.Again
        SELECT(?lbOptions,SELF.CurrentChoice)
        ReturnValue = Level:Benign
      ELSE
        SELF.CurrentChoice = CHOICE(?lbOptions)
        SELF.Response = RequestCancelled           ! Avoid cursor following 'new' record
        ReturnValue = Level:Fatal
      END
    END
  END
  RETURN(ReturnValue)



QEIP3::opt:szValue.CreateControl    PROCEDURE
  CODE
  IF qOptions.IsBool
     SELF.Feq = CREATE(0,CREATE:droplist)
     SELF.Feq{PROP:FROM} = 'false|true'
     SELF.Feq{PROP:Drop} = 2
  ELSIF qOptions.szOption = 'asp.default.language'
     SELF.Feq = CREATE(0,CREATE:droplist)
     SELF.Feq{PROP:FROM} = 'JavaScript|VBScript|Python'
     SELF.Feq{PROP:Drop} = 3
  ELSE
  PARENT.CreateControl()
  END
  RETURN

QEIP3::opt:szValue.Init    PROCEDURE(UNSIGNED FieldNumber,UNSIGNED ListBox,*? UseVar)
  CODE
  PARENT.Init(FieldNumber,ListBox,UseVar)
  RETURN

QEIP3::opt:szValue.Kill    PROCEDURE
  CODE
  PARENT.Kill()
  RETURN

QEIP3::opt:szValue.SetAlerts    PROCEDURE
  CODE
  PARENT.SetAlerts()
  RETURN

QEIP3::opt:szValue.SetReadOnly    PROCEDURE(BYTE State)
  CODE
  PARENT.SetReadOnly(State)
  RETURN

QEIP3::opt:szValue.TakeAccepted    PROCEDURE(BYTE Action)
ReturnValue BYTE
  CODE
  ReturnValue = PARENT.TakeAccepted(Action)
  CASE ReturnValue
    OF EditAction:Cancel OROF EditAction:None
       !nothing changed
  ELSE
       bDirty = TRUE
  END
  RETURN(ReturnValue)

QEIP3::opt:szValue.TakeEvent    PROCEDURE(UNSIGNED Event)
ReturnValue BYTE
  CODE
  ReturnValue = PARENT.TakeEvent(Event)
  RETURN(ReturnValue)

QEIP4:EM.AddControl            PROCEDURE(<EditClass E>,UNSIGNED Column,BYTE AutoFree)
  CODE
  PARENT.AddControl(E,Column,AutoFree)
  RETURN

QEIP4:EM.ClearColumn           PROCEDURE
  CODE
  IF KEYCODE() <> EscKey
     IF SELF.LastColumn
        UPDATE
        GET(SELF.EQ,SELF.Column)
        PUT(FilePatternQueue)
        ASSERT(~ERRORCODE())
     END
  END
  PARENT.ClearColumn()
  RETURN

QEIP4:EM.Init                  PROCEDURE
RetVal BYTE(RequestCancelled)
AtEnd  BYTE,AUTO
  CODE
  SELF.ArrowAction = EIPAction:Default+EIPAction:Remain+EIPAction:RetainColumn
  SELF.Arrow &= SELF.ArrowAction
  SELF.Enter &= SELF.EnterAction
  SELF.EQ &= QEIP4:EditList
  SELF.Errors &= NULL
  SELF.Fields &= QEIP4:Fields
  SELF.FocusLoss &= SELF.FocusLossAction
  SELF.ListControl = ?lbFilePatterns
  SELF.Tab &= SELF.TabAction
  SELF.VCRRequest &= ThisWindow.VCRRequest
  SELF.CurrentChoice = CHOICE(?lbFilePatterns)
  IF ~SELF.CurrentChoice
     SELF.CurrentChoice = 1
     ?lbFilePatterns{PROP:Selected} = 1
  END
  GET(FilePatternQueue,SELF.CurrentChoice)
  CASE SELF.Req
  OF InsertRecord
    IF RECORDS(FilePatternQueue)
      AtEnd = CHOOSE(SELF.CurrentChoice = RECORDS(FilePatternQueue))
      CASE(SELF.Insert)
        OF EIPAction:Before
           !Default
        OF EIPAction:Append
           SELF.CurrentChoice = RECORDS(FilePatternQueue)+1
      ELSE
           SELF.CurrentChoice += 1
      END
    ELSE
      SELF.CurrentChoice = 1
    END
    SELF.PrimeRecord()
    ADD(FilePatternQueue,SELF.CurrentChoice)
    ASSERT(~ERRORCODE())
    DISPLAY(?lbFilePatterns)
    SELECT(?lbFilePatterns,SELF.CurrentChoice)
    SELF.Column = 1
  OF DeleteRecord
    RetVal = CHOOSE(GlobalErrors.Throw(Msg:ConfirmDelete) = Level:Benign,RequestCompleted,RequestCancelled)
    IF RetVal = RequestCompleted
       DELETE(FilePatternQueue)
    END
    SELF.Response = RetVal
    RETURN Level:Fatal
  OF ChangeRecord
    QEIP4:SaveEntry = FilePatternQueue
    IF KEYCODE() = MouseLeft2
      SELF.Column = ?lbFilePatterns{PROPLIST:MouseUpField}
    END
  ELSE
    ASSERT(0)
  END
  GET(FilePatternQueue,SELF.CurrentChoice)
  SELF.Fields.AssignRightToLeft()
  ?lbFilePatterns{PROP:Alrt,QEIP:MouseLeft2Index} = 0 ! Prevent alert short-stopping double click
  RetVal = PARENT.Init()
  RETURN(RetVal)

QEIP4:EM.InitControls          PROCEDURE
  CODE
  SELF.EQ.Field = 1
  PARENT.InitControls()
  RETURN

QEIP4:EM.Kill                  PROCEDURE
ReturnValue BYTE,AUTO
I           LONG,AUTO
J           LONG,AUTO
  CODE
  ReturnValue = PARENT.Kill()
  !Now dispose of any edit classes we created
  J = RECORDS(QEIP4:EditList)
  LOOP I = 1 TO J
    GET(QEIP4:EditList,I)
    IF ~QEIP4:EditList.Control &= NULL AND QEIP4:EditList.FreeUp = TRUE
       DISPOSE(QEIP4:EditList.Control)
    END
  END
  !and free up the edit queue
  FREE(QEIP4:EditList)
  RETURN(ReturnValue)

QEIP4:EM.Next                  PROCEDURE
  CODE
  PARENT.Next()
  RETURN

QEIP4:EM.GetEdit               PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  ReturnValue = PARENT.GetEdit()
  RETURN(ReturnValue)

QEIP4:EM.PrimeRecord           PROCEDURE(BYTE SC)
  CODE
  IF ~SC
     CLEAR(FilePatternQueue)
  END
  RETURN

QEIP4:EM.ResetColumn           PROCEDURE
  CODE
  PARENT.ResetColumn()
  RETURN

QEIP4:EM.Run                   PROCEDURE(BYTE Req)
ReturnValue BYTE,AUTO
  CODE
  ReturnValue = PARENT.Run(Req)
  RETURN(ReturnValue)

QEIP4:EM.TakeAction            PROCEDURE(UNSIGNED Action)
  CODE
  PARENT.TakeAction(Action)

QEIP4:EM.TakeCompleted         PROCEDURE(BYTE Force)
SaveAns UNSIGNED,AUTO
  CODE
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
          DELETE(FilePatternQueue)
          IF SELF.CurrentChoice AND SELF.Insert <> EIPAction:Before
             SELF.CurrentChoice -= 1
          END
       ELSE
          SELF.Fields.AssignLeftToRight
          PUT(FilePatternQueue)
       END
    OF Button:Yes
       SELF.Response = RequestCompleted
  END
  PARENT.TakeCompleted(Force)
  RETURN

QEIP4:EM.TakeEvent             PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  ReturnValue = PARENT.TakeEvent()
  RETURN(ReturnValue)

QEIP4:EM.TakeFieldEvent        PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  ReturnValue = PARENT.TakeFieldEvent()
  RETURN(ReturnValue)

QEIP4:EM.TakeFocusLoss         PROCEDURE
  CODE
  PARENT.TakeFocusLoss()
  RETURN

QEIP4:EM.TakeNewSelection      PROCEDURE
ReturnValue BYTE,AUTO
  CODE
  IF FIELD() = ?lbFilePatterns
    IF CHOICE(?lbFilePatterns) = SELF.CurrentChoice
      ReturnValue = PARENT.TakeNewSelection()
    ELSE                                  ! Focus change to different record
      SELF.TakeFocusLoss
      IF SELF.Again
        SELECT(?lbFilePatterns,SELF.CurrentChoice)
        ReturnValue = Level:Benign
      ELSE
        SELF.CurrentChoice = CHOICE(?lbFilePatterns)
        SELF.Response = RequestCancelled           ! Avoid cursor following 'new' record
        ReturnValue = Level:Fatal
      END
    END
  END
  RETURN(ReturnValue)

