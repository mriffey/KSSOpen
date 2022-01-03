

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
SaveResults PROCEDURE (FindStrOptionsGroupType FindStrOptions, *CSTRING szSendToFilename)

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
Window               WINDOW('Save Results'),AT(,,470,168),FONT('Segoe UI',10),CENTER,GRAY,HLP('SaveResults.htm'), |
  SYSTEM
                       PANEL,AT(5,5,460,140),USE(?PANEL1),BEVEL(1)
                       PROMPT('Save to'),AT(10,10),USE(?SaveTo:Prompt)
                       OPTION('Save To'),AT(72,10,223,10),USE(SaveTo)
                         RADIO(' &Clipboard'),AT(75,10),USE(?SaveToClipboard),TRN,VALUE('0')
                         RADIO(' &Text File'),AT(131,10),USE(?SaveToTextFile),TRN,VALUE('1')
                         RADIO(' &Re-loadable Result List'),AT(182,10),USE(?SaveToRestorePoint),TRN,VALUE('2')
                       END
                       PROMPT('&Filename'),AT(10,25),USE(?Filename:Prompt)
                       ENTRY(@s255),AT(72,22,354,10),USE(szTextFilename)
                       BUTTON('...'),AT(431,21,14,11),USE(?LookupFile:2)
                       PROMPT('Column Delimiter'),AT(10,40),USE(?ColumnDelimiter:Prompt)
                       OPTION('Delimiter'),AT(72,40,142,10),USE(ColumnDelimiter)
                         RADIO(' Co&mma'),AT(75,40),USE(?CommaFormat),TIP('Comma delimited columns'),TRN,VALUE('0')
                         RADIO(' T&ab'),AT(130,40),USE(?TabFormat),TIP('Tab delimited columns'),TRN,VALUE('1')
                         RADIO('Co&lumn'),AT(169,40),USE(?ColumnFormat),TIP('fixed width columns'),TRN,VALUE('2')
                       END
                       CHECK(' &Quote Strings'),AT(10,55),USE(bQuoteStrings),TIP('Places double quotation mark' & |
  's around strings when checked')
                       CHECK(' Save Filename'),AT(10,70),USE(bSaveFilename)
                       PROMPT('Show File as'),AT(10,85),USE(?FileNameFormat:Prompt),DISABLE
                       OPTION('Formatting'),AT(72,85,388,10),USE(FormatOption),DISABLE
                         RADIO(' F&ullpath'),AT(75,85),USE(?FullPathFormat),TRN,VALUE('0')
                         RADIO(' Fol&der|Filename'),AT(126,85),USE(?FolderFilenameFormat),TRN,VALUE('1')
                         RADIO(' Fold&er|Basename|Extension'),AT(201,85),USE(?BasenameExtensionFormat),TRN,VALUE('2')
                         RADIO(' File&name'),AT(315,85),USE(?FilenameFormat),TRN,VALUE('3')
                         RADIO(' &Basename|Extension'),AT(369,85),USE(?BasenameExtensionFormat:2),TRN,VALUE('4')
                       END
                       CHECK(' Save Line Number'),AT(10,100),USE(bSaveLineNumber)
                       CHECK(' Save Location'),AT(10,115),USE(bSaveLocation)
                       CHECK(' Save Text'),AT(10,130),USE(bSaveText)
                       CHECK(' Send To command after save'),AT(246,152),USE(bSendToAfterSave)
                       BUTTON('&Save'),AT(370,150,45,14),USE(?cmdSave)
                       BUTTON('&Cancel'),AT(419,150,45,14),USE(?cmdCancel)
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

      IF SaveTo <> SaveToRestorePoint
         IF bQuoteStrings = TRUE
            Quote = '"'
         ELSE
            Quote = ''
         END

         NumberOfColumns = 0
         IF bSaveFilename = TRUE
            CASE FormatOption
              OF FullPathFormat
                 NumberOfColumns += 1  ! = 4
              OF FolderFilenameFormat
                 NumberOfColumns += 2  ! = 5
              OF FolderBasenameExtensionFormat
                 NumberOfColumns += 3  ! = 6
              OF FilenameFormat
                 NumberOfColumns += 1  ! = 4
              OF BasenameExtensionFormat
                 NumberOfColumns += 2  ! = 5
            END
         END
         IF bSaveLineNumber = TRUE
            NumberOfColumns += 1
         END
         IF bSaveLocation = TRUE
            NumberOfColumns += 1
         END
         IF bSaveText = TRUE
            NumberOfColumns += 1
         END

         CASE ColumnDelimiter
           OF CommaDelimited
              Delimiter = ','
              ExtaCharacters = (((LEN(Quote) * 2) * NumberOfColumns) + (NumberOfColumns - 1)) + 2
              j = RECORDS(ResultQueue)
              nTextLength = (j * (SIZE(ResultQueue)+ExtaCharacters)) + 1

           OF TabDelimited
              Delimiter = '<9>'
              ExtaCharacters = (((LEN(Quote) * 2) * NumberOfColumns) + (NumberOfColumns - 1)) + 2
              j = RECORDS(ResultQueue)
              nTextLength = (j * (SIZE(ResultQueue)+ExtaCharacters)) + 1

           OF ColumnDelimited
              Delimiter = ' '

              DO DetermineMaxColumnWidths

              TotalColumnWidth = 0
              IF bSaveFilename = TRUE
                 CASE FormatOption
                   OF FullPathFormat
                      TotalColumnWidth += arMaxWidth[1]  !Formatted Filename
                   OF FolderFilenameFormat
                      TotalColumnWidth += arMaxWidth[2]  !Path
                      TotalColumnWidth += arMaxWidth[1]  !Formatted Filename
                   OF FolderBasenameExtensionFormat
                      TotalColumnWidth += arMaxWidth[2]  !Path
                      TotalColumnWidth += arMaxWidth[3]  !Filename
                      TotalColumnWidth += arMaxWidth[4]  !Extension
                   OF FilenameFormat
                      TotalColumnWidth += arMaxWidth[1]  !Formatted Filename
                   OF BasenameExtensionFormat
                      TotalColumnWidth += arMaxWidth[3]  !Filename
                      TotalColumnWidth += arMaxWidth[4]  !Extension
                 END
              END
              IF bSaveLineNumber = TRUE
                 TotalColumnWidth += arMaxWidth[5]
              END
              IF bSaveLocation = TRUE
                 TotalColumnWidth += arMaxWidth[6]
              END
              IF bSaveText = TRUE
                 TotalColumnWidth += arMaxWidth[7]
              END

              !LOOP j = 1 TO NumberOfColumns
              !   TotalColumnWidth += arMaxWidth[j]
              !END
              TotalColumnWidth += ((LEN(Quote) * 2) * NumberOfColumns) + 2
              j = RECORDS(ResultQueue)
              nTextLength = (j * TotalColumnWidth) + 1
         END

         !ASSERT(0,eqDBG & 'NEW szClipboardText [' & ADDRESS(szClipboardText) & ']')
         szClipboardText &= NEW CSTRING(nTextLength)

         LOOP i = 1 TO j
            GET(ResultQueue,i)
            DO FormatFilename
            szLineNo = CLIP(LEFT(FORMAT(ResultQueue.LineNo,@N_10B)))

            buffSizeNeeded = DoubleQuote(ResultQueue.Text,quotedText,0,quote)
            !ASSERT(0,eqDBG & 'NEW quotedText [' & ADDRESS(quotedText) & ']')
            quotedText &= NEW CSTRING(buffSizeNeeded+1)
            DoubleQuote(ResultQueue.Text,quotedText,SIZE(quotedText),quote)

            IF bSaveFilename = TRUE
               CASE FormatOption
                 OF FullPathFormat
                    szClipboardText = szClipboardText & |
                                      Quote & FormattedFilename & |
                                      CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[1] - LEN(FormattedFilename)),'') & |
                                      Quote
                 OF FolderFilenameFormat
                    szClipboardText = szClipboardText & |
                                      Quote & ResultQueue.Path & |
                                      CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[2] - LEN(ResultQueue.Path)),'') & |
                                      Quote & Delimiter & |
                                      Quote & FormattedFilename & |
                                      CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[1] - LEN(FormattedFilename)),'') & |
                                      Quote
                 OF FolderBasenameExtensionFormat
                    szClipboardText = szClipboardText & |
                                      Quote & ResultQueue.Path & |
                                      CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[2] - LEN(ResultQueue.Path)),'') & |
                                      Quote & Delimiter & |
                                      Quote & ResultQueue.Filename & |
                                      CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[3] - LEN(ResultQueue.Filename)),'') & |
                                      Quote & Delimiter & |
                                      Quote & ResultQueue.szExtension & |
                                      CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[4] - LEN(ResultQueue.szExtension)),'') & |
                                      Quote
                 OF FilenameFormat
                    szClipboardText = szClipboardText & |
                                      Quote & FormattedFilename & |
                                      CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[1] - LEN(FormattedFilename)),'') & |
                                      Quote
                 OF BasenameExtensionFormat
                    szClipboardText = szClipboardText & |
                                      Quote & ResultQueue.Filename & |
                                      CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[3] - LEN(ResultQueue.Filename)),'') & |
                                      Quote & Delimiter & |
                                      Quote & ResultQueue.szExtension & |
                                      CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[4] - LEN(ResultQueue.szExtension)),'') & |
                                      Quote
               END
            END

            IF bSaveLineNumber = TRUE
               IF szClipboardText
                  szClipboardText = szClipboardText & Delimiter
               END
               szClipboardText = szClipboardText & |
                                 Quote & |
                                 CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[5] - LEN(szLineNo)),'') & |
                                 szLineNo & Quote
            END

            IF bSaveLocation = TRUE
               IF szClipboardText
                  szClipboardText = szClipboardText & Delimiter
               END
               szClipboardText = szClipboardText & |
                                 Quote & |
                                 CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[6] - LEN(ResultQueue.ProcedureName)),'') & |
                                 ResultQueue.ProcedureName & Quote
            END

            IF bSaveText = TRUE
               IF szClipboardText
                  szClipboardText = szClipboardText & Delimiter
               END
               szClipboardText = szClipboardText & |
                                 Quote & quotedText & Quote
            END
            szClipboardText = szClipboardText & crlf

            !!!Region Omitted Code
      OMIT('***OLD_CODE***')
            CASE FormatOption
              OF FullPathFormat
                 szClipboardText = szClipboardText & |
                                   Quote & FormattedFilename & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[1] - LEN(FormattedFilename)),'') & |
                                   Quote & Delimiter & |
                                   Quote & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[2] - LEN(szLineNo)),'') & |
                                   szLineNo & Quote & Delimiter & |
                                   Quote & ResultQueue.ProcedureName & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[3] - LEN(ResultQueue.ProcedureName)),'') & |
                                   Quote & Delimiter & |
                                   Quote & quotedText & Quote & crlf

              OF FolderFilenameFormat
                 szClipboardText = szClipboardText & |
                                   Quote & ResultQueue.Path & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[1] - LEN(ResultQueue.Path)),'') & |
                                   Quote & Delimiter & |
                                   Quote & FormattedFilename & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[2] - LEN(FormattedFilename)),'') & |
                                   Quote & Delimiter & |
                                   Quote & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[3] - LEN(szLineNo)),'') & |
                                   szLineNo & Quote & Delimiter & |
                                   Quote & ResultQueue.ProcedureName & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[4] - LEN(ResultQueue.ProcedureName)),'') & |
                                   Quote & Delimiter & |
                                   Quote & quotedText & Quote & crlf

              OF FolderBasenameExtensionFormat
                 szClipboardText = szClipboardText & |
                                   Quote & ResultQueue.Path & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[1] - LEN(ResultQueue.Path)),'') & |
                                   Quote & Delimiter & |
                                   Quote & ResultQueue.Filename & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[2] - LEN(ResultQueue.Filename)),'') & |
                                   Quote & Delimiter & |
                                   Quote & ResultQueue.szExtension & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[3] - LEN(ResultQueue.szExtension)),'') & |
                                   Quote & Delimiter & |
                                   Quote & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[4] - LEN(szLineNo)),'') & |
                                   szLineNo & Quote & Delimiter & |
                                   Quote & ResultQueue.ProcedureName & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[5] - LEN(ResultQueue.ProcedureName)),'') & |
                                   Quote & Delimiter & |
                                   Quote & quotedText & Quote & crlf

              OF FilenameFormat
                 szClipboardText = szClipboardText & |
                                   Quote & FormattedFilename & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[1] - LEN(FormattedFilename)),'') & |
                                   Quote & Delimiter & |
                                   Quote & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[2] - LEN(szLineNo)),'') & |
                                   szLineNo & Quote & Delimiter & |
                                   Quote & ResultQueue.ProcedureName & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[3] - LEN(ResultQueue.ProcedureName)),'') & |
                                   Quote & Delimiter & |
                                   Quote & quotedText & Quote & crlf

              OF BasenameExtensionFormat
                 szClipboardText = szClipboardText & |
                                   Quote & ResultQueue.Filename & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[1] - LEN(ResultQueue.Filename)),'') & |
                                   Quote & Delimiter & |
                                   Quote & ResultQueue.szExtension & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[2] - LEN(ResultQueue.szExtension)),'') & |
                                   Quote & Delimiter & |
                                   Quote & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[3] - LEN(szLineNo)),'') & |
                                   szLineNo & Quote & Delimiter & |
                                   Quote & ResultQueue.ProcedureName & |
                                   CHOOSE(ColumnDelimiter = ColumnDelimited, ALL(' ',arMaxWidth[4] - LEN(ResultQueue.ProcedureName)),'') & |
                                   Quote & Delimiter & |
                                   Quote & quotedText & Quote & crlf
            END
      !***OLD_CODE***
            !!!EndRegion

            !ASSERT(0,eqDBG & 'DISPOSE quotedText [' & ADDRESS(quotedText) & ']')
            DISPOSE(quotedText)
            quotedText &= NULL

         END
      END

      CASE SaveTo
        OF SaveToClipboard
           SETCLIPBOARD(szClipboardText)
        OF SaveToTextFile
           DosFilename = szTextFilename
           CREATE(DosFile)
           IF NOT ERRORCODE()
              !SEND(DosFile,'FILEBUFFERS=' & nTextLength)
              nTextLength = LEN(szClipboardText)
              OPEN(DosFile)
              IF NOT ERRORCODE()
                 IF nTextLength <= DosBufferSize
                    DosFile.Record = szClipboardText
                    ADD(DosFile,nTextLength)
                 ELSE
                    LOOP WHILE ((nTextLength - pBuffer) + 1) > DosBufferSize
                       pBuffer = 1
                       DosFile.Record = szClipboardText[pBuffer : DosBufferSize-1]
                       ADD(DosFile)
                       pBuffer += DosBufferSize
                    END
                    IF pBuffer < nTextLength
                       DosFile.Record = szClipboardText[pBuffer : nTextLength]
                       ADD(DosFile)
                    END
                 END
                 CLOSE(DosFile)
                 IF bSendToAfterSave = TRUE
                    szSendToFilename = szTextFilename
                    bRetVal = TRUE
                 ELSE
                    bRetVal = FALSE
                 END
              ELSE
              END
           ELSE
           END
        OF SaveToRestorePoint
           CreateRestorePoint(FindStrOptions,szTextFilename)
      END

      !ASSERT(0,eqDBG & 'DISPOSE szClipboardText [' & ADDRESS(szClipboardText) & ']')
      DISPOSE(szClipboardText)
      szClipboardText &= NULL
FormatFilename ROUTINE
      CASE FormatOption
        OF FullPathFormat
           FormattedFilename = ResultQueue.Path & ResultQueue.Filename & ResultQueue.szExtension
        OF FolderFilenameFormat
           FormattedFilename = ResultQueue.Filename & ResultQueue.szExtension
        OF FolderBasenameExtensionFormat
           FormattedFilename = ''
        OF FilenameFormat
           FormattedFilename = ResultQueue.Filename & ResultQueue.szExtension
        OF BasenameExtensionFormat
           FormattedFilename = ''
      END
DetermineMaxColumnWidths   ROUTINE
   DATA
RowIndx     LONG
LastRow     LONG
ColumnIndx  LONG
LastColumn  LONG(7)
MaxWidth    &LONG
thisColumn  ANY
szLineNo    CSTRING(12)
nullString  CSTRING('')

   CODE

      LastRow = RECORDS(ResultQueue)
      LOOP RowIndx = 1 TO LastRow
         GET(ResultQueue,RowIndx)
         DO FormatFilename
         szLineNo = CLIP(LEFT(FORMAT(ResultQueue.LineNo,@N_10B)))


         LOOP ColumnIndx = 1 TO LastColumn
            MaxWidth &= arMaxWidth[ColumnIndx]
            EXECUTE ColumnIndx
               thisColumn &= FormattedFilename
               thisColumn &= ResultQueue.Path
               thisColumn &= ResultQueue.Filename
               thisColumn &= ResultQueue.szExtension
               thisColumn &= szLineNo
               thisColumn &= ResultQueue.ProcedureName
               thisColumn &= ResultQueue.Text
            END
            IF LEN(thisColumn) > MaxWidth
               MaxWidth = LEN(thisColumn)
            END
         END

         !!!Region Omitted Code
      OMIT('***OLD_CODE***')
         LOOP ColumnIndx = 1 TO NumberOfColumns
            MaxWidth &= arMaxWidth[ColumnIndx]
            CASE ColumnIndx
              OF 1
                 CASE FormatOption
                   OF FullPathFormat
                      thisColumn &= FormattedFilename
                   OF FolderFilenameFormat
                      thisColumn &= ResultQueue.Path
                   OF FolderBasenameExtensionFormat
                      thisColumn &= ResultQueue.Path
                   OF FilenameFormat
                      thisColumn &= FormattedFilename
                   OF BasenameExtensionFormat
                      thisColumn &= ResultQueue.Filename
                 END
              OF 2
                 CASE FormatOption
                   OF FullPathFormat
                      thisColumn &= szLineNo
                   OF FolderFilenameFormat
                      thisColumn &= FormattedFilename
                   OF FolderBasenameExtensionFormat
                      thisColumn &= ResultQueue.Filename
                   OF FilenameFormat
                      thisColumn &= szLineNo
                   OF BasenameExtensionFormat
                      thisColumn &= ResultQueue.szExtension
                 END
              OF 3
                 CASE FormatOption
                   OF FullPathFormat
                      thisColumn &= ResultQueue.ProcedureName
                   OF FolderFilenameFormat
                      thisColumn &= szLineNo
                   OF FolderBasenameExtensionFormat
                      thisColumn &= ResultQueue.szExtension
                   OF FilenameFormat
                      thisColumn &= ResultQueue.ProcedureName
                   OF BasenameExtensionFormat
                      thisColumn &= szLineNo
                 END
              OF 4
                 CASE FormatOption
                   OF FullPathFormat
                      thisColumn &= ResultQueue.Text
                   OF FolderFilenameFormat
                      thisColumn &= ResultQueue.ProcedureName
                   OF FolderBasenameExtensionFormat
                      thisColumn &= szLineNo
                   OF FilenameFormat
                      thisColumn &= ResultQueue.Text
                   OF BasenameExtensionFormat
                      thisColumn &= ResultQueue.ProcedureName
                 END
              OF 5
                 CASE FormatOption
                   OF FullPathFormat
                      thisColumn &= nullString
                   OF FolderFilenameFormat
                      thisColumn &= ResultQueue.Text
                   OF FolderBasenameExtensionFormat
                      thisColumn &= ResultQueue.ProcedureName
                   OF FilenameFormat
                      thisColumn &= nullString
                   OF BasenameExtensionFormat
                      thisColumn &= ResultQueue.Text
                 END
              OF 6
                 CASE FormatOption
                   OF FullPathFormat
                      thisColumn &= nullString
                   OF FolderFilenameFormat
                      thisColumn &= nullString
                   OF FolderBasenameExtensionFormat
                      thisColumn &= ResultQueue.Text
                   OF FilenameFormat
                      thisColumn &= nullString
                   OF BasenameExtensionFormat
                      thisColumn &= nullString
                 END
            END
            IF LEN(thisColumn) > MaxWidth
               MaxWidth = LEN(thisColumn)
            END
         END
      !***OLD_CODE***
         !!!EndRegion

      END
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
  POST(EVENT:Accepted,?SaveTo)
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
    OF ?SaveTo
      CASE SaveTo
        OF SaveToClipboard
           bSendToAfterSave = FALSE
           DISPLAY(?bSendToAfterSave)
           szTextFilename = ''
           DISPLAY(?szTextFilename)
           DISABLE(?Filename:Prompt,?LookupFile:2)
           ENABLE(?ColumnDelimiter:Prompt,?bSaveText)
           IF bSaveFilename = FALSE
              DISABLE(?FileNameFormat:Prompt,?FormatOption)
           END
           DISABLE(?bSendToAfterSave)
      
        OF SaveToTextFile
           ENABLE(?ColumnDelimiter:Prompt,?bSendToAfterSave)
           IF bSaveFilename = FALSE
              DISABLE(?FileNameFormat:Prompt,?FormatOption)
           END
           IF szTextFilename = ''
              INIMgr.Fetch('SaveResults','szTextFilename',szTextFilename)
           END
           IF szTextFilename <> ''
              IF UPPER(szTextFilename[LEN(szTextFilename)-3 : LEN(szTextFilename)]) = '.RRL'
                 szTextFilename = szTextFilename[1 : LEN(szTextFilename)-3] & 'txt'
                 DISPLAY(?szTextFilename)
              END
           END
           ENABLE(?Filename:Prompt,?LookupFile:2)
      
        OF SaveToRestorePoint
           bSaveFilename = TRUE
           bSaveLineNumber = TRUE
           bSaveLocation = TRUE
           bSaveText = TRUE
           bSendToAfterSave = FALSE
           FormatOption = FolderBasenameExtensionFormat
           DISPLAY(?bSaveFilename,?bSendToAfterSave)
           DISABLE(?ColumnDelimiter:Prompt,?bSendToAfterSave)
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
    OF ?bSaveFilename
      IF bSaveFilename = TRUE
         ENABLE(?FileNameFormat:Prompt,?FormatOption)
      ELSE
         DISABLE(?FileNameFormat:Prompt,?FormatOption)
      END
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
      IF SaveTo = SaveToClipboard
         DO HandleSave
         POST(EVENT:CloseWindow)
      ELSIF szTextFilename = ''
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

