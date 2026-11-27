

   MEMBER('KSSOpen.clw')                                       ! This is a MEMBER module

   MAP
     MODULE('kernel32')
       kss_MoveFileEx(*CSTRING lpExistingFileName, *CSTRING lpNewFileName, ULONG dwFlags),BOOL,PASCAL,RAW,NAME('MoveFileExA')
     END
   END
MOVEFILE_REPLACE_EXISTING  EQUATE(1)
MOVEFILE_WRITE_THROUGH     EQUATE(8)

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
CreateRestorePoint   PROCEDURE  (FindStrOptionsGroupType pFindStrOptions, *CSTRING szFilename, BYTE pCompact=0) ! Declare Procedure
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
lQueuePointer   LONG,AUTO
lQueueRecords   LONG,AUTO
ReturnValue     LONG(Level:Benign)
sProgramName    STRING(8),AUTO
sProcVersion    STRING('03')
ulFilePointer   ULONG,AUTO
ulLength        ULONG,AUTO
szImpExFileName CSTRING(FILE:MaxFilePath),AUTO,STATIC
! format 02 (pCompact): rows are packed (strings with their used length only) into blocks of up to
! 32 KB, each written as one DOS record; a 2.1 KB fixed record per row made large lists huge.
blockUsed       LONG
rowBuffer       STRING(4096)
rowUsed         LONG
packLong        LONG
packLen         LONG
rowSource       &ResultQueueType
bWriteFailed    BOOL(FALSE)                     ! any ADD failed: the .tmp is not used
ImpExFile       FILE,DRIVER('DOS'),NAME(szImpExFileName),PRE(_DOS),CREATE,BINDABLE,THREAD
Record              RECORD,PRE()
ulRecordSize            ULONG
FileByte                BYTE,DIM(32 * 1024)
                    END
                END
oHH           &tagHTMLHelp

  CODE
    
      !IF FILEDIALOG('Save As ...',szImpExFileName,'Keystone Source Search Files (*.kss)|*.kss',FILE:Save + FILE:KeepDir + FILE:LongName)
         IF pCompact
            szImpExFileName = szFilename & '.tmp'     ! written completely, then renamed over the old file
         ELSE
            szImpExFileName = szFilename
         END
         DO ExportData
         IF pCompact
            ! Replace the previous restore point only with a completely written file, in one step
            ! (MoveFileEx); on any failure the previous file stays and the .tmp is removed.
            IF ReturnValue = Level:Benign AND bWriteFailed = FALSE
               IF NOT kss_MoveFileEx(szImpExFileName, szFilename, BOR(MOVEFILE_REPLACE_EXISTING, MOVEFILE_WRITE_THROUGH))
                  ReturnValue = Level:Notify
               END
            ELSE
               ReturnValue = Level:Notify
            END
            IF ReturnValue <> Level:Benign
               IF kcr_DeleteFile(szImpExFileName)
               END
            END
         END
      !ELSE
      !   ReturnValue = Level:User
      !END
      RETURN ReturnValue
ExportData  ROUTINE
      !Create a new file
      CREATE(ImpExFile)
      OPEN(ImpExFile,ReadWrite+DenyAll)
      IF ~ERRORCODE()
         sProgramName = 'KSS'
         _DOS:ulRecordSize = SIZE(sProgramName)
         kcr_MemCpy(ADDRESS(_DOS:FileByte),ADDRESS(sProgramName),SIZE(sProgramName))
         ADD(ImpExFile,_DOS:ulRecordSize+4)
         IF ERRORCODE() THEN bWriteFailed = TRUE.
         !Write Procedure Version
         sProcVersion = CHOOSE(pCompact <> 0, '02', '01')
         _DOS:ulRecordSize = SIZE(sProcVersion)
         kcr_MemCpy(ADDRESS(_DOS:FileByte),ADDRESS(sProcVersion),SIZE(sProcVersion))
         ADD(ImpExFile,_DOS:ulRecordSize+4)
         IF ERRORCODE() THEN bWriteFailed = TRUE.

         !Save FindStrOptions ---------------------------
         IF SIZE(FindStrOptionsGroupType) > SIZE(_DOS:Record)
            HALT('Buffer Too Small (' & SIZE(FindStrOptionsGroupType) & ')')
         END
         _DOS:ulRecordSize = SIZE(FindStrOptionsGroupType)
         kcr_MemCpy(ADDRESS(_DOS:FileByte),ADDRESS(pFindStrOptions),SIZE(FindStrOptionsGroupType))
         ADD(ImpExFile,_DOS:ulRecordSize+4)
         IF ERRORCODE() THEN bWriteFailed = TRUE.

         !Save ResultsQueue ---------------------------
         IF SIZE(ResultQueueType) > SIZE(_DOS:Record)
            HALT('Buffer Too Small (' & SIZE(ResultQueueType) & ')')
         END
         IF pCompact
            rowSource &= pFindStrOptions.ResultQueue
            DO WriteCompactQueue
            rowSource &= pFindStrOptions.UndoQueue
            DO WriteCompactQueue
            CLOSE(ImpExFile)
            IF ERRORCODE() THEN bWriteFailed = TRUE.
            EXIT
         END
         _DOS:ulRecordSize = RECORDS(pFindStrOptions.ResultQueue)
         ADD(ImpExFile,4)
         lQueueRecords = RECORDS(pFindStrOptions.ResultQueue)
         LOOP lQueuePointer = 1 TO lQueueRecords
           GET(pFindStrOptions.ResultQueue,lQueuePointer)
           _DOS:ulRecordSize = SIZE(ResultQueueType)
           kcr_MemCpy(ADDRESS(_DOS:FileByte),ADDRESS(pFindStrOptions.ResultQueue),SIZE(ResultQueueType))
           ADD(ImpExFile,_DOS:ulRecordSize+4)
         END

         !Save UndoQueue ---------------------------
         _DOS:ulRecordSize = RECORDS(pFindStrOptions.UndoQueue)
         ADD(ImpExFile,4)
         lQueueRecords = RECORDS(pFindStrOptions.UndoQueue)
         LOOP lQueuePointer = 1 TO lQueueRecords
           GET(pFindStrOptions.UndoQueue,lQueuePointer)
           _DOS:ulRecordSize = SIZE(ResultQueueType)
           kcr_MemCpy(ADDRESS(_DOS:FileByte),ADDRESS(pFindStrOptions.UndoQueue),SIZE(ResultQueueType))
           ADD(ImpExFile,_DOS:ulRecordSize+4)
         END
         !Close the file
         CLOSE(ImpExFile)
      ELSE
         MESSAGE(szImpExFileName & ' [' & ERRORCODE() & '] ' & ERROR(),'Error - Save Aborted',ICON:Hand)
         ReturnValue = Level:Notify
      END
!-----------------------------------------------------------------------------
! Format 02: row count, then blocks. Each row in a block is [LONG row length] followed by the
! strings as [LONG length][bytes] and the LONG fields, in ResultQueueType order.
WriteCompactQueue ROUTINE
      _DOS:ulRecordSize = RECORDS(rowSource)
      ADD(ImpExFile,4)
      IF ERRORCODE() THEN bWriteFailed = TRUE.
      blockUsed = 0
      LOOP lQueuePointer = 1 TO RECORDS(rowSource)
         GET(rowSource,lQueuePointer)
         rowUsed = 4                                ! room for the row length
         packLen = LEN(rowSource.Path) ; DO PackLen ; kcr_MemCpy(ADDRESS(rowBuffer)+rowUsed,ADDRESS(rowSource.Path),packLen) ; rowUsed += packLen
         packLen = LEN(rowSource.Filename) ; DO PackLen ; kcr_MemCpy(ADDRESS(rowBuffer)+rowUsed,ADDRESS(rowSource.Filename),packLen) ; rowUsed += packLen
         packLen = LEN(rowSource.szExtension) ; DO PackLen ; kcr_MemCpy(ADDRESS(rowBuffer)+rowUsed,ADDRESS(rowSource.szExtension),packLen) ; rowUsed += packLen
         packLong = rowSource.LineNo ; DO PackLong
         packLen = LEN(rowSource.ProcedureName) ; DO PackLen ; kcr_MemCpy(ADDRESS(rowBuffer)+rowUsed,ADDRESS(rowSource.ProcedureName),packLen) ; rowUsed += packLen
         packLen = LEN(rowSource.Text) ; DO PackLen ; kcr_MemCpy(ADDRESS(rowBuffer)+rowUsed,ADDRESS(rowSource.Text),packLen) ; rowUsed += packLen
         packLen = LEN(rowSource.SortName) ; DO PackLen ; kcr_MemCpy(ADDRESS(rowBuffer)+rowUsed,ADDRESS(rowSource.SortName),packLen) ; rowUsed += packLen
         packLen = LEN(rowSource.szSection) ; DO PackLen ; kcr_MemCpy(ADDRESS(rowBuffer)+rowUsed,ADDRESS(rowSource.szSection),packLen) ; rowUsed += packLen
         packLong = rowSource.Position ; DO PackLong
         packLong = rowSource.DeleteInstance ; DO PackLong
         packLong = rowSource.FileDate ; DO PackLong
         packLong = rowSource.FileTime ; DO PackLong
         kcr_MemCpy(ADDRESS(rowBuffer),ADDRESS(rowUsed),4)
         IF blockUsed + rowUsed > SIZE(_DOS:FileByte)
            DO FlushBlock
         END
         kcr_MemCpy(ADDRESS(_DOS:FileByte)+blockUsed,ADDRESS(rowBuffer),rowUsed)
         blockUsed += rowUsed
      END
      DO FlushBlock
   EXIT
PackLen ROUTINE
      kcr_MemCpy(ADDRESS(rowBuffer)+rowUsed,ADDRESS(packLen),4)
      rowUsed += 4
   EXIT
PackLong ROUTINE
      kcr_MemCpy(ADDRESS(rowBuffer)+rowUsed,ADDRESS(packLong),4)
      rowUsed += 4
   EXIT
FlushBlock ROUTINE
      IF blockUsed > 0
         _DOS:ulRecordSize = blockUsed
         ADD(ImpExFile,blockUsed+4)
         IF ERRORCODE() THEN bWriteFailed = TRUE.
         blockUsed = 0
      END
   EXIT
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
LoadRestorePoint     PROCEDURE  (FindStrOptionsGroupType pFindStrOptions,<*CSTRING szRestorePointFile>) ! Declare Procedure
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
pResultQueue    &ResultQueueType
pUndoQueue      &ResultQueueType
saveTabNumber   LONG
saveFeqProgress LONG
lQueuePointer   LONG,AUTO
lQueueRecords   LONG,AUTO
ReturnValue     LONG(Level:Benign)
sProgramName    STRING(8),AUTO
sProcVersion    STRING('03')
ulFilePointer   ULONG,AUTO
ulLength        ULONG,AUTO
blockPos        LONG
rowEnd          LONG
rowLen          LONG
fieldLen        LONG
bReadOk         BOOL
bQueueComplete  BOOL                            ! ReadCompactQueue read every row it announced
rowTarget       &ResultQueueType
tmpResults      QUEUE(ResultQueueType)          ! a format-02 file is read here first and only copied
                END                             ! into the tab when both queues are complete
tmpUndo         QUEUE(ResultQueueType)
                END
szImpExFileName CSTRING(FILE:MaxFilePath),AUTO,STATIC,THREAD
ImpExFile       FILE,DRIVER('DOS'),NAME(szImpExFileName),PRE(_DOS),CREATE,BINDABLE,THREAD
Record              RECORD,PRE()
ulRecordSize            ULONG
FileByte                BYTE,DIM(32 * 1024)
                    END
                END
oHH           &tagHTMLHelp

  CODE
    
      IF OMITTED(szRestorePointFile)
         szImpExFileName = svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS') & '\KSS_Results.rrl'
         IF FILEDIALOG('Restore From ...',szImpExFileName,'Re-loadable Result List Files|*.RRL',BOR(FILE:KeepDir,FILE:LongName))
            DO ImportData
         ELSE
            ReturnValue = Level:User
         END
      ELSE
         szImpExFileName = szRestorePointFile
         DO ImportData
      END
      RETURN ReturnValue
ImportData  ROUTINE
      OPEN(ImpExFile,ReadWrite+DenyAll)
      IF ~ERRORCODE()
         !Read Program Name Length
         ulFilePointer = 1
         GET(ImpExFile,ulFilePointer,4)
         ulFilePointer += 4
         ulLength = _DOS:ulRecordSize
         sProgramName = ''
         IF ulLength <= SIZE(sProgramName)            ! lengths come from the file: never copy past the destination
            GET(ImpExFile,ulFilePointer,ulLength)
            ulFilePointer += ulLength
            kcr_MemCpy(ADDRESS(sProgramName),ADDRESS(_DOS:Record),ulLength)
         END
         IF sProgramName = 'KSS'
            GET(ImpExFile,ulFilePointer,4)
            IF _DOS:ulRecordSize = 2
               ulFilePointer += 4
               ulLength = _DOS:ulRecordSize
               GET(ImpExFile,ulFilePointer,ulLength)
               ulFilePointer += ulLength
               kcr_MemCpy(ADDRESS(sProcVersion),ADDRESS(_DOS:Record),ulLength)
               IF sProcVersion <> '01'
                  !wrong verson
               END
            END

            !Load FindStrOptions ---------------------------
            pResultQueue &= pFindStrOptions.ResultQueue
            pUndoQueue &= pFindStrOptions.UndoQueue
            saveTabNumber = pFindStrOptions.tabNumber
            saveFeqProgress = pFindStrOptions.feqSearchProgress

            GET(ImpExFile,ulFilePointer,4)
            ulFilePointer += 4
            ulLength = _DOS:ulRecordSize
            IF ulLength > SIZE(FindStrOptionsGroupType)
               MESSAGE(szImpExFileName & ' Invalid File Type','Error - Load Aborted',ICON:Hand)
               ReturnValue = Level:Notify
               CLOSE(ImpExFile)
               EXIT
            END
            GET(ImpExFile,ulFilePointer,ulLength)
            IF ~ERRORCODE()
               ulFilePointer += ulLength
               kcr_MemCpy(ADDRESS(pFindStrOptions),ADDRESS(_DOS:Record),ulLength)
               pFindStrOptions.ResultQueue &= pResultQueue
               pFindStrOptions.UndoQueue &= pUndoQueue
               pFindStrOptions.tabNumber = saveTabNumber
               pFindStrOptions.feqSearchProgress = saveFeqProgress
            ELSE
               !big problems
            END


            IF sProcVersion = '02'                    ! compact restore point
               rowTarget &= tmpResults
               DO ReadCompactQueue
               IF bQueueComplete = TRUE
                  rowTarget &= tmpUndo
                  DO ReadCompactQueue
               END
               IF bQueueComplete = TRUE
                  ! copy in order (a DELETE from the front of the temporary queue per row would shift it every time)
                  FREE(pFindStrOptions.ResultQueue)
                  LOOP lQueuePointer = 1 TO RECORDS(tmpResults)
                     GET(tmpResults,lQueuePointer)
                     pFindStrOptions.ResultQueue = tmpResults
                     ADD(pFindStrOptions.ResultQueue)
                     IF ERRORCODE() THEN bQueueComplete = FALSE ; BREAK.
                  END
                  FREE(tmpResults)
                  FREE(pFindStrOptions.UndoQueue)
                  LOOP lQueuePointer = 1 TO RECORDS(tmpUndo)
                     IF bQueueComplete = FALSE THEN BREAK.
                     GET(tmpUndo,lQueuePointer)
                     pFindStrOptions.UndoQueue = tmpUndo
                     ADD(pFindStrOptions.UndoQueue)
                     IF ERRORCODE() THEN bQueueComplete = FALSE ; BREAK.
                  END
                  IF bQueueComplete = FALSE                 ! out of memory while copying: no partial results
                     FREE(pFindStrOptions.ResultQueue)
                     FREE(pFindStrOptions.UndoQueue)
                     MESSAGE('There was not enough memory to load ' & szImpExFileName & '.','Error - Load Aborted',ICON:Hand)
                     ReturnValue = Level:Notify
                  END
               ELSE
                  MESSAGE(szImpExFileName & ' is damaged or incomplete and was not loaded.','Error - Load Aborted',ICON:Hand)
                  ReturnValue = Level:Notify
               END
               FREE(tmpResults)
               FREE(tmpUndo)
               CLOSE(ImpExFile)
               EXIT
            END
            !Load ResultQueue ---------------------------
            GET(ImpExFile,ulFilePointer,4)
            IF ~ERRORCODE()
               ulFilePointer += 4
               kcr_MemCpy(ADDRESS(lQueueRecords),ADDRESS(_DOS:Record),4)
               FREE(pFindStrOptions.ResultQueue)
               LOOP lQueuePointer = 1 TO lQueueRecords
                 GET(ImpExFile,ulFilePointer,4)
                 IF ~ERRORCODE()
                    ulFilePointer += 4
                    ulLength = _DOS:ulRecordSize
                    IF ulLength > SIZE(ResultQueueType) THEN BREAK.
                    GET(ImpExFile,ulFilePointer,ulLength)
                    IF ~ERRORCODE()
                       ulFilePointer += ulLength
                       kcr_MemCpy(ADDRESS(pFindStrOptions.ResultQueue),ADDRESS(_DOS:Record),ulLength)
                    END
                 END
                 IF ~ERRORCODE()
                    ADD(pFindStrOptions.ResultQueue)
                 ELSE
                    BREAK
                 END
               END
            END

            !Load UndoQueue ---------------------------
            GET(ImpExFile,ulFilePointer,4)
            IF ~ERRORCODE()
               ulFilePointer += 4
               kcr_MemCpy(ADDRESS(lQueueRecords),ADDRESS(_DOS:Record),4)
               FREE(pFindStrOptions.UndoQueue)
               LOOP lQueuePointer = 1 TO lQueueRecords
                 GET(ImpExFile,ulFilePointer,4)
                 IF ~ERRORCODE()
                    ulFilePointer += 4
                    ulLength = _DOS:ulRecordSize
                    IF ulLength > SIZE(ResultQueueType) THEN BREAK.
                    GET(ImpExFile,ulFilePointer,ulLength)
                    IF ~ERRORCODE()
                       ulFilePointer += ulLength
                       kcr_MemCpy(ADDRESS(pFindStrOptions.UndoQueue),ADDRESS(_DOS:Record),ulLength)
                    END
                 END
                 IF ~ERRORCODE()
                    ADD(pFindStrOptions.UndoQueue)
                 ELSE
                    BREAK
                 END
               END
            END
         ELSE
            MESSAGE(szImpExFileName & ' Invalid File Type','Error - Load Aborted',ICON:Hand)
            ReturnValue = Level:Notify
         END

         !Close the file
         CLOSE(ImpExFile)
      ELSE
         MESSAGE(szImpExFileName & ' [' & ERRORCODE() & '] ' & ERROR(),'Error - Load Aborted',ICON:Hand)
         ReturnValue = Level:Notify
      END
!-----------------------------------------------------------------------------
! Reads a format-02 queue written by WriteCompactQueue (CreateRestorePoint). Lengths are checked
! against the block and the field sizes, so a damaged file cannot write past a field.
ReadCompactQueue ROUTINE
      FREE(rowTarget)
      bQueueComplete = FALSE
      GET(ImpExFile,ulFilePointer,4)
      IF ERRORCODE() THEN EXIT.
      ulFilePointer += 4
      kcr_MemCpy(ADDRESS(lQueueRecords),ADDRESS(_DOS:Record),4)
      lQueuePointer = 0
      LOOP WHILE lQueuePointer < lQueueRecords
         GET(ImpExFile,ulFilePointer,4)             ! block length
         IF ERRORCODE() THEN BREAK.
         ulFilePointer += 4
         ulLength = _DOS:ulRecordSize
         IF ulLength = 0 OR ulLength > SIZE(_DOS:Record) THEN BREAK.
         GET(ImpExFile,ulFilePointer,ulLength)
         IF ERRORCODE() THEN BREAK.
         ulFilePointer += ulLength
         blockPos = 0
         LOOP WHILE blockPos + 4 <= ulLength AND lQueuePointer < lQueueRecords
            kcr_MemCpy(ADDRESS(rowLen),ADDRESS(_DOS:Record)+blockPos,4)
            IF rowLen < 4 OR blockPos + rowLen > ulLength THEN BREAK.
            rowEnd = blockPos + rowLen
            blockPos += 4
            CLEAR(rowTarget)
            bReadOk = TRUE
            fieldLen = SIZE(rowTarget.Path) ; DO UnpackString ; IF bReadOk THEN kcr_MemCpy(ADDRESS(rowTarget.Path),ADDRESS(_DOS:Record)+blockPos-fieldLen,fieldLen) ; rowTarget.Path[fieldLen+1] = '<0>'.
            fieldLen = SIZE(rowTarget.Filename) ; DO UnpackString ; IF bReadOk THEN kcr_MemCpy(ADDRESS(rowTarget.Filename),ADDRESS(_DOS:Record)+blockPos-fieldLen,fieldLen) ; rowTarget.Filename[fieldLen+1] = '<0>'.
            fieldLen = SIZE(rowTarget.szExtension) ; DO UnpackString ; IF bReadOk THEN kcr_MemCpy(ADDRESS(rowTarget.szExtension),ADDRESS(_DOS:Record)+blockPos-fieldLen,fieldLen) ; rowTarget.szExtension[fieldLen+1] = '<0>'.
            DO UnpackLong ; rowTarget.LineNo = fieldLen
            fieldLen = SIZE(rowTarget.ProcedureName) ; DO UnpackString ; IF bReadOk THEN kcr_MemCpy(ADDRESS(rowTarget.ProcedureName),ADDRESS(_DOS:Record)+blockPos-fieldLen,fieldLen) ; rowTarget.ProcedureName[fieldLen+1] = '<0>'.
            fieldLen = SIZE(rowTarget.Text) ; DO UnpackString ; IF bReadOk THEN kcr_MemCpy(ADDRESS(rowTarget.Text),ADDRESS(_DOS:Record)+blockPos-fieldLen,fieldLen) ; rowTarget.Text[fieldLen+1] = '<0>'.
            fieldLen = SIZE(rowTarget.SortName) ; DO UnpackString ; IF bReadOk THEN kcr_MemCpy(ADDRESS(rowTarget.SortName),ADDRESS(_DOS:Record)+blockPos-fieldLen,fieldLen) ; rowTarget.SortName[fieldLen+1] = '<0>'.
            fieldLen = SIZE(rowTarget.szSection) ; DO UnpackString ; IF bReadOk THEN kcr_MemCpy(ADDRESS(rowTarget.szSection),ADDRESS(_DOS:Record)+blockPos-fieldLen,fieldLen) ; rowTarget.szSection[fieldLen+1] = '<0>'.
            DO UnpackLong ; rowTarget.Position = fieldLen
            DO UnpackLong ; rowTarget.DeleteInstance = fieldLen
            DO UnpackLong ; rowTarget.FileDate = fieldLen
            DO UnpackLong ; rowTarget.FileTime = fieldLen
            IF bReadOk = FALSE OR blockPos <> rowEnd THEN BREAK.   ! damaged row: stop here
            ADD(rowTarget)
            IF ERRORCODE() THEN BREAK.                ! out of memory: the queue reads as incomplete
            lQueuePointer += 1
         END
         IF blockPos < ulLength AND lQueuePointer < lQueueRecords THEN BREAK.   ! damaged block
      END
      bQueueComplete = CHOOSE(lQueuePointer = lQueueRecords AND lQueueRecords >= 0)
   EXIT
! In: fieldLen = SIZE of the CSTRING field. Out: fieldLen = stored length, blockPos past the bytes.
UnpackString ROUTINE
   DATA
maxLen   LONG
   CODE
      maxLen = fieldLen - 1
      IF bReadOk = FALSE OR blockPos + 4 > rowEnd
         bReadOk = FALSE
         EXIT
      END
      kcr_MemCpy(ADDRESS(fieldLen),ADDRESS(_DOS:Record)+blockPos,4)
      blockPos += 4
      IF fieldLen < 0 OR fieldLen > maxLen OR blockPos + fieldLen > rowEnd
         bReadOk = FALSE
         EXIT
      END
      blockPos += fieldLen
   EXIT
UnpackLong ROUTINE
      IF bReadOk = FALSE OR blockPos + 4 > rowEnd
         bReadOk = FALSE
         fieldLen = 0
         EXIT
      END
      kcr_MemCpy(ADDRESS(fieldLen),ADDRESS(_DOS:Record)+blockPos,4)
      blockPos += 4
   EXIT