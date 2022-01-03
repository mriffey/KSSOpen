

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
CreateRestorePoint   PROCEDURE  (FindStrOptionsGroupType pFindStrOptions, *CSTRING szFilename) ! Declare Procedure
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
ImpExFile       FILE,DRIVER('DOS'),NAME(szImpExFileName),PRE(_DOS),CREATE,BINDABLE,THREAD
Record              RECORD,PRE()
ulRecordSize            ULONG
FileByte                BYTE,DIM(32 * 1024)
                    END
                END
oHH           &tagHTMLHelp

  CODE
    
      !IF FILEDIALOG('Save As ...',szImpExFileName,'Keystone Source Search Files (*.kss)|*.kss',FILE:Save + FILE:KeepDir + FILE:LongName)
         szImpExFileName = szFilename
         DO ExportData
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
         !Write Procedure Version
         sProcVersion = '01'
         _DOS:ulRecordSize = SIZE(sProcVersion)
         kcr_MemCpy(ADDRESS(_DOS:FileByte),ADDRESS(sProcVersion),SIZE(sProcVersion))
         ADD(ImpExFile,_DOS:ulRecordSize+4)

         !Save FindStrOptions ---------------------------
         IF SIZE(FindStrOptionsGroupType) > SIZE(_DOS:Record)
            HALT('Buffer Too Small (' & SIZE(FindStrOptionsGroupType) & ')')
         END
         _DOS:ulRecordSize = SIZE(FindStrOptionsGroupType)
         kcr_MemCpy(ADDRESS(_DOS:FileByte),ADDRESS(pFindStrOptions),SIZE(FindStrOptionsGroupType))
         ADD(ImpExFile,_DOS:ulRecordSize+4)

         !Save ResultsQueue ---------------------------
         IF SIZE(ResultQueueType) > SIZE(_DOS:Record)
            HALT('Buffer Too Small (' & SIZE(ResultQueueType) & ')')
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
         GET(ImpExFile,ulFilePointer,ulLength)
         ulFilePointer += ulLength
         kcr_MemCpy(ADDRESS(sProgramName),ADDRESS(_DOS:Record),ulLength)
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
