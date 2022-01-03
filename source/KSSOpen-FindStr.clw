

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
FindStr              PROCEDURE  (STRING strOptionsGroup)   ! Declare Procedure
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
oXML         xFileXML 
oST          StringTheory 
intLoop      LONG 

qOpenedFiles QUEUE
filegr         STRING(1),NAME('file') 
OFFileName     STRING(256),NAME('name')
             END 
!samples for testing
!f:\
!f:\softvelocity
!f:\softvelocity\
!f:\softvelocity\\
!'f:\
!'f:\softvelocity'
!'f:\softvelocity\'
!'f:\softvelocity\\'
oHH           &tagHTMLHelp
saAttr               LIKE(SECURITY_ATTRIBUTES)
SavePath             CSTRING(261)
szCmdLine            CSTRING(261)
ResultQueue          &ResultQueueType
lpResultQueue        LONG
bExcludeComments     BYTE
bMatchMode           LONG
FindStrOptions       GROUP(FindStrOptionsGroupType)
                     END
szFixedFileMask      LIKE(FindStrOptions.szFileMask)
szFixedExcludeMask   LIKE(FindStrOptions.szExcludeMask)

i                    LONG
j                    LONG
k                    LONG
pStringStart         LONG
pStringEnd           LONG
pCommentMarker       LONG
pMatch               LONG
szMatchText          LIKE(ResultQueue.Text)
szSearchFolder       CSTRING(261)
szSearchStringFilename  CSTRING(260)
szFileListFilename   CSTRING(260)
FileQueue            QUEUE(FILE:queue),PRE(FileQueue)
                     END
thisFile             LIKE(ResultQueue.Filename)
thisExtension        LIKE(ResultQueue.szExtension)
szPathName           CSTRING(MAXPATH)
szPrefixString       CSTRING('KSS')
cc                   LONG
progressCalls        LONG
szAnySetOfChars      CSTRING(512)
szBackslashes        CSTRING(512)

  CODE
    
      !ASSERT(0,eqDBG & 'Thread ' & thread() & ' started')
      FindStrOptions = strOptionsGroup
      ReplaceChr(FindStrOptions.szPattern,'§','''')
      ResultQueue &= FindStrOptions.ResultQueue

      !szCmdline   CSTRING('findstr /s /i /n /p "keystone" "d:\program files (x86)\softvelocity\clarion8\accessory\libsrc\win\*.inc"')
      IF glo:DisableSlashP = TRUE ! mr 20180912 
         szCmdLine = 'findstr /n '
      ELSE
         szCmdLine = 'findstr /n /p '
      END 
      
      IF FindStrOptions.bMatchPatternStartOfLine
         szCmdLine = szCmdLine & '/b '
      END
      IF FindStrOptions.bMatchPatternEndOfLine
         szCmdLine = szCmdLine & '/e '
      END
      IF FindStrOptions.bUseRegularExpressions
         szCmdLine = szCmdLine & '/r '
      ELSE
         szCmdLine = szCmdLine & '/l '
      END
      IF NOT FindStrOptions.bCaseSensitive
         szCmdLine = szCmdLine & '/i '
      END
      IF FindStrOptions.bExactMatch
         szCmdLine = szCmdLine & '/x '
      END
      IF FindStrOptions.bExcludeMatch
         szCmdLine = szCmdLine & '/v '
      END
      IF FindStrOptions.bFilenamesOnly
         szCmdLine = szCmdLine & '/m '
      END
      IF FindStrOptions.bFileListFromFile = FALSE
         IF FindStrOptions.bSearchSubdirectories
            IF FindStrOptions.nLevels = 0
               szCmdLine = szCmdLine & '/s '
            ELSE
               FREE(FileQueue)

               j = 1
               k = INSTRING(';',FindStrOptions.szSearchPath,1,j)
               LOOP WHILE k
                  szSearchFolder = FindStrOptions.szSearchPath[j : k-1]
                  BuildFileList(0, FindStrOptions.nLevels, szSearchFolder, FindStrOptions.szFileMask, FileQueue)
                  j = k+1
                  k = INSTRING(';',FindStrOptions.szSearchPath,1,j)
               END
               szSearchFolder = FindStrOptions.szSearchPath[j : LEN(FindStrOptions.szSearchPath)]
               BuildFileList(0, FindStrOptions.nLevels, szSearchFolder, FindStrOptions.szFileMask, FileQueue)

               szFileListFilename = ''
               IF kcr_GetTempPath(SIZE(szPathName),szPathName) > 0
                  IF kcr_GetTempFileName(szPathName, szPrefixString, THREAD(), szFileListFilename)
                     AsciiFilename = szFileListFilename
                     CREATE(AsciiFile)
                     OPEN(AsciiFile)
                     LOOP i = 1 TO RECORDS(FileQueue)
                        GET(FileQueue,i)
                        AsciiFile.Buffer = FileQueue.Name
                        APPEND(AsciiFile)
                     END
                     CLOSE(AsciiFile)
                     szCmdLine = szCmdLine & '/f:"' & szFileListFilename & '" '
                  ELSE
                     MESSAGE('GetTempFileName Failed')
                  END
               ELSE
                  MESSAGE('GetTempPath Failed')
               END
            END
         END
      ELSE
         !MR 20180815 add support for yada.cwproj.FileList.xml 
         !kss.cwproj.FileList.xml 
         IF INSTRING('.cwproj.filelist.xml',LOWER(CLIP(FindStrOptions.szFileListFilename)),1,1) > 0
            ! read cwproj.filelist.xml
            ! get filenames
            ! write temp file, add file name to szcmdline with /f: 
            oXML.Load(qOpenedFiles,FindStrOptions.szFileListFilename,'Opened_Files','file')
            !MESSAGE('records loaded=' & RECORDS(qOpenedFiles))
            oST.SetValue('')
            UD.DebugOff = FALSE 
            LOOP intLoop = 1 TO RECORDS(qOpenedFiles) 
               GET(qOpenedFiles,intLoop)
               !MESSAGE('file=' & CLIP(qOpenedFiles.OFFileName))
               CASE LOWER(oST.ExtensionOnly(qOpenedFiles.OFFileName))
                  OF 'clw'
                  OROF 'inc'
                  OROF 'def'
                  OROF 'equ'
                  OROF 'trn'
                  OROF 'int' ! MR 20190217 
                  ELSE
                     CYCLE 
               END
               oST.Append(CLIP(qOpenedFiles.OFFileName) & '<13,10>',TRUE)
            END 
            IF kcr_GetTempPath(SIZE(szPathName),szPathName) > 0
               IF kcr_GetTempFileName(szPathName, szPrefixString, THREAD(), szFileListFilename)
                  oST.SaveFile(szFileListFilename)     
                  szCmdLine = szCmdLine & '/f:"' & CLIP(szFileListFilename) & '" '
!                  IF INSTRING('|',FindStrOptions.szPattern)
!                     szSearchStringFilename = SavePatternToFile(FindStrOptions.szPattern)
!                     IF szSearchStringFilename <> ''
!                        szCmdLine = szCmdLine & '/g:"' & szSearchStringFilename & '"'
!                     END
!                  ELSE
!                     szCmdLine = szCmdLine & '/c:"' & FindStrOptions.szPattern & '"'
!                  END  
                  !mr 20180912 SETCLIPBOARD(szCmdLine) 
               END
            END 
         ELSE
            szCmdLine = szCmdLine & '/f:"' & FindStrOptions.szFileListFilename & '" '
         END 
      END

      szFixedFileMask = FindStrOptions.szFileMask
      j = LEN(szFixedFileMask)
      LOOP i = 1 TO j
         IF szFixedFileMask[i] = ';' OR szFixedFileMask[i] = '|'
            szFixedFileMask[i] = ' '
         END
      END

      !setup exclusions for use with MASK procedure
      j = LEN(FindStrOptions.szExcludeMask)
      k = 1
      LOOP i = 1 TO j
         CASE FindStrOptions.szExcludeMask[i]
           OF ';' OROF '|'
              szFixedExcludeMask[k] = ' '
              k += 1
           OF '*'
              IF k = 1 OR szFixedExcludeMask[k-1] = ' '
                 szFixedExcludeMask[k] = '^'
                 k += 1
                 szFixedExcludeMask[k] = '.'
                 k += 1
                 szFixedExcludeMask[k] = '*'
                 k += 1
              ELSIF i < j
                 CASE FindStrOptions.szExcludeMask[i+1]
                   OF ' ' OROF ';' OROF '|'
                      szFixedExcludeMask[k] = '.'
                      k += 1
                      szFixedExcludeMask[k] = '*'
                      k += 1
                      szFixedExcludeMask[k] = '$'
                      k += 1
                 ELSE
                    IF FindStrOptions.szExcludeMask[i-1] <> '.' 
                       szFixedExcludeMask[k] = '.'
                       k += 1
                    END 
                    szFixedExcludeMask[k] = '*'
                    k += 1
                 END
              ELSE
                 szFixedExcludeMask[k] = '.'
                 k += 1
                 szFixedExcludeMask[k] = '*'
                 k += 1
                 szFixedExcludeMask[k] = '$'
                 k += 1
              END
         ELSE
            szFixedExcludeMask[k] = UPPER(FindStrOptions.szExcludeMask[i])
            k += 1
         END
      END
      szFixedExcludeMask[k] = '<0>'


      IF NOT FindStrOptions.bUseRegularExpressions
!     http://stackoverflow.com/questions/8844868/what-are-the-undocumented-features-and-limitations-of-the-windows-findstr-comman
!     Escaping Quote within command line search strings
!        Quotes within command line search strings must be escaped with backslash like \". This is true for both literal and regex search strings.

!        Note: The quote may also need to be escaped for the CMD.EXE parser, but this has nothing to do with FINDSTR. For example, to search for a single quote you could use:
!        FINDSTR \^" file && echo found || echo not found

!     Escaping Backslash within command line literal search strings
!        Backslash in a literal search string must generally be escaped with backslash like \\.
!        But there is a special case when the search string contains the following form:
!        [quote][any set of chars][1 or more backslashes][quote]
!        Each backslash in [1 or more backslashes] must be double escaped as \\\\
!        Any backslash in [any set of chars] is escaped normally as \\ as long as the last character in the set is not a backslash.

!        The quotes are escaped normally as \"

!        For example, "\a\b\\" is escaped as \"\\a\\b\\\\\\\\\"

         !need to escape any " in the pattern

         !find [any set of chars]
         j = LEN(FindStrOptions.szPattern)
         LOOP i = j TO 1 BY -1
           IF FindStrOptions.szPattern[i] <> '\'
              BREAK
           END
         END
         IF i = 0    !szPattern is all \ chars
            szAnySetOfChars = ''
            szBackslashes = FindStrOptions.szPattern
         ELSIF i < j
            szAnySetOfChars = FindStrOptions.szPattern[1 : i]
            szBackslashes = FindStrOptions.szPattern[i+1 : j]
         ELSE        !i = j
            szAnySetOfChars = FindStrOptions.szPattern
            szBackslashes = ''
         END
         j = LEN(szAnySetOfChars)
         k = 0
         FindStrOptions.szPattern = ''
         LOOP i = 1 TO j
           IF szAnySetOfChars[i] <> '\'
              k += 1
              FindStrOptions.szPattern[k] = szAnySetOfChars[i]
           ELSE   !double up the '\' chars
              k += 1
              FindStrOptions.szPattern[k] = '\'
              k += 1
              FindStrOptions.szPattern[k] = '\'
           END
         END
         !now terminate the cstring
         k += 1
         FindStrOptions.szPattern[k] = '<0>'

         j = LEN(szBackslashes)
         LOOP j TIMES
           FindStrOptions.szPattern = FindStrOptions.szPattern & '\\\\'
         END

         !now escape any " chars
         j = 1
         i = INSTRING('"',FindStrOptions.szPattern,,j)
         LOOP WHILE i
            FindStrOptions.szPattern = FindStrOptions.szPattern[j : i-1] & '\' & FindStrOptions.szPattern[i : LEN(FindStrOptions.szPattern)]
            j = i + 2
            i = INSTRING('"',FindStrOptions.szPattern,,j)
         END


      ELSE  !using regular expressions
      !  Escaping Backslash within command line regex search strings
      !     Backslash in a regex must be either double escaped like \\\\, or else single escaped within a character class set like [\\]
      END

      IF FindStrOptions.bSearchStringsFromFile
         szCmdLine = szCmdLine & '/g:"' & FindStrOptions.szSearchStringFilename & '"'
      ELSE
         IF INSTRING('|',FindStrOptions.szPattern)
            szSearchStringFilename = SavePatternToFile(FindStrOptions.szPattern)
            IF szSearchStringFilename <> ''
               szCmdLine = szCmdLine & '/g:"' & szSearchStringFilename & '"'
            END
         ELSE
            szCmdLine = szCmdLine & '/c:"' & FindStrOptions.szPattern & '"'
         END
      END

      IF FindStrOptions.bFileListFromFile = FALSE
         IF FindStrOptions.bSearchSubdirectories = TRUE
            IF FindStrOptions.nLevels = 0
               szCmdLine = szCmdLine & ' /d:"' & FindStrOptions.szSearchPath & '" ' & szFixedFileMask
            ELSE
               !files from file list
            END
         ELSE
            szCmdLine = szCmdLine & ' /d:"' & FindStrOptions.szSearchPath & '" ' & szFixedFileMask
         END
      END

      !MESSAGE('normal command line=' & szCmdLine)
      !SETCLIPBOARD(szCmdLine) 
      glo:findstrCommandLine = szCmdLine

      saAttr.nLength = SIZE(SECURITY_ATTRIBUTES)
      saAttr.bInheritHandle = TRUE
      saAttr.lpSecurityDescriptor = 0

      ! Create a pipe for the child process's STDOUT.
      IF kcr_CreatePipe(g_hChildStd_OUT_Rd, g_hChildStd_OUT_Wr, saAttr, 0)
         !Ensure the read handle to the pipe for STDOUT is not inherited.
         IF kcr_SetHandleInformation(g_hChildStd_OUT_Rd, HANDLE_FLAG_INHERIT, 0)
         !IF kcr_SetHandleInformation(g_hChildStd_OUT_Rd, BOR(HANDLE_FLAG_INHERIT,FILE_FLAG_OVERLAPPED), 0)
            ! Create a pipe for the child process's STDIN.
            IF kcr_CreatePipe(g_hChildStd_IN_Rd, g_hChildStd_IN_Wr, saAttr, 0)
               ! Ensure the write handle to the pipe for STDIN is not inherited.
               IF kcr_SetHandleInformation(g_hChildStd_IN_Wr, HANDLE_FLAG_INHERIT, 0)
                  !save the current path
                  SavePath = PATH()

                  FREE(ResultQueue)
                  CreateChildProcess(szCmdLine)
                  progressCalls = ReadFromPipe(FindStrOptions.feqSearchProgress, CancelFlag)

                  IF szFileListFilename <> ''
                     REMOVE(szFileListFilename)
                  END
                  IF szSearchStringFilename <> ''
                     REMOVE(szSearchStringFilename)
                  END
                  FillResultQueue(ResultQueue, FindStrOptions.feqSearchProgress, szFixedFileMask, szFixedExcludeMask, CancelFlag, progressCalls, FindStrOptions.bFilenamesOnly, FindStrOptions.bFileListFromFile)

                  IF FindStrOptions.bExcludeComments
                     bMatchMode = Match:Simple
                     IF NOT FindStrOptions.bCaseSensitive
                        bMatchMode = BOR(bMatchMode,Match:NoCase)
                     END
                     IF FindStrOptions.bUseRegularExpressions
                        bMatchMode = BOR(bMatchMode,Match:Regular)
                     END

                     j = RECORDS(ResultQueue)
                     LOOP i = j TO 1 BY -1
                        GET(ResultQueue,i)
                        x# = 1
                        IF thisFile <> ResultQueue.Filename & ResultQueue.szExtension
                           thisFile = ResultQueue.Filename & ResultQueue.szExtension
                        END
                        IF NOT MatchWithoutComment(ResultQueue.Text,FindStrOptions.szPattern,bMatchMode,ResultQueue.szExtension)
                           DELETE(ResultQueue)
                        END
                        YIELD()
                     END
                  END

                  SETPATH(SavePath)
               ELSE
                  MESSAGE('Stdin SetHandleInformation')
               END
            ELSE
               MESSAGE('Stdin CreatePipe')
            END
         ELSE
            MESSAGE('Stdout SetHandleInformation')
         END
      ELSE
         MESSAGE('StdoutRd CreatePipe')
      END
      kcr_CloseHandle(g_hChildStd_IN_Rd)
      kcr_CloseHandle(g_hChildStd_IN_Wr)
      kcr_CloseHandle(g_hChildStd_OUT_Rd)
      !kcr_CloseHandle(g_hChildStd_OUT_Wr)
      kcr_CloseHandle(piProcInfo.hProcess)

      !dbx.Debugout('Thread Event posted for tab ' & FindStrOptions.tabNumber & '  in FindStr')
      POST(EVENT:THREAD+FindStrOptions.tabNumber,,1)
      !ASSERT(0,eqDBG & 'Thread ' & thread() & ' finished')
