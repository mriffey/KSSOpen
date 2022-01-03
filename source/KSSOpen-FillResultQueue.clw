

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
!!! Generated from procedure template - Source
!!! Process STDOUT_Queue and store lines in ResultQueue
!!! </summary>
FillResultQueue      PROCEDURE  (*ResultQueueType ResultQueue, LONG feqSearchProgress, *CSTRING szFixedFileMask, *CSTRING szFixedExcludeMask, *BYTE bCancelFlag, LONG progressCalls, *BOOL  bFilenamesOnly, *BOOL bFileListFromFile) ! Declare Procedure
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
thisBuffer     &CSTRING
tempBuffer     &CSTRING
thisFolder     CSTRING(261)
thisFile       CSTRING(261)
thisLine       LONG
thisText       CSTRING(2048)
thisString     CSTRING(BUFSIZE+1)
i              LONG
j              LONG
n              LONG
p              LONG
x              LONG

!ProcNameQueue  QUEUE,PRE(PNQ)
!LineNo            LONG
!ProcedureName     CSTRING(256)
!               END
!SectionQueue   QUEUE,PRE(SQ)
!lowLineNo         LONG
!highLineNo        LONG
!szSection         CSTRING(5)
!               END


ProcNameQueue  QUEUE(ProcNameQueueType),PRE(PNQ)
               END
SectionQueue   QUEUE(SectionQueueType),PRE(SQ)
               END

sizeNeeded     LONG
modulo         LONG
CommentStyle   LONG
excludeQueue   QUEUE,PRE(exq)
excludemask       CSTRING(256)
               END


  CODE
    
   thisBuffer &= NEW CSTRING((2*BUFSIZE)+1)
   !ASSERT(0,eqDBG & 'NEW thisBuffer [' & ADDRESS(thisBuffer) & ']')

   IF szFixedExcludeMask <> ''
      i = 1
      j = INSTRING(' ',szFixedExcludeMask)
      LOOP WHILE j
         excludeQueue.excludemask = szFixedExcludeMask[i : j-1]
         ADD(excludeQueue)
         i = j+1
         j = INSTRING(' ',szFixedExcludeMask,1,i)
      END
      excludeQueue.excludemask = szFixedExcludeMask[i : LEN(szFixedExcludeMask)]
      ADD(excludeQueue)
   END

   FREE(ResultQueue)
   j = RECORDS(STDOUT_Queue)
   LOOP i = 1 TO j
      GET(STDOUT_Queue,i)
      sizeNeeded = LEN(thisBuffer) + LEN(STDOUT_Queue.Buffer) + 1
      IF sizeNeeded > SIZE(thisBuffer)
         tempBuffer &= NEW CSTRING(LEN(thisBuffer)+1)
         !ASSERT(0,eqDBG & 'NEW tempBuffer [' & ADDRESS(tempBuffer) & ']')
         tempBuffer = thisBuffer
         !ASSERT(0,eqDBG & 'DISPOSE thisBuffer [' & ADDRESS(thisBuffer) & ']')
         DISPOSE(thisBuffer)
         thisBuffer &= NULL
         thisBuffer &= NEW CSTRING(sizeNeeded)
         thisBuffer = tempBuffer & STDOUT_Queue.Buffer
         !ASSERT(0,eqDBG & 'DISPOSE tempBuffer [' & ADDRESS(tempBuffer) & ']')
         DISPOSE(tempBuffer)
         tempBuffer &= NULL
      ELSE
         thisBuffer = thisBuffer & STDOUT_Queue.Buffer
      END
      DO ProcessBuffer
      YIELD()
   END

   j = LEN(CLIP(thisBuffer))
   IF j > 0
      thisBuffer[j+1] = '<13>'
      thisBuffer[j+2] = '<10>'
      DO ProcessBuffer
   END

   !ASSERT(0,eqDBG & 'DISPOSE thisBuffer [' & ADDRESS(thisBuffer) & ']')
   DISPOSE(thisBuffer)
   thisBuffer &= NULL

   thisFile = ''
   j = RECORDS(ResultQueue)

   progressCalls = progressCalls % 100
   modulo = j / (100 - progressCalls)

   LOOP i = 1 TO j
      IF bCancelFlag = TRUE
         BREAK
      ELSE
         GET(ResultQueue,i)
         IF ResultQueue.szExtension
            CommentStyle = GetCommentStyle(ResultQueue.szExtension)
            IF glo:bAllExtensions = TRUE
               DO ProcessFile
            ELSE
               LOOP p = 1 TO RECORDS(ClarionExtensionsQueue)
                  GET(ClarionExtensionsQueue,p)
                  IF INSTRING(ClarionExtensionsQueue.FileExtension,UPPER(ResultQueue.Filename & ResultQueue.szExtension),1)
                     DO ProcessFile
                     BREAK
                  END
               END
            END
         END
         IF modulo > 0
            IF (i % modulo) = 0
               POST(EVENT:PROGRESS+feqSearchProgress,,1)
            END
         END
         YIELD()
      END
   END

   SORT(ResultQueue,+ResultQueue.SortName,+ResultQueue.LineNo)

   IF NOT thisBuffer &= NULL
      !ASSERT(0,eqDBG & 'DISPOSE thisBuffer [' & ADDRESS(thisBuffer) & ']')
      DISPOSE(thisBuffer)
      thisBuffer &= NULL
   END
   IF NOT tempBuffer &= NULL
      !ASSERT(0,eqDBG & 'DISPOSE tempBuffer [' & ADDRESS(tempBuffer) & ']')
      DISPOSE(tempBuffer)
      tempBuffer &= NULL
   END
ProcessBuffer  ROUTINE
   DATA
i              LONG
j              LONG
k              LONG
p              LONG
pDelimiter     LONG
Delimiter      STRING('<10>')
CR             STRING('<13>')
currentPath    LIKE(ResultQueue.Path)

   CODE
      !ASSERT(0,eqDBG & 'Thread ' & thread() & ' ProcessBuffer')
      i = 1
      pDelimiter = INSTRING(Delimiter,thisBuffer,1,i)
      LOOP WHILE pDelimiter
         IF thisBuffer[pDelimiter-1] = CR
            thisString = thisBuffer[i : pDelimiter-2]
         ELSE
            thisString = thisBuffer[i : pDelimiter-1]
         END
         !ASSERT(0,eqDBG & 'Thread ' & thread() & ' look for delimiter ' & thisString)
         IF bFileListFromFile = TRUE
            DO ProcessThisString
         ELSE
            IF thisString[1 : 2] = '  '
               thisFolder = CLIP(LEFT(thisString))
               thisFolder[LEN(thisFolder)] = '\'
            ELSE
               !check for 'nasty' bug
               IF thisString[LEN(thisString)] = ':'
                  !ASSERT(0,eqDBG & 'Thread ' & thread() & ' fix nasty bug ' & thisString)
                  LOOP n = LEN(thisString) TO 1 BY -1
                     IF thisString[n : n+1] = '  '
                        BREAK
                     END
                  END
                  IF n > 0
                     IF thisString[n+3 : n+4] = ':\'
                        pDelimiter = n-1
                        thisString = thisString[1 : pDelimiter]
                     END
                  END
               END

               !process thisString
               !!!Region('OldCode')
!               IF NOT INSTRING('*',szFixedFileMask,1) AND NOT INSTRING(' ',szFixedFileMask,1) AND NOT INSTRING('?',szFixedFileMask,1)
!                  IF NOT NUMERIC(thisString[1])
!                  !IF UPPER(szFixedFileMask) = UPPER(thisString[1 : LEN(szFixedFileMask)])
!                     thisString = thisFolder & thisString
!                  ELSE
!                     thisString = thisFolder & szFixedFileMask & ':' & thisString
!                  END
!               ELSE
!                  thisString = thisFolder & thisString
!               END
               !!!EndRegion
               thisString = thisFolder & thisString

               LOOP n = 1 TO RECORDS(excludeQueue)
                  GET(excludeQueue,n)
                  IF MATCH(UPPER(thisString),excludeQueue.excludemask,Match:Regular)
                     BREAK
                  END
               END
               IF n > RECORDS(excludeQueue)
                  DO ProcessThisString
               END

            END
         END

         i = pDelimiter + 1
         pDelimiter = INSTRING(Delimiter,thisBuffer,1,i)
      END   !LOOP
      IF LEN(thisBuffer) > 0
         IF i <= LEN(thisBuffer)
            thisBuffer = thisBuffer[i : LEN(thisBuffer)]
         ELSE
            thisBuffer = ''
         END
      END

ProcessThisString    ROUTINE
   DATA
k              LONG
pDot           LONG
pColon         LONG
Colon          STRING(':')
szPath         CSTRING(MAXPATH+1)
szDrive        CSTRING(MAXDRIVE+1)
szDir          CSTRING(MAXDIR+1)
szName         CSTRING(MAXFILE+1)
szExtension    CSTRING(MAXEXT+1)
cc             LONG
strText        STRING(1025)

resultFile        CSTRING(MAXPATH+1)
currentFile       CSTRING(MAXPATH+1)
currentFileDate   LONG
currentFileTime   LONG
fileQueue         QUEUE(FILE:queue),PRE(fq)
                  END

   CODE
            x# = 1
            k = 1
            pColon = INSTRING(Colon,thisString,1,3)   !skip ':' from path
            IF pColon
               szPath = thisString[k : pColon-1]
               szDrive = ''
               szDir = ''
               szName = ''
               szExtension = ''
               cc = kcr_fnSplit(szPath, szDrive, szDir, szName, szExtension)
               ResultQueue.Path = szDrive & szDir
               ResultQueue.Filename = szName
               ResultQueue.szExtension = szExtension

               IF NOT INSTRING('/s',glo:findstrCommandLine,1)
                  IF NUMERIC(szName) AND szExtension = ''
                     pDot = INSTRING('.',szFixedFileMask)
                     szName = szFixedFileMask[1 : pDot-1]
                     szExtension = szFixedFileMask[pDot : LEN(szFixedFileMask)]
                     ResultQueue.Filename = szName
                     ResultQueue.szExtension = szExtension
                     szPath = szDrive & szDir & szName & szExtension
                     k = LEN(szDrive & szDir) + 1
                  ELSE
                     k = pColon + 1
                  END
               ELSE
                  k = pColon + 1
               END

!               k = pColon + 1
               pColon = INSTRING(Colon,thisString,1,k)
               IF pColon
                  ResultQueue.LineNo = thisString[k : pColon-1]
                  ResultQueue.ProcedureName = ''
                  strText = thisString[pColon+1 : LEN(thisString)]
                  ReplaceTabs(strText)
                  ResultQueue.Text = CLIP(strText)
                  ResultQueue.SortName = UPPER(szPath)
                  ResultQueue.Position = 0
                  ResultQueue.DeleteInstance = 0
                  ResultQueue.szSection = ''

                  resultFile = UPPER(ResultQueue.Path & ResultQueue.Filename & ResultQueue.szExtension)
                  IF currentFile <> resultFile
                     currentFile = resultFile
                     DIRECTORY(fileQueue, currentFile, ff_:Normal)
                     currentFileDate = fileQueue.date
                     currentFileTime = fileQueue.time
                  END
                  ResultQueue.FileDate = currentFileDate
                  ResultQueue.FileTime = currentFileTime

                  ADD(ResultQueue,+ResultQueue.SortName,+ResultQueue.LineNo)
               END
            ELSIF bFilenamesOnly
               szPath = thisString
               szDrive = ''
               szDir = ''
               szName = ''
               szExtension = ''
               cc = kcr_fnSplit(szPath, szDrive, szDir, szName, szExtension)
               ResultQueue.Path = szDrive & szDir
               ResultQueue.Filename = szName
               ResultQueue.szExtension = szExtension

               ResultQueue.LineNo = 1
               ResultQueue.ProcedureName = ''
               ResultQueue.Text = ''
               ResultQueue.SortName = UPPER(szPath)
               ResultQueue.Position = 0
               ResultQueue.DeleteInstance = 0
               ResultQueue.szSection = ''

               resultFile = ResultQueue.Path & ResultQueue.Filename & ResultQueue.szExtension
               IF currentFile <> resultFile
                  currentFile = resultFile
                  DIRECTORY(fileQueue, currentFile, ff_:Normal)
                  currentFileDate = fileQueue.date
                  currentFileTime = fileQueue.time
               END
               ResultQueue.FileDate = currentFileDate
               ResultQueue.FileTime = currentFileTime

               ADD(ResultQueue,+ResultQueue.SortName,+ResultQueue.LineNo)
            END

ProcessFile    ROUTINE
   DATA
p                    LONG

   CODE
      IF ResultQueue.SortName = thisFile
         IF RECORDS(ProcNameQueue) = 1
            GET(ProcNameQueue,n)
            ResultQueue.ProcedureName = ProcNameQueue.ProcedureName
         ELSE
            LOOP n = 1 TO RECORDS(ProcNameQueue)
               GET(ProcNameQueue,n)
               IF ProcNameQueue.LineNo <= ResultQueue.LineNo
                  ResultQueue.ProcedureName = ProcNameQueue.ProcedureName
               ELSE
                  BREAK
               END
            END
         END

         LOOP p = 1 TO RECORDS(SectionQueue)
            GET(SectionQueue,p)
            IF INRANGE(ResultQueue.LineNo,SectionQueue.lowLineNo,SectionQueue.highLineNo)
               ResultQueue.szSection = SectionQueue.szSection
               BREAK
            END
         END
         PUT(ResultQueue)

      ELSE
         !ASSERT(1,eqDBG & THREAD() & ' scanning: ' & ResultQueue.SortName)
         !dbx.debugout(THREAD() & ' scanning: ' & ResultQueue.SortName)
         thisFile = ResultQueue.SortName
         FREE(ProcNameQueue)
         FREE(SectionQueue)
         thisLine = 0
         AsciiFilename = thisFile
         OPEN(AsciiFile,ReadOnly+DenyWrite)
         IF ~ERRORCODE()

            CASE CommentStyle
              OF CommentStyleClarion
                 DO ProcessClarionFile
              OF CommentStyleCpp
                 DO ProcessCppFile
              OF CommentStylePython
                 DO ProcessPythonFile
              OF CommentStyleVb
                 DO ProcessVbFile
            END

            CLOSE(AsciiFile)
            POST(EVENT:PROGRESS+feqSearchProgress,,1)

            SectionQueue.highLineNo = thisLine
            PUT(SectionQueue)

            !COMPILE('***',_DEBUG_)
            IF COMMAND('/DEBUG')
               setcursor()
               BrowseQueues(ProcNameQueue, SectionQueue)
               setcursor(cursor:wait)
            END 
            !***

            IF RECORDS(ProcNameQueue) = 1
               GET(ProcNameQueue,1)
               ResultQueue.ProcedureName = ProcNameQueue.ProcedureName
            ELSE
               LOOP n = 1 TO RECORDS(ProcNameQueue)
                  GET(ProcNameQueue,n)
                  IF ProcNameQueue.LineNo <= ResultQueue.LineNo
                     ResultQueue.ProcedureName = ProcNameQueue.ProcedureName
                  ELSE
                     n -= 1
                     BREAK
                  END
               END
            END

            LOOP p = 1 TO RECORDS(SectionQueue)
               GET(SectionQueue,p)
               IF INRANGE(ResultQueue.LineNo,SectionQueue.lowLineNo,SectionQueue.highLineNo)
                  ResultQueue.szSection = SectionQueue.szSection
                  BREAK
               END
            END

            PUT(ResultQueue)
         END
      END
   EXIT
ProcessClarionFile    ROUTINE
   DATA
p                    LONG
bInMap               BOOL(FALSE)
bInClassDeclaration  BOOL(FALSE)
appProcedureName     LIKE(ProcNameQueue.ProcedureName)
ClassNameQueue       QUEUE,PRE(cnq)
ClassName               CSTRING(256)
ProcedureName           CSTRING(256)
                     END
LastProcedureName    LIKE(ProcNameQueue.ProcedureName)

   CODE
      SectionQueue.lowLineNo = 1
      SectionQueue.highLineNo = 1
      SectionQueue.szSection = 'DATA'
      ADD(SectionQueue)

      SET(AsciiFile)
      LOOP
         NEXT(AsciiFile)
         IF ERRORCODE()
            BREAK
         ELSE
            ReplaceTabs(AsciiFile.Buffer)
            thisLine += 1

            CASE UPPER(CLIP(LEFT(AsciiFile.Buffer)))
              OF 'DATA'
                 IF SectionQueue.szSection <> 'DATA'
                    SectionQueue.highLineNo = thisLine - 1
                    PUT(SectionQueue)
                    SectionQueue.lowLineNo = thisLine
                    SectionQueue.highLineNo = thisLine
                    SectionQueue.szSection = 'DATA'
                    ADD(SectionQueue)
                 END
              OF 'CODE'
                 IF SectionQueue.szSection <> 'CODE'
                    SectionQueue.highLineNo = thisLine - 1
                    PUT(SectionQueue)
                    SectionQueue.lowLineNo = thisLine
                    SectionQueue.highLineNo = thisLine
                    SectionQueue.szSection = 'CODE'
                    ADD(SectionQueue)
                 END
            END

            CASE AsciiFile.Buffer[1]
              OF ' '
                 thisText = CLIP(LEFT(AsciiFile.Buffer))
                 !IF UPPER(thisText[1 : 3]) = 'MAP'
                 IF UPPER(thisText) = 'MAP'
                    bInMap = TRUE
                    ProcNameQueue.LineNo = thisLine
                    !LastProcedureName = ProcNameQueue.ProcedureName
                    ProcNameQueue.ProcedureName = 'MAP'
                    ADD(ProcNameQueue)
                    !dbx.Debugout('1 [' & appProcedureName & '] ' & ProcNameQueue.LineNo & ': ' & ProcNameQueue.ProcedureName)
                 ELSIF UPPER(thisText[1 : 6]) = 'MODULE'
                    bInClassDeclaration = TRUE
                 ELSIF bInClassDeclaration = TRUE
                    IF UPPER(thisText[1 : 3]) = 'END'
                       bInClassDeclaration = FALSE
                    END
                 ELSIF bInMap = TRUE
                    IF UPPER(thisText[1 : 3]) = 'END'
                       bInMap = FALSE
                       ProcNameQueue.LineNo = thisLine
                       !ProcNameQueue.ProcedureName = ''
                       ProcNameQueue.ProcedureName = CHOOSE(appProcedureName='',LastProcedureName,appProcedureName)
                       ADD(ProcNameQueue)
                       !dbx.Debugout('2 [' & appProcedureName & '] ' & ProcNameQueue.LineNo & ': ' & ProcNameQueue.ProcedureName)
                    END
                 END
                 CYCLE

              OF '!'
                 CYCLE
            ELSE
              IF bInMap = FALSE
                 p = INSTRING(' ',CLIP(AsciiFile.Buffer))
                 IF p
                    thisText = CLIP(LEFT(AsciiFile.Buffer[p+1 : LEN(CLIP(AsciiFile.Buffer))]))
                    IF UPPER(thisText[1 : 5]) = 'CLASS' OR UPPER(thisText[1 : 9]) = 'INTERFACE'
                       bInClassDeclaration = TRUE
                       ClassNameQueue.ClassName = AsciiFile.Buffer[1 : p-1]
                       ClassNameQueue.ProcedureName = appProcedureName
                       ADD(ClassNameQueue,+ClassNameQueue.ClassName,+ClassNameQueue.ProcedureName)
                       !dbx.Debugout('3 ' & ClassNameQueue.ClassName & ': ' & ClassNameQueue.ProcedureName)

                    END
                    IF bInClassDeclaration = TRUE
                       IF UPPER(thisText[1 : 3]) = 'END'
                          bInClassDeclaration = FALSE
                       END
                    ELSE
                       IF UPPER(thisText[1 : 9]) = 'PROCEDURE' OR UPPER(thisText[1 : 8]) = 'FUNCTION'
                          ProcNameQueue.LineNo = thisLine
                          x = INSTRING('.',AsciiFile.Buffer[1 : p-1])
                          IF x
                             IF appProcedureName = 'Dec2Hex'
                                appProcedureName = ''
                             END
                             ClassNameQueue.ClassName = AsciiFile.Buffer[1 : x-1]
                             ClassNameQueue.ProcedureName = appProcedureName
                             GET(ClassNameQueue,+ClassNameQueue.ClassName,+ClassNameQueue.ProcedureName)
                             IF ERRORCODE()
                                ClassNameQueue.ClassName = AsciiFile.Buffer[1 : x-1]
                                GET(ClassNameQueue,+ClassNameQueue.ClassName)
                                IF NOT ERRORCODE()
                                   appProcedureName = ClassNameQueue.ProcedureName
                                END
                             END
                             ProcNameQueue.ProcedureName = CHOOSE(appProcedureName = '', AsciiFile.Buffer[1 : p-1], appProcedureName & '.' & AsciiFile.Buffer[1 : p-1])
                          ELSE
                             ProcNameQueue.ProcedureName = AsciiFile.Buffer[1 : p-1]
                             !appProcedureName     = ProcNameQueue.ProcedureName
                          END
                          ADD(ProcNameQueue)
                          LastProcedureName = ProcNameQueue.ProcedureName
                          !dbx.Debugout('4 [' & appProcedureName & '] ' & ProcNameQueue.LineNo & ': ' & ProcNameQueue.ProcedureName)

                          IF SectionQueue.szSection = 'DATA'
                             SectionQueue.highLineNo = thisLine - 1
                             PUT(SectionQueue)
                             SectionQueue.lowLineNo = thisLine
                             SectionQueue.highLineNo = thisLine
                             SectionQueue.szSection = 'CODE'
                             ADD(SectionQueue)
                             SectionQueue.lowLineNo = thisLine + 1
                             SectionQueue.highLineNo = thisLine + 1
                             SectionQueue.szSection = 'DATA'
                             ADD(SectionQueue)
                          ELSE
                             SectionQueue.highLineNo = thisLine
                             PUT(SectionQueue)
                             SectionQueue.lowLineNo = thisLine + 1
                             SectionQueue.highLineNo = thisLine + 1
                             SectionQueue.szSection = 'DATA'
                             ADD(SectionQueue)
                          END

                       ELSIF UPPER(thisText[1 : 7]) = 'ROUTINE'
                          ProcNameQueue.LineNo = thisLine
                          ProcNameQueue.ProcedureName = CHOOSE(appProcedureName = '', LastProcedureName & ' > ' & AsciiFile.Buffer[1 : p-1], appProcedureName & ' > ' & AsciiFile.Buffer[1 : p-1])
                          ADD(ProcNameQueue,+ProcNameQueue.LineNo)
                          !dbx.Debugout('5 [' & appProcedureName & '] ' & ProcNameQueue.LineNo & ': ' & ProcNameQueue.ProcedureName)

                          IF SectionQueue.szSection <> 'CODE'
                             SectionQueue.highLineNo = thisLine - 1
                             PUT(SectionQueue)
                             SectionQueue.lowLineNo = thisLine
                             SectionQueue.highLineNo = thisLine
                             SectionQueue.szSection = 'CODE'
                             ADD(SectionQueue)
                          END
                       END
                    END
                 END
              END
            END
         END
      END
ProcessCppFile    ROUTINE
   DATA
n                 LONG
p                 LONG
q1                LONG
q2                LONG
InBlockComment    BOOL(FALSE)
InMethod          BOOL(FALSE)
nBraceCount       LONG
nClassCount       LONG
thisNamespace     CSTRING(256)
thisClass         CSTRING(256)
thisMethod        CSTRING(256)
lastText          CSTRING(2048)
lastTextLine      LONG
clText            LIKE(thisText)
stopLine          LONG(1)

   CODE
      SET(AsciiFile)
      LOOP
         NEXT(AsciiFile)
         IF ERRORCODE()
            BREAK
         ELSE
            ReplaceTabs(AsciiFile.Buffer)
            thisLine += 1

            IF thisline = stopLine
               x# = 0
            END

            thisText = CLIP(LEFT(AsciiFile.Buffer))
            IF thisText = ''
               IF NOT INSTRING('(',lastText)
                  lastText = ''
               END
               CYCLE
            ELSE

               !remove comments
               IF thisText[1 : 2] = '//'
                  CYCLE
               ELSE
                  p = INSTRING(' //',thisText,1)
                  IF p > 1
                     thisText = CLIP(thisText[1 : p-1])
                   END
               END

               !remove anything in quotes
               n = 0
               clText = ''
               LOOP p = 1 TO LEN(thisText)
                  IF thisText[p] = ''''
                     n += 1
                     clText[n] = thisText[p]
                     p += 1
                     p = INSTRING('''',thisText,1,p)
                     IF p = 0
                        clText = thisText
                        BREAK
                     ELSE
                        LOOP WHILE thisText[p-1] = '\'  !was it escaped?
                           p = INSTRING('''',thisText,1,p+1)
                           IF p = 0
                              clText = thisText
                              BREAK
                           END
                        END
                        IF p = 0
                           BREAK
                        ELSE
                           n += 1
                           clText[n] = thisText[p]
                        END
                     END
                  ELSE
                     n += 1
                     clText[n] = thisText[p]
                  END
               END
               IF p > LEN(thisText)
                  clText[n+1] = '<0>'
                  thisText = clText
               END

               !remove anything in double quotes
               n = 0
               clText = ''
               LOOP p = 1 TO LEN(thisText)
                  IF thisText[p] = '"'
                     n += 1
                     clText[n] = thisText[p]
                     p += 1
                     p = INSTRING('"',thisText,1,p)
                     IF p = 0
                        clText = thisText
                        BREAK
                     ELSE
                        LOOP WHILE thisText[p-1] = '\'  !was it escaped?
                           p = INSTRING('"',thisText,1,p+1)
                           IF p = 0
                              clText = thisText
                              BREAK
                           END
                        END
                        IF p = 0
                           BREAK
                        ELSE
                           n += 1
                           clText[n] = thisText[p]
                        END
                     END
                  ELSE
                     n += 1
                     clText[n] = thisText[p]
                  END
               END
               IF p > LEN(thisText)
                  clText[n+1] = '<0>'
                  thisText = clText
               END
               clText = ''

               IF InMethod

                  IF InBlockComment
                     IF INSTRING('*/',thisText,1,1)
                        InBlockComment = FALSE
                     END
                     CYCLE
                  ELSE
                     IF thisText[1 : 2] = '/*'
                        IF NOT INSTRING('*/',thisText,1,3)
                           InBlockComment = TRUE
                        END
                        CYCLE
                     !ELSIF thisText[1 : 2] = '//'
                     !   CYCLE
                     END
                  END


                  p = INSTRING('{{',thisText)
                  LOOP WHILE p
                     nBraceCount += 1
                     IF p < LEN(thisText)
                        p = INSTRING('{{',thisText,1,p+1)
                     ELSE
                        p = 0
                     END
                  END
                  p = INSTRING('}',thisText)
                  LOOP WHILE p
                     nBraceCount -= 1
                     IF p < LEN(thisText)
                        p = INSTRING('}',thisText,1,p+1)
                     ELSE
                        p = 0
                     END
                  END
                  IF nClassCount > 0 AND nBraceCount = nClassCount
                     InMethod = FALSE
                  ELSIF nBraceCount = 0
                     InMethod = FALSE
                  END
                  CYCLE
               ELSE   !not in method
                  IF InBlockComment
                     IF INSTRING('*/',thisText,1,1)
                        InBlockComment = FALSE
                     END
                     CYCLE
                  ELSE
                     IF thisText[1 : 2] = '/*'
                        IF NOT INSTRING('*/',thisText,1,3)
                           InBlockComment = TRUE
                        END
                        CYCLE
                     ELSIF thisText[1 : 2] = '//'
                        CYCLE
                     ELSE
                        p = INSTRING('{{',thisText)
                        IF p
                           IF INSTRING('}',thisText)
                              CYCLE
                           ELSIF INSTRING('enum ',thisText[1 : p-1],1)
                              nBraceCount += 1
                              CYCLE
                           ELSIF INSTRING('struct ',thisText[1 : p-1],1)
                              nBraceCount += 1
                              CYCLE
                           END

                           clText = CLIP(LEFT(thisText))
                           p = INSTRING('(',lastText)
                           IF p
                              IF NOT INSTRING(')',lastText,1,p+1)
                                 thisText = lastText & thisText
                              ELSIF lastText[LEN(lastText)] = ':'   |
                                 OR lastText[LEN(lastText)] = ','   |
                                 OR clText[1] = ','
                                 thisText = lastText & thisText
                              END
                           ELSE
                              IF clText[1] = '('
                                 thisText = lastText & thisText
                              END
                           END
                           p = INSTRING('{{',thisText)

                           IF p > 1 !something precedes {
                              IF lastText = ''
                                 lastTextLine = thisLine
                              END
                              lastText = thisText[1 : p-1]
                           END
                           p = INSTRING('namespace ',lastText,1)
                           IF p
                              thisNamespace = lastText[p+10 : LEN(lastText)]
                              thisNamespace = CLIP(thisNamespace)
                              nBraceCount += 1
                              CYCLE
                           END
                           p = INSTRING('class ',lastText,1)
                           IF p
                              thisClass = lastText[p+6 : LEN(lastText)]
                              p = INSTRING(':',thisClass)
                              IF p
                                 thisClass[p] = '<0>'
                              END
                              thisClass = CLIP(thisClass)
                              nBraceCount += 1
                              nClassCount = nBraceCount
                              CYCLE
                           END

                           p = INSTRING('STDMETHODIMP_(',lastText,1)
                           IF p
                              p = INSTRING(')',lastText,1,p+14)
                              IF p
                                 lastText = lastText[p+1 : LEN(lastText)]
                              END
                           END

                           p = INSTRING('(',lastText)
                           IF p
                              IF lastText[p-1] = '>' OR lastText[p-1] = '*' OR lastText[p-1] = '_'
                                 LOOP p = p-2 TO 1 BY -1
                                    IF lastText[p] = ' '
                                       BREAK
                                    END
                                 END
                              END
                              thisMethod = lastText[1 : p-1]
                           ELSE
                              thisMethod = lastText
                           END
                           thisMethod = CLIP(thisMethod)
                           LOOP p = LEN(thisMethod) TO 1 BY -1
                              IF thisMethod[p] = ' '
                                 BREAK
                              END
                           END
                           IF p > 0
                              thisMethod = thisMethod[p+1 : LEN(thisMethod)]
                           END
                           IF thisMethod[1] = '*' OR thisMethod[1] = '&'
                              thisMethod = thisMethod[2 : LEN(thisMethod)]
                           END
                           thisMethod = CLIP(thisMethod)

                           !if not some sort of initialization '= {' in thisText
                           IF thisMethod[LEN(thisMethod)] <> '=' AND thisMethod <> '""'
                              ProcNameQueue.LineNo = lastTextLine
                              ProcNameQueue.ProcedureName = ''
                              IF thisNamespace
                                 ProcNameQueue.ProcedureName = thisNamespace & '.'
                              END
                              IF thisClass
                                 ProcNameQueue.ProcedureName = ProcNameQueue.ProcedureName & thisClass & '.'
                              END
                              ProcNameQueue.ProcedureName = ProcNameQueue.ProcedureName & thisMethod
                              ADD(ProcNameQueue)
                              !dbx.debugout('[' & ProcNameQueue.LineNo & '] ' & ProcNameQueue.ProcedureName)
                           END
                           InMethod = TRUE
                           nBraceCount += 1
                           lastText = ''
                        ELSE

                           p = INSTRING('}',thisText)
                           LOOP WHILE p
                              nBraceCount -= 1
                              IF p < LEN(thisText)
                                 p = INSTRING('}',thisText,1,p+1)
                              ELSE
                                 p = 0
                              END
                           END
                           IF nBraceCount < nClassCount
                              nClassCount = 0
                              thisClass = ''
                           END

                           p = INSTRING('(',lastText)
                           IF p
                              IF NOT INSTRING(')',lastText,1,p+1)
                                 lastText = lastText & thisText
                              ELSIF lastText[LEN(lastText)] = ':'   |
                                 OR lastText[LEN(lastText)] = ','
                                    lastText = lastText & thisText
                              ELSE
                                 lastText = thisText
                                 lastTextLine = thisLine
                              END
                           ELSE
                              !IF nClassCount = 0
                                 lastText = thisText
                                 lastTextLine = thisLine
                              !END
                           END
                        END
                     END
                  END   !IF InBlockComment
               END   !IF InMethod
            END
         END
      END
   EXIT
ProcessPythonFile    ROUTINE
   DATA
n                 LONG
p                 LONG
q1                LONG
q2                LONG
InMethod          BOOL(FALSE)
MethodTerminator  CSTRING(21)
nBraceCount       LONG
nClassCount       LONG
thisNamespace     CSTRING(256)
thisClass         CSTRING(256)
thisMethod        CSTRING(256)
lastText          CSTRING(2048)
lastTextLine      LONG
clText            LIKE(thisText)
stopLine          LONG(1)

   CODE
      SET(AsciiFile)
      LOOP
         NEXT(AsciiFile)
         IF ERRORCODE()
            BREAK
         ELSE
            ReplaceTabs(AsciiFile.Buffer)
            thisLine += 1

            IF thisline = stopLine
               x# = 0
            END

            IF thisClass <> '' AND NOT INSTRING(AsciiFile.Buffer[1],' #')
               thisClass = ''
            END

            thisText = CLIP(LEFT(AsciiFile.Buffer))

            !replace tabs with spaces
            p = LEN(thisText)
            LOOP n = 1 TO p
               IF thisText[n] = '<9>'
                  thisText[n] = ' '
               END
            END
            thisText = CLIP(LEFT(thisText))

            IF thisText = ''
               IF NOT INSTRING('(',lastText)
                  lastText = ''
               END
               CYCLE
            ELSE

               !remove comments
               IF thisText[1] = '#'
                  CYCLE
               ELSE
                  p = INSTRING(' #',thisText,1)
                  IF p > 1
                     thisText = CLIP(thisText[1 : p-1])
                   END
               END

               !remove anything in double quotes
               n = 0
               clText = ''
               LOOP p = 1 TO LEN(thisText)
                  IF thisText[p] = '"'
                     n += 1
                     clText[n] = thisText[p]
                     p += 1
                     p = INSTRING('"',thisText,1,p)
                     IF p = 0
                        clText = thisText
                        BREAK
                     ELSE
                        LOOP WHILE thisText[p-1] = '"'  !was it escaped?
                           p = INSTRING('"',thisText,1,p+1)
                           IF p = 0
                              clText = thisText
                              BREAK
                           END
                        END
                        IF p = 0
                           BREAK
                        ELSE
                           n += 1
                           clText[n] = thisText[p]
                        END
                     END
                  ELSE
                     n += 1
                     clText[n] = thisText[p]
                  END
               END
               IF p > LEN(thisText)
                  clText[n+1] = '<0>'
                  thisText = clText
               END
               clText = ''

               IF InMethod
                  !look for terminator
                  IF INSTRING(MethodTerminator,UPPER(CLIP(LEFT(thisText))),1)
                     InMethod = FALSE
                  ELSIF UPPER(CLIP(LEFT(thisText[1 : 4]))) = 'DEF '
                     n = INSTRING('(',thisText)
                     IF n = 0
                        n = LEN(thisText)+1
                     END
                     thisMethod = CLIP(LEFT(thisText[5 : n-1]))
                     InMethod = TRUE
                     MethodTerminator = 'RETURN '
                     ProcNameQueue.LineNo = thisLine
                     ProcNameQueue.ProcedureName = ''
                     IF thisNamespace
                        ProcNameQueue.ProcedureName = thisNamespace & '.'
                     END
                     IF thisClass
                        ProcNameQueue.ProcedureName = ProcNameQueue.ProcedureName & thisClass & '.'
                     END
                     ProcNameQueue.ProcedureName = ProcNameQueue.ProcedureName & thisMethod
                     ADD(ProcNameQueue)
                     !dbx.debugout('[' & ProcNameQueue.LineNo & '] ' & ProcNameQueue.ProcedureName)
                  END
               ELSE   !not in method
                  IF UPPER(CLIP(LEFT(thisText[1 : 6]))) = 'CLASS '
                     n = INSTRING(':',thisText)
                     IF n = 0
                        n = LEN(thisText)+1
                     END
                     thisClass = CLIP(LEFT(thisText[7 : n-1]))

                  ELSIF UPPER(CLIP(LEFT(thisText[1 : 4]))) = 'DEF '
                     n = INSTRING('(',thisText)
                     IF n = 0
                        n = LEN(thisText)+1
                     END
                     thisMethod = CLIP(LEFT(thisText[5 : n-1]))
                     InMethod = TRUE
                     MethodTerminator = 'RETURN '
                     ProcNameQueue.LineNo = thisLine
                     ProcNameQueue.ProcedureName = ''
                     IF thisNamespace
                        ProcNameQueue.ProcedureName = thisNamespace & '.'
                     END
                     IF thisClass
                        ProcNameQueue.ProcedureName = ProcNameQueue.ProcedureName & thisClass & '.'
                     END
                     ProcNameQueue.ProcedureName = ProcNameQueue.ProcedureName & thisMethod
                     ADD(ProcNameQueue)
                     !dbx.debugout('[' & ProcNameQueue.LineNo & '] ' & ProcNameQueue.ProcedureName)
                  END
               END   !IF InMethod
            END
         END
      END
   EXIT
ProcessVBFile    ROUTINE
   DATA
n                 LONG
p                 LONG
q1                LONG
q2                LONG
InMethod          BOOL(FALSE)
MethodTerminator  CSTRING(21)
nBraceCount       LONG
nClassCount       LONG
thisNamespace     CSTRING(256)
thisClass         CSTRING(256)
thisMethod        CSTRING(256)
lastText          CSTRING(2048)
lastTextLine      LONG
clText            LIKE(thisText)
stopLine          LONG(1)

   CODE
      SET(AsciiFile)
      LOOP
         NEXT(AsciiFile)
         IF ERRORCODE()
            BREAK
         ELSE
            ReplaceTabs(AsciiFile.Buffer)
            thisLine += 1

            IF thisline = stopLine
               x# = 0
            END

            thisText = CLIP(LEFT(AsciiFile.Buffer))

            !replace tabs with spaces
            p = LEN(thisText)
            LOOP n = 1 TO p
               IF thisText[n] = '<9>'
                  thisText[n] = ' '
               END
            END
            thisText = CLIP(LEFT(thisText))

            IF thisText = ''
               IF NOT INSTRING('(',lastText)
                  lastText = ''
               END
               CYCLE
            ELSE

               !remove comments
               IF thisText[1] = ''''
                  CYCLE
               ELSE
                  p = INSTRING(' ''',thisText,1)
                  IF p > 1
                     thisText = CLIP(thisText[1 : p-1])
                   END
               END

               !remove anything in double quotes
               n = 0
               clText = ''
               LOOP p = 1 TO LEN(thisText)
                  IF thisText[p] = '"'
                     n += 1
                     clText[n] = thisText[p]
                     p += 1
                     p = INSTRING('"',thisText,1,p)
                     IF p = 0
                        clText = thisText
                        BREAK
                     ELSE
                        LOOP WHILE thisText[p-1] = '"'  !was it escaped?
                           p = INSTRING('"',thisText,1,p+1)
                           IF p = 0
                              clText = thisText
                              BREAK
                           END
                        END
                        IF p = 0
                           BREAK
                        ELSE
                           n += 1
                           clText[n] = thisText[p]
                        END
                     END
                  ELSE
                     n += 1
                     clText[n] = thisText[p]
                  END
               END
               IF p > LEN(thisText)
                  clText[n+1] = '<0>'
                  thisText = clText
               END
               clText = ''

               IF InMethod
                  !look for terminator
                  IF INSTRING(MethodTerminator,UPPER(CLIP(LEFT(thisText))),1)
                     InMethod = FALSE
                  END
                  CYCLE
               ELSE   !not in method
                  p = INSTRING('NAMESPACE ',UPPER(thisText),1)
                  IF p > 0
                     thisNamespace = CLIP(LEFT(thisText[p+10 : LEN(thisText)]))
                     CYCLE
                  END

                  p = INSTRING('END NAMESPACE',UPPER(thisText),1)
                  IF p > 0
                     thisNamespace = ''
                     CYCLE
                  END

                  p = INSTRING('MODULE ',UPPER(thisText),1)
                  IF p > 0
                     thisClass = CLIP(LEFT(thisText[p+7 : LEN(thisText)]))
                     CYCLE
                  END

                  p = INSTRING('END MODULE',UPPER(thisText),1)
                  IF p > 0
                     thisClass = ''
                     CYCLE
                  END

                  p = INSTRING('CLASS ',UPPER(thisText),1)
                  IF p > 0
                     thisClass = CLIP(LEFT(thisText[p+6 : LEN(thisText)]))
                     p = INSTRING('(',thisClass)
                     IF p > 0
                        thisClass[p] = '<0>'
                     END
                     CYCLE
                  END

                  p = INSTRING('END CLASS',UPPER(thisText),1)
                  IF p > 0
                     thisClass = ''
                     CYCLE
                  END

                  IF INSTRING('(',thisText) AND NOT INSTRING('DECLARE ',UPPER(thisText),1)
                     p = INSTRING('FUNCTION ',UPPER(thisText),1)
                     IF p > 0
                        p += 9
                        n = INSTRING('(',thisText,1,p)
                        IF n > 0
                           thisMethod = CLIP(LEFT(thisText[p : n-1]))
                           InMethod = TRUE
                           MethodTerminator = 'END FUNCTION'
                           ProcNameQueue.LineNo = thisLine
                           ProcNameQueue.ProcedureName = ''
                           IF thisNamespace
                              ProcNameQueue.ProcedureName = thisNamespace & '.'
                           END
                           IF thisClass
                              ProcNameQueue.ProcedureName = ProcNameQueue.ProcedureName & thisClass & '.'
                           END
                           ProcNameQueue.ProcedureName = ProcNameQueue.ProcedureName & thisMethod
                           ADD(ProcNameQueue)
                           !dbx.debugout('[' & ProcNameQueue.LineNo & '] ' & ProcNameQueue.ProcedureName)
                        END
                        CYCLE
                     END

                     p = INSTRING('PROPERTY ',UPPER(thisText),1)
                     IF p > 0
                        p += 9
                        n = INSTRING('(',thisText,1,p)
                        IF n > 0
                           thisMethod = CLIP(LEFT(thisText[p : n-1]))
                           InMethod = TRUE
                           MethodTerminator = 'END PROPERTY'
                           ProcNameQueue.LineNo = thisLine
                           ProcNameQueue.ProcedureName = ''
                           IF thisNamespace
                              ProcNameQueue.ProcedureName = thisNamespace & '.'
                           END
                           IF thisClass
                              ProcNameQueue.ProcedureName = ProcNameQueue.ProcedureName & thisClass & '.'
                           END
                           ProcNameQueue.ProcedureName = ProcNameQueue.ProcedureName & thisMethod
                           ADD(ProcNameQueue)
                           !dbx.debugout('[' & ProcNameQueue.LineNo & '] ' & ProcNameQueue.ProcedureName)
                        END
                        CYCLE
                     END

                     p = INSTRING('SUB ',UPPER(thisText),1)
                     IF p > 0
                        p += 4
                        n = INSTRING('(',thisText,1,p)
                        IF n > 0
                           thisMethod = CLIP(LEFT(thisText[p : n-1]))
                           InMethod = TRUE
                           MethodTerminator = 'END SUB'
                           ProcNameQueue.LineNo = thisLine
                           ProcNameQueue.ProcedureName = ''
                           IF thisNamespace
                              ProcNameQueue.ProcedureName = thisNamespace & '.'
                           END
                           IF thisClass
                              ProcNameQueue.ProcedureName = ProcNameQueue.ProcedureName & thisClass & '.'
                           END
                           ProcNameQueue.ProcedureName = ProcNameQueue.ProcedureName & thisMethod
                           ADD(ProcNameQueue)
                           !dbx.debugout('[' & ProcNameQueue.LineNo & '] ' & ProcNameQueue.ProcedureName)
                        END
                        CYCLE
                     END
                  ELSE
                     CYCLE
                  END
               END   !IF InMethod
            END
         END
      END
   EXIT
!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
BrowseQueues PROCEDURE (*ProcNameQueueType ProcNameQueue,*SectionQueueType SectionQueue)

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
Window               WINDOW('Location Queues'),AT(,,476,225),FONT('Segoe UI Semilight',11),DOUBLE,GRAY,MODAL,SYSTEM
                       LIST,AT(16,16,211,196),USE(?LIST1),VSCROLL,FORMAT('40R(1)|M~LINE~C(0)@n10@1024L(2)|M~PR' & |
  'OCEDURE~@s255@'),FROM(ProcNameQueue)
                       LIST,AT(241,16,217,196),USE(?LIST2),VSCROLL,FORMAT('40R(1)|M~HIGH LINE~C(0)@n10@40R(1)|' & |
  'M~LOW LINE~C(0)@n10@124L(2)|M~SECTION~@s255@'),FROM(SectionQueue)
                       STRING('Procedure Queue'),AT(16,3),USE(?STRING1),FONT(,14)
                       STRING('Section Queue'),AT(240,3),USE(?STRING2),FONT(,14)
                     END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
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
    
  GlobalErrors.SetProcedureName('BrowseQueues')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?LIST1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.Open(Window)                                        ! Open window
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  ?LIST1{PROP:LineHeight} = 10
  ?LIST2{PROP:LineHeight} = 10
  Do DefineListboxStyle
  Alert(AltKeyPressed)  ! WinEvent : These keys cause a program to crash on Windows 7 and Windows 10.
  Alert(F10Key)         !
  Alert(CtrlF10)        !
  Alert(ShiftF10)       !
  Alert(CtrlShiftF10)   !
  Alert(AltSpace)       !
 ! WinAlertMouseZoom()
 ! WinAlert(WE::WM_QueryEndSession,,Return1+PostUser)
  Window{Prop:Alrt,255} = CtrlShiftP
  INIMgr.Fetch('BrowseQueues',Window)                      ! Restore window settings from non-volatile store
  SELF.SetAlerts()
  oHH &= NEW tagHTMLHelp
  oHH.Init( 'kss.chm' )
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
 ! If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.Opened
    INIMgr.Update('BrowseQueues',Window)                   ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  IF ~oHH &= NULL
    oHH.Kill()
    DISPOSE( oHH )
  END
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

