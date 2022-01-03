

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
MatchWithoutComment  PROCEDURE  (*CSTRING szText, *CSTRING szPattern, BYTE bMatchMode, * CSTRING szExtension) ! Declare Procedure
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
CommentStyle         LONG
bInstring            BOOL(FALSE)
pCommentMarker       LONG
pMatch               LONG
szMatchText          CSTRING(1025)
CommentMarker        CSTRING(3)
StatementQueue       QUEUE
statement               &CSTRING
                     END
clText               &CSTRING
i                    LONG
j                    LONG

  CODE
    
      CommentStyle = GetCommentStyle(szExtension)
      clText &= NEW CSTRING(SIZE(szText)+1)
      clText = szText
      j = LEN(clText)
      LOOP i = 1 TO j
         !replace tab characters with space
         IF clText[i] = '<9>'
            clText[i] = ' '
         END
      END
      clText = CLIP(LEFT(clText))
      IF CommentStyle = CommentStyleCpp
           IF clText[1 : 2] = '//'
              pMatch = 0
           ELSE
              DO ProcessText
           END
      ELSIF CommentStyle = CommentStyleClarion
           IF clText[1] = '!'
              pMatch = 0
           ELSE
              DO ProcessText
           END
      ELSIF CommentStyle = CommentStyleVB
           IF clText[1] = ''''
              pMatch = 0
           ELSE
              DO ProcessText
           END
      ELSIF CommentStyle = CommentStylePython
           IF clText[1] = '#'
              pMatch = 0
           ELSE
              DO ProcessText
           END
      ELSIF CommentStyle = CommentStyleSQL
           IF clText[1 : 2] = '--'
              pMatch = 0
           ELSE
              DO ProcessText
           END
      ELSE
         pMatch = TRUE
      END
      DISPOSE(clText)
      RETURN pMatch

ProcessText   ROUTINE
   DATA
indx                LONG

   CODE
      !break text into multiple statements
      DO SplitTextIntoStatements
      !now loop through the queue and examine each statement on the line
      LOOP indx = 1 TO RECORDS(StatementQueue)
         GET(StatementQueue,indx)

         !find comment marker not in a string
         IF CommentStyle = CommentStyleCpp
              CommentMarker = '//'
              DO LookForCommentMarker
         ELSIF CommentStyle = CommentStyleVB
              CommentMarker = ''''
              DO LookForCommentMarker
         ELSIF CommentStyle = CommentStylePython
              CommentMarker = '#'
              DO LookForCommentMarker
         ELSIF CommentStyle = CommentStyleSQL
              CommentMarker = '--'
              DO LookForCommentMarker
         ELSE
              CommentMarker = '|'
              DO LookForCommentMarker
              IF NOT pCommentMarker
                 CommentMarker = '!'
                 DO LookForCommentMarker
              END
         END

         DO LookForMatch
         IF pMatch
            BREAK
         END
      END

      !clean up newed strings
      LOOP indx = 1 TO RECORDS(StatementQueue)
         GET(StatementQueue,indx)
         DISPOSE(StatementQueue.statement)
         !ASSERT(0,eqDBG & 'DISPOSE statement [' & ADDRESS(StatementQueue.statement) & ']')
         StatementQueue.statement &= NULL
         PUT(StatementQueue)
      END
      FREE(StatementQueue)
SplitTextIntoStatements ROUTINE
   DATA
i           LONG
j           LONG
chString    CSTRING(2)
chSeparator CSTRING(2)

   CODE
      CASE CommentStyle
        OF CommentStyleClarion
           chString = ''''
           chSeparator = ';'
        OF CommentStyleCpp
           chString = ''''
           chSeparator = ';'
        OF CommentStyleVB
           chString = '"'
           chSeparator = ':'
        OF CommentStylePython
           chString = '"'
           chSeparator = ';'
        OF CommentStyleSQL
           chString = '"'
           chSeparator = ';'
      END
      j = 1
      LOOP i = 1 TO LEN(szText)
        IF szText[i] = chString
           IF bInstring = TRUE
              IF i < LEN(szText)
                 IF szText[i+1] = chString
                    i += 1
                    !escaped quote so still in string
                 ELSE
                    bInstring = 1 - bInstring
                 END
              ELSE
                 bInString = FALSE
              END
           ELSE
              bInstring = TRUE
           END
        ! szRetVal      = CLIP(xFolder) !~Mar/16/09 added CLIP                        !; !ASSERT(0,eqDBG&'szRetVal['& szRetVal &']')
        ELSIF ((CommentStyle = CommentStyleClarion) AND (szText[i] = '!' OR szText[i] = '|'))   OR |
              ((CommentStyle = CommentStyleCpp)     AND (szText[i : i+1] = '//'))               OR |
              ((CommentStyle = CommentStyleVB)      AND (szText[i] = ''''))                     OR |
              ((CommentStyle = CommentStylePython)  AND (szText[i] = '#'))                      OR |
              ((CommentStyle = CommentStyleSQL)     AND (szText[i : i+1] = '--'))
           IF bInstring = FALSE
              !rest of line is a comment
              StatementQueue.statement &= NEW CSTRING((i-j)+1)
              StatementQueue.statement = szText[j : i-1]
              ADD(StatementQueue)
              i = LEN(szText)
              j = i + 1
           END
        ELSIF szText[i] = chSeparator
           IF bInstring = FALSE
              StatementQueue.statement &= NEW CSTRING((i-j)+1)
              !ASSERT(0,eqDBG & 'NEW statement [' & ADDRESS(StatementQueue.statement) & ']')
              StatementQueue.statement = szText[j : i-1]
              ADD(StatementQueue)
              j = i + 1
           END
        ELSE
        END
      END
      IF i > j
         StatementQueue.statement &= NEW CSTRING((i-j)+1)
         !ASSERT(0,eqDBG & 'NEW statement [' & ADDRESS(StatementQueue.statement) & ']')
         StatementQueue.statement = szText[j : i-1]
         ADD(StatementQueue)
      END
   EXIT
LookForCommentMarker    ROUTINE
   DATA
pStringEnd          LONG
pStringStart        LONG

   CODE
      LOOP pCommentMarker = 1 TO LEN(StatementQueue.statement)
         IF StatementQueue.statement[pCommentMarker : pCommentMarker+(LEN(CommentMarker)-1)] = CommentMarker
            BREAK
         END
      END
      IF pCommentMarker > LEN(StatementQueue.statement)
         pCommentMarker = 0
      END
      pStringStart = 0
      LOOP WHILE pCommentMarker
         LOOP pStringStart = 1 TO LEN(StatementQueue.statement)
            IF StatementQueue.statement[pStringStart] = ''''
               BREAK
            END
         END
         IF pStringStart < pCommentMarker
            LOOP pStringEnd = (pStringStart+1) TO LEN(StatementQueue.statement)
               IF StatementQueue.statement[pStringEnd] = ''''
                  BREAK
               END
            END
            IF pStringEnd > LEN(StatementQueue.statement)
               pStringEnd = 0
            END
            IF pStringEnd
               IF INRANGE(pCommentMarker,pStringStart,pStringEnd)
                  LOOP pCommentMarker = (pStringEnd+1) TO LEN(StatementQueue.statement)
                     IF StatementQueue.statement[pCommentMarker : pCommentMarker+(LEN(CommentMarker)-1)] = CommentMarker
                        BREAK
                     END
                  END
               ELSE
                  BREAK
               END
            ELSE
               BREAK
            END
         ELSE
            BREAK
         END
      END
   EXIT
LookForMatch   ROUTINE
      IF pCommentMarker
         szMatchText = StatementQueue.statement[1 : pCommentMarker-1]
      ELSE
         szMatchText = StatementQueue.statement
      END

      IF BAND(bMatchMode,Match:Regular)   !regular expression
         pMatch = MATCH(szMatchText,szPattern,bMatchMode)
      ELSE
         IF BAND(bMatchMode,Match:NoCase)
            pMatch = INSTRING(UPPER(szPattern),UPPER(szMatchText),1)
         ELSE
            pMatch = INSTRING(szPattern,szMatchText,1)
         END
      END
   EXIT
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
GetCommentStyle      PROCEDURE  (*CSTRING szExtension)     ! Declare Procedure
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
ReturnValue   LONG

  CODE
    
      FileExtensionQueue.Extension = '*' & UPPER(szExtension)
      GET(FileExtensionQueue,FileExtensionQueue.Extension)
      IF ERRORCODE()
         ReturnValue = -1
      ELSE
         CASE FileExtensionQueue.nLexer
           OF SCLEX_CLWNOCASE OROF SCLEX_CLARION
              ReturnValue = CommentStyleClarion
           OF SCLEX_CPP OROF SCLEX_CPPNOCASE
              ReturnValue = CommentStyleCpp
           OF SCLEX_VB OROF SCLEX_VBSCRIPT
              ReturnValue = CommentStyleVB
           OF SCLEX_PYTHON
              ReturnValue = CommentStylePython
           OF SCLEX_SQL OROF SCLEX_MSSQL OROF SCLEX_MYSQL
              ReturnValue = CommentStyleSQL
         ELSE
              ReturnValue = -1
         END
      END
      RETURN ReturnValue
