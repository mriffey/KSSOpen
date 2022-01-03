

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
LoadFileExtensionQueue PROCEDURE  (FileModeQueueType FileModeQueue) ! Declare Procedure
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
FileModes      QUEUE(FILE:queue),PRE(fm)
               END
bClarionFound  BOOL(FALSE)
bTextFound     BOOL(FALSE)
szSearchFolder CSTRING(260)
FileModeMenu   CSTRING(1025)

  CODE
    
      FREE(FileExtensionQueue)

      !get user properties
      szSearchFolder = svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS\')
      DO GetFileExtensions

      !get system properties
      szSearchFolder = LONGPATH() & '\'
      DO GetFileExtensions

      !if clarion properties file not found
      !add the default file extensions
      !clw cln equ inc int prj tpl tpw trn txa txd txr
      IF bClarionFound = FALSE
         DO AddClarionDefault
      END

      !if text properties file not found
      !add the default file extensions
      IF bTextFound = FALSE
         DO AddTextDefault
      END

      !setup the file mode menu
      DO SetupFileModeMenu

      RETURN FileModeMenu
GetFileExtensions    ROUTINE
   DATA
i              LONG
j              LONG
cc             LONG
pStart         LONG
pSpace         LONG
szPath         CSTRING(MAXPATH+1)
szDrive        CSTRING(MAXDRIVE+1)
szDir          CSTRING(MAXDIR+1)
szName         CSTRING(MAXFILE+1)
szExtension    CSTRING(MAXEXT+1)
szProperties   CSTRING(MAXPATH)
szBuffer       CSTRING(SIZE(AsciiFile.Buffer)+1)
szLexer        LIKE(FileExtensionQueue.Lexer)
nLexer         LONG

   CODE
      szLexer = 'text'
      FREE(FileModes)
      !IF szSearchFolder[LEN(szSearchFolder)] <> '\'
      !   szSearchFolder = szSearchFolder & '\'
      !END
      DIRECTORY(FileModes,szSearchFolder & '*.properties',0)
      j = RECORDS(FileModes)
      IF j > 0
         LOOP i = 1 TO j
            GET(FileModes,i)
            szPath = CLIP(FileModes.Name)
            cc = kcr_fnSplit(szPath, szDrive, szDir, szName, szExtension)
            CASE LOWER(szName)
              OF 'clarion'
                 bClarionFound = TRUE
              OF 'text'
                 bTextFound = TRUE
            END
            AsciiFilename = szSearchFolder & szPath
            OPEN(AsciiFile,ReadOnly+DenyNone)
            IF NOT ERRORCODE()
               SET(AsciiFile)
               NEXT(AsciiFile)
               LOOP UNTIL ERRORCODE()
                  szBuffer = CLIP(LEFT(AsciiFile.Buffer))
                  IF UPPER(szBuffer[1 : 6]) = 'LEXER='
                     szLexer = LOWER(CLIP(LEFT(szBuffer[7 : LEN(szBuffer)])))
                     CASE szLexer
                       OF 'clarion'
                          nLexer = SCLEX_CLWNOCASE
                       OF 'cpp'
                          nLexer = SCLEX_CPP
                       OF 'html'
                          nLexer = SCLEX_HTML
                       OF 'text'
                          nLexer = SCLEX_NULL
                     ELSE
                        IF NUMERIC(szLexer)
                           nLexer = szLexer
                        ELSIF szLexer[1 : 6] = 'sclex_'
                           nLexer = GetLexerNumber(szLexer)
                        ELSE
                           nLexer = SCLEX_NULL
                        END
                        szLexer = 'text'
                     END
                  ELSIF UPPER(AsciiFile.Buffer[1 : 7]) = '[FILEPA'   !File Patterns
                     !pStart = 1
                     NEXT(AsciiFile)
                     LOOP UNTIL ERRORCODE()
                        pStart = 1
                        szBuffer = CLIP(LEFT(AsciiFile.Buffer))
                        IF szBuffer[1] = '['
                           BREAK
                        ELSIF szBuffer <> '' AND szBuffer[1] <> '!'
                           !clw cln equ inc int prj tpl tpw trn txa txd txr
                           pSpace = INSTRING(' ',szBuffer,,pStart)
                           LOOP WHILE pSpace > 0
                              FileExtensionQueue.Extension = UPPER(szBuffer[pStart : pSpace-1])
                              GET(FileExtensionQueue,FileExtensionQueue.Extension)
                              !IF ERRORCODE() !not found
                                 FileExtensionQueue.Extension = UPPER(szBuffer[pStart : pSpace-1])
                                 FileExtensionQueue.FileMode = LOWER(szName)
                                 FileExtensionQueue.Lexer = szLexer
                                 FileExtensionQueue.nLexer = nLexer
                                 ADD(FileExtensionQueue,+FileExtensionQueue.Extension)
                              !END
                              pStart = pSpace+1
                              pSpace = INSTRING(' ',szBuffer,,pStart)
                           END
                           FileExtensionQueue.Extension = UPPER(szBuffer[pStart : LEN(szBuffer)])
                           GET(FileExtensionQueue,FileExtensionQueue.Extension)
                           !IF ERRORCODE() !not found
                              FileExtensionQueue.Extension = UPPER(szBuffer[pStart : LEN(szBuffer)])
                              FileExtensionQueue.FileMode = LOWER(szName)
                              FileExtensionQueue.Lexer = szLexer
                              FileExtensionQueue.nLexer = nLexer
                              ADD(FileExtensionQueue,+FileExtensionQueue.Extension)
                           !END
                        END
                        NEXT(AsciiFile)
                     END
                     BREAK
                  END
                  NEXT(AsciiFile)
               END
               CLOSE(AsciiFile)
            ELSE
               MESSAGE('Unexpected error (' & ERRORCODE() & ') opening ' & AsciiFilename & '|' & ERROR(),'Load File Extension Queue',ICON:HAND)
            END
         END
      END

AddClarionDefault    ROUTINE
   DATA
i              LONG
j              LONG

   CODE
      FileModes.Name = LONGPATH() & '\clarion.properties'      
      ADD(FileModes,+FileModes.Name)
      LOOP i = 1 TO 13
         EXECUTE i
            FileExtensionQueue.Extension = '*.CLA'
            FileExtensionQueue.Extension = '*.CLW'
            FileExtensionQueue.Extension = '*.CLN'
            FileExtensionQueue.Extension = '*.EQU'
            FileExtensionQueue.Extension = '*.INC'
            FileExtensionQueue.Extension = '*.INT'
            FileExtensionQueue.Extension = '*.PRJ'
            FileExtensionQueue.Extension = '*.TPL'
            FileExtensionQueue.Extension = '*.TPW'
            FileExtensionQueue.Extension = '*.TRN'
            FileExtensionQueue.Extension = '*.TXA'
            FileExtensionQueue.Extension = '*.TXD'
            FileExtensionQueue.Extension = '*.TXR'
         END
         GET(FileExtensionQueue,FileExtensionQueue.Extension)
         IF ERRORCODE() !not found
            EXECUTE i
               FileExtensionQueue.Extension = '*.CLA'
               FileExtensionQueue.Extension = '*.CLW'
               FileExtensionQueue.Extension = '*.CLN'
               FileExtensionQueue.Extension = '*.EQU'
               FileExtensionQueue.Extension = '*.INC'
               FileExtensionQueue.Extension = '*.INT'
               FileExtensionQueue.Extension = '*.PRJ'
               FileExtensionQueue.Extension = '*.TPL'
               FileExtensionQueue.Extension = '*.TPW'
               FileExtensionQueue.Extension = '*.TRN'
               FileExtensionQueue.Extension = '*.TXA'
               FileExtensionQueue.Extension = '*.TXD'
               FileExtensionQueue.Extension = '*.TXR'
            END
            FileExtensionQueue.FileMode = 'clarion'
            FileExtensionQueue.Lexer = 'clarion'
            FileExtensionQueue.nLexer = SCLEX_CLWNOCASE
            ADD(FileExtensionQueue,+FileExtensionQueue.Extension)
         END
      END
      
      IF EXISTS(svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS') & '\clarion.properties') = TRUE
      ELSE
         COPY(FileModes.Name,svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS') & '\clarion.properties') ! in case youre running this from IDE and never installed it.
      END 
   EXIT

AddTextDefault    ROUTINE
   DATA
i              LONG
j              LONG

   CODE
      FileModes.Name = LONGPATH() & '\text.properties'
      ADD(FileModes,+FileModes.Name)
      LOOP i = 1 TO 3
         EXECUTE i
            FileExtensionQueue.Extension = '*.ASC'
            FileExtensionQueue.Extension = '*.CSV'
            FileExtensionQueue.Extension = '*.TXT'
         END
         GET(FileExtensionQueue,FileExtensionQueue.Extension)
         IF ERRORCODE() !not found
            EXECUTE i
               FileExtensionQueue.Extension = '*.ASC'
               FileExtensionQueue.Extension = '*.CSV'
               FileExtensionQueue.Extension = '*.TXT'
            END
            FileExtensionQueue.FileMode = 'text'
            FileExtensionQueue.Lexer = 'text'
            FileExtensionQueue.nLexer = SCLEX_NULL
            ADD(FileExtensionQueue,+FileExtensionQueue.Extension)
         END
      END
      
      IF EXISTS(svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS') & '\text.properties') = TRUE
      ELSE
         COPY(FileModes.Name,svSpecialFolder.GetDir(SV:CSIDL_APPDATA, 'Devuna' & '\' & 'KSS') & '\text.properties') ! in case youre running this from IDE and never installed it.
      END 
   EXIT

SetupFileModeMenu    ROUTINE
   DATA
i              LONG
j              LONG
cc             LONG
szPath         CSTRING(MAXPATH+1)
szDrive        CSTRING(MAXDRIVE+1)
szDir          CSTRING(MAXDIR+1)
szName         CSTRING(MAXFILE+1)
szExtension    CSTRING(MAXEXT+1)

   CODE
      j = RECORDS(FileExtensionQueue)
      FREE(FileModeQueue)
      FileModeQueue.FileMode = ''
      LOOP i = 1 TO j
         GET(FileExtensionQueue,i)
         IF FileModeQueue.FileMode <> FileExtensionQueue.FileMode
            FileModeQueue.FileMode = FileExtensionQueue.FileMode
            GET(FileModeQueue,+FileModeQueue.FileMode)
            IF ERRORCODE()
               FileModeQueue.FileMode = FileExtensionQueue.FileMode
               FileModeQueue.Lexer = FileExtensionQueue.Lexer
               FileModeQueue.nLexer = FileExtensionQueue.nLexer
               ADD(FileModeQueue,+FileModeQueue.FileMode)
            END
         END
      END

      FileModeMenu = 'File Mode{{'
      j = RECORDS(FileModeQueue)
      LOOP i = 1 TO j
         GET(FileModeQueue,i)
         FileModeMenu = FileModeMenu & FileModeQueue.FileMode & '|'
      END
      FileModeMenu[LEN(FileModeMenu)] = '}'
   EXIT
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
GetLexerNumber       PROCEDURE  (*CSTRING szLexer)         ! Declare Procedure
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
nLexer        LONG
oHH           &tagHTMLHelp

  CODE
    
      CASE UPPER(szLexer)
      OF 'SCLEX_NULL'
          nLexer = SCLEX_NULL
      OF 'SCLEX_PYTHON'
          nLexer = SCLEX_PYTHON
      OF 'SCLEX_CPP'
          nLexer = SCLEX_CPP
      OF 'SCLEX_HTML'
          nLexer = SCLEX_HTML
      OF 'SCLEX_XML'
          nLexer = SCLEX_XML
      OF 'SCLEX_PERL'
          nLexer = SCLEX_PERL
      OF 'SCLEX_SQL'
          nLexer = SCLEX_SQL
      OF 'SCLEX_VB'
          nLexer = SCLEX_VB
      OF 'SCLEX_PROPERTIES'
          nLexer = SCLEX_PROPERTIES
      OF 'SCLEX_ERRORLIST'
          nLexer = SCLEX_ERRORLIST
      OF 'SCLEX_MAKEFILE'
          nLexer = SCLEX_MAKEFILE
      OF 'SCLEX_BATCH'
          nLexer = SCLEX_BATCH
      OF 'SCLEX_XCODE'
          nLexer = SCLEX_XCODE
      OF 'SCLEX_LATEX'
          nLexer = SCLEX_LATEX
      OF 'SCLEX_LUA'
          nLexer = SCLEX_LUA
      OF 'SCLEX_DIFF'
          nLexer = SCLEX_DIFF
      OF 'SCLEX_CONF'
          nLexer = SCLEX_CONF
      OF 'SCLEX_PASCAL'
          nLexer = SCLEX_PASCAL
      OF 'SCLEX_AVE'
          nLexer = SCLEX_AVE
      OF 'SCLEX_ADA'
          nLexer = SCLEX_ADA
      OF 'SCLEX_LISP'
          nLexer = SCLEX_LISP
      OF 'SCLEX_RUBY'
          nLexer = SCLEX_RUBY
      OF 'SCLEX_EIFFEL'
          nLexer = SCLEX_EIFFEL
      OF 'SCLEX_EIFFELKW'
          nLexer = SCLEX_EIFFELKW
      OF 'SCLEX_TCL'
          nLexer = SCLEX_TCL
      OF 'SCLEX_NNCRONTAB'
          nLexer = SCLEX_NNCRONTAB
      OF 'SCLEX_BULLANT'
          nLexer = SCLEX_BULLANT
      OF 'SCLEX_VBSCRIPT'
          nLexer = SCLEX_VBSCRIPT
      OF 'SCLEX_ASP'
          nLexer = SCLEX_ASP
      OF 'SCLEX_PHP'
          nLexer = SCLEX_PHP
      OF 'SCLEX_BAAN'
          nLexer = SCLEX_BAAN
      OF 'SCLEX_MATLAB'
          nLexer = SCLEX_MATLAB
      OF 'SCLEX_SCRIPTOL'
          nLexer = SCLEX_SCRIPTOL
      OF 'SCLEX_ASM'
          nLexer = SCLEX_ASM
      OF 'SCLEX_CPPNOCASE'
          nLexer = SCLEX_CPPNOCASE
      OF 'SCLEX_FORTRAN'
          nLexer = SCLEX_FORTRAN
      OF 'SCLEX_F77'
          nLexer = SCLEX_F77
      OF 'SCLEX_CSS'
          nLexer = SCLEX_CSS
      OF 'SCLEX_POV'
          nLexer = SCLEX_POV
      OF 'SCLEX_LOUT'
          nLexer = SCLEX_LOUT
      OF 'SCLEX_ESCRIPT'
          nLexer = SCLEX_ESCRIPT
      OF 'SCLEX_PS'
          nLexer = SCLEX_PS
      OF 'SCLEX_NSIS'
          nLexer = SCLEX_NSIS
      OF 'SCLEX_MMIXAL'
          nLexer = SCLEX_MMIXAL
      OF 'SCLEX_CLARION'
          nLexer = SCLEX_CLARION
      OF 'SCLEX_CLWNOCASE'
          nLexer = SCLEX_CLWNOCASE
      OF 'SCLEX_LOT'
          nLexer = SCLEX_LOT
      OF 'SCLEX_YAML'
          nLexer = SCLEX_YAML
      OF 'SCLEX_TEX'
          nLexer = SCLEX_TEX
      OF 'SCLEX_METAPOST'
          nLexer = SCLEX_METAPOST
      OF 'SCLEX_POWERBASIC'
          nLexer = SCLEX_POWERBASIC
      OF 'SCLEX_FORTH'
          nLexer = SCLEX_FORTH
      OF 'SCLEX_ERLANG'
          nLexer = SCLEX_ERLANG
      OF 'SCLEX_OCTAVE'
          nLexer = SCLEX_OCTAVE
      OF 'SCLEX_MSSQL'
          nLexer = SCLEX_MSSQL
      OF 'SCLEX_VERILOG'
          nLexer = SCLEX_VERILOG
      OF 'SCLEX_KIX'
          nLexer = SCLEX_KIX
      OF 'SCLEX_GUI4CLI'
          nLexer = SCLEX_GUI4CLI
      OF 'SCLEX_SPECMAN'
          nLexer = SCLEX_SPECMAN
      OF 'SCLEX_AU3'
          nLexer = SCLEX_AU3
      OF 'SCLEX_APDL'
          nLexer = SCLEX_APDL
      OF 'SCLEX_BASH'
          nLexer = SCLEX_BASH
      OF 'SCLEX_ASN1'
          nLexer = SCLEX_ASN1
      OF 'SCLEX_VHDL'
          nLexer = SCLEX_VHDL
      OF 'SCLEX_CAML'
          nLexer = SCLEX_CAML
      OF 'SCLEX_BLITZBASIC'
          nLexer = SCLEX_BLITZBASIC
      OF 'SCLEX_PUREBASIC'
          nLexer = SCLEX_PUREBASIC
      OF 'SCLEX_HASKELL'
          nLexer = SCLEX_HASKELL
      OF 'SCLEX_PHPSCRIPT'
          nLexer = SCLEX_PHPSCRIPT
      OF 'SCLEX_TADS3'
          nLexer = SCLEX_TADS3
      OF 'SCLEX_REBOL'
          nLexer = SCLEX_REBOL
      OF 'SCLEX_SMALLTALK'
          nLexer = SCLEX_SMALLTALK
      OF 'SCLEX_FLAGSHIP'
          nLexer = SCLEX_FLAGSHIP
      OF 'SCLEX_CSOUND'
          nLexer = SCLEX_CSOUND
      OF 'SCLEX_FREEBASIC'
          nLexer = SCLEX_FREEBASIC
      OF 'SCLEX_INNOSETUP'
          nLexer = SCLEX_INNOSETUP
      OF 'SCLEX_OPAL'
          nLexer = SCLEX_OPAL
      OF 'SCLEX_SPICE'
          nLexer = SCLEX_SPICE
      OF 'SCLEX_D'
          nLexer = SCLEX_D
      OF 'SCLEX_CMAKE'
          nLexer = SCLEX_CMAKE
      OF 'SCLEX_GAP'
          nLexer = SCLEX_GAP
      OF 'SCLEX_PLM'
          nLexer = SCLEX_PLM
      OF 'SCLEX_PROGRESS'
          nLexer = SCLEX_PROGRESS
      OF 'SCLEX_ABAQUS'
          nLexer = SCLEX_ABAQUS
      OF 'SCLEX_ASYMPTOTE'
          nLexer = SCLEX_ASYMPTOTE
      OF 'SCLEX_R'
          nLexer = SCLEX_R
      OF 'SCLEX_MAGIK'
          nLexer = SCLEX_MAGIK
      OF 'SCLEX_POWERSHELL'
          nLexer = SCLEX_POWERSHELL
      OF 'SCLEX_MYSQL'
          nLexer = SCLEX_MYSQL
      OF 'SCLEX_PO'
          nLexer = SCLEX_PO
      OF 'SCLEX_TAL'
          nLexer = SCLEX_TAL
      OF 'SCLEX_COBOL'
          nLexer = SCLEX_COBOL
      OF 'SCLEX_TACL'
          nLexer = SCLEX_TACL
      OF 'SCLEX_SORCUS'
          nLexer = SCLEX_SORCUS
      OF 'SCLEX_POWERPRO'
          nLexer = SCLEX_POWERPRO
      OF 'SCLEX_NIMROD'
          nLexer = SCLEX_NIMROD
      OF 'SCLEX_SML'
          nLexer = SCLEX_SML
      OF 'SCLEX_MARKDOWN'
          nLexer = SCLEX_MARKDOWN
      OF 'SCLEX_TXT2TAGS'
          nLexer = SCLEX_TXT2TAGS
      OF 'SCLEX_A68K'
          nLexer = SCLEX_A68K
      OF 'SCLEX_MODULA'
          nLexer = SCLEX_MODULA
      OF 'SCLEX_AUTOMATIC'
          nLexer = SCLEX_AUTOMATIC
    ELSE
          nLexer = SCLEX_NULL
    END
    RETURN nLexer
