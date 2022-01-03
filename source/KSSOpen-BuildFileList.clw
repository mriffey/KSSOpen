

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
BuildFileList        PROCEDURE  (LONG pCurrentLevel, LONG pMaxLevel, *CSTRING pCurrentPath, *CSTRING pFileMask, *FILE:queue pFileQueue) ! Declare Procedure
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
i                    LONG
j                    LONG
strSemicolon         CSTRING(';')
strBar               CSTRING('|')
strSpace             CSTRING(' ')
strDelimiter         CSTRING(2)
nCurrentLevel        LONG
DirectoryQueue       QUEUE(FILE:queue),PRE(DirectoryQueue)
                     END
szDirectoryName      CSTRING(FILE:MAXFILENAME+1)

  CODE
    
    IF pCurrentLevel <= pMaxLevel
       !determine filemask delimiter
       IF INSTRING(strSemicolon,pFileMask)
          strDelimiter = strSemicolon
       ELSIF INSTRING(strBar, pFilemask)
          strDelimiter = strBar
       ELSIF INSTRING(strSpace, pFilemask)
          strDelimiter = strSpace
       ELSE
          strDelimiter = ''
       END

       !find all matching files in the current path
       FREE(DirectoryQueue)
       IF LEN(strDelimiter) = 0
          DIRECTORY(DirectoryQueue, pCurrentPath & '\' & pFileMask, ff_:NORMAL)
       ELSE
          i = 1
          j = INSTRING(strDelimiter, pFileMask, 1, i)
          LOOP WHILE j
             DIRECTORY(DirectoryQueue, pCurrentPath & '\' & pFileMask[i : j-1], ff_:NORMAL)
             i = j + 1
             j = INSTRING(strDelimiter, pFileMask, 1, i)
          END
          DIRECTORY(DirectoryQueue, pCurrentPath & '\' & pFileMask[i : LEN(pFileMask)], ff_:NORMAL)
       END
       LOOP i = 1 TO RECORDS(DirectoryQueue)
          GET(DirectoryQueue,i)
          DirectoryQueue.Name = pCurrentPath & '\' & CLIP(DirectoryQueue.Name)
          pFileQueue = DirectoryQueue
          ADD(pFileQueue)
       END

       !find all subdirectories in current path
       FREE(DirectoryQueue)
       DIRECTORY(DirectoryQueue, pCurrentPath & '\*.*', ff_:DIRECTORY)
       LOOP i = 1 TO RECORDS(DirectoryQueue)
          GET(DirectoryQueue,i)
          IF BOR(DirectoryQueue.Attrib,ff_:DIRECTORY)
             CASE CLIP(DirectoryQueue.Name)
               OF '.' OROF '..'
             ELSE
                szDirectoryName = pCurrentPath & '\' & CLIP(DirectoryQueue.Name)
                BuildFileList(pCurrentLevel+1, pMaxLevel, szDirectoryName, pFileMask, pFileQueue)
             END
          END
       END
    END
