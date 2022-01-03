

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
SavePatternToFile    PROCEDURE  (*CSTRING szPattern)       ! Declare Procedure
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
szPathName           CSTRING(MAXPATH)
szPrefixString       CSTRING('KSS')
szFilename           CSTRING(260)                          ! 
argQueue    QUEUE,PRE(aq)
argument       CSTRING(256)
            END

  CODE
    
      i = 1
      j = INSTRING('|',szPattern,1,i)
      LOOP WHILE j
         IF j < LEN(szPattern)
            IF szPattern[j+1] = '|'
               j = INSTRING('|',szPattern,1,j+2)
            ELSE
               argQueue.argument = szPattern[i : j - 1]
               DO RemoveDoubleBars
               ADD(argQueue)
               i = j + 1
               j = INSTRING('|',szPattern,1,i)
            END
         ELSIF J = LEN(szPattern)
            BREAK
         END
      END
      argQueue.argument = szPattern[i : LEN(szPattern)]
      DO RemoveDoubleBars
      ADD(argQueue)

      AsciiFilename = ''
      IF kcr_GetTempPath(SIZE(szPathName),szPathName) > 0
         IF kcr_GetTempFileName(szPathName, szPrefixString, THREAD(), AsciiFilename)
            CREATE(AsciiFile)
            OPEN(AsciiFile)
            LOOP i = 1 TO RECORDS(argQueue)
               GET(argQueue,i)
               AsciiFile.Buffer = argQueue.argument
               APPEND(AsciiFile)
            END
            CLOSE(AsciiFile)
         ELSE
         END
      ELSE
      END
      RETURN AsciiFilename

RemoveDoubleBars  ROUTINE
   DATA
n     LONG
p     LONG
   CODE
      n = 1
      p = INSTRING('||',argQueue.argument,1,n)
      LOOP WHILE p
         argQueue.argument = argQueue.argument[1 : p] & argQueue.argument[p+2 : LEN(argQueue.argument)]
         n = p + 1
         p = INSTRING('||',argQueue.argument,1,n)
      END
