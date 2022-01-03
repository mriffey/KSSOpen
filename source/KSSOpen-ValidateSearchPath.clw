

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
ValidateSearchPath   PROCEDURE  (*CSTRING szSearchPath)    ! Declare Procedure
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
pStart              LONG
pDelimiter          LONG
szPath              CSTRING(MAXPATH)
errFlag             BOOL
errMsg              CSTRING(MAXPATH)

  CODE
    
  errFlag = FALSE
  pStart = 1
  pDelimiter = INSTRING(';',szSearchPath,1,pStart)
  LOOP WHILE pDelimiter > 0
     szPath = szSearchPath[pStart : pDelimiter-1] & '\'
     IF NOT EXISTS(szPath)
        errMsg = szPath & '|'
        errFlag = TRUE
        BREAK
     ELSE
        pStart = pDelimiter+1
        pDelimiter = INSTRING(';',szSearchPath,1,pStart)
     END
  END
  IF NOT errFlag
     pDelimiter = LEN(szSearchPath)
     szPath = szSearchPath[pStart : pDelimiter] & '\'
     IF NOT EXISTS(szPath)
        errMsg = szPath & '|'
        errFlag = TRUE
     END
  END
  IF errFlag = TRUE
     errMsg[LEN(errMsg)] = '<0>'
     MESSAGE('Folder in search path not found!|' & errMsg,'Validation Error',ICON:Hand)
  END
  RETURN errFlag
