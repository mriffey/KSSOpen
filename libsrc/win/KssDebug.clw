!region Notices
! ================================================================================
! Notice : Distributed under the MIT License (https://opensource.org/licenses/MIT)
!
!    This file is part of KSSOpen, a derived work of Devuna-KwikSourceSearch
!    (https://github.com/Devuna/Devuna-KwikSourceSearch).
! ================================================================================
!endregion Notices

  MEMBER()
  MAP
    MODULE('kernel32')
      kd_OutputDebugString(*CSTRING lpOutputString),PASCAL,RAW,NAME('OutputDebugStringA')
    END
  END
  INCLUDE('KssDebug.inc'),ONCE

!-----------------------------------------------------------------------------
KssDebug.Construct   PROCEDURE
  CODE
  SELF.Prefix = '!'

!-----------------------------------------------------------------------------
KssDebug.Debug       PROCEDURE(STRING pDebugString)
szOut                  &CSTRING
  CODE
  IF SELF.DebugOff
     RETURN
  END
  szOut &= NEW CSTRING(LEN(SELF.Prefix) + LEN(CLIP(pDebugString)) + 1)
  szOut = SELF.Prefix & CLIP(pDebugString)
  kd_OutputDebugString(szOut)
  DISPOSE(szOut)
