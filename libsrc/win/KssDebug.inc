!region Notices
! ================================================================================
! Notice : Distributed under the MIT License (https://opensource.org/licenses/MIT)
!
!    This file is part of KSSOpen, a derived work of Devuna-KwikSourceSearch
!    (https://github.com/Devuna/Devuna-KwikSourceSearch).
!
!    KssDebug is a minimal replacement for the parts of UltimateDebug that KSSOpen
!    used: Debug() sends a line to OutputDebugString (view it with DebugView),
!    and DebugOff suppresses output.
! ================================================================================
!endregion Notices

KssDebug             CLASS,TYPE,MODULE('KssDebug.clw'),LINK('KssDebug.clw')
DebugOff               BYTE(FALSE)               ! TRUE suppresses all output
Prefix                 CSTRING(21)               ! Prepended to every line ('!' by default)
Construct              PROCEDURE
Debug                  PROCEDURE(STRING pDebugString)
                     END
