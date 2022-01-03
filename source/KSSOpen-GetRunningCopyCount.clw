

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
GetRunningCopyCount  PROCEDURE  ()                         ! Declare Procedure
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
aProcesses        DWORD,DIM(1024)
cbNeeded          DWORD
cProcesses        DWORD
i                 UNSIGNED
lCopyCount        LONG
szProcessName     CSTRING(MAX_PATH)
hProcess          HANDLE
hMod              HMODULE
thisProcessId     DWORD
hwnd              HWND
dwPID             DWORD
szWindowCaption   CSTRING('Kwik Source Search')
oHH           &tagHTMLHelp

  CODE
    
  thisProcessId = kcr_GetCurrentProcessId()
  lCopyCount = 0
  IF kcr_EnumProcesses( aProcesses[1], SIZE(aProcesses), cbNeeded )
     cProcesses = cbNeeded / 4
     LOOP i = 1 TO cProcesses
       IF aProcesses[i] <> 0
          szProcessName = '<<unknown>'
          hProcess = kcr_OpenProcess(BOR(PROCESS_QUERY_INFORMATION,PROCESS_VM_READ),FALSE,aProcesses[i])
          ! Get the process name.
          IF hProcess <> 0
             IF kcr_EnumProcessModules( hProcess, hMod, SIZE(hMod), cbNeeded)
                kcr_GetModuleBaseName( hProcess, hMod, szProcessName, SIZE(szProcessName))
                IF UPPER(szProcessName) = 'KSS.EXE'
                   IF aProcesses[i] <> thisProcessId
                      hwnd = kcr_FindWindowEx(0, 0, 0, szWindowCaption)
                      IF hwnd = 0
                         !Kaspersky Anti-Virus may be running
                      ELSE
                         kcr_GetWindowThreadProcessId(hwnd, dwPID)
                         IF dwPID <> thisProcessId
                            kcr_ShowWindowAsync(hwnd, SW_RESTORE)
                            kcr_SetForegroundWindow(hwnd)
                         ELSE
                         END
                         lCopyCount += 1
                      END
                   END
!                   lCopyCount += 1
                END
             END
             kcr_CloseHandle(hProcess)
          END
       END
     END
  ELSE
  END
  RETURN lCopyCount
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
ApplicationIsRunning PROCEDURE  ()                         ! Declare Procedure
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
aProcesses              DWORD,DIM(1024)
cbNeeded                DWORD
cProcesses              DWORD
i                       UNSIGNED
lCopyCount              LONG
szProcessName           CSTRING(MAX_PATH)
hProcess                HANDLE
hMod                    HMODULE
thisProcessId           DWORD
hwnd                    HWND
dwPID                   DWORD
szWindowCaption         CSTRING('Kwik Source Search')
AlreadyRunning          BOOL(FALSE)
oHH           &tagHTMLHelp

  CODE
    
  thisProcessId = kcr_GetCurrentProcessId()
  IF kcr_EnumProcesses( aProcesses[1], SIZE(aProcesses), cbNeeded )
     cProcesses = cbNeeded / 4
     LOOP i = 1 TO cProcesses
       IF aProcesses[i] <> 0
          szProcessName = '<<unknown>'
          hProcess = kcr_OpenProcess(BOR(PROCESS_QUERY_INFORMATION,PROCESS_VM_READ),FALSE,aProcesses[i])
          ! Get the process name.
          IF hProcess <> 0
             IF kcr_EnumProcessModules( hProcess, hMod, SIZE(hMod), cbNeeded)
                kcr_GetModuleBaseName( hProcess, hMod, szProcessName, SIZE(szProcessName))
                IF UPPER(szProcessName) = 'KSS.EXE'
                   IF aProcesses[i] <> thisProcessId
                      hwnd = kcr_FindWindowEx(0, 0, 0, szWindowCaption)
                      IF hwnd = 0
                         !maybe Kaspersky anti-virus is running so keep looking
                      ELSE
                         kcr_GetWindowThreadProcessId(hwnd, dwPID)
                         IF dwPID <> thisProcessId
                            kcr_ShowWindowAsync(hwnd, SW_RESTORE)
                            kcr_SetForegroundWindow(hwnd)
                            AlreadyRunning = TRUE
                         END
                      END
                   END
                END
             END
             kcr_CloseHandle(hProcess)
             IF AlreadyRunning = TRUE
                BREAK
             END
          END
       END
     END
  ELSE
  END
  RETURN AlreadyRunning
