

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
!!! Create a child process that uses the previously created pipes for STDIN and STDOUT.
!!! </summary>
CreateChildProcess   PROCEDURE  (*CSTRING szCmdLine)       ! Declare Procedure
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
!szCmdline   CSTRING('findstr /s /i /n /p "keystone" "*.inc *.clw"')
siStartInfo LIKE(STARTUPINFOTYPE)
bSuccess    BOOL(FALSE)

  CODE
    
      ! Create a child process that uses the previously created pipes for STDIN and STDOUT.
      ! Set up members of the PROCESS_INFORMATION structure.
      kcr_ZeroMemory(ADDRESS(piProcInfo), SIZE(PROCESS_INFORMATION))

      ! Set up members of the STARTUPINFO structure.
      ! This structure specifies the STDIN and STDOUT handles for redirection.

      kcr_ZeroMemory(ADDRESS(siStartInfo), SIZE(STARTUPINFOTYPE))
      siStartInfo.cb = SIZE(STARTUPINFOTYPE);
      siStartInfo.hStdError = g_hChildStd_OUT_Wr
      siStartInfo.hStdOutput = g_hChildStd_OUT_Wr
      siStartInfo.hStdInput = g_hChildStd_IN_Rd
      siStartInfo.dwFlags = BOR(siStartInfo.dwFlags,STARTF_USESTDHANDLES)
      siStartInfo.dwFlags = BOR(siStartInfo.dwFlags,STARTF_USESHOWWINDOW)
      siStartInfo.wShowWindow = SW_HIDE
      ! Create the child process.
      bSuccess = kcr_CreateProcess(0,              |  ! application name
                                   szCmdline,      |  ! command line
                                   0,              |  ! process security attributes
                                   0,              |  ! primary thread security attributes
                                   TRUE,           |  ! handles are inherited
                                   0,              |  ! creation flags
                                   0,              |  ! use parent's environment
                                   0,              |  ! use parent's current directory
                                   siStartInfo,    |  ! STARTUPINFO pointer
                                   piProcInfo)        ! receives PROCESS_INFORMATION

      ! If an error occurs, exit the application.
      IF bSuccess = TRUE
         ! Close handles to the child process and its primary thread.
         ! Some applications might keep these handles to monitor the status
         ! of the child process, for example.
         !kcr_CloseHandle(piProcInfo.hProcess)
         kcr_CloseHandle(piProcInfo.hThread)
      ELSE
         !MESSAGE('CreateProcess')
      END
      RETURN
