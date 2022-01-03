

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
!!! Read output from the child process's pipe for STDOUT
!!! </summary>
ReadFromPipe         PROCEDURE  (LONG feqSearchProgress, *BYTE bCancelFlag) ! Declare Procedure
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
w              WINDOW,TIMER(100)
               END
thrd           LONG
ReadFileParams LIKE(ReadFileParamsType)
progressCalls  LONG

  CODE
    
! Read output from the child process's pipe for STDOUT
! Stop when there is no more data.

! Close the write end of the pipe before reading from the
! read end of the pipe, to control child process execution.
! The pipe is assumed to have enough buffer space to hold the
! data the child process has already written to it.
      progressCalls = 0
      FREE(STDOUT_Queue)
      IF CloseHandle(g_hChildStd_OUT_Wr)
         OPEN(w)
         w{PROP:HIDE} = TRUE
         ACCEPT
            IF bCancelFlag = TRUE
               POST(EVENT:CloseDown,,thrd,0)
               BREAK
            ELSE
               CASE EVENT()
                 OF EVENT:CloseDown
                    BREAK
                 OF EVENT:CloseWindow
                    BREAK
                 OF EVENT:OpenWindow
                    ReadFileParams.ParentThread = THREAD()
                    ReadFileParams.ParentPipe   = g_hChildStd_OUT_Rd
                    ReadFileParams.ParentQueue  &= STDOUT_Queue
                    thrd = START(ReadFile,25000,ReadFileParams)
                    RESUME(thrd)
                 OF EVENT:User
                    POST(EVENT:CloseWindow)
                 OF EVENT:Timer
                    progressCalls += 1
                    POST(EVENT:PROGRESS+feqSearchProgress,,1,1)
                    YIELD()
               END
            END
         END
         CLOSE(w)
      ELSE
         MESSAGE('StdOutWr CloseHandle')
      END
      RETURN progressCalls
!!! <summary>
!!! Generated from procedure template - Source
!!! Run this on it's own thread cause it block until input is available
!!! </summary>
ReadFile             PROCEDURE  (STRING ReadFileParams)    ! Declare Procedure
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
dwRead        DWORD
chBuf         STRING(BUFSIZE)
bSuccess      BOOL
param         LIKE(ReadFileParamsType)
w             WINDOW,AT(0,0,10,10),TIMER(1)
              END

  CODE
    
     param = ReadFileParams
     OPEN(w)
     w{PROP:hide} = TRUE
     ACCEPT
        CASE EVENT()
          OF EVENT:CloseDown
             BREAK
          OF EVENT:CloseWindow
             POST(EVENT:User,,param.ParentThread,0)
             BREAK
          OF EVENT:Timer
             CLEAR(chBuf,-1)
             bSuccess = kcr_ReadFile( param.ParentPipe, ADDRESS(chBuf), BUFSIZE, ADDRESS(dwRead), 0)
             IF bSuccess = FALSE OR dwRead = 0
                w{PROP:timer} = 0
                param.ParentQueue.Buffer = chBuf
                ADD(param.ParentQueue)
                POST(EVENT:CloseWindow)
             ELSE
                param.ParentQueue.Buffer = chBuf
                ADD(param.ParentQueue)
             END
        END
     END
     CLOSE(w)
     RETURN
