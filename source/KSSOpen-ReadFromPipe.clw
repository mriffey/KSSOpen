

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
bStopping      BOOL(FALSE)
stopTicks      LONG

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
         ! ReadFile uses ReadFileParams (a local here) and adds to STDOUT_Queue until it posts
         ! EVENT:User, so this loop only ends after that, or before ReadFile was started.
         ACCEPT
            IF bCancelFlag = TRUE AND bStopping = FALSE
               DO StopReading
               IF thrd = 0 THEN BREAK.
            END
            CASE EVENT()
              OF EVENT:CloseDown
              OROF EVENT:CloseWindow
                 IF thrd = 0 THEN BREAK.
                 IF bStopping = FALSE THEN DO StopReading.
                 CYCLE
              OF EVENT:OpenWindow
                 IF bStopping = FALSE
                    ReadFileParams.ParentThread = THREAD()
                    ReadFileParams.ParentPipe   = g_hChildStd_OUT_Rd
                    ReadFileParams.ParentQueue  &= STDOUT_Queue
                    thrd = START(ReadFile,25000,ReadFileParams)
                    IF thrd = 0 THEN BREAK.
                    RESUME(thrd)
                 END
              OF EVENT:User
                 BREAK                                         ! ReadFile has finished
              OF EVENT:Timer
                 IF bStopping = TRUE
                    stopTicks += 1
                    IF stopTicks >= 10 THEN BREAK.             ! ReadFile did not finish within 10 s
                 ELSE
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

! Cancel: stop findstr so ReadFile's blocking read returns, and ask ReadFile to stop.
StopReading    ROUTINE
      bStopping = TRUE
      kcr_TerminateProcess(piProcInfo.hProcess,0)
      IF thrd
         POST(EVENT:CloseDown,,thrd,0)
      END
   EXIT
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
             BREAK
          OF EVENT:Timer
             ! Read until findstr closes its end of the pipe (ReadFile then fails or returns 0 bytes).
             ! Reading in a loop rather than once per timer tick keeps the pipe drained, so findstr is
             ! never left waiting on a full pipe between ticks. Cancel terminates findstr, which ends the loop.
             w{PROP:timer} = 0
             LOOP
                CLEAR(chBuf,-1)
                bSuccess = kcr_ReadFile( param.ParentPipe, ADDRESS(chBuf), BUFSIZE, ADDRESS(dwRead), 0)
                param.ParentQueue.Buffer = chBuf
                ADD(param.ParentQueue)
                IF bSuccess = FALSE OR dwRead = 0
                   BREAK
                END
             END
             POST(EVENT:CloseWindow)
        END
     END
     CLOSE(w)
     POST(EVENT:User,,param.ParentThread,0)       ! on every exit path: ReadFromPipe waits for this
     RETURN
