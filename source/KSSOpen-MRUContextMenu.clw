

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
MRUContextMenu       PROCEDURE  ( MRUQueueType MRUQueue, LONG feqControl, STRING strDefault) ! Declare Procedure
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
szClipboard   LIKE(MRUQueueType.szValue)
szPopUpMenu   CSTRING(22)
oHH           &tagHTMLHelp

  CODE
    
      UPDATE()
      GET(MRUQueue,CHOICE(feqControl))
      IF strDefault <> '' AND UPPER(MRUQueue.szValue) = UPPER(strDefault)
         !no popup for default value
      ELSE
         szPopUpMenu = 'Cut|Copy|Paste|Delete'
         EXECUTE POPUP(szPopUpMenu)
            BEGIN
               SETCLIPBOARD(MRUQueue.szValue)
               DELETE(MRUQueue)
               feqControl{PROP:From} = MRUQueue
               GET(MRUQueue,1)
               IF ERRORCODE()
                  CHANGE(feqControl,'')
               ELSE
                  CHANGE(feqControl,MRUQueue.szValue)
                  (feqControl{PROP:ListFeq}){PROP:Selected} = POINTER(MRUQueue)
               END
               DISPLAY(feqControl)
            END
            BEGIN
               SETCLIPBOARD(MRUQueue.szValue)
            END
            BEGIN
               szClipboard = CLIPBOARD()
               CHANGE(feqControl,szClipboard)
               DISPLAY(feqControl)
               IF szClipboard <> ''
                  DO AddCommand
               END
            END
            BEGIN
               DELETE(MRUQueue)
               feqControl{PROP:From} = MRUQueue
               GET(MRUQueue,1)
               IF ERRORCODE()
                  CHANGE(feqControl,'')
               ELSE
                  CHANGE(feqControl,MRUQueue.szValue)
                  (feqControl{PROP:ListFeq}){PROP:Selected} = POINTER(MRUQueue)
               END
               DISPLAY(feqControl)
            END
         END
      END

AddCommand    ROUTINE
   DATA
i     LONG
j     LONG

   CODE
      j = RECORDS(MRUQueue)
      LOOP i = 1 TO j
         GET(MRUQueue,i)
         IF MRUQueue.szValue = szClipboard
            BREAK
         END
      END
      IF i > j
         IF j = MAXMRU
            GET(MRUQueue,j)
            DELETE(MRUQueue)
         END
         MRUQueue.szValue = szClipboard
         ADD(MRUQueue,1)
      ELSE
         DELETE(MRUQueue)
         MRUQueue.szValue = szClipboard
         ADD(MRUQueue,1)
      END
