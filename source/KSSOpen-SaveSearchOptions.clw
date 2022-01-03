

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
SaveSearchOptions    PROCEDURE  (*FindStrOptionsGroupType SearchOptions) ! Declare Procedure
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

  CODE
    
      SyncOptions.szPattern = SearchOptions.szPattern
      GET(SyncOptions,SyncOptions.PatternKey)

      IF ERRORCODE() = NoRecErr
         !add one
         SyncOptions.szPattern = SearchOptions.szPattern
         ADD(SyncOptions)
      END

      IF ERRORCODE()
         MESSAGE(ERROR() & ' accessing SyncOptions','Unexpected Error',ICON:HAND)
      ELSE
         SyncOptions.bMatchPatternStartOfLine   = SearchOptions.bMatchPatternStartOfLine
         SyncOptions.bMatchPatternEndOfLine     = SearchOptions.bMatchPatternEndOfLine
         SyncOptions.bUseRegularExpressions     = SearchOptions.bUseRegularExpressions
         SyncOptions.bSearchSubdirectories      = SearchOptions.bSearchSubdirectories
         SyncOptions.nLevels                    = SearchOptions.nLevels
         SyncOptions.bCaseSensitive             = SearchOptions.bCaseSensitive
         SyncOptions.bExactMatch                = SearchOptions.bExactMatch
         SyncOptions.bExcludeMatch              = SearchOptions.bExcludeMatch
         SyncOptions.bExcludeComments           = SearchOptions.bExcludeComments
         SyncOptions.szSearchPath               = SearchOptions.szSearchPath
         SyncOptions.szFileMask                 = SearchOptions.szFileMask
         SyncOptions.bFilenamesOnly             = SearchOptions.bFilenamesOnly
         SyncOptions.bFileListFromFile          = SearchOptions.bFileListFromFile
         SyncOptions.szFileListFilename         = SearchOptions.szFileListFilename
         SyncOptions.bSearchStringsFromFile     = SearchOptions.bSearchStringsFromFile
         SyncOptions.szSearchStringFilename     = SearchOptions.szSearchStringFilename
         SyncOptions.szExcludeMask              = SearchOptions.szExcludeMask
         PUT(SyncOptions)
      END

      RETURN ERRORCODE()
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
LoadSearchOptions    PROCEDURE  (*FindStrOptionsGroupType SearchOptions) ! Declare Procedure
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

  CODE
    
      SyncOptions.szPattern = SearchOptions.szPattern
      GET(SyncOptions,SyncOptions.PatternKey)
      CASE ERRORCODE()
        OF NoError
           SearchOptions.bMatchPatternStartOfLine   = SyncOptions.bMatchPatternStartOfLine
           SearchOptions.bMatchPatternEndOfLine     = SyncOptions.bMatchPatternEndOfLine
           SearchOptions.bUseRegularExpressions     = SyncOptions.bUseRegularExpressions
           SearchOptions.bSearchSubdirectories      = SyncOptions.bSearchSubdirectories
           SearchOptions.nLevels                    = SyncOptions.nLevels
           SearchOptions.bCaseSensitive             = SyncOptions.bCaseSensitive
           SearchOptions.bExactMatch                = SyncOptions.bExactMatch
           SearchOptions.bExcludeMatch              = SyncOptions.bExcludeMatch
           SearchOptions.bExcludeComments           = SyncOptions.bExcludeComments
           SearchOptions.szSearchPath               = SyncOptions.szSearchPath
           SearchOptions.szFileMask                 = SyncOptions.szFileMask
           SearchOptions.bFilenamesOnly             = SyncOptions.bFilenamesOnly
           SearchOptions.bFileListFromFile          = SyncOptions.bFileListFromFile
           SearchOptions.szFileListFilename         = SyncOptions.szFileListFilename
           SearchOptions.bSearchStringsFromFile     = SyncOptions.bSearchStringsFromFile
           SearchOptions.szSearchStringFilename     = SyncOptions.szSearchStringFilename
           SearchOptions.szExcludeMask              = SyncOptions.szExcludeMask
        OF NoRecErr
           !that's ok
      ELSE
         MESSAGE(ERROR() & ' accessing SyncOptions','Unexpected Error',ICON:HAND)
      END
      RETURN ERRORCODE()
