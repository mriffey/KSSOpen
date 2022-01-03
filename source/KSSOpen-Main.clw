

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

   INCLUDE('ABRESIZE.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('csciviewer.inc'),ONCE

   MAP
BookmarkAdd             PROCEDURE(LONG lineno)              ! New method added to this class instance
BookmarkDelete          PROCEDURE(LONG lineno)              ! New method added to this class instance
BookmarkPresent         PROCEDURE(LONG lineno),BOOL         ! New method added to this class instance
BookmarkToggle          PROCEDURE(LONG lineno)              ! New method added to this class instance
BookmarkNext            PROCEDURE(BOOL forwardScan, BOOL select) ! New method added to this class instance
GetCurrentLineNumber    PROCEDURE(),LONG                    ! New method added to this class instance
FixFileFolding          PROCEDURE()
GetSearchSubdirectories PROCEDURE(),LONG
ConfirmAutoSave         PROCEDURE(),LONG
ResultList::WndProc     PROCEDURE(HWND hWnd, UNSIGNED wMsg, UNSIGNED wParam, LONG lParam),LONG,PASCAL
   END

ResultList::OrigWndProc LONG
feqResultList           LONG

!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
Main PROCEDURE 

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
!endregion Notices sec
saveFilename   CSTRING(261)
saveLineNo     LONG
savePosition   LONG 
saveColWidth   LONG     !saved Location column width

oST StringTheory 


!dCompileDate   LONG
!tCompileTime   LONG 
oHH           &tagHTMLHelp
baseFontName            CSTRING('Verdana')
baseFontSize            BYTE(10)
FileModeMenu            CSTRING(1025)
FileModeQueue           QUEUE(FileModeQueueType),PRE(fmq)
                        END
!FileExtensionQueue      QUEUE(FileExtensionQueueType),PRE(fxq)
!                        END
DisplayDayString        STRING('Sunday   Monday   Tuesday  WednesdayThursday Friday   Saturday ')
DisplayDayText          STRING(9),DIM(7),OVER(DisplayDayString)
StartMinute             CSTRING(3)
EditPaneRefreshPending  BOOL(FALSE)
LastNavTime             LONG
redoSearch              BOOL(FALSE)
bClosingDown            BOOL(FALSE)
NewSearchText           EQUATE('    +')
szNewTabFontName        CSTRING('Arial Bold')
szNewTabFontColor       LONG(00FF901Eh)
szNewTabFontStyle       LONG(FONT:Bold)
szNewTabFontSize        LONG(12)
szTabFontName           CSTRING('Segoe UI')
szTabFontColor          LONG(COLOR:Black)
szTabFontStyle          LONG(FONT:Regular)
szTabFontSize           LONG(10)
keyCodeName             ctKssKeyCodeName
!keyCodeName             CLASS(ctKeyCodeName)
!MissingCode                PROCEDURE(LONG _ID),STRING,DERIVED
!                        END


hMonitor                HANDLE
ptFrom                  LIKE(POINT)
rcMain                  LIKE(RECT)
mi                      LIKE(tagMONITORINFO)
!FindStr Parameters
!/b   : Matches the pattern if at the beginning of a line.
!/e   : Matches the pattern if at the end of a line.
!/l   : Uses search strings literally.
!/r   : Uses search strings as regular expressions. Findstr interprets all metacharacters as regular expressions unless you use /l.
!/s   : Searches for matching files in the current directory and all subdirectories.
!/i   : Specifies that the search is not to be case-sensitive.
!/x   : Prints lines that match exactly.
!/v   : Prints only lines that do not contain a match.
!/n   : Prints the line number before each line that matches.
!/m   : Prints only the file name if a file contains a match.
!/o   : Prints seek offset before each matching line.

!/p   : Skips files with non-printable characters.
!/offline   : Processes files with offline attribute set.
!/f: file   : Reads file list from the specified file.
!/c: string   : Uses specified text as a literal search string.
!/g: file   : Gets search strings from the specified file.
!/d: dirlist   : Searches a comma-delimited list of directories.
!/a: ColorAttribute   : Specifies color attributes with two hexadecimal digits.
!strings   : Specified text to be searched for in FileName.
![ Drive : ][ Path ] FileName [...] : Specifies a file or files to search.
!/?   : Displays help at the command prompt.
ClockQueue              QUEUE,PRE(ClockQueue)
tabNumber                  LONG
lClock                     LONG
                        END

MacroPlayerThread       LONG
bRecording              BOOL(FALSE)
bPlaying                LONG(0)

SearchQueue             QUEUE,PRE(SearchQueue)
tabNumber                  LONG
bMatchPatternStartOfLine   BOOL
bMatchPatternEndOfLine     BOOL
bUseRegularExpressions     BOOL
bSearchSubdirectories      BOOL
nLevels                    BYTE
nCurrentLevel              BYTE
bCaseSensitive             BOOL
bExactMatch                BOOL
bExcludeMatch              BOOL
bExcludeComments           BOOL
!bIncludeBinary             BOOL
bSearchPressed             BOOL
szPattern                  CSTRING(1025)
szSearchPath               CSTRING(1025)
szFileMask                 CSTRING(256)
szMatchesFound             CSTRING(256)
ResultQueue                &ResultQueueType
UndoQueue                  &ResultQueueType
feqSearchProgress          LONG
lPointer                   LONG
bFilenamesOnly             BOOL
bFileListFromFile          BOOL
szFileListFilename         CSTRING(261)
bSearchStringsFromFile     BOOL
szSearchStringFilename     CSTRING(261)
szPropertyFile             CSTRING(33)
szExcludeMask              CSTRING(256)
szListBoxFormat            CSTRING(256)
FindGroup                  LIKE(FindGrp)
szReplaceWith              LIKE(FindGrp.What)
                        END
BookMarkGroup           GROUP
BookmarkQueue              &BookmarkQueueType
                        END

EasyListPrintQueue      ResultQueueType

szListBoxFormat         CSTRING(256)
ListFormatQueue         QUEUE(ListFormatQueueType),PRE(lfq)
                        END
szInstallProgram        CSTRING(260)
szNull                  CSTRING('')
FileQueue               QUEUE(FILE:queue),PRE(FileQueue)
                        END
SearchFindOptions       GROUP(SearchFindOptionsGroupType)
                        END
thisDeleteInstance      LONG
ResultCount             LONG
DeleteCount             LONG
RemainingCount          LONG
DeleteQueue             QUEUE,PRE(DQ)
pointer                    LONG
                        END
bTrackMouse             BOOL
ViewerActive            BYTE
ListWithFocus           LONG(34)  !-1)
CurrentFilename         CSTRING(MAX_PATH)
szTitle                 CSTRING(256)
bAutoSearch             BOOL
bDoAutoSize             BOOL(FALSE)
FocusPosition           LONG
FoldMarginClicked       BOOL
bShowEvalMessage        BOOL(FALSE)
ExpiryDate              LONG
ShowWaitCursor          BOOL(FALSE)
szSendToFilename        CSTRING(MAX_PATH)
bMatchPatternStartOfLine BOOL                              ! 
strBuildDate         STRING(21)                            ! 
strBuildNumber       STRING(20)                            ! 
dCompileDate         LONG                                  ! 
tCompileTime         LONG                                  ! 
bMatchPatternEndOfLine BOOL                                ! 
bUseRegularExpressions BOOL                                ! 
bSearchSubdirectories BOOL                                 ! 
nLevels              BYTE                                  ! Subdirectory Levels to Search
nCurrentLevel        BYTE                                  ! Current Subdirectory Search Level
bCaseSensitive       BOOL                                  ! 
bExactMatch          BOOL                                  ! 
bExcludeMatch        BOOL                                  ! 
bExcludeComments     BOOL                                  ! 
bIncludeBinary       BOOL                                  ! 
bFilenamesOnly       BOOL                                  ! 
bFileListFromFile    BOOL                                  ! 
szFileListFilename   CSTRING(261)                          ! 
bSearchStringsFromFile BOOL                                ! 
szSearchStringFilename CSTRING(261)                        ! 
szPattern            CSTRING(1025)                         ! 
szSearchPath         CSTRING(1025)                         ! Search Path
szFileMask           CSTRING(256)                          ! 
szCmdLine            CSTRING(1025)                         ! Command Line
SavePath             CSTRING(261)                          ! 
NewTab               LONG                                  ! 
LastTabNumber        LONG                                  ! 
ResultListMenu       PopupClass                            ! 
PatternQueue         QUEUE,PRE(PatternQueue)               ! 
szPattern            LIKE(szPattern)                       ! 
                     END                                   ! 
SearchPathQueue      QUEUE,PRE(SearchPathQueue)            ! 
szSearchPath         LIKE(szSearchPath)                    ! 
                     END                                   ! 
FileMaskQueue        QUEUE,PRE(FileMaskQueue)              ! 
szFileMask           LIKE(szFileMask)                      ! 
                     END                                   ! 
Window               WINDOW('KSSOpen (Hand code edition)'),AT(,,600,273),FONT('Segoe UI',10,,,CHARSET:ANSI), |
  RESIZE,ALRT(03DCh),ALRT(CtrlEnd),ALRT(CtrlHome),ALRT(DownKey),ALRT(PgDnKey),ALRT(PgUpKey), |
  ALRT(UpKey),COLOR(,COLOR:Yellow),ICON('kss.ico'),GRAY,IMM,MAX,HLP('Main.htm'),STATUS(-1, |
  50,170,100),SYSTEM,TIMER(5)
                       TOOLBAR,AT(0,0,600,30),USE(?Toolbar1),COLOR(00F2E4D7h)
                         BUTTON('&Search'),AT(2,2,50,14),USE(?cmdSearch),LEFT,ICON('search.ico'),DEFAULT,FLAT,TIP('Search for' & |
  '...<09H>[Ctrl+F]')
                         BUTTON,AT(55,2,18,14),USE(?cmdRedoSearch),ICON('RedoSearch.ico'),FLAT,TIP('Redo the las' & |
  't search<0DH,0AH>without dialog<09H>[F5]')
                         PANEL,AT(75,3,1,12),USE(?Separator3),BEVEL(1)
                         BUTTON,AT(78,2,18,14),USE(?cmdPreviousFolder),ICON('PreviousFolder.ico'),FLAT,TIP('Jump to Pr' & |
  'evious Folder<0DH,0AH>in Results List')
                         BUTTON,AT(98,2,18,14),USE(?cmdPreviousFile),ICON('PreviousFile.ico'),FLAT,TIP('Jump to Pr' & |
  'evious File<0DH,0AH>in Results List')
                         BUTTON,AT(118,2,18,14),USE(?cmdPreviousLine),ICON('PreviousLine.ico'),FLAT,TIP('Jump to Pr' & |
  'evious Line<0DH,0AH>in Results List<09H>[MinusKey]')
                         BUTTON,AT(138,2,18,14),USE(?cmdNextLine),ICON('NextLine.ico'),FLAT,TIP('Jump to Next Li' & |
  'ne<0DH,0AH>in Results List<09H>[PlusKey]')
                         BUTTON,AT(158,2,18,14),USE(?cmdNextFile),ICON('NextFile.ico'),FLAT,TIP('Jump to Next Fi' & |
  'le<0DH,0AH>in Results List')
                         BUTTON,AT(178,2,18,14),USE(?cmdNextFolder),ICON('NextFolder.ico'),FLAT,TIP('Jump to Nex' & |
  't Folder<0DH,0AH>in Results List')
                         PANEL,AT(198,3,1,12),USE(?Separator1),BEVEL(1)
                         BUTTON,AT(202,2,18,14),USE(?cmdDeleteLine),ICON('DeleteLine.ico'),FLAT,TIP('Delete Sele' & |
  'cted Line<0DH,0AH>from Results List<09H>[Delete]')
                         BUTTON,AT(222,2,18,14),USE(?cmdDeleteFile),ICON('DeleteFile.ico'),FLAT,TIP('Delete Sele' & |
  'cted File<0DH,0AH>from Results List<09H>[Alt+Delete]')
                         BUTTON,AT(242,2,18,14),USE(?cmdDeleteExtension),ICON('DeleteExtension.ico'),FLAT,TIP('Delete Sel' & |
  'ected File Extension<0DH,0AH>from Results List')
                         BUTTON,AT(262,2,18,14),USE(?cmdDeletePath),ICON('DeletePath.ico'),FLAT,TIP('Delete Sele' & |
  'cted Path<0DH,0AH>from Results List')
                         BUTTON,AT(282,2,18,14),USE(?cmdDeleteComments),ICON('DeleteComment.ico'),FLAT,TIP('Delete Com' & |
  'ments<0DH,0AH>from Results List')
                         BUTTON,AT(302,2,18,14),USE(?cmdDeleteLabels),ICON('DeleteLabel.ico'),FLAT,TIP('Delete Lab' & |
  'els<0DH,0AH>from Results List')
                         BUTTON,AT(322,2,18,14),USE(?cmdDeleteCode),ICON('DeleteCode.ico'),FLAT,TIP('Delete Matc' & |
  'hes Found in Code<0DH,0AH>from Results List')
                         BUTTON,AT(342,2,18,14),USE(?cmdDeleteData),ICON('DeleteData.ico'),FLAT,TIP('Delete Matc' & |
  'hes Found in Data<0DH,0AH>from Results List')
                         BUTTON,AT(363,2,18,14),USE(?cmdDeleteBuiltInClw),COLOR(00F2E4D7h),ICON('ClarionBuiltIn.ico'), |
  FLAT,TIP('Delete *_BC*.clw, *_R*.clw, *_SF.clw files from Results List')
                         BUTTON,AT(389,2,18,14),USE(?cmdFindAndDelete),ICON('FindDelete.ico'),FLAT,TIP('Find and D' & |
  'elete<0DH,0AH>in Results List...<09H>[Ctrl+Delete]')
                         BUTTON,AT(409,2,18,14),USE(?cmdUndoDelete),ICON('UndoDelete.ico'),FLAT,TIP('Undo Delete' & |
  '<0DH,0AH>Ctrl+Z')
                         PANEL,AT(430,3,1,12),USE(?Separator2),BEVEL(1)
                         BUTTON,AT(433,2,18,14),USE(?cmdReplaceResults),ICON('Replace.ico'),FLAT,TIP('Replace ..' & |
  '.<09H>[Ctrl+R]')
                         BUTTON,AT(453,2,18,14),USE(?cmdSave),ICON('FileSave.ico'),FLAT,TIP('Save Results ...')
                         BUTTON,AT(473,2,18,14),USE(?cmdEdit),ICON('SendTo.ico'),FLAT,TIP('Send To Command<09H>[Ctrl+E]')
                         PANEL,AT(493,3,1,12),USE(?Separator4),BEVEL(1)
                         BUTTON,AT(496,2,18,14),USE(?cmdUserOptions),ICON('UserOptions.ico'),FLAT,TIP('Options ...')
                         BUTTON,AT(516,2,18,14),USE(?cmdLayout),ICON('splith.ico'),FLAT,TIP('Switch to Horizontal Layout')
                         BUTTON,AT(536,2,18,14),USE(?cmdHelp),ICON('help.ico'),FLAT,TIP('Help')
                         STRING(@S255),AT(566,4,,8),USE(szTitle),RIGHT
                         BUTTON,AT(598,4,10,8),USE(?cmdSaveWarn),ICON('save-warn.png'),DISABLE,FLAT,SKIP
                         PROMPT('Cmt'),AT(286,19),USE(?PROMPT1),FONT(,8),TRN
                         PROMPT('Lbl'),AT(306,19),USE(?PROMPT1:2),FONT(,8),COLOR(00F2E4D7h),TRN
                         PROMPT('Code'),AT(323,19),USE(?PROMPT1:3),FONT(,8),COLOR(00F2E4D7h),TRN
                         PROMPT('Data'),AT(344,19),USE(?PROMPT1:4),FONT(,8),COLOR(00F2E4D7h),TRN
                         PROMPT('File'),AT(226,19),USE(?PROMPT1:5),FONT(,8),COLOR(00F2E4D7h),TRN
                         PROMPT('Ext'),AT(247,19),USE(?PROMPT1:6),FONT(,8),COLOR(00F2E4D7h),TRN
                         PROMPT('Folder'),AT(262,19),USE(?PROMPT1:7),FONT(,8),COLOR(00F2E4D7h),TRN
                         PROMPT('Line'),AT(205,19),USE(?PROMPT1:8),FONT(,8),COLOR(00F2E4D7h),TRN
                         PROMPT('BC*/SF/R*'),AT(361,19),USE(?PROMPT1:9),FONT(,8),COLOR(00F2E4D7h),TRN
                       END
                       TEXT,AT(462,1,136,240),USE(?sciControl:Region),FONT(,,COLOR:BTNTEXT)
                       BOX,AT(0,0,460,25),USE(?Application:Box),COLOR(COLOR:Red),FILL(00C0C0FFh),LINEWIDTH(1)
                       SHEET,AT(0,0,460,255),USE(CurrentSearch),HSCROLL,COLOR(00F0F0F0h)
                         TAB('New Search'),USE(?Search1)
                         END
                       END
                       BUTTON,AT(2,14,16,12),USE(?cmdCloseTab),ICON('CloseTab.ico'),FLAT,SKIP,TIP('Close Tab')
                       BUTTON,AT(2,14,16,12),USE(?cmdCancelSearch),ICON('CancelSearch.ico'),FLAT,HIDE,SKIP,TIP('Cancel Search')
                       STRING(@s255),AT(20,16,398),USE(SearchQueue.szMatchesFound,,?szMatchesFound),TRN
                       LIST,AT(1,28,456,213),USE(?ResultList),VSCROLL,ALRT(CtrlC),ALRT(CtrlE),ALRT(CtrlF),ALRT(CtrlF4), |
  ALRT(CtrlT),ALRT(CtrlV),ALRT(CtrlW),ALRT(CtrlZ),ALRT(DeleteKey),ALRT(EnterKey),ALRT(F5Key), |
  ALRT(MouseLeft2),ALRT(MouseRight),COLOR(COLOR:WINDOW),FORMAT('175L(2)|M~Path~S(1024)@' & |
  's255@62L(2)|M~Filename~S(1024)@s255@28L(2)|M~Ext~C(0)@s15@28R(2)|M~Line~C(0)@n15@50L' & |
  '(2)|M~Location~S(1024)@s255@175L(2)|M~Text~S(1024)@s255@'),FROM(EasyListPrintQueue)
                       REGION,AT(460,0,2,260),USE(?SplitterBar),FILL(00B48246h),IMM
                       STRING(@S255),AT(10,147,568,8),USE(szTitle,,?szTitle:2),COLOR(COLOR:Red)
                       BUTTON,AT(0,147,10,8),USE(?cmdSaveWarn:2),ICON('save-warn.png'),DISABLE,FLAT,SKIP
                     END

LastToolbarButton  EQUATE(?cmdHelp)
SplitterBarSize    EQUATE(4)
    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
SetAlerts              PROCEDURE(),DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeCloseEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeNewSelection       PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
Update                 PROCEDURE(),DERIVED
                     END

Toolbar              ToolbarClass
SciControl           CLASS(CSciViewer)                     ! Scintilla using ?sciControl:Region
Colourise              PROCEDURE(LONG lStart,LONG lEnd),DERIVED
FindWindowTakeOpenWindow PROCEDURE(),DERIVED
FoldAll                PROCEDURE(),DERIVED
FoldMargin             PROCEDURE(),DERIVED
GetModify              PROCEDURE(),BOOL,DERIVED
GrabFocus              PROCEDURE(),DERIVED
Init                   PROCEDURE(*WINDOW W,LONG feq,UNSIGNED id,BOOL Themed = 0),BYTE,DERIVED
Kill                   PROCEDURE(),DERIVED
OpenFile               PROCEDURE(*CSTRING szFileName),BOOL,PROC,DERIVED
PrintAsk               PROCEDURE(),DERIVED
SearchAsk              PROCEDURE(BOOL bShowWindow,tagHTMLHelp HTMLHelp),DERIVED
SetAlerts              PROCEDURE(),DERIVED
SetClarionLexer        PROCEDURE(),DERIVED
SetColors              PROCEDURE(*COLORGROUPTYPE color),DERIVED
SetDefaults            PROCEDURE(),DERIVED
SetLexerType           PROCEDURE(STRING szFileType),DERIVED
SetTextLexer           PROCEDURE(),DERIVED
TakeContextMenu        PROCEDURE(),BYTE,DERIVED
TakeEvent              PROCEDURE(),BYTE,DERIVED
TakeOpenWindow         PROCEDURE(),BYTE,DERIVED
ToggleFold             PROCEDURE(LONG lLine),DERIVED
GetHwnd                PROCEDURE(),LONG                    ! New method added to this class instance
ReplaceAsk             PROCEDURE()                         ! New method added to this class instance
SaveFile               PROCEDURE(*CSTRING szFilename),LONG,PROC ! New method added to this class instance
SaveFileAs             PROCEDURE(*CSTRING szFilename),LONG,PROC ! New method added to this class instance
SetLexer               PROCEDURE(LONG Lexer, *CSTRING PropertyFile),VIRTUAL ! New method added to this class instance
SetDefaultStyles       PROCEDURE()                         ! New method added to this class instance
ResetPopupMenu         PROCEDURE()                         ! New method added to this class instance
                     END

Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END

AutoSizer            CLASS(AutoSizeColumnClassType)
Reset                   PROCEDURE(SIGNED ListControl,QUEUE ListQueue)
                     END

bControlInitialised  BOOL(FALSE)

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
SetupResultListMenu  ROUTINE
   ResultListMenu.AddItem('Search<9>Ctrl+F','Search')
   ResultListMenu.AddItem('Redo Search<9>F5','RedoSearch')
   ResultListMenu.AddItem('Show Match Summary','Summary')
   ResultListMenu.AddItem('-','Separator3')
   ResultListMenu.AddItem('-','Separator0')
   ResultListMenu.AddItem('-','Separator1')
   ResultListMenu.AddItem('Delete matches in CODE','DeleteInCodeDirect')
   ResultListMenu.AddItem('Delete matches in DATA','DeleteInDataDirect')
   ResultListMenu.AddItem('Delete matches in Filename.ext','DeleteFileNameDirect')
   ResultListMenu.AddItem('Delete commented matches','DeleteCommentLinesDirect')   
   ResultListMenu.AddItem('Delete _BC*/_SF/_R* .clw *FILES*','DeleteBuiltinClwDirect')
   ResultListMenu.AddItem('Undo Delete','Undo')
   ResultListMenu.AddItem('-','Separator2')
   ResultListMenu.AddItem('Replace...<9>Ctrl+R','Replace')
   ResultListMenu.AddItem('Save Results...','Save')
   ResultListMenu.AddItem('Send To<9>Ctrl+E','SendTo')
   ResultListMenu.AddItem('-','Separator3')
   ResultListMenu.AddItem('Copy Fullname to Clipboard<9>Ctrl+C','Copy')
   ResultListMenu.AddItem('Copy Path to Clipboard','CopyPath')
   ResultListMenu.AddItem('Copy Findstr to Clipboard<9>Ctrl+?','CopyFindstr')
   ResultListMenu.AddItem('Reveal in Explorer','ExplorePath')
   ResultListMenu.AddItem('-','Separator4')
   ResultListMenu.AddItem('Options','Options')
   ResultListMenu.AddItem('Change Layout','Layout')
   ResultListMenu.AddItem('Center Resizer Bar<9>Ctrl+<166>','CenterBar')
   ResultListMenu.AddItem('Change Results List Format','Format')
   ResultListMenu.AddItem('Auto Size Columns','AutoSizeColumns')
   ResultListMenu.AddItem('-','Separator5')
   ResultListMenu.AddItem('Close Tab','Close')
   ResultListMenu.AddItem('Cancel Search','Cancel')
   ResultListMenu.AddItem('-','Separator6')
   ResultListMenu.AddItem('Hide Edit Panel','HideEdit')
   ResultListMenu.AddSubMenu('Jump','{{Previous Folder|Previous File|Previous Line<9>' & keyCodeName.ToName(glo:MinusKey) & '|Next Line<9>' & keyCodeName.ToName(glo:PlusKey) & '|Next File|Next Folder}','Separator0')
   ResultListMenu.AddSubMenu('Delete','{{Selected{{Line|Filename|Extension|Path}|-|Comment Lines|Label Lines|-|Matches in CODE|Matches in DATA|-|Find and Delete...}','Separator1')
   ResultListMenu.AddSubMenu('Macro','{{Player|Record}','Undo')
   ResultListMenu.DeleteItem('Separator0')   

   ResultListMenu.SetIcon('Search','Search.ico')
   ResultListMenu.SetIcon('RedoSearch','RedoSearch.ico')
   ResultListMenu.SetIcon('Replace','Replace.ico')
   ResultListMenu.SetIcon('Save','FileSave.ico')
   ResultListMenu.SetIcon('SendTo','SendTo.ico')
   ResultListMenu.SetIcon('Print','Printer.ico')
   ResultListMenu.SetIcon('Close','CloseTab.ico')
   ResultListMenu.SetIcon('Cancel','CancelSearch.ico')
   ResultListMenu.SetIcon('HideEdit',CHOOSE(glo:bHideEditPanel,'Checkbox_on.ico','Checkbox_off.ico'))
   ResultListMenu.SetIcon('Line','DeleteLine.ico')
   ResultListMenu.SetIcon('Filename','DeleteFile.ico')
   ResultListMenu.SetIcon('DeleteFileNameDirect','DeleteFile.ico')   
   ResultListMenu.SetIcon('Extension','DeleteExtension.ico')
   ResultListMenu.SetIcon('Path','DeletePath.ico')   
   ResultListMenu.SetIcon('CommentLines','DeleteComment.ico')
   ResultListMenu.SetIcon('DeleteCommentLinesDirect','DeleteComment.ico')
   ResultListMenu.SetIcon('LabelLines','DeleteLabel.ico')
   ResultListMenu.SetIcon('MatchesinCODE','DeleteCode.ico')
   ResultListMenu.SetIcon('DeleteInCodeDirect','DeleteCode.ico')
   ResultListMenu.SetIcon('MatchesinDATA','DeleteData.ico')
   ResultListMenu.SetIcon('DeleteInDataDirect','DeleteData.ico')
   ResultListMenu.SetIcon('DeleteBuiltinClwDirect','ClarionBuiltIn.ico') !rif 20190101
   ResultListMenu.SetIcon('FindandDelete','FindDelete.ico')
   ResultListMenu.SetIcon('Undo','UndoDelete.ico')
   ResultListMenu.SetIcon('PreviousFolder','PreviousFolder.ico')
   ResultListMenu.SetIcon('NextFolder','NextFolder.ico')
   ResultListMenu.SetIcon('PreviousFile','PreviousFile.ico')
   ResultListMenu.SetIcon('NextFile','NextFile.ico')
   ResultListMenu.SetIcon('PreviousLine' & keyCodeName.ToName(glo:MinusKey),'PreviousLine.ico')
   ResultListMenu.SetIcon('NextLine' & keyCodeName.ToName(glo:PlusKey),'NextLine.ico')
   ResultListMenu.SetIcon('Options','UserOptions.ico')
   IF glo:SplitterOrientation = SplitterOrientation:Vertical
      ResultListMenu.SetIcon('Layout','SplitH.ico')
   ELSE
      ResultListMenu.SetIcon('Layout','SplitV.ico')
   END
   !  ResultListMenu.SetIcon('Play','button_play.ico')
   !  ResultListMenu.SetIcon('Record','button_record.ico')
   !  ResultListMenu.SetIcon('Stop','button_stop.ico')
   !ResultListMenu.SetItemCheck('Stop',TRUE)

   ResultListMenu.SetItemEnable('SendTo',TRUE)
   ResultListMenu.SetItemEnable('Replace',FALSE)
   ResultListMenu.SetItemEnable('Save',FALSE)
   IF Records(SearchQueue.UndoQueue) = 0
      ResultListMenu.SetItemEnable('Undo',FALSE)
   END
   ResultListMenu.SetItemEnable('Cancel',FALSE)
   ResultListMenu.SetItemEnable('HideEdit',CHOOSE(glo:bHideResultsPanel=FALSE,TRUE,FALSE))
AddSearchQueueRecord    ROUTINE
  SearchQueue.tabNumber = NewTab
  SearchQueue.bMatchPatternStartOfLine = bMatchPatternStartOfLine
  SearchQueue.bMatchPatternEndOfLine   = bMatchPatternEndOfLine
  SearchQueue.bUseRegularExpressions   = bUseRegularExpressions
  SearchQueue.bSearchSubdirectories    = bSearchSubdirectories
  SearchQueue.nLevels                  = nLevels
  SearchQueue.nCurrentLevel            = nCurrentLevel
  SearchQueue.bCaseSensitive           = bCaseSensitive
  SearchQueue.bExactMatch              = bExactMatch
  SearchQueue.bExcludeMatch            = bExcludeMatch
  SearchQueue.bExcludeComments         = bExcludeComments
  !SearchQueue.bIncludeBinary           = bIncludeBinary
  SearchQueue.bSearchPressed           = FALSE
  SearchQueue.szPattern                = szPattern
  SearchQueue.szSearchPath             = szSearchPath
  SearchQueue.szFileMask               = szFileMask

  SearchQueue.bFilenamesOnly           = bFilenamesOnly
  SearchQueue.bFileListFromFile        = bFileListFromFile
  SearchQueue.szFileListFilename       = szFileListFilename
  SearchQueue.bSearchStringsFromFile   = bSearchStringsFromFile
  SearchQueue.szSearchStringFilename   = szSearchStringFilename

  SearchQueue.szMatchesFound           = ''
  SearchQueue.ResultQueue              &= NEW(ResultQueueType)
  !ASSERT(0,eqDBG & 'NEW ResultQueue [' & ADDRESS(SearchQueue.ResultQueue) & ']')
  SearchQueue.UndoQueue                &= NEW(ResultQueueType)
  !ASSERT(0,eqDBG & 'NEW UndoQueue [' & ADDRESS(SearchQueue.UndoQueue) & ']')
  SearchQueue.feqSearchProgress        = CREATE(0,CREATE:progress,NewTab)
  SETPOSITION(SearchQueue.feqSearchProgress,?szMatchesFound{PROP:XPos},?szMatchesFound{PROP:YPos},160,8)
  SearchQueue.feqSearchProgress{PROP:RangeLow} = 0
  SearchQueue.feqSearchProgress{PROP:RangeHigh} = MAX_PROGRESS
  SearchQueue.lPointer = 1
  ?ResultList{PROP:Selected} = 1

  SearchQueue.szListBoxFormat = ?ResultList{PROP:Format}

  SearchQueue.FindGroup.What = ''   !SELF.GetFindWhat()
  SearchQueue.FindGroup.Direction = 'Down'
  SearchQueue.FindGroup.MatchCase = FALSE
  SearchQueue.FindGroup.WholeWord = FALSE
  SearchQueue.FindGroup.WordStart = FALSE
  SearchQueue.FindGroup.RegExp    = FALSE
  SearchQueue.FindGroup.POSIX     = FALSE
  SearchQueue.szReplaceWith       = ''
  SearchQueue.FindGroup.bWordWrap = TRUE

  ADD(SearchQueue,SearchQueue.tabNumber)
GetSearchQueueRecord    ROUTINE
  SearchQueue.tabNumber = ?CurrentSearch{PROP:ChoiceFeq}
  GET(SearchQueue,SearchQueue.tabNumber)
  IF NOT ERRORCODE()
     DO UpdateLocalSearchOptions
  END
UpdateLocalSearchOptions   ROUTINE
     bMatchPatternStartOfLine = SearchQueue.bMatchPatternStartOfLine
     bMatchPatternEndOfLine   = SearchQueue.bMatchPatternEndOfLine
     bUseRegularExpressions   = SearchQueue.bUseRegularExpressions
     bSearchSubdirectories    = SearchQueue.bSearchSubdirectories
     nLevels                  = SearchQueue.nLevels
     nCurrentLevel            = SearchQueue.nCurrentLevel
     bCaseSensitive           = SearchQueue.bCaseSensitive
     bExactMatch              = SearchQueue.bExactMatch
     bExcludeMatch            = SearchQueue.bExcludeMatch
     bExcludeComments         = SearchQueue.bExcludeComments
     szPattern                = SearchQueue.szPattern
     szSearchPath             = SearchQueue.szSearchPath
     szFileMask               = SearchQueue.szFileMask

     bFilenamesOnly           = SearchQueue.bFilenamesOnly
     bFileListFromFile        = SearchQueue.bFileListFromFile
     szFileListFilename       = SearchQueue.szFileListFilename
     bSearchStringsFromFile   = SearchQueue.bSearchStringsFromFile
     szSearchStringFilename   = SearchQueue.szSearchStringFilename
UpdateSearchQueueRecord    ROUTINE
  GET(SearchQueue.ResultQueue,?ResultList{PROP:Selected})
  SearchQueue.lPointer        = POINTER(SearchQueue.ResultQueue)
  PUT(SearchQueue)
SaveSearchParameters ROUTINE
   DATA
n                 LONG
LocationColWidth  LONG

   CODE
      INIMgr.Update('Search Options','bMatchPatternStartOfLine',SearchQueue.bMatchPatternStartOfLine)
      INIMgr.Update('Search Options','bMatchPatternEndOfLine',SearchQueue.bMatchPatternEndOfLine)
      INIMgr.Update('Search Options','bUseRegularExpressions',SearchQueue.bUseRegularExpressions)
      INIMgr.Update('Search Options','bSearchSubdirectories',SearchQueue.bSearchSubdirectories)
      INIMgr.Update('Search Options','nLevels',SearchQueue.nLevels)
      INIMgr.Update('Search Options','bCaseSensitive',SearchQueue.bCaseSensitive)
      INIMgr.Update('Search Options','bExactMatch',SearchQueue.bExactMatch)
      INIMgr.Update('Search Options','bExcludeMatch',SearchQueue.bExcludeMatch)
      INIMgr.Update('Search Options','bExcludeComments',SearchQueue.bExcludeComments)
      !INIMgr.Update('Search Options','bIncludeBinary',SearchQueue.bIncludeBinary)

      ReplaceChr(SearchQueue.szPattern,'''','§')
      !ReplaceChr(SearchQueue.szPattern,'''','<A7h>')  ! MR 20190217 avoid losing this important hex char to random translation. 
      INIMgr.Update('Search Options','szPattern',SearchQueue.szPattern)

      INIMgr.Update('Search Options','szSearchPath',SearchQueue.szSearchPath)
      INIMgr.Update('Search Options','szFileMask',SearchQueue.szFileMask)

      INIMgr.Update('Search Options','bFilenamesOnly',SearchQueue.bFilenamesOnly)
      INIMgr.Update('Search Options','bFileListFromFile',SearchQueue.bFileListFromFile)
      INIMgr.Update('Search Options','szFileListFilename',SearchQueue.szFileListFilename)
      INIMgr.Update('Search Options','bSearchStringsFromFile',SearchQueue.bSearchStringsFromFile)
      INIMgr.Update('Search Options','szSearchStringFilename',SearchQueue.szSearchStringFilename)

      INIMgr.Update('Search Options','FindText',SearchFindOptions.szFindText)

      INIMgr.Update('Find and Delete Options','SearchLocation',SearchFindOptions.SearchLocation)
      INIMgr.Update('Find and Delete Options','DeleteCondition',SearchFindOptions.DeleteCondition)
      INIMgr.Update('Find and Delete Options','MatchType',SearchFindOptions.MatchType)
      INIMgr.Update('Find and Delete Options','MatchCase',SearchFindOptions.MatchCase)

      INIMgr.Update('Global','szEditCommand',glo:szEditorCommand)
      INIMgr.Update('Global','PromptForEditor',glo:PromptForEditor)
      INIMgr.Update('Global','AllExtensions',glo:bAllExtensions)
      INIMgr.Update('Global','NewSearchAction',glo:NewSearchAction)
      INIMgr.Update('Global','NoDownloadVersion',glo:szNoDownloadVersion)
      INIMgr.Update('Global','DeleteWarningCount',glo:nDeleteWarningCount)
      INIMgr.Update('Global','DefaultSearchButton',glo:nDefaultSearchButton)
      INIMgr.Update('Global','DontShowSubdirectoryWarning',glo:bDontShowSubdirectoryWarning)
      INIMgr.Update('Global','SyncPathWithPattern',glo:SyncPathWithPattern)
      INIMgr.Update('Global','DefaultPropertyFile',glo:szDefaultPropertyFile)

      INIMgr.Update('Global','SelectedBack',glo:SelectedBack)
      INIMgr.Update('Global','BookmarkBack',glo:BookmarkBack)
      INIMgr.Update('Global','DisableSlashP',glo:DisableSlashP) ! mr 20180912 

      INIMgr.Update('Global','ResultListFontName',glo:ResultListFontName)
      INIMgr.Update('Global','ResultListFontSize',glo:ResultListFontSize)
      INIMgr.Update('Global','ResultListForeColor',glo:ResultListForeColor)
      INIMgr.Update('Global','ResultListFontStyle',glo:ResultListFontStyle)
      INIMgr.Update('Global','ResultListPlusKey',glo:PlusKey)
      INIMgr.Update('Global','ResultListMinusKey',glo:MinusKey)

      INIMgr.Update('Global','ApplicationColor',glo:ApplicationColor)
      INIMgr.Update('Global','ToolbarColor',glo:ToolbarColor)
      INIMgr.Update('Global','SplitOrientation',glo:SplitterOrientation)
      INIMgr.Update('Global','SplitX',?SplitterBar{PROP:XPos})
      INIMgr.Update('Global','SplitY',?SplitterBar{PROP:YPos})
      INIMgr.Update('Global','Zoom',glo:Zoom)

      INIMgr.Update('Global','ClarionHelpFile',glo:szClarionHelpFile)
      INIMgr.Update('Global','HotSpotsEnabled',glo:bHotSpotsEnabled)
      INIMgr.Update('Global','AutoSave',glo:bAutoSave)

      INIMgr.Update('Global','AllowMultipleInstances',glo:AllowMultipleInstances)
      INIMgr.Update('Global','AutoSizeResultColumns',glo:AutoSizeResultColumns)
      INIMgr.Update('Global','RestorePointTimerInterval',glo:RestorePointTimerInterval)

      INIMgr.Update('Global','sqlProperties',glo:sqlProperties)

      !only save the listbox format if the 'location' column is not hidden
      LOOP n = 1 TO 6
         IF ?ResultList{PROPLIST:Header,n} = 'Location'
            LocationColWidth = ?ResultList{PROPLIST:width,n}
            IF LocationColWidth <> 0
               szListBoxFormat = ?ResultList{PROP:Format}
               INIMgr.Update('Main','szListBoxFormat',szListBoxFormat)
               BREAK
            END
         END
      END

      INIMgr.UpdateQueue('Macro Queue','Macro',MacroQueue,MacroQueue.feqButton,MacroQueue.szField1,MacroQueue.szField2,MacroQueue.szField3)
   EXIT
HandleCloseTab ROUTINE
      IF ?CurrentSearch{PROP:ChoiceFEQ} <> ?Search1
         DO CloseTab
      ELSE
         (?CurrentSearch{PROP:ChoiceFEQ}){PROP:Text} = 'New Search'
         SearchQueue.szMatchesFound = ''
         DISPLAY(?szMatchesFound)
         FREE(SearchQueue.ResultQueue)
         FREE(SearchQueue.UndoQueue)
         DISPLAY(?ResultList)
         IF ViewerActive
            !SciControl.SetReadOnly(FALSE)
            SciControl.ClearBuffer()
            ViewerActive = FALSE
         END
         szTitle = ''
         DISPLAY(?szTitle)
         DISPLAY(?szTitle:2)
         POST(EVENT:Accepted,?cmdSearch)
      END

CloseTab    ROUTINE
   DATA
i                    LONG
j                    LONG
p                    LONG
thisProcInfo         &PROCESS_INFORMATION

   CODE
      SearchQueue.tabNumber = ?CurrentSearch{PROP:ChoiceFEQ}
      GET(SearchQueue,SearchQueue.tabNumber)
      IF ~ERRORCODE()
         p = POINTER(SearchQueue)

         cs.wait()
         j = RECORDS(ThreadQueue)
         LOOP i = 1 TO j
            GET(ThreadQueue,i)
            IF ThreadQueue.tabNumber = SearchQueue.tabNumber
               thisProcInfo &= (INSTANCE(piProcInfo,ThreadQueue.ID))
               IF NOT thisProcInfo &= NULL
                  kcr_TerminateProcess(thisProcInfo.hProcess,0)
               END
               POST(EVENT:CloseWindow,,ThreadQueue.ID)
               !YIELD()
            END
         END
         cs.release()
         YIELD()

         ?ResultList{PROP:From} = ''

         FREE(SearchQueue.ResultQueue)
         !ASSERT(0,eqDBG & 'DISPOSE ResultQueue [' & ADDRESS(SearchQueue.ResultQueue) &']')
         DISPOSE(SearchQueue.ResultQueue)
         SearchQueue.ResultQueue &= NULL

         FREE(SearchQueue.UndoQueue)
         !ASSERT(0,eqDBG & 'DISPOSE UndoQueue [' & ADDRESS(SearchQueue.UndoQueue) &']')
         DISPOSE(SearchQueue.UndoQueue)
         SearchQueue.UndoQueue &= NULL

         DESTROY(SearchQueue.feqSearchProgress)
         SearchQueue.feqSearchProgress = 0
         DESTROY(SearchQueue.tabNumber)
         SearchQueue.tabNumber = 0
         PUT(SearchQueue)
         DELETE(SearchQueue)
         GET(SearchQueue,p-1)
         ?CurrentSearch{PROP:ChoiceFEQ} = SearchQueue.tabNumber
      END
      POST(EVENT:NewSelection,?CurrentSearch)

CheckEditor ROUTINE
      IF RECORDS(EditorQueue) > 1 AND glo:PromptForEditor = TRUE
         IF SelectSendToCommand() = Level:Benign
            IF glo:szEditorCommand <> ''
               DO HandleEdit
            END
         END
      ELSE
         IF glo:szEditorCommand <> ''
            DO HandleEdit
         END
      END
HandleEdit  ROUTINE
   DATA
szURL                CSTRING(1024)
szFilename           CSTRING(MAXPATH)
i                    LONG
j                    LONG

   CODE
      szURL = glo:szEditorCommand

      !GET(SearchQueue.ResultQueue,CHOICE(?ResultList))
      !szSendToFilename = SearchQueue.ResultQueue.SortName

      j = 0
      IF INSTRING('SCITE',UPPER(szURL),1)
         LOOP i = 1 TO LEN(szSendToFilename)
            j += 1
            szFilename[j] = szSendToFilename[i]
            IF szSendToFilename[i] = '\'
               j += 1
               szFilename[j] = szSendToFilename[i]
            END
         END
      ELSE
         szFilename = szSendToFilename
      END

      i = INSTRING('%1',szURL,1,1)
      IF i
         szURL = szURL[1 : i-1] & szFilename & szURL[i+2 : LEN(szURL)]
      ELSE
         i = INSTRING('%FILE',UPPER(szURL),1,1)
         IF i
            szURL = szURL[1 : i-1] & szFilename & szURL[i+5 : LEN(szURL)]
         END
      END

      i = INSTRING('%2',szURL,1,1)
      IF i
         szURL = szURL[1 : i-1] & SearchQueue.ResultQueue.LineNo & szURL[i+2 : LEN(szURL)]
      ELSE
         i = INSTRING('%LINE',UPPER(szURL),1,1)
         IF i
            szURL = szURL[1 : i-1] & SearchQueue.ResultQueue.LineNo & szURL[i+5 : LEN(szURL)]
         END
      END
      IF UPPER(szURL) = '<<USE WINDOWS DEFAULT>'
         szNull = ''
         kcr_ShellExecute(window{prop:handle},0,szFilename,0,szNull,1)
      ELSE
         RUN(szURL)
         IF RUNCODE() = -4
            MESSAGE('An unexpected error has occurred.|' & ERROR() & '|attempting to run ' & szURL,'Error',ICON:Exclamation)
         END
      END
HandleUnDo  ROUTINE
   DATA
indx     LONG

   CODE
      saveFilename = SearchQueue.ResultQueue.SortName
      saveLineNo   = SearchQueue.ResultQueue.LineNo
      GET(SearchQueue.UndoQueue,RECORDS(SearchQueue.UndoQueue))
      thisDeleteInstance = SearchQueue.UndoQueue.DeleteInstance
      LOOP WHILE SearchQueue.UndoQueue.DeleteInstance = thisDeleteInstance
         SearchQueue.ResultQueue = SearchQueue.UndoQueue
         ADD(SearchQueue.ResultQueue,SearchQueue.UndoQueue.Position)
         IF CurrentFilename = SearchQueue.ResultQueue.SortName
            BookmarkAdd(SearchQueue.ResultQueue.LineNo-1)
         END
         DELETE(SearchQueue.UndoQueue)
         GET(SearchQueue.UndoQueue,RECORDS(SearchQueue.UndoQueue))
         IF ERRORCODE()
            BREAK
         END
      END
      IF RECORDS(SearchQueue.UndoQueue) = 0
         ResultListMenu.SetItemEnable('Undo',FALSE)
      ELSE
         ResultListMenu.SetItemEnable('Undo',TRUE)
      END

      indx = POINTER(SearchQueue.ResultQueue)
      SearchQueue.ResultQueue.SortName = saveFilename
      SearchQueue.ResultQueue.LineNo   = saveLineNo
      GET(SearchQueue.ResultQueue,+SearchQueue.ResultQueue.SortName,+SearchQueue.ResultQueue.LineNo)
      IF ERRORCODE()
         GET(SearchQueue.ResultQueue,indx)
      END
      ?ResultList{PROP:Selected} = POINTER(SearchQueue.ResultQueue)

      DO UpdateMatchCount
      POST(EVENT:NewSelection,?ResultList)
HandleNewSelection   ROUTINE
  ListWithFocus = ?ResultList
  Window{Prop:StatusText,2} = 'RESULTS LIST'

  CASE KEYCODE()
     OF UpKey OROF DownKey OROF PgUpKEy OROF PgDnKey OROF glo:PlusKey OROF glo:MinusKey
        Window{PROP:Timer} = 5
        LastNavTime = CLOCK()
        EditPaneRefreshPending = TRUE
  ELSE
        DO RefreshEditPane
  END


RefreshEditPane   ROUTINE
   DATA
p              LONG
targetLine     LONG
scrollDelta    LONG

   CODE
      IF SearchQueue.feqSearchProgress{PROP:Hide} = FALSE
         EXIT
      END

      EditPaneRefreshPending = FALSE
      ThisWindow.Update()

      IF bAutoSearch = TRUE AND bClosingDown = FALSE
         bAutoSearch = FALSE
         POST(EVENT:Accepted,?cmdSearch)
      END

      IF RECORDS(SearchQueue.ResultQueue) = 0
         IF ViewerActive
            IF SciControl.GetModify() AND szTitle <> ''
               DO Handle_FileModified
            END
            SciControl.SetDefaultStyles()
            ViewerActive = FALSE
         END
         Window{PROP:StatusText,1} = ''
      ELSE
         IF CHOICE(?ResultList) = 0
            GET(SearchQueue.ResultQueue,1)
         ELSE
            GET(SearchQueue.ResultQueue,CHOICE(?ResultList))
         END
         IF ViewerActive AND CurrentFilename <> SearchQueue.ResultQueue.SortName
            IF SciControl.GetModify() AND szTitle <> ''
               DO Handle_FileModified
            END
            ViewerActive = FALSE
         END
         IF ViewerActive = FALSE
            CurrentFilename = CLIP(SearchQueue.ResultQueue.SortName)
            szTitle = SearchQueue.ResultQueue.Path & SearchQueue.ResultQueue.Filename & SearchQueue.ResultQueue.szExtension
            IF glo:bHideEditPanel = FALSE
               DISPLAY(?szTitle)
            END
            ViewerActive = SciControl.OpenFile(CurrentFilename)
            IF ViewerActive = TRUE
               SciControl.MarkerDeleteAll(markerBookmark)
               !set bookmarks
               p = CHOOSE(POINTER(SearchQueue.ResultQueue) > 0,POINTER(SearchQueue.ResultQueue),1)
               GET(SearchQueue.ResultQueue,+SearchQueue.ResultQueue.SortName)
               LOOP WHILE ((NOT ERRORCODE()) AND (SearchQueue.ResultQueue.SortName = CurrentFilename))
                  BookmarkAdd(SearchQueue.ResultQueue.LineNo-1)
                  GET(SearchQueue.ResultQueue,POINTER(SearchQueue.ResultQueue)+1)
               END
               GET(SearchQueue.ResultQueue,p)
               ?ResultList{PROP:Selected} = p

               IF glo:bHideEditPanel = TRUE
                  SciControl.SetHide(TRUE)
               END

               !EVENT:GOTONEWLINE sometimes goes missing
               !this code forces the issue on a new file load
               SciControl.GoToLine(SearchQueue.ResultQueue.LineNo-1)
               targetLine = SciControl.GetFirstVisibleLine()+(SciControl.LinesOnScreen()/2)
               scrollDelta = SearchQueue.ResultQueue.LineNo - targetLine
               SciControl.LineScroll(0,scrollDelta)

            END
         END
         POST(EVENT:GOTONEWLINE)
      END
HandleOrientationChange ROUTINE
   IF glo:SplitterOrientation = SplitterOrientation:Vertical
      ResultListMenu.SetIcon('Layout','SplitH.ico')
      ?cmdLayout{PROP:Icon} = '~SplitH.ico'
      ?cmdLayout{PROP:Tip} = 'Switch to Horizontal Layout'
      HIDE(?szTitle:2)
      HIDE(?cmdSaveWarn:2)
      IF glo:bHideEditPanel = FALSE
         UNHIDE(?szTitle)
         UNHIDE(?cmdSaveWarn)
      END
      IF glo:SplitX = 0
         glo:SplitX = (Window{PROP:Width}/2)
      END
      SETPOSITION(?SplitterBar,glo:SplitX,0,SplitterBarSize,Window{PROP:Height}+1)
   ELSE
      ResultListMenu.SetIcon('Layout','SplitV.ico')
      ?cmdLayout{PROP:Icon} = '~SplitV.ico'
      ?cmdLayout{PROP:Tip} = 'Switch to Vertical Layout'
      HIDE(?szTitle)
      HIDE(?cmdSaveWarn)
      IF glo:bHideEditPanel = FALSE
         UNHIDE(?szTitle:2)
         UNHIDE(?cmdSaveWarn:2)
      END
      IF glo:SplitY = 0
         glo:SplitY = (Window{PROP:Height}/2)-1
      END
      SETPOSITION(?SplitterBar,0,glo:SplitY,Window{PROP:Width}+1,SplitterBarSize)
   END
   DO HandleResize
HandleCenterBar   ROUTINE
   CASE glo:SplitterOrientation
     OF SplitterOrientation:Vertical
        glo:SplitX = (Window{PROP:Width}/2)
        SETPOSITION(?SplitterBar,glo:SplitX,0,SplitterBarSize,Window{PROP:Height}+1)
     OF SplitterOrientation:Horizontal
        glo:SplitY = (Window{PROP:Height}/2)
        SETPOSITION(?SplitterBar,0,glo:SplitY,Window{PROP:Width}+1,SplitterBarSize)
   END
   DO HandleResize
HandleHideResults ROUTINE
      IF glo:bHideResultsPanel = TRUE
         HIDE(?Application:Box)
         HIDE(?CurrentSearch)
         HIDE(?cmdCloseTab)
         HIDE(?szMatchesFound)
         HIDE(?ResultList)
         HIDE(?SplitterBar)
      ELSE
         UNHIDE(?Application:Box)
         UNHIDE(?CurrentSearch)
         UNHIDE(?cmdCloseTab)
         UNHIDE(?szMatchesFound)
         UNHIDE(?ResultList)
         UNHIDE(?SplitterBar)
      END
      DO HandleResize
HandleHideEdit    ROUTINE
      IF glo:bHideEditPanel = TRUE
         CASE glo:SplitterOrientation
           OF SplitterOrientation:Vertical
              HIDE(?szTitle)
              HIDE(?cmdSaveWarn)
         ELSE
              HIDE(?szTitle:2)
              HIDE(?cmdSaveWarn:2)
         END
         HIDE(?SplitterBar)
         SciControl.SetHide(TRUE)
      ELSE
         CASE glo:SplitterOrientation
           OF SplitterOrientation:Vertical
              UNHIDE(?szTitle)
              UNHIDE(?cmdSaveWarn)
         ELSE
              UNHIDE(?szTitle:2)
              UNHIDE(?cmdSaveWarn:2)
         END
         UNHIDE(?SplitterBar)
         SciControl.SetHide(FALSE)
      END
      DO HandleResize
HandleResize      ROUTINE
   DATA
X        LONG
Y        LONG
W        LONG
H        LONG
Xpixels  LONG
Ypixels  LONG
Wpixels  LONG
Hpixels  LONG

   CODE
      IF glo:bHideEditPanel = TRUE OR glo:bHideResultsPanel = TRUE
         IF glo:bHideEditPanel = TRUE
            SETPOSITION(?CurrentSearch,,,Window{PROP:Width},Window{PROP:Height})
            SETPOSITION(?ResultList,,,Window{PROP:Width}-2,Window{PROP:Height}-47)
            ?Application:Box{PROP:Width} = Window{PROP:Width}
         ELSE
            CASE glo:SplitterOrientation
              OF SplitterOrientation:Vertical
                 SETPOSITION(?SciControl:Region,1,-18,Window{PROP:Width}-3,Window{PROP:Height}-20)
              OF SplitterOrientation:Horizontal
                 SETPOSITION(?SciControl:Region,1,-9,Window{PROP:Width}-3,Window{PROP:Height}-27)
            END
            Window{PROP:Pixels} = TRUE
            GETPOSITION(?sciControl:Region,Xpixels,Ypixels,Wpixels,Hpixels)
            Wpixels += 2
            SETPOSITION(?sciControl:Region,Xpixels,Ypixels,Wpixels,Hpixels)
            Window{PROP:Pixels} = FALSE

            SciControl.Reset(FALSE)

            CASE glo:SplitterOrientation
              OF SplitterOrientation:Vertical
            ELSE
                 ?szTitle:2{PROP:Width} = Window{PROP:Width}
                 ?szTitle:2{PROP:YPos} = 0
                 ?cmdSaveWarn:2{PROP:XPos} = 0
                 ?cmdSaveWarn:2{PROP:YPos} = 0
            END
         END
      ELSE
         CASE glo:SplitterOrientation
           OF SplitterOrientation:Vertical
              X = ?SplitterBar{PROP:Xpos}
              ?SplitterBar{PROP:Height} = Window{PROP:Height}

              !SETPOSITION(?SciControl:Region,X+2,-18,Window{PROP:Width}-(X+3),Window{PROP:Height}-20)
              !mr 20180912 SETPOSITION(?SciControl:Region,X+SplitterBarSize,-18,Window{PROP:Width}-(X+SplitterBarSize+1),Window{PROP:Height}-20)
              SETPOSITION(?SciControl:Region,X+SplitterBarSize,-30,Window{PROP:Width}-(X+SplitterBarSize+1),Window{PROP:Height}-25) !MR 20181001 adjusted height from -20 to -25
              Window{PROP:Pixels} = TRUE
              W = ?SciControl:Region{PROP:Height}
              W += 2
              ?SciControl:Region{PROP:Height} = W
              Window{PROP:Pixels} = FALSE
              SciControl.Reset(FALSE)

              SETPOSITION(?CurrentSearch,,,X,Window{PROP:Height})
              SETPOSITION(?ResultList,,,X-2,Window{PROP:Height}-47)

              W = CHOOSE(X <= 420, 396 - (420 - X),398)
              W = CHOOSE(W < 0,0,W)
              ?szMatchesFound{Prop:Width} = W

              SearchQueue.feqSearchProgress{PROP:Width} = W
              DISPLAY(SearchQueue.feqSearchProgress)

              IF X < LastToolbarButton{PROP:Xpos} + LastToolbarButton{PROP:Width} - 3
                 ?szTitle{PROP:Xpos} = LastToolbarButton{PROP:Xpos} + LastToolbarButton{PROP:Width} + 5
              ELSE
                 ?szTitle{PROP:Xpos} = ?SciControl:Region{PROP:Xpos} + 5
              END
              W = (Window{PROP:Width} - ?szTitle{PROP:Xpos}) - 20
              W = CHOOSE(W < 0,0,W)
              ?szTitle{PROP:Width} = W

              ?cmdSaveWarn{PROP:XPos} = ?szTitle{PROP:Xpos} + W + 4
              ?cmdSaveWarn{PROP:YPos} = ?szTitle{PROP:Ypos}



           OF SplitterOrientation:Horizontal
              Y = ?SplitterBar{PROP:Ypos}
              IF Y < 10
                 Y = (Window{PROP:Height}/2) - 1
              END
              ?SplitterBar{PROP:Width} = Window{PROP:Width}

              !SETPOSITION(?SciControl:Region,1,Y-8,Window{PROP:Width}-3,Window{PROP:Height}-(Y+28))
              ! mr 20180912 SETPOSITION(?SciControl:Region,1,Y-6,Window{PROP:Width}-3,Window{PROP:Height}-(Y+31))
              SETPOSITION(?SciControl:Region,1,Y-19,Window{PROP:Width}-3,Window{PROP:Height}-(Y+36)) !mr 20180912 adjusted Y to -19  !mr 20181001 adjusted height from +31
              Window{PROP:Pixels} = TRUE
              GETPOSITION(?sciControl:Region,Xpixels,Ypixels,Wpixels,Hpixels)
              !Ypixels -= 1
              Wpixels += 2
              !Hpixels += 3
              SETPOSITION(?sciControl:Region,Xpixels,Ypixels,Wpixels,Hpixels)
              Window{PROP:Pixels} = FALSE

              SciControl.Reset(FALSE)

              SETPOSITION(?CurrentSearch,0,,Window{PROP:Width},Y)
              SETPOSITION(?ResultList,1,,Window{PROP:Width}-2,Y-29)

              ?szMatchesFound{Prop:Width} = 398

              ?szTitle:2{PROP:Width} = Window{PROP:Width}
              ?szTitle:2{PROP:YPos} = Y+(SplitterBarSize-1)
              Window{PROP:Pixels} = TRUE
              ?szTitle:2{PROP:YPos} = ?szTitle:2{PROP:YPos} + 1
              Window{PROP:Pixels} = FALSE

              ?cmdSaveWarn:2{PROP:XPos} = 0
              ?cmdSaveWarn:2{PROP:YPos} = Y+(SplitterBarSize-1)
              Window{PROP:Pixels} = TRUE
              ?cmdSaveWarn:2{PROP:YPos} = ?cmdSaveWarn:2{PROP:YPos} + 1
              Window{PROP:Pixels} = FALSE

         END
         ?Application:Box{PROP:Width} = ?CurrentSearch{PROP:Width}
      END
Handle_FileModified  ROUTINE
   IF glo:bAutoSave = TRUE
      IF glo:bShowAutoSaveWarning = TRUE
         CASE ConfirmAutoSave()
           OF BUTTON:YES
              SciControl.SaveFile(szTitle)
           OF BUTTON:NO
         END
      ELSE
         SciControl.SaveFile(szTitle)
      END
   ELSE
      CASE MESSAGE('Save changes to ' & szTitle & '?','Kwik Source Search',ICON:Exclamation,BUTTON:YES+BUTTON:NO,BUTTON:YES)
        OF BUTTON:YES
           SciControl.SaveFile(szTitle)
        OF BUTTON:NO
      END
   END
   EXIT
UpdateMatchCount     ROUTINE
   DATA
Matches        LONG
Deletes        LONG
Remaining      LONG

      CODE
         ClockQueue.tabNumber = SearchQueue.tabNumber
         GET(ClockQueue,ClockQueue.tabNumber)
         Deletes = RECORDS(SearchQueue.UndoQueue)
         Remaining = RECORDS(SearchQueue.ResultQueue)
         Matches = Remaining + Deletes
         SearchQueue.szMatchesFound = CLIP(LEFT(FORMAT(Matches,@N_14)))   & ' match' & CHOOSE(Matches = 1,'','es') & ' found in '          & |
                                      CLIP(LEFT(FORMAT(ClockQueue.lClock/100,@N7.1)))  & ' seconds' ! mr 20180912 Findstr command placed in clipboard.'
         IF Deletes = 0
            SearchQueue.szMatchesFound = SearchQueue.szMatchesFound & '.'
         ELSE
            SearchQueue.szMatchesFound = SearchQueue.szMatchesFound & ', ' & CLIP(LEFT(FORMAT(Remaining,@N_14))) & ' remaining.'
         END

         DISPLAY(?szMatchesFound)
PreDelete   ROUTINE
   GET(SearchQueue.UndoQueue,RECORDS(SearchQueue.UndoQueue))
   IF ERRORCODE()
      thisDeleteInstance = 1
   ELSE
      thisDeleteInstance = SearchQueue.UndoQueue.DeleteInstance + 1
   END
   DeleteCount = 0
   FREE(DeleteQueue)
   GET(SearchQueue.ResultQueue,CHOICE(?ResultList))
   saveFilename = SearchQueue.ResultQueue.SortName
   saveLineNo   = SearchQueue.ResultQueue.LineNo
   savePosition = POINTER(SearchQueue.ResultQueue)
PostDelete  ROUTINE
   ResultCount = RECORDS(SearchQueue.ResultQueue)
   DeleteCount = RECORDS(DeleteQueue)
   RemainingCount = ResultCount - DeleteCount
   IF DeleteCount > 0
      IF glo:nDeleteWarningCount > 0 AND DeleteCount >= glo:nDeleteWarningCount
         CASE MESSAGE(DeleteCount & ' record' & CHOOSE(DeleteCount > 1,'s','') & ' out of ' & ResultCount & ' will be deleted leaving ' & RemainingCount & ' record' & CHOOSE(RemainingCount > 1,'s','') & '.|Do you want to continue?','Confirm Delete',ICON:Question,BUTTON:YES+BUTTON:NO,BUTTON:YES)
           OF BUTTON:YES
              DO ProcessDeleteQueue
           OF BUTTON:NO
         END
      ELSE
         DO ProcessDeleteQueue
      END
      FREE(DeleteQueue)
      DISPLAY(?ResultList)
      DO UpdateMatchCount
      DO HandleNewSelection
      IF bPlaying
         MacroQueue.mark = FALSE
         PUT(MacroQueue)
         POST(EVENT:PLAYNEXTSELECTION)
      END
   END
   EXIT
ProcessDeleteQueue      ROUTINE
   DATA
indx        LONG

   CODE
      LOOP indx = 1 TO RECORDS(DeleteQueue)
         GET(DeleteQueue,indx)
         GET(SearchQueue.ResultQueue,DeleteQueue.pointer)
         SearchQueue.UndoQueue = SearchQueue.ResultQueue
         SearchQueue.UndoQueue.Position = POINTER(SearchQueue.ResultQueue)
         SearchQueue.UndoQueue.DeleteInstance = thisDeleteInstance
         ADD(SearchQueue.UndoQueue)
         IF CurrentFilename = SearchQueue.ResultQueue.SortName
            BookmarkDelete(SearchQueue.ResultQueue.LineNo-1)
         END
         DELETE(SearchQueue.ResultQueue)
      END
      IF bRecording = TRUE
         ADD(MacroQueue)
      END

      SearchQueue.ResultQueue.SortName = saveFilename
      SearchQueue.ResultQueue.LineNo   = saveLineNo
      GET(SearchQueue.ResultQueue,+SearchQueue.ResultQueue.SortName,+SearchQueue.ResultQueue.LineNo)
      IF ERRORCODE()
         IF savePosition > RECORDS(SearchQueue.ResultQueue)
            savePosition = RECORDS(SearchQueue.ResultQueue)
         END
         GET(SearchQueue.ResultQueue,savePosition)
      END
      ?ResultList{PROP:Selected} = POINTER(SearchQueue.ResultQueue)
   EXIT
DeleteLine  ROUTINE
   DATA
errFlag        BOOL(FALSE)

   CODE
      DO PreDelete

      IF bPlaying > 0
         SearchQueue.ResultQueue.SortName = MacroQueue.szField1
         SearchQueue.ResultQueue.LineNo   = MacroQueue.szField2
         GET(SearchQueue.ResultQueue,+SearchQueue.ResultQueue.SortName,+SearchQueue.ResultQueue.LineNo)
         IF ERRORCODE()
            errFlag = TRUE
         END
         bPlaying -= 1
      END

      IF ErrFlag = FALSE
         DeleteQueue.pointer = POINTER(SearchQueue.ResultQueue)
         ADD(DeleteQueue)

         IF bRecording = TRUE
            MacroQueue.feqButton  = BUTTON:DeleteLine
            MacroQueue.szField1   = SearchQueue.ResultQueue.SortName
            MacroQueue.szField2   = SearchQueue.ResultQueue.LineNo
         END

         DO PostDelete
      END
DeletePath  ROUTINE
   DATA
thisPath             LIKE(SearchQueue.ResultQueue.Path)
i                    LONG
j                    LONG

   CODE
      DO PreDelete

      IF bPlaying > 0
         thisPath = MacroQueue.szField1
         bPlaying -= 1
      ELSE
         thisPath = UPPER(SearchQueue.ResultQueue.Path)
      END

      j = RECORDS(SearchQueue.ResultQueue)
      LOOP i = j TO 1 BY -1
         GET(SearchQueue.ResultQueue,i)
         IF UPPER(SearchQueue.ResultQueue.Path) = thisPath
            DeleteQueue.pointer = POINTER(SearchQueue.ResultQueue)
            ADD(DeleteQueue)
         END
      END

      IF bRecording = TRUE
         MacroQueue.feqButton  = BUTTON:DeletePath
         MacroQueue.szField1   = thisPath
         MacroQueue.szField2   = ''
      END

      DO PostDelete
DeleteFilename ROUTINE
   DATA
thisSortname         LIKE(SearchQueue.ResultQueue.SortName)
i                    LONG
j                    LONG

   CODE
      DO PreDelete

      IF bPlaying > 0
         thisSortname = UPPER(MacroQueue.szField3) & UPPER(MacroQueue.szField2) & UPPER(MacroQueue.szField3)
         bPlaying -= 1
      ELSE
         thisSortname = SearchQueue.ResultQueue.SortName
      END

      j = RECORDS(SearchQueue.ResultQueue)
      LOOP i = j TO 1 BY -1
         GET(SearchQueue.ResultQueue,i)
         IF SearchQueue.ResultQueue.SortName = thisSortname
            DeleteQueue.pointer = POINTER(SearchQueue.ResultQueue)
            ADD(DeleteQueue)
         END
      END

      IF bRecording = TRUE
         MacroQueue.feqButton   = BUTTON:DeleteFile
         MacroQueue.szField1    = SearchQueue.ResultQueue.Filename
         MacroQueue.szField2    = SearchQueue.ResultQueue.szExtension
         MacroQueue.szField3    = SearchQueue.ResultQueue.Path
      END

      DO PostDelete
DeleteExtension   ROUTINE
   DATA
thisExtension        LIKE(SearchQueue.ResultQueue.szExtension)
i                    LONG
j                    LONG

   CODE
      DO PreDelete

      IF bPlaying > 0
        thisExtension = MacroQueue.szField1
        bPlaying -= 1
      ELSE
        thisExtension = UPPER(SearchQueue.ResultQueue.szExtension)
      END

      j = RECORDS(SearchQueue.ResultQueue)
      LOOP i = j TO 1 BY -1
         GET(SearchQueue.ResultQueue,i)
         IF UPPER(SearchQueue.ResultQueue.szExtension) = thisExtension
            DeleteQueue.pointer = POINTER(SearchQueue.ResultQueue)
            ADD(DeleteQueue)
         END
      END

      IF bRecording = TRUE
         MacroQueue.feqButton   = BUTTON:DeleteExtension
         MacroQueue.szField1    = thisExtension
         MacroQueue.szField2    = ''
      END

      DO PostDelete
DeleteBuiltInClwFiles ROUTINE  !added MR 20190101
   DATA
thisClwFile          LIKE(SearchQueue.ResultQueue.Filename)
i                    LONG
j                    LONG

   CODE
      DO PreDelete

      j = RECORDS(SearchQueue.ResultQueue)
      SETCURSOR(CURSOR:Wait)
      LOOP i = j TO 1 BY -1
         GET(SearchQueue.ResultQueue,i)
         thisClwFile = SearchQueue.ResultQueue.Filename & SearchQueue.ResultQueue.szExtension
         IF (INSTRING('_BC.CLW',UPPER(ThisClwFile),1,1) > 0 OR |
            INSTRING('_BC0.CLW',UPPER(ThisClwFile),1,1) > 0 OR |
            INSTRING('_BC1.CLW',UPPER(ThisClwFile),1,1) > 0 OR |
            INSTRING('_BC2.CLW',UPPER(ThisClwFile),1,1) > 0 OR |
            INSTRING('_BC3.CLW',UPPER(ThisClwFile),1,1) > 0 OR |
            INSTRING('_BC4.CLW',UPPER(ThisClwFile),1,1) > 0 OR |
            INSTRING('_SF.CLW',UPPER(ThisClwFile),1,1) > 0 OR |
            INSTRING('_RU.CLW',UPPER(ThisClwFile),1,1) > 0 OR |
            INSTRING('_RD.CLW',UPPER(ThisClwFile),1,1) > 0 )
            DeleteQueue.pointer = POINTER(SearchQueue.ResultQueue)
            ADD(DeleteQueue)
         END
      END
      SETCURSOR() 

      IF bRecording = TRUE
         MacroQueue.feqButton   = BUTTON:DeleteBuiltInClw
         MacroQueue.szField1    = ''
         MacroQueue.szField2    = ''
      END

      DO PostDelete
DeleteCommentLines      ROUTINE
   DATA
i           LONG
j           LONG
bMatchMode  BYTE
thisText    LIKE(SearchQueue.ResultQueue.Text)

   CODE
      DO PreDelete

      j = RECORDS(SearchQueue.ResultQueue)
      LOOP i = j TO 1 BY -1
         GET(SearchQueue.ResultQueue,i)
         thisText = CLIP(LEFT(SearchQueue.ResultQueue.Text))
         bMatchMode = Match:Simple
         IF NOT SearchQueue.bCaseSensitive
            bMatchMode = BOR(bMatchMode,Match:NoCase)
         END
         IF SearchQueue.bUseRegularExpressions
            bMatchMode = BOR(bMatchMode,Match:Regular)
         END
         IF NOT MatchWithoutComment(thisText,SearchQueue.szPattern,bMatchMode,SearchQueue.ResultQueue.szExtension)
            DeleteQueue.pointer = POINTER(SearchQueue.ResultQueue)
            ADD(DeleteQueue)
         END
      END

      IF bRecording = TRUE
         MacroQueue.feqButton   = BUTTON:DeleteComments
         MacroQueue.szField1    = ''
         MacroQueue.szField2    = ''
      END

      DO PostDelete
   EXIT
DeleteLabelLines     ROUTINE
   DATA
i        LONG
j        LONG
thisText LIKE(SearchQueue.ResultQueue.Text)

   CODE
      DO PreDelete

      j = RECORDS(SearchQueue.ResultQueue)
      LOOP i = j TO 1 BY -1
         GET(SearchQueue.ResultQueue,i)
         IF SearchQueue.ResultQueue.Text[1] <> ' ' AND SearchQueue.ResultQueue.Text[1] <> '!'
            DeleteQueue.pointer = POINTER(SearchQueue.ResultQueue)
            ADD(DeleteQueue)
         END
      END

      IF bRecording = TRUE
         MacroQueue.feqButton   = BUTTON:DeleteLabels
         MacroQueue.szField1    = ''
         MacroQueue.szField2    = ''
      END

      DO PostDelete
   EXIT
DeleteCodeMatches    ROUTINE
   DATA
i                    LONG
j                    LONG

   CODE
      DO PreDelete

      j = RECORDS(SearchQueue.ResultQueue)
      LOOP i = j TO 1 BY -1
         GET(SearchQueue.ResultQueue,i)
         IF SearchQueue.ResultQueue.szSection = 'CODE'
            DeleteQueue.pointer = POINTER(SearchQueue.ResultQueue)
            ADD(DeleteQueue)
         END
      END

      IF bRecording = TRUE
         MacroQueue.feqButton   = BUTTON:DeleteCode
         MacroQueue.szField1    = ''
         MacroQueue.szField2    = ''
      END

      DO PostDelete
DeleteDataMatches    ROUTINE
   DATA
i                    LONG
j                    LONG

   CODE
      DO PreDelete

      j = RECORDS(SearchQueue.ResultQueue)
      LOOP i = j TO 1 BY -1
         GET(SearchQueue.ResultQueue,i)
         IF SearchQueue.ResultQueue.szSection = 'DATA'
            DeleteQueue.pointer = POINTER(SearchQueue.ResultQueue)
            ADD(DeleteQueue)
         END
      END

      IF bRecording = TRUE
         MacroQueue.feqButton   = BUTTON:DeleteData
         MacroQueue.szField1    = ''
         MacroQueue.szField2    = ''
      END

      DO PostDelete
FindAndDelete     ROUTINE
   DATA
i              LONG
j              LONG
cc             LONG
MatchString    &CSTRING
bMatchFound    LONG
szOption       CSTRING(256)

   CODE
      SearchFindOptions.szText = CLIP(LEFT(SearchQueue.ResultQueue.Text)) & '<13,10>' & |
                                 CHOOSE(SearchQueue.ResultQueue.ProcedureName='','',SearchQueue.ResultQueue.ProcedureName & '<13,10>') & |
                                 SearchQueue.ResultQueue.Filename & SearchQueue.ResultQueue.szExtension & '<13,10>' & |
                                 SearchQueue.ResultQueue.Path

      IF bPlaying > 0
         SearchFindOptions.szFindText = MacroQueue.szField1
         i = 1
         j = INSTRING(';',MacroQueue.szField2,1,i)
         IF j
            szOption = MacroQueue.szField2[1 : j-1]
            CASE szOption
              OF 'Search:Path'
                 SearchFindOptions.SearchLocation = Search:Path
              OF 'Search:Filename'
                 SearchFindOptions.SearchLocation = Search:Filename
              OF 'Search:Extension'
                 SearchFindOptions.SearchLocation = Search:Extension
              OF 'Search:Procedure'
                 SearchFindOptions.SearchLocation = Search:Procedure
            ELSE !'Search:Text'
                 SearchFindOptions.SearchLocation = Search:Text
            END
            i = j+1
            j = INSTRING(';',MacroQueue.szField2,1,i)
            IF j
               szOption = MacroQueue.szField2[i : j-1]
               CASE szOption
                 OF 'Match:Regular'
                    SearchFindOptions.MatchType = Match:Regular
               ELSE !Match:Simple
                    SearchFindOptions.MatchType = Match:Simple
               END
            END
            szOption = MacroQueue.szField2[j+1 : LEN(MacroQueue.szField2)]
            IF szOption = 'Match:NoCase'
               SearchFindOptions.MatchCase = FALSE
            ELSE
               SearchFindOptions.MatchCase = TRUE
            END
         END
         bPlaying -= 1
         cc = Level:Benign
      ELSE
         cc = GetFindDeleteOptions(SearchFindOptions)
      END

      IF cc = Level:Benign    !NOT GetFindDeleteOptions(SearchFindOptions)
         !do search and delete
         DO PreDelete

         CASE SearchFindOptions.SearchLocation
           OF Search:Path
              MatchString &= SearchQueue.ResultQueue.Path
           OF Search:Filename
              MatchString &= SearchQueue.ResultQueue.Filename
           OF Search:Extension
              MatchString &= SearchQueue.ResultQueue.szExtension
           OF Search:Procedure
              MatchString &= SearchQueue.ResultQueue.ProcedureName
         ELSE !Search:Text
              MatchString &= SearchQueue.ResultQueue.Text
         END

         j = RECORDS(SearchQueue.ResultQueue)
         LOOP i = j TO 1 BY -1
            GET(SearchQueue.ResultQueue,i)
            IF SearchFindOptions.MatchType <> Match:Regular
               IF SearchFindOptions.MatchCase
                  bMatchFound = INSTRING(SearchFindOptions.szFindText,MatchString,1)
               ELSE
                  bMatchFound = INSTRING(UPPER(SearchFindOptions.szFindText),UPPER(MatchString),1)
               END
            ELSE
               bMatchFound = MATCH(MatchString,SearchFindOptions.szFindText,Match:Regular)
            END
            IF bMatchFound
               IF SearchFindOptions.DeleteCondition = Delete:Contains
                  DeleteQueue.pointer = POINTER(SearchQueue.ResultQueue)
                  ADD(DeleteQueue)
               END
            ELSE
               IF SearchFindOptions.DeleteCondition = Delete:DoesNotContain
                  DeleteQueue.pointer = POINTER(SearchQueue.ResultQueue)
                  ADD(DeleteQueue)
               END
            END
         END

         IF bRecording = TRUE
            MacroQueue.feqButton   = BUTTON:FindAndDelete
            MacroQueue.szField1    = SearchFindOptions.szFindText
            MacroQueue.szField2    = ''

            CASE SearchFindOptions.SearchLocation
              OF Search:Path
                 MacroQueue.szField2 = 'Search:Path;'
              OF Search:Filename
                 MacroQueue.szField2 = 'Search:Filename;'
              OF Search:Extension
                 MacroQueue.szField2 = 'Search:Extension;'
              OF Search:Procedure
                 MacroQueue.szField2 = 'Search:Procedure;'
            ELSE !'Search:Text'
                 MacroQueue.szField2 = 'Search:Text;'
            END

            CASE SearchFindOptions.MatchType
              OF 'Match:Regular'
                 MacroQueue.szField2 = MacroQueue.szField2 & 'Match:Regular;'
            ELSE !Match:Simple
                 MacroQueue.szField2 = MacroQueue.szField2 & 'Match:Simple;'
            END

            IF SearchFindOptions.MatchCase = FALSE
               MacroQueue.szField2 = MacroQueue.szField2 & 'Match:NoCase'
            ELSE
               MacroQueue.szField2 = MacroQueue.szField2 & 'Match:Case'
            END
         END

         DO PostDelete
      END
MoveToNextLine    ROUTINE
   DATA

i     LONG
j     LONG

   CODE
       j = RECORDS(SearchQueue.ResultQueue)
       i = POINTER(SearchQueue.ResultQueue)
       IF i < j
          i += 1
          GET(SearchQueue.ResultQueue,i)
          ?ResultList{PROP:Selected} = POINTER(SearchQueue.ResultQueue)
          POST(EVENT:NewSelection,?ResultList)
       !ELSE
       !   GET(SearchQueue.ResultQueue,1)
       END

       !?ResultList{PROP:Selected} = POINTER(SearchQueue.ResultQueue)
       !POST(EVENT:NewSelection,?ResultList)
MoveToPreviousLine   ROUTINE
   DATA

i     LONG
j     LONG

   CODE
       j = RECORDS(SearchQueue.ResultQueue)
       i = POINTER(SearchQueue.ResultQueue)
       IF i > 1
          i -= 1
          GET(SearchQueue.ResultQueue,i)
          ?ResultList{PROP:Selected} = POINTER(SearchQueue.ResultQueue)
          POST(EVENT:NewSelection,?ResultList)
       !ELSE
       !   GET(SearchQueue.ResultQueue,j)
       END

       !?ResultList{PROP:Selected} = POINTER(SearchQueue.ResultQueue)
       !POST(EVENT:NewSelection,?ResultList)
MoveToNextFile    ROUTINE
   DATA
thisFile       LIKE(SearchQueue.ResultQueue.Filename)
thisExtension  LIKE(SearchQueue.ResultQueue.szExtension)
i              LONG
j              LONG

   CODE

       thisFile      = SearchQueue.ResultQueue.Filename
       thisExtension = SearchQueue.ResultQueue.szExtension

       j = RECORDS(SearchQueue.ResultQueue)
       LOOP i = POINTER(SearchQueue.ResultQueue) TO j
        GET(SearchQueue.ResultQueue,i)
          IF SearchQueue.ResultQueue.Filename <> thisFile OR SearchQueue.ResultQueue.szExtension <> thisExtension
             BREAK
          END
       END
       IF i > j
          GET(SearchQueue.ResultQueue,1)
       END

       ?ResultList{PROP:Selected} = POINTER(SearchQueue.ResultQueue)
       POST(EVENT:NewSelection,?ResultList)
MoveToPreviousFile    ROUTINE
   DATA
thisFile       LIKE(SearchQueue.ResultQueue.Filename)
thisExtension  LIKE(SearchQueue.ResultQueue.szExtension)
i              LONG

   CODE
       thisFile      = SearchQueue.ResultQueue.Filename
       thisExtension = SearchQueue.ResultQueue.szExtension

       LOOP i = POINTER(SearchQueue.ResultQueue) TO 1 BY -1
          GET(SearchQueue.ResultQueue,i)
          IF SearchQueue.ResultQueue.Filename <> thisFile OR SearchQueue.ResultQueue.szExtension <> thisExtension
             BREAK
          END
       END
       IF i = 0
          GET(SearchQueue.ResultQueue,RECORDS(SearchQueue.ResultQueue))
       END

       thisFile = SearchQueue.ResultQueue.Filename
       thisExtension = SearchQueue.ResultQueue.szExtension
       LOOP i = POINTER(SearchQueue.ResultQueue) TO 1 BY -1
          GET(SearchQueue.ResultQueue,i)
          IF SearchQueue.ResultQueue.Filename <> thisFile OR SearchQueue.ResultQueue.szExtension <> thisExtension
             BREAK
          END
       END
       IF i > 0
          GET(SearchQueue.ResultQueue,POINTER(SearchQueue.ResultQueue)+1)
       END

       ?ResultList{PROP:Selected} = POINTER(SearchQueue.ResultQueue)
       POST(EVENT:NewSelection,?ResultList)
MoveToNextFolder     ROUTINE
   DATA
thisFolder  LIKE(SearchQueue.ResultQueue.Path)
i           LONG
j           LONG

   CODE
       thisFolder = SearchQueue.ResultQueue.Path

       j = RECORDS(SearchQueue.ResultQueue)
       LOOP i = POINTER(SearchQueue.ResultQueue) TO j
          GET(SearchQueue.ResultQueue,i)
          IF SearchQueue.ResultQueue.Path <> thisFolder
             BREAK
          END
       END
       IF i > j
          GET(SearchQueue.ResultQueue,1)
       END

       ?ResultList{PROP:Selected} = POINTER(SearchQueue.ResultQueue)
       POST(EVENT:NewSelection,?ResultList)
MoveToPreviousFolder     ROUTINE
   DATA
thisFolder    LIKE(SearchQueue.ResultQueue.Path)
i           LONG

   CODE
       thisFolder = SearchQueue.ResultQueue.Path

       LOOP i = POINTER(SearchQueue.ResultQueue) TO 1 BY -1
          GET(SearchQueue.ResultQueue,i)
          IF SearchQueue.ResultQueue.Path <> thisFolder
             BREAK
          END
       END
       IF i = 0
          GET(SearchQueue.ResultQueue,RECORDS(SearchQueue.ResultQueue))
       END

       thisFolder = SearchQueue.ResultQueue.Path
       LOOP i = POINTER(SearchQueue.ResultQueue) TO 1 BY -1
          GET(SearchQueue.ResultQueue,i)
          IF SearchQueue.ResultQueue.Path <> thisFolder
             BREAK
          END
       END
       IF i > 0
          GET(SearchQueue.ResultQueue,POINTER(SearchQueue.ResultQueue)+1)
       END

       ?ResultList{PROP:Selected} = POINTER(SearchQueue.ResultQueue)
       POST(EVENT:NewSelection,?ResultList)
SaveViewerStyles    ROUTINE
   DATA
K                    LONG
loc:szViewerStyle    CSTRING(256)

   CODE
      LOOP K = 1 TO SCE_CLW_LAST
         loc:szViewerStyle = CLIP(glo:ViewerStyles.StyleGroup[K].Font) & ',' & |
                             glo:ViewerStyles.StyleGroup[K].FontSize   & ',' & |
                             glo:ViewerStyles.StyleGroup[K].FontStyle  & ',' & |
                             glo:ViewerStyles.StyleGroup[K].Bold       & ',' & |
                             glo:ViewerStyles.StyleGroup[K].Italic     & ',' & |
                             glo:ViewerStyles.StyleGroup[K].Underline  & ',' & |
                             glo:ViewerStyles.StyleGroup[K].Fore       & ',' & |
                             glo:ViewerStyles.StyleGroup[K].Back       & ',' & |
                             glo:ViewerStyles.StyleGroup[K].EolFilled  & ',' & |
                             glo:ViewerStyles.StyleGroup[K].CaseOpt    & ',' & |
                             glo:ViewerStyles.StyleGroup[K].Visible    & ',' & |
                             glo:ViewerStyles.StyleGroup[K].HotSpot
         INIMgr.Update('Viewer Styles','ViewerStyle'& FORMAT(K-1,@n02),loc:szViewerStyle)
      END
   EXIT
UpdateStatusBar   ROUTINE
      IF ListWithFocus = ?ResultList
         Window{Prop:StatusText,2} = 'RESULTS LIST'
      ELSE
         Window{Prop:StatusText,2} = 'EDITOR'
      END
      Window{Prop:StatusText,4} = 'Version ' & glo:szVersion
   EXIT
FillListFormatQueue     ROUTINE
   DATA
i     LONG
j     LONG
n     LONG
p1    LONG
p2    LONG
delimiter STRING(1)

   CODE
      FREE(ListFormatQueue)
      j = 1
      n = 1
      delimiter = '#'
      szListBoxFormat = ?ResultList{PROP:Format}
      i = INSTRING(delimiter,szListBoxFormat,1,j)
      IF i = 0
         delimiter = '@'
      END
      i = INSTRING(delimiter,szListBoxFormat,1,j)
      LOOP WHILE i > 0
         j = i + 1
         i = INSTRING(delimiter,szListBoxFormat,1,j)
         ListFormatQueue.ColumnFormat = szListBoxFormat[n : i]
         p1 = INSTRING('~',ListFormatQueue.ColumnFormat,1,1) + 1
         p2 = INSTRING('~',ListFormatQueue.ColumnFormat,1,p1) - 1
         ListFormatQueue.ColumnName = ListFormatQueue.ColumnFormat[p1 : p2]
         IF delimiter = '@'
            CASE ListFormatQueue.ColumnName
              OF 'Path'
                 ListFormatQueue.ColumnFormat = ListFormatQueue.ColumnFormat & '#1#'
              OF 'Filename'
                 ListFormatQueue.ColumnFormat = ListFormatQueue.ColumnFormat & '#2#'
              OF 'Ext'
                 ListFormatQueue.ColumnFormat = ListFormatQueue.ColumnFormat & '#3#'
              OF 'Line'
                 ListFormatQueue.ColumnFormat = ListFormatQueue.ColumnFormat & '#4#'
              OF 'Location'
                 ListFormatQueue.ColumnFormat = ListFormatQueue.ColumnFormat & '#5#'
              OF 'Text'
                 ListFormatQueue.ColumnFormat = ListFormatQueue.ColumnFormat & '#6#'
            END
         END
         ListFormatQueue.ColumnSequence = RECORDS(ListFormatQueue) + 1
         ADD(ListFormatQueue)
         n = i + 1
         IF n > LEN(szListBoxFormat)
            BREAK
         ELSE
            j = n
            i = INSTRING(delimiter,szListBoxFormat,1,j)
         END
      END
   EXIT
SetNewTabFont  ROUTINE
  NewTab{PROP:FontName}  = szNewTabFontName
  NewTab{PROP:FontSize}  = szNewTabFontSize
  NewTab{PROP:FontColor} = szNewTabFontColor
  NewTab{PROP:FontStyle} = szNewTabFontStyle
SetTabFont  ROUTINE
  NewTab{PROP:FontName}  = szTabFontName
  NewTab{PROP:FontSize}  = szTabFontSize
  NewTab{PROP:FontColor} = szTabFontColor
  NewTab{PROP:FontStyle} = szTabFontStyle
AdjustFontColour  ROUTINE
  IF ColourBrightness(glo:ApplicationColor) > 130
     !black
     ?szTitle:2{PROP:FontColor} = COLOR:Black
  ELSE
     !white
     ?szTitle:2{PROP:FontColor} = COLOR:White
  END

  IF ColourBrightness(glo:ToolbarColor) > 130
     !black
     ?cmdSearch{PROP:FontColor} = COLOR:Black
     ?szTitle{PROP:FontColor} = COLOR:Black
  ELSE
     !white
     ?cmdSearch{PROP:FontColor} = COLOR:White
     ?szTitle{PROP:FontColor} = COLOR:White
  END
SearchReplaceAll  ROUTINE
CreateRestorePoint   ROUTINE
   DATA
i           LONG
p           LONG
szFilename  CSTRING(261)
Options     &FindStrOptionsGroupType
dwId        DWORD
rrlQueue    QUEUE(ff_:queue),PRE(RRL)
            END

   CODE
      dwId = kcr_GetCurrentProcessId()
      svSpecialFolder.CreateDirIn(SV:CSIDL_APPDATA, '\Devuna\KSS\RestorePoint\' & dwId)

      !make sure folder is empty
      szFilename = svSpecialFolder.GetDir(SV:CSIDL_APPDATA) & '\Devuna\KSS\RestorePoint\' & dwId & '\*.RRL'
      DIRECTORY(rrlQueue,szFilename,ff_:NORMAL)
      LOOP i = 1 TO RECORDS(rrlQueue)
         GET(rrlQueue,i)
         szFilename = svSpecialFolder.GetDir(SV:CSIDL_APPDATA) & '\Devuna\KSS\RestorePoint\' & dwId & '\' & rrlQueue.name
         IF kcr_DeleteFile(szFilename)
         END
      END

      !now create the restore point
      p = POINTER(SearchQueue)
      options &= ADDRESS(SearchQueue)
      LOOP i = 1 to RECORDS(SearchQueue)
         GET(SearchQueue,i)
         szFilename = svSpecialFolder.GetDir(SV:CSIDL_APPDATA) & '\Devuna\KSS\RestorePoint\' & dwId & '\KSS_Results_Tab' & FORMAT(SearchQueue.tabNumber,@N03) & '.rrl'
         CreateRestorePoint(Options, szFilename)
      END
      GET(SearchQueue,p)
   EXIT

ThisWindow.Ask PROCEDURE

  CODE
  DO UpdateStatusBar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

i                    LONG
j                    LONG
k                    LONG
hKeyResult           ULONG
pType                ULONG
pData                ULONG
RetVal               LONG
szSubKey             CSTRING(256)
szValueName          CSTRING(256)
loc:szViewerStyle    CSTRING(256)

cMonitors            LONG
newX                 LONG
newY                 LONG
newW                 LONG
newH                 LONG

MONITOR_DEFAULTTOPRIMARY   EQUATE(00000001h)
  CODE
    
  GlobalErrors.SetProcedureName('Main')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ! Initialize Styles
  !----------------------------------------------------------------------------
  LOOP K = 1 TO SCE_CLW_LAST
     EXECUTE K
        loc:szViewerStyle = 'Consolas,10,400,0,0,0,0,16777215,0,0,1,0'
        loc:szViewerStyle = 'Consolas,10,400,0,0,0,255,16777215,0,0,1,0'
        loc:szViewerStyle = 'Consolas,10,400,0,0,0,32768,16777215,0,0,1,0'
        loc:szViewerStyle = 'Consolas,10,700,1,0,0,8421504,16777215,0,0,1,0'
        loc:szViewerStyle = 'Consolas,10,700,1,0,0,0,16777215,0,0,1,0'
        loc:szViewerStyle = 'Consolas,10,700,1,0,0,14056154,16777215,0,0,1,0'
        loc:szViewerStyle = 'Consolas,10,700,1,0,0,14056154,16777215,0,0,1,0'
        loc:szViewerStyle = 'Consolas,10,700,1,0,0,8421504,16777215,0,0,1,0'
        loc:szViewerStyle = 'Consolas,10,700,1,0,0,16711680,16777215,0,0,1,1'
        loc:szViewerStyle = 'Consolas,10,700,1,0,0,11829830,16777215,0,0,1,0'
        loc:szViewerStyle = 'Consolas,10,700,1,0,0,7346457,16777215,0,0,1,0'
        loc:szViewerStyle = 'Consolas,10,700,1,0,0,7346457,16777215,0,0,1,1'
        loc:szViewerStyle = 'Consolas,10,700,1,0,0,14381203,16777215,0,0,1,1'
        loc:szViewerStyle = 'Consolas,10,400,0,0,0,16711680,16777215,0,0,1,1'
        loc:szViewerStyle = 'Consolas,10,700,1,0,0,16760576,16777215,0,0,1,1'
        loc:szViewerStyle = 'Consolas,10,700,1,0,0,0,16777215,0,0,1,0'
        loc:szViewerStyle = 'Consolas,10,700,1,0,0,8421376,16777215,0,0,1,0'
     END
  
     INIMgr.Fetch('Viewer Styles','ViewerStyle'& FORMAT(K-1,@n02),loc:szViewerStyle)
     I = 1
     J = INSTRING(',',loc:szViewerStyle,,I)
     glo:ViewerStyles.StyleGroup[K].Font = loc:szViewerStyle[I : J-1]
     I = J+1
     J = INSTRING(',',loc:szViewerStyle,,I)
     glo:ViewerStyles.StyleGroup[K].FontSize = loc:szViewerStyle[I : J-1]
     I = J+1
     J = INSTRING(',',loc:szViewerStyle,,I)
     glo:ViewerStyles.StyleGroup[K].FontStyle = loc:szViewerStyle[I : J-1]
     I = J+1
     J = INSTRING(',',loc:szViewerStyle,,I)
     glo:ViewerStyles.StyleGroup[K].Bold = loc:szViewerStyle[I : J-1]
     I = J+1
     J = INSTRING(',',loc:szViewerStyle,,I)
     glo:ViewerStyles.StyleGroup[K].Italic = loc:szViewerStyle[I : J-1]
     I = J+1
     J = INSTRING(',',loc:szViewerStyle,,I)
     glo:ViewerStyles.StyleGroup[K].Underline = loc:szViewerStyle[I : J-1]
     I = J+1
     J = INSTRING(',',loc:szViewerStyle,,I)
     glo:ViewerStyles.StyleGroup[K].Fore = loc:szViewerStyle[I : J-1]
     I = J+1
     J = INSTRING(',',loc:szViewerStyle,,I)
     glo:ViewerStyles.StyleGroup[K].Back = loc:szViewerStyle[I : J-1]
     I = J+1
     J = INSTRING(',',loc:szViewerStyle,,I)
     glo:ViewerStyles.StyleGroup[K].EolFilled = loc:szViewerStyle[I : J-1]
     I = J+1
     J = INSTRING(',',loc:szViewerStyle,,I)
     glo:ViewerStyles.StyleGroup[K].CaseOpt = loc:szViewerStyle[I : J-1]
     I = J+1
     J = INSTRING(',',loc:szViewerStyle,,I)
     glo:ViewerStyles.StyleGroup[K].Visible = loc:szViewerStyle[I : J-1]
     I = J+1
     J = LEN(CLIP(loc:szViewerStyle))
     glo:ViewerStyles.StyleGroup[K].HotSpot = loc:szViewerStyle[I : J]
  END
  dCompileDate = 80724
  tCompileTime = 7296044
  strBuildDate = Left(Clip(Format(80724,@d10))) & ' - ' & Left(Clip(Format(7296044,@t04)))
  dCompileDate = 80724
  tCompileTime = 7296044
   strBuildNumber = '2022.1.' & 97 + 1  ! because link comes after generate, so the # will be 1 higher. 
  strBuildDate = Left(Clip(Format(80724,@d17))) & ' - ' & Left(Clip(Format(7296044,@t04)))
   glo:szVersion = strBuildNumber !mr 20180819
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?sciControl:Region
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  StartMinute = ''
  SELF.Open(Window)                                        ! Open window
  Window{PROP:Timer} = 0
  SYSTEM{PROP:LazyDisplay} = 1
  glo:MainWindow &= Window
  BUTTON:NextFolder      = ?cmdNextFolder
  BUTTON:PreviousFolder  = ?cmdPreviousFolder
  BUTTON:NextFile        = ?cmdNextFile
  BUTTON:PreviousFile    = ?cmdPreviousFile
  BUTTON:NextLine        = ?cmdNextLine
  BUTTON:PreviousLine    = ?cmdPreviousLine
  BUTTON:DeleteLine      = ?cmdDeleteLine
  BUTTON:DeleteFile      = ?cmdDeleteFile
  BUTTON:DeleteExtension = ?cmdDeleteExtension
  BUTTON:DeletePath      = ?cmdDeletePath
  BUTTON:DeleteComments  = ?cmdDeleteComments
  BUTTON:DeleteLabels    = ?cmdDeleteLabels
  BUTTON:DeleteCode      = ?cmdDeleteCode
  BUTTON:DeleteData      = ?cmdDeleteData
  BUTTON:FindAndDelete   = ?cmdFindAndDelete
  BUTTON:DeleteBuiltInClw = ?cmdDeleteBuiltInClw !rif 20190101 
  ?CurrentSearch{PROP:TabSheetStyle} = TabStyle:BlackAndWhite
  ?CurrentSearch{PROP:NoTheme} = True
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  ?ResultList{PROP:LineHeight} = 10
  feqResultList = ?ResultList
  ResultList::OrigWndProc = ?ResultList{Prop:WndProc}           ! Save address OF code that handles window messages
  ?ResultList{Prop:WndProc} = ADDRESS(ResultList::WndProc)      ! Re-assign address OF code that handles window messages
  Do DefineListboxStyle
  Alert(AltKeyPressed)  ! WinEvent : These keys cause a program to crash on Windows 7 and Windows 10.
  Alert(F10Key)         !
  Alert(CtrlF10)        !
  Alert(ShiftF10)       !
  Alert(CtrlShiftF10)   !
  Alert(AltSpace)       !
!  WinAlertMouseZoom()
!  WinAlert(WE::WM_QueryEndSession,,Return1+PostUser)
  Window{Prop:Alrt,255} = CtrlShiftP
  HIDE(?sciControl:Region)
  ReturnValue = SciControl.Init(Window, ?sciControl:Region, 1004)
  SciControl.SetContextMenuEvent(EVENT:USER)
  IF ReturnValue = Level:Benign
     ThisWindow.AddItem(SciControl.WindowComponent)
  END
  IF ReturnValue = Level:Benign
     bControlInitialised = TRUE
  ELSE
     bControlInitialised = FALSE
  END
  Resizer.Init(AppStrategy:NoResize,Resize:SetMinSize)     ! Don't change the windows controls when window resized
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  INIMgr.Fetch('Main',Window)                              ! Restore window settings from non-volatile store
  !Code from Randy 20190217
  !cMonitors = kcr_GetSystemMetrics(SM_CMONITORS)
  !IF cMonitors = 1  
  !   ptFrom.x = 0
  !   ptFrom.y = 0
  !   hMonitor = kcr_MonitorFromPoint(ptFrom,MONITOR_DEFAULTTOPRIMARY)
  !   mi.cbSize = SIZE(mi)
  !   IF kcr_GetMonitorInfo(hMonitor,ADDRESS(mi))
  !      kcr_GetWindowRect(Window{PROP:Handle}, rcMain)
  !      Window{PROP:Pixels} = TRUE        
  !      newX = (mi.rcMonitor.right - Window{PROP:Width})/2
  !      newy = (mi.rcMonitor.bottom - Window{PROP:Height})/2
  !      newW = Window{PROP:Width}
  !      newH = Window{PROP:Height}
  !      Window{PROP:Pixels} = FALSE
  !      kcr_MoveWindow(Window{PROP:Handle},newX,newY,newW,newH,SWP_SHOWWINDOW)        
  !   ELSE
  !      message('GetMonitorInfo failed')
  !   END  
  !END 
  !end of code from Randy 20190217
  
  szSearchPath = LONGPATH()
  !CorrectForOffscreen(Window) !1 line code change from Randy 20190217
  szFileMask = '*.*'
  bUseRegularExpressions = FALSE
  szListBoxFormat = ?ResultList{PROP:Format}
  ! glo:szEditorCommand = '"C:\Program Files (x86)\TextPad 4\TextPad.exe" -q -am"%1"(%2)'
  glo:szEditorCommand = 'Notepad.exe "%1"'
  glo:NewSearchAction = 0
  glo:nDeleteWarningCount = 1
  glo:bDontShowSubdirectoryWarning = FALSE
  glo:SyncPathWithPattern = FALSE
  glo:bHotSpotsEnabled = TRUE
  glo:bAutoSave = FALSE
  
  glo:bHideResultsPanel = FALSE
  glo:bHideEditPanel = FALSE
  
  glo:ResultListFontName  = 'Segoe UI'
  glo:ResultListFontSize  = 10
  glo:ResultListForeColor = COLOR:Black
  glo:ResultListFontStyle = FONT:regular
  glo:MinusKey = MinusKey
  glo:PlusKey  = PlusKey
  glo:szDefaultPropertyFile = 'text'
  glo:sqlProperties = 'mssql'
  
  glo:ApplicationColor = BOR(COLOR_GRADIENTACTIVECAPTION,080000000H)
  glo:ToolbarColor = BOR(COLOR_GRADIENTACTIVECAPTION,080000000H)
  glo:SelectedBack = COLOR:Gray
  glo:BookmarkBack = ColourRGB(0A0h,0FFh,0FFh)
  SearchFindOptions.SearchLocation = Search:Text
  SearchFindOptions.DeleteCondition = Delete:Contains
  SearchFindOptions.MatchType = Match:Wild
  SearchFindOptions.MatchCase = FALSE
  
  INIMgr.Fetch('Search Options','bMatchPatternStartOfLine',bMatchPatternStartOfLine)
  INIMgr.Fetch('Search Options','bMatchPatternEndOfLine',bMatchPatternEndOfLine)
  INIMgr.Fetch('Search Options','bUseRegularExpressions',bUseRegularExpressions)
  INIMgr.Fetch('Search Options','bSearchSubdirectories',bSearchSubdirectories)
  INIMgr.Fetch('Search Options','nLevels',nLevels)
  INIMgr.Fetch('Search Options','bCaseSensitive',bCaseSensitive)
  INIMgr.Fetch('Search Options','bExactMatch',bExactMatch)
  INIMgr.Fetch('Search Options','bExcludeMatch',bExcludeMatch)
  INIMgr.Fetch('Search Options','bExcludeComments',bExcludeComments)
  
  INIMgr.Fetch('Search Options','szPattern',szPattern)
  ReplaceChr(szPattern,'§','''')
  !ReplaceChr(szPattern,'<A7h>','''')
  
  INIMgr.Fetch('Search Options','szSearchPath',szSearchPath)
  INIMgr.Fetch('Search Options','szFileMask',szFileMask)
  INIMgr.Fetch('Search Options','FindText',SearchFindOptions.szFindText)
  
  INIMgr.Fetch('Search Options','bFilenamesOnly',bFilenamesOnly)
  INIMgr.Fetch('Search Options','bFileListFromFile',bFileListFromFile)
  INIMgr.Fetch('Search Options','szFileListFilename',szFileListFilename)
  INIMgr.Fetch('Search Options','bSearchStringsFromFile',bSearchStringsFromFile)
  INIMgr.Fetch('Search Options','szSearchStringFilename',szSearchStringFilename)
  
  INIMgr.Fetch('Find and Delete Options','SearchLocation',SearchFindOptions.SearchLocation)
  INIMgr.Fetch('Find and Delete Options','DeleteCondition',SearchFindOptions.DeleteCondition)
  INIMgr.Fetch('Find and Delete Options','MatchType',SearchFindOptions.MatchType)
  INIMgr.Fetch('Find and Delete Options','MatchCase',SearchFindOptions.MatchCase)
  
  INIMgr.Fetch('Global','szEditCommand',glo:szEditorCommand)
  INIMgr.Fetch('Global','PromptForEditor',glo:PromptForEditor)
  INIMgr.Fetch('Global','AllExtensions',glo:bAllExtensions)
  INIMgr.Fetch('Global','NewSearchAction',glo:NewSearchAction)
  INIMgr.Fetch('Global','NoDownloadVersion',glo:szNoDownloadVersion)
  INIMgr.Fetch('Global','DeleteWarningCount',glo:nDeleteWarningCount)
  INIMgr.Fetch('Global','DefaultSearchButton',glo:nDefaultSearchButton)
  INIMgr.Fetch('Global','DontShowSubdirectoryWarning',glo:bDontShowSubdirectoryWarning)
  INIMgr.Fetch('Global','SyncPathWithPattern',glo:SyncPathWithPattern)
  INIMgr.Fetch('Global','DefaultPropertyFile',glo:szDefaultPropertyFile)
  INIMgr.Fetch('Global','DisableSlashP',glo:DisableSlashP) ! mr 20180912 
  
  INIMgr.Fetch('Global','ResultListFontName',glo:ResultListFontName)
  INIMgr.Fetch('Global','ResultListFontSize',glo:ResultListFontSize)
  INIMgr.Fetch('Global','ResultListForeColor',glo:ResultListForeColor)
  INIMgr.Fetch('Global','ResultListFontStyle',glo:ResultListFontStyle)
  
  INIMgr.Fetch('Global','ResultListMinusKey',glo:MinusKey)
  ?cmdPreviousLine{PROP:Tip} = 'Jump to Previous Line<0DH,0AH>in Results List<09H>[' & keyCodeName.ToName(glo:MinusKey) & ']'
  
  INIMgr.Fetch('Global','ResultListPlusKey',glo:PlusKey)
  ?cmdNextLine{PROP:Tip} = 'Jump to Next Line<0DH,0AH>in Results List<09H>[' & keyCodeName.ToName(glo:PlusKey) & ']'
  
  ?ResultList{PROP:FontName}   = glo:ResultListFontName
  ?ResultList{PROP:FontSize}   = glo:ResultListFontSize
  ?ResultList{PROP:FontColor}  = glo:ResultListForeColor
  ?ResultList{PROP:FontStyle}  = glo:ResultListFontStyle
  ?ResultList{PROP:LineHeight} = glo:ResultListFontSize
  
  INIMgr.Fetch('Global','ApplicationColor',glo:ApplicationColor)
  !Window{PROP:Color}    = glo:ApplicationColor
  ?Application:Box{PROP:Fill} = glo:ApplicationColor
  ?Application:Box{PROP:COLOR} = glo:ApplicationColor
  ?szTitle:2{PROP:COLOR} = glo:ApplicationColor
  
  INIMgr.Fetch('Global','ToolbarColor',glo:ToolbarColor)
  ?Toolbar1{PROP:Color} = glo:ToolbarColor
  
  DO AdjustFontColour   !adjust some font colours based on brightness of toolbar and application colours
  
  INIMgr.Fetch('Global','SelectedBack',glo:SelectedBack)
  INIMgr.Fetch('Global','BookmarkBack',glo:BookmarkBack)
  
  INIMgr.Fetch('Global','SplitOrientation',glo:SplitterOrientation)
  INIMgr.Fetch('Global','SplitX',glo:SplitX)
  INIMgr.Fetch('Global','SplitY',glo:SplitY)
  INIMgr.Fetch('Global','Zoom',glo:Zoom)
  
  INIMgr.Fetch('Global','ClarionHelpFile',glo:szClarionHelpFile)
  INIMgr.Fetch('Global','HotSpotsEnabled',glo:bHotSpotsEnabled)
  INIMgr.Fetch('Global','AutoSave',glo:bAutoSave)
  glo:bShowAutoSaveWarning = glo:bAutoSave
  
  INIMgr.Fetch('Global','sqlProperties',glo:sqlProperties)
  
  INIMgr.Fetch('Global','AllowMultipleInstances',glo:AllowMultipleInstances)
  INIMgr.Fetch('Global','AutoSizeResultColumns',glo:AutoSizeResultColumns)
  INIMgr.Fetch('Global','RestorePointTimerInterval',glo:RestorePointTimerInterval)
  
  INIMgr.Fetch('Main','szListBoxFormat',szListBoxFormat)
  !fix format ---------------------------
  i = INSTRING('~Text',szListBoxFormat,1)
  IF szListBoxFormat[i-2 : i-1] <> '|M'
     szListBoxFormat = szListBoxFormat[1 : i-1] & '|M' & szListBoxFormat[i : LEN(szListBoxFormat)]
  END
  !--------------------------------------
  ?ResultList{PROP:Format} = szListBoxFormat
  DO FillListFormatQueue
  
  INIMgr.FetchQueue('ClarionExtensions Queue','ClarionExtension',ClarionExtensionsQueue,ClarionExtensionsQueue.FileExtension)
  IF RECORDS(ClarionExtensionsQueue) = 0
     ClarionExtensionsQueue.FileExtension='.CLA'
     ADD(ClarionExtensionsQueue,+ClarionExtensionsQueue.FileExtension)
     ClarionExtensionsQueue.FileExtension='.CLW'
     ADD(ClarionExtensionsQueue,+ClarionExtensionsQueue.FileExtension)
     ClarionExtensionsQueue.FileExtension='.INC'
     ADD(ClarionExtensionsQueue,+ClarionExtensionsQueue.FileExtension)
     ClarionExtensionsQueue.FileExtension='.EQU'
     ADD(ClarionExtensionsQueue,+ClarionExtensionsQueue.FileExtension)
     ClarionExtensionsQueue.FileExtension='.INT'
     ADD(ClarionExtensionsQueue,+ClarionExtensionsQueue.FileExtension)
  END
  
  INIMgr.FetchQueue('Editor Queue','EditCommand',EditorQueue,EditorQueue.szValue)
  IF RECORDS(EditorQueue) = 0
     EditorQueue.szValue = '<<Use Windows Default>'
     ADD(EditorQueue,+EditorQueue.szValue)
     EditorQueue.szValue = glo:szEditorCommand
     ADD(EditorQueue,+EditorQueue.szValue)
  ELSE
     LOOP i = 1 TO RECORDS(EditorQueue)
        GET(EditorQueue,i)
        IF UPPER(EditorQueue.szValue) = '<<USE WINDOWS DEFAULT>'
           BREAK
        END
     END
     IF i > RECORDS(EditorQueue)
        EditorQueue.szValue = '<<Use Windows Default>'
        ADD(EditorQueue,+EditorQueue.szValue)
     END
  END
  SORT(EditorQueue,+EditorQueue.szValue)
  
  INIMgr.FetchQueue('Macro Queue','Macro',MacroQueue,MacroQueue.feqButton,MacroQueue.szField1,MacroQueue.szField2,MacroQueue.szField3)
  
  NewTab = ?Search1
  LastTabNumber = 1
  DO AddSearchQueueRecord
  SearchQueue.bSearchPressed = TRUE
  ?ResultList{PROP:From} = SearchQueue.ResultQueue
  ?ResultList{PROP:Format} = SearchQueue.szListBoxFormat
  
  AutoSizer.Reset(?ResultList,SearchQueue.ResultQueue)
  
  NewTab = CREATE(0,CREATE:tab,?CurrentSearch)
  NewTab{PROP:Text} = NewSearchText
  DO SetNewTabFont
  UNHIDE(NewTab)
  bAutoSearch = TRUE
  
  ResultListMenu.Init(INIMgr)
  DO SetupResultListMenu
  SELF.SetAlerts()
  DO HandleOrientationChange
  oHH &= NEW tagHTMLHelp
  oHH.Init( 'kss.chm' )
  oHH.SetTopic('Main.htm')
  ListWithFocus = ?ResultList
  Window{Prop:StatusText,2} = 'RESULTS LIST'
  DO RefreshEditPane
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

i                    LONG
j                    LONG
thisProcInfo         &PROCESS_INFORMATION
dwId                 DWORD
szDirPath            CSTRING(261)
  CODE
  IF glo:RestorePointTimerThread > 0
     POST(EVENT:CloseWindow,,glo:RestorePointTimerThread)
  END
  IF ResultList::OrigWndProc
     ?ResultList{Prop:WndProc} = ResultList::OrigWndProc        ! Restore the handler for this window
  END
  glo:Zoom = SciControl.GetZoom()
 ! If self.opened Then WinAlert().
  cs.wait()
  j = RECORDS(ThreadQueue)
  LOOP i = 1 TO j
     GET(ThreadQueue,i)
     thisProcInfo &= (INSTANCE(piProcInfo,ThreadQueue.ID))
     IF NOT thisProcInfo &= NULL
        kcr_TerminateProcess(thisProcInfo.hProcess,0)
     END
     POST(EVENT:CloseWindow,,ThreadQueue.ID)
     !YIELD()
  END
  cs.release()
  YIELD()
  ReturnValue = PARENT.Kill()
  
  IF ReturnValue THEN RETURN ReturnValue.
  AutoSizer.Kill()
  IF SELF.Opened
    INIMgr.Update('Main',Window)                           ! Save window data to non-volatile store
  END
  DO SaveSearchParameters
  DO SaveViewerStyles
  
  ResultListMenu.Kill()
  
  j = RECORDS(SearchQueue)
  LOOP i = j TO 1 BY -1
     GET(SearchQueue,i)
     IF NOT SearchQueue.ResultQueue &= NULL
        FREE(SearchQueue.ResultQueue)
        !ASSERT(0,eqDBG & 'DISPOSE ResultQueue [' & ADDRESS(SearchQueue.ResultQueue) &']')
        DISPOSE(SearchQueue.ResultQueue)
        SearchQueue.ResultQueue &= NULL
     END
  
     IF NOT SearchQueue.UndoQueue &= NULL
        FREE(SearchQueue.UndoQueue)
        !ASSERT(0,eqDBG & 'DISPOSE UndoQueue [' & ADDRESS(SearchQueue.UndoQueue) &']')
        DISPOSE(SearchQueue.UndoQueue)
        SearchQueue.UndoQueue &= NULL
     END
  
     IF SearchQueue.feqSearchProgress
        DESTROY(SearchQueue.feqSearchProgress)
        SearchQueue.feqSearchProgress = 0
     END
     IF SearchQueue.tabNumber
        DESTROY(SearchQueue.tabNumber)
        SearchQueue.tabNumber = 0
     END
  
     PUT(SearchQueue)
     DELETE(SearchQueue)
  END
  FREE(SearchQueue)
  GlobalErrors.SetProcedureName
  IF ~oHH &= NULL
    oHH.Kill()
    DISPOSE( oHH )
  END
  dwId = kcr_GetCurrentProcessId()
  szDirPath = svSpecialFolder.GetDir(SV:CSIDL_APPDATA) & '\Devuna\KSS\RestorePoint\' & dwId
  SilentlyRemoveDirectory(szDirPath)
  RETURN ReturnValue


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  
  ReturnValue = PARENT.Run()
  RETURN ReturnValue


ThisWindow.SetAlerts PROCEDURE

  CODE
  PARENT.SetAlerts
  SELF.MyWindow{PROP:Alrt,255} = CtrlShiftHook
  SELF.MyWindow{PROP:Alrt,255} = CtrlDelete
  SELF.MyWindow{PROP:Alrt,255} = AltDelete
  SELF.MyWindow{PROP:Alrt,255} = glo:PlusKey
  SELF.MyWindow{PROP:Alrt,255} = glo:MinusKey
  !  SELF.MyWindow{PROP:Alrt,255} = ShiftF11
  ?Search1{PROP:Alrt,255} = MouseRight


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

saAttr               LIKE(SECURITY_ATTRIBUTES)
szSelectedFolder     CSTRING(261)
szSearchFolder       CSTRING(261)
szFixedFileMask      LIKE(szFileMask)
i                    LONG
j                    LONG
k                    LONG
pStringStart         LONG
pStringEnd           LONG
pCommentMarker       LONG
bMatchMode           LONG
pMatch               LONG
szMatchText          LIKE(SearchQueue.ResultQueue.Text)
strOptions           &STRING
findStrOptions       GROUP(FindStrOptionsGroupType),PRE(fso)
                     END
thisProcInfo         &PROCESS_INFORMATION
cc                   LONG
dwExitCode           DWORD
errMsg               CSTRING(1024)
thisCancelFlag       &BYTE
currentOrientation   BYTE
currentSplitX        BYTE
currentSplitY        BYTE
XMax                 LONG
YMax                 LONG
PropertiesChanged    BOOL(FALSE)
tabText              CSTRING(256)
bRebuildResultListMenu  BOOL(FALSE)

rrlQueue             QUEUE(ff_:queue),PRE(rrl)
                     END
szRRLFileName        CSTRING(261)
Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE ACCEPTED()
    OF ?cmdUserOptions
      CurrentOrientation = glo:SplitterOrientation
      currentSplitX      = glo:SplitX
      currentSplitY      = glo:SplitY
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?cmdSearch
      ThisWindow.Update()
       UPDATE()
      
       IF SciControl.GetModify() AND szTitle <> ''
          DO Handle_FileModified
       END
      
       findStrOptions = SearchQueue
       IF redoSearch = TRUE
          redoSearch = FALSE
          cc = Level:Benign
       ELSE
          cc = winGetSearchParameters(findStrOptions)
       END
      
       CASE cc
         OF Level:Benign
            SearchQueue = findStrOptions
      
            IF glo:NewSearchAction = 1 AND (?CurrentSearch{PROP:ChoiceFEQ}){PROP:Text} <> 'New Search'
               DO UpdateLocalSearchOptions
      
               LastTabNumber += 1
               DO SetTabFont
               NewTab{PROP:Text} = 'New Search'
               DO AddSearchQueueRecord
               SELECT(NewTab)
               NewTab = CREATE(0,CREATE:tab,?CurrentSearch)
               NewTab{PROP:Text} = NewSearchText
               DO SetNewTabFont
               NewTab{PROP:Hide} = FALSE
      
               ?ResultList{PROP:From} = SearchQueue.ResultQueue
               ?ResultList{PROP:Format} = SearchQueue.szListBoxFormat
      
               AutoSizer.Reset(?ResultList,SearchQueue.ResultQueue)
      
               HIDE(SearchQueue.feqSearchProgress)
               IF glo:bHideResultsPanel = FALSE
                  UNHIDE(?szMatchesFound)
               END
               SearchQueue.szMatchesFound = ''
            END
      
            IF LEN(SearchQueue.szPattern) > 23
               !(?CurrentSearch{PROP:ChoiceFEQ}){PROP:Text} = SearchQueue.szPattern[1 : 10] & '...' & SearchQueue.szPattern[LEN(SearchQueue.szPattern)-9 : LEN(SearchQueue.szPattern)]
               tabText = SearchQueue.szPattern[1 : 10] & '...' & SearchQueue.szPattern[LEN(SearchQueue.szPattern)-9 : LEN(SearchQueue.szPattern)]
            ELSE
               !(?CurrentSearch{PROP:ChoiceFEQ}){PROP:Text} = SearchQueue.szPattern
               tabText = SearchQueue.szPattern
            END
      
            i = INSTRING('&',tabText)
            LOOP WHILE i > 0
               tabText = tabText[1 : i] & '&' & tabText[i+1 : LEN(tabText)]
               i = INSTRING('&',tabText,1,i+2)
            END
            (?CurrentSearch{PROP:ChoiceFEQ}){PROP:Text} = tabText
      
            SearchQueue.szMatchesFound = ''
            SearchQueue.bSearchPressed = TRUE
            IF SearchQueue.szSearchPath[LEN(SearchQueue.szSearchPath)] = '\'
               SearchQueue.szSearchPath[LEN(SearchQueue.szSearchPath)] = '<0>'
            END
      
            SearchQueue.lPointer = 1
            PUT(SearchQueue)
            !DO UpdateSearchQueueRecord
      
            FREE(SearchQueue.ResultQueue)
            FREE(SearchQueue.UndoQueue)
      
            DISPLAY(?ResultList)
            DISPLAY(?szMatchesFound)
      
            HIDE(?szMatchesFound)
            SearchQueue.feqSearchProgress{PROP:Progress} = 0
            UNHIDE(SearchQueue.feqSearchProgress)
      
            DO SaveSearchParameters
      
            CurrentFilename = ''
            IF ViewerActive
               !SciControl.SetReadOnly(FALSE)
               SciControl.ClearBuffer()
               ViewerActive = FALSE
            END
      
            strOptions &= NEW STRING(SIZE(FindStrOptionsGroupType))
            !ASSERT(0,eqDBG & 'NEW FindStrOptions [' & ADDRESS(strOptions) & ']')
            strOptions = SearchQueue
      
            cs.wait()
            ThreadQueue.ID = START(FindStr,25000,strOptions)
            ThreadQueue.tabNumber = SearchQueue.tabNumber
            ThreadQueue.ThreadNo = 0
            ThreadQueue.TargetProcessHandle = kcr_GetCurrentProcess()
            ADD(ThreadQueue,+ThreadQueue.tabNumber)
            cs.release()
      
            RESUME(ThreadQueue.ID)
            ClockQueue.tabNumber = SearchQueue.tabNumber
            GET(ClockQueue,ClockQueue.tabNumber)
            IF ERRORCODE()
               ClockQueue.lClock = CLOCK()
               ADD(ClockQueue,+ClockQueue.tabNumber)
            ELSE
               ClockQueue.lClock = CLOCK()
               PUT(ClockQueue)
            END
      
            !ASSERT(0,eqDBG & 'DISPOSE FindStrOptions [' & ADDRESS(strOptions) &']')
            DISPOSE(strOptions)
            strOptions &= NULL
            ShowWaitCursor = TRUE
            SETCURSOR(CURSOR:Wait)
            DISABLE(?cmdSearch)
            ResultListMenu.SetItemEnable('Search',FALSE)
            ResultListMenu.SetItemEnable('RedoSearch',FALSE)
      
            HIDE(?cmdCloseTab)
            ResultListMenu.SetItemEnable('Close',FALSE)
      
            UNHIDE(?cmdCancelSearch)
            ENABLE(?cmdCancelSearch)
            ResultListMenu.SetItemEnable('Cancel',TRUE)
      
            DISPLAY()
      
            IF glo:RestorePointTimerInterval > 0 AND glo:RestorePointTimerThread = 0
               START(RestorePointTimer,25000)
            END
      
         OF Level:User    !reload saved
            IF glo:NewSearchAction = 1
               LastTabNumber += 1
               DO SetTabFont
               NewTab{PROP:Text} = 'New Search'
               DO AddSearchQueueRecord
               SELECT(NewTab)
               NewTab = CREATE(0,CREATE:tab,?CurrentSearch)
               DO SetNewTabFont
               NewTab{PROP:Text} = NewSearchText
               NewTab{PROP:Hide} = FALSE
            END
            findStrOptions = SearchQueue
            IF LoadRestorePoint(findStrOptions) = Level:Benign
               SearchQueue = findStrOptions
               SearchQueue.tabNumber = (?CurrentSearch{PROP:ChoiceFEQ})
               PUT(SearchQueue)
               (?CurrentSearch{PROP:ChoiceFEQ}){PROP:Text} = SearchQueue.szPattern
               ?ResultList{PROP:From} = SearchQueue.ResultQueue
               ?ResultList{PROP:Format} = SearchQueue.szListBoxFormat
      
               AutoSizer.Reset(?ResultList,SearchQueue.ResultQueue)
      
               HIDE(SearchQueue.feqSearchProgress)
               IF glo:bHideResultsPanel = FALSE
                  UNHIDE(?szMatchesFound)
               END
            ELSE
               IF ?CurrentSearch{PROP:ChoiceFEQ} <> ?Search1 AND glo:NewSearchAction = 1
                  !DO HandleCloseTab
                  MESSAGE((?CurrentSearch{PROP:ChoiceFEQ}){PROP:Text})
                  DO CloseTab
               END
            END
      
            IF glo:RestorePointTimerInterval > 0 AND glo:RestorePointTimerThread = 0
               START(RestorePointTimer,25000)
            END
      
         OF Level:Program    !reload restore point
      
            DIRECTORY(rrlQueue,glo:RestorePointFolder & '\*.rrl',ff_:NORMAL)
            SORT(rrlQueue,rrlQueue.name)
            !SELECT(NewTab)
            LOOP i = 1 TO RECORDS(rrlQueue)
               GET(rrlQueue,i)
               IF i > 1
               !IF glo:NewSearchAction = 1
                  LastTabNumber += 1
                  DO SetTabFont
                  NewTab{PROP:Text} = 'New Search'
                  DO AddSearchQueueRecord
                  SELECT(NewTab)
                  NewTab = CREATE(0,CREATE:tab,?CurrentSearch)
                  DO SetNewTabFont
                  NewTab{PROP:Text} = NewSearchText
                  NewTab{PROP:Hide} = FALSE
               END
      
               findStrOptions = SearchQueue
               szRRLFileName = glo:RestorePointFolder & '\' & rrlQueue.name
               IF LoadRestorePoint(findStrOptions, szRRLFileName) = Level:Benign
                  SearchQueue = findStrOptions
                  SearchQueue.tabNumber = (?CurrentSearch{PROP:ChoiceFEQ})
                  PUT(SearchQueue)
                  (?CurrentSearch{PROP:ChoiceFEQ}){PROP:Text} = SearchQueue.szPattern
                  ?ResultList{PROP:From} = SearchQueue.ResultQueue
                  ?ResultList{PROP:Format} = SearchQueue.szListBoxFormat
      
                  AutoSizer.Reset(?ResultList,SearchQueue.ResultQueue)
      
                  HIDE(SearchQueue.feqSearchProgress)
                  IF glo:bHideResultsPanel = FALSE
                     UNHIDE(?szMatchesFound)
                  END
               ELSE
                  IF ?CurrentSearch{PROP:ChoiceFEQ} <> ?Search1 AND glo:NewSearchAction = 1
                     !DO HandleCloseTab
                     DO CloseTab
                  END
               END
            END
      
            !NewTab = CREATE(0,CREATE:tab,?CurrentSearch)
            !DO SetNewTabFont
            !NewTab{PROP:Text} = NewSearchText
            !NewTab{PROP:Hide} = FALSE
      
            SilentlyRemoveDirectory(glo:RestorePointFolder)
            glo:RestorePointFolder = ''
      
            IF glo:RestorePointTimerInterval > 0 AND glo:RestorePointTimerThread = 0
               START(RestorePointTimer,25000)
            END
      
         OF Level:Cancel
            IF ?CurrentSearch{PROP:ChoiceFEQ} <> ?Search1 AND glo:NewSearchAction = 1
               IF (?CurrentSearch{PROP:ChoiceFEQ}){PROP:Text} = 'New Search'
                  DO CloseTab
               ELSE
               END
            END
       END  !CASE
    OF ?cmdRedoSearch
      ThisWindow.Update()
      redoSearch = TRUE
      POST(EVENT:Accepted,?cmdSearch)
    OF ?cmdPreviousFolder
      ThisWindow.Update()
      DO MoveToPreviousFolder
    OF ?cmdPreviousFile
      ThisWindow.Update()
      DO MoveToPreviousFile
    OF ?cmdPreviousLine
      ThisWindow.Update()
      DO MoveToPreviousLine
    OF ?cmdNextLine
      ThisWindow.Update()
      DO MoveToNextLine
    OF ?cmdNextFile
      ThisWindow.Update()
      DO MoveToNextFile
    OF ?cmdNextFolder
      ThisWindow.Update()
      DO MoveToNextFolder
    OF ?cmdDeleteLine
      ThisWindow.Update()
      DO DeleteLine
    OF ?cmdDeleteFile
      ThisWindow.Update()
      DO DeleteFilename
    OF ?cmdDeleteExtension
      ThisWindow.Update()
      DO DeleteExtension
    OF ?cmdDeletePath
      ThisWindow.Update()
      DO DeletePath
    OF ?cmdDeleteComments
      ThisWindow.Update()
      DO DeleteCommentLines
    OF ?cmdDeleteLabels
      ThisWindow.Update()
      DO DeleteLabelLines
    OF ?cmdDeleteCode
      ThisWindow.Update()
      DO DeleteCodeMatches
    OF ?cmdDeleteData
      ThisWindow.Update()
      DO DeleteDataMatches
    OF ?cmdDeleteBuiltInClw
      ThisWindow.Update()
       DO DeleteBuiltInCLWFiles 
    OF ?cmdFindAndDelete
      ThisWindow.Update()
      DO FindAndDelete
    OF ?cmdUndoDelete
      ThisWindow.Update()
      DO HandleUndo
    OF ?cmdReplaceResults
      ThisWindow.Update()
      SciControl.ReplaceAsk()
    OF ?cmdSave
      ThisWindow.Update()
      findStrOptions = SearchQueue
      IF SaveResults(findStrOptions, szSendToFilename)
         !SendTo
         !POST(EVENT:Accepted,?cmdEdit)
         DO CheckEditor
      END
    OF ?cmdEdit
      ThisWindow.Update()
      GET(SearchQueue.ResultQueue,CHOICE(?ResultList))
      szSendToFilename = SearchQueue.ResultQueue.SortName
      DO CheckEditor
    OF ?cmdUserOptions
      ThisWindow.Update()
      szInstallProgram = ''
      PropertiesChanged = UserOptions( (2^SciControl.GetStyleBits()) - 1, szInstallProgram )
      IF currentOrientation <> glo:SplitterOrientation
         IF currentOrientation = SplitterOrientation:Vertical
            !switching to horizontal
            YMax = Window{PROP:Height}-(MIN_PANELHEIGHT-8)
            glo:SplitY = (glo:SplitX / Window{PROP:Width}) * Window{PROP:Height}
            IF NOT INRANGE(glo:SplitY,MIN_PANELHEIGHT,YMax)
               IF glo:SplitY < MIN_PANELHEIGHT
                  glo:SplitY = MIN_PANELHEIGHT
               ELSIF glo:SplitY < YMax
                  glo:SplitY = YMax
               END
            END
      
         ELSE
            XMax = Window{PROP:Width}-MIN_PANELWIDTH
            glo:SplitX = (glo:SplitY / Window{PROP:Height}) * Window{PROP:Width}
            IF NOT INRANGE(glo:SplitX,MIN_PANELWIDTH,XMax)
               IF glo:SplitX < MIN_PANELWIDTH
                  glo:SplitX = MIN_PANELWIDTH
               ELSIF glo:SplitX > XMax
                  glo:SplitX = XMax
               END
            END
         END
         DO HandleOrientationChange
      END
      
      IF PropertiesChanged
         SciControl.ResetPopupMenu()
         LoadLexerProperties(SciControl,SearchQueue.szPropertyFile)
         SciControl.Colourise(0,-1)
         SciControl.MarkerSetBack(markerBookmark, glo:BookmarkBack)
         SciControl.SetSelBack(TRUE,glo:SelectedBack)
      
         !Window{PROP:Color}    = glo:ApplicationColor
         ?Application:Box{PROP:Fill} = glo:ApplicationColor
         ?Application:Box{PROP:COLOR} = glo:ApplicationColor
         ?szTitle:2{PROP:COLOR} = glo:ApplicationColor
      
         ?Toolbar1{PROP:Color} = glo:ToolbarColor
      
         DO AdjustFontColour   !adjust some font colours based on brightness of toolbar and application colours
      
         ?ResultList{PROP:FontName}   = glo:ResultListFontName
         ?ResultList{PROP:FontSize}   = glo:ResultListFontSize
         ?ResultList{PROP:FontColor}  = glo:ResultListForeColor
         ?ResultList{PROP:FontStyle}  = glo:ResultListFontStyle
         ?ResultList{PROP:LineHeight} = glo:ResultListFontSize
      
         IF glo:OldMinusKey <> glo:MinusKey
            LOOP I# = 1 TO 255
               IF SELF.MyWindow{Prop:ALRT,I#} = glo:OldMinusKey
                  SELF.MyWindow{Prop:ALRT,I#} = glo:MinusKey
                  BREAK
                END
            END
            glo:OldMinusKey = glo:MinusKey
            ?cmdPreviousLine{PROP:Tip} = 'Jump to Previous Line<0DH,0AH>in Results List<09H>[' & keyCodeName.ToName(glo:MinusKey) & ']'
            bRebuildResultListMenu = TRUE
         END
      
         IF glo:OldPlusKey <> glo:PlusKey
            LOOP I# = 1 TO 255
               IF SELF.MyWindow{Prop:ALRT,I#} = glo:OldPlusKey
                  SELF.MyWindow{Prop:ALRT,I#} = glo:PlusKey
                  BREAK
                END
            END
            glo:OldPlusKey = glo:PlusKey
            ?cmdNextLine{PROP:Tip} = 'Jump to Next Line<0DH,0AH>in Results List<09H>[' & keyCodeName.ToName(glo:PlusKey) & ']'
            bRebuildResultListMenu = TRUE
         END
      
         IF bRebuildResultListMenu = TRUE
            bRebuildResultListMenu = FALSE
            ResultListMenu.Kill()
            ResultListMenu.Init(INIMgr)
            DO SetupResultListMenu
         END
      
      END
      
      IF szInstallProgram <> ''
         bClosingDown = TRUE
         POST(EVENT:CloseWindow)
      END
      
      !ThisWindow.Reset
    OF ?cmdLayout
      ThisWindow.Update()
      glo:SplitterOrientation = 1 - glo:SplitterOrientation
      DO HandleOrientationChange
    OF ?cmdHelp
      ThisWindow.Update()
      PRESSKEY(F1Key)
    OF ?cmdSaveWarn
      ThisWindow.Update()
      SciControl.SaveFileAs(szTitle)
    OF ?cmdCloseTab
      ThisWindow.Update()
      DO HandleCloseTab
    OF ?cmdCancelSearch
      ThisWindow.Update()
      cs.Wait()
      ThreadQueue.tabNumber = SearchQueue.tabNumber
      GET(ThreadQueue,ThreadQueue.tabNumber)
      IF NOT ERRORCODE()
         thisProcInfo &= (INSTANCE(piProcInfo,ThreadQueue.ID))
         IF NOT thisProcInfo &= NULL
            cc = kcr_TerminateProcess(thisProcInfo.hProcess,0)
            !ASSERT(0,eqDBG & 'TerminateProcess called for thread ' & ThreadQueue.ID & ' cc=' & cc)
            IF cc = 0
               cc = kcr_FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, 0, kcr_GetLastError(), 0, errMsg, SIZE(errMsg), 0)
               !ASSERT(0,eqDBG & 'TerminateProcess failed for thread ' & ThreadQueue.ID & ' ' & errMsg)
            END
            !YIELD()
         ELSE
            !ASSERT(0,eqDBG & 'thisProcInfo is NULL')
         END
         ThisCancelFlag &= INSTANCE(CancelFlag,ThreadQueue.ID)
         ThisCancelFlag = TRUE
      ELSE
         !ASSERT(0,eqDBG & ERROR())
      END
      cs.Release()
      YIELD()
    OF ?ResultList
      ListWithFocus = ?ResultList
      Window{Prop:StatusText,2} = 'RESULTS LIST'
    OF ?cmdSaveWarn:2
      ThisWindow.Update()
      SciControl.SaveFileAs(szTitle)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeCloseEvent PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  IF SciControl.GetModify() AND szTitle <> ''
     DO Handle_FileModified
  END
  ReturnValue = PARENT.TakeCloseEvent()
  RETURN ReturnValue


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
!saveTabNumber        LONG
feqProgress          LONG
thisProgress         LONG
i                    LONG
k                    LONG
MenuSelection        CSTRING(32)
szSelection          CSTRING(261)
targetLine           LONG
scrollDelta          LONG
  CODE
  
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  !   CASE EVENT()
  !     OF EVENT:TIMER OROF SCN_UPDATEUI
  !   ELSE
  !        dbx.PrintEvent()
  !   END
  ReturnValue = PARENT.TakeEvent()
!  If event() = event:VisibleOnDesktop !or event() = event:moved
!    ds_VisibleOnDesktop()
!  end
    IF INRANGE(EVENT(),EVENT:THREAD,EVENT:THREADLAST)
       cs.wait()
       ThreadQueue.tabNumber = EVENT() - EVENT:THREAD
       GET(ThreadQueue,+ThreadQueue.tabNumber)
       IF ~ERRORCODE()
          ClockQueue.tabNumber = ThreadQueue.tabNumber
          GET(ClockQueue,ClockQueue.tabNumber)
          ClockQueue.lClock = ClockQueue.lCLock - CLOCK()
          PUT(ClockQueue)
          DELETE(ThreadQueue)
          !dbx.Debugout('ThreadQueue for tab ' & ThreadQueue.tabNumber & ' deleted in take event')
       ELSE
          !dbx.Debugout('ThreadQueue for tab ' & ThreadQueue.tabNumber & ' not deleted in take event [' & error() & ']')
       END
       bDoAutoSize = TRUE
       POST(EVENT:NewSelection,?CurrentSearch,,1)
       cs.release()
  
    ELSIF INRANGE(EVENT(),EVENT:PROGRESS,EVENT:PROGRESSLAST)
       feqProgress = EVENT() - EVENT:PROGRESS
       thisProgress = feqProgress{PROP:Progress}
       IF thisProgress = MAX_PROGRESS
          thisProgress = 0
       END
       feqProgress{PROP:Progress} = thisProgress + 1
  
       DISPLAY(feqProgress)
    ELSE
       CASE EVENT()
         OF EVENT:SHOWFINDHELP
            oHH.ShowTopic('Find.htm')
  
         OF EVENT:SHOWREPLACEHELP
            oHH.ShowTopic('Replace.htm')
  
         OF EVENT:MACROPLAYERCLOSED
            MacroPlayerThread = 0
            ResultListMenu.SetItemEnable('Player',TRUE)
            ResultListMenu.SetItemEnable('Record',TRUE)
  
         OF EVENT:PLAYMACRO
            bPlaying = TRUE
            POST(EVENT:Accepted,MacroQueue.feqButton)
  
         OF EVENT:PLAYSELECTIONS
            bPlaying = FALSE
            LOOP i = 1 TO RECORDS(MacroQueue)
               GET(MacroQueue,i)
               IF MacroQueue.mark = TRUE
                  bPlaying += 1
               END
            END
            POST(EVENT:PLAYNEXTSELECTION)
  
         OF EVENT:PLAYNEXTSELECTION
            LOOP i = 1 TO RECORDS(MacroQueue)
               GET(MacroQueue,i)
               IF MacroQueue.mark = TRUE
                  POST(EVENT:Accepted,MacroQueue.feqButton)
                  BREAK
               END
            END
  
         OF EVENT:GOTONEWLINE
            SciControl.GoToLine(SearchQueue.ResultQueue.LineNo-1)
            targetLine = SciControl.GetFirstVisibleLine()+(SciControl.LinesOnScreen()/2)
            scrollDelta = SearchQueue.ResultQueue.LineNo - targetLine
            SciControl.LineScroll(0,scrollDelta)
            CASE KEYCODE()
              OF glo:PlusKey OROF glo:MinusKey
               ListWithFocus = -1
               Window{Prop:StatusText,2} = 'EDITOR'
               SciControl.GrabFocus()
            END
       END
    END
     IF KEYCODE()=CtrlShiftP AND EVENT() = Event:PreAlertKey
       CYCLE
     END
     IF KEYCODE()=CtrlShiftP  
    
       CYCLE
     END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeFieldEvent PROCEDURE

ReturnValue          BYTE,AUTO

i                    LONG
j                    LONG
sLineNo              STRING(10)
SortName             LIKE(SearchQueue.ResultQueue.SortName)
lpBookmarkQueue      LONG
pBookmarkQueue       STRING(4),OVER(lpBookmarkQueue)
thisText             LIKE(SearchQueue.ResultQueue.Text)
findStrOptions       GROUP(FindStrOptionsGroupType),PRE(fso)
                     END
X                    LONG
XMax                 LONG
Y                    LONG
YMax                 LONG
MenuSelection        CSTRING(32)
szSelection          CSTRING(261)
MouseDownRow         LONG
Looped BYTE
  CODE
  LOOP                                                     ! This method receives all field specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  CASE FIELD()
  OF ?CurrentSearch{PROP:ChoiceFEQ}
     CASE EVENT()
       OF EVENT:AlertKey
          IF KEYCODE() = MouseRight
             POST(EVENT:Accepted,?cmdSearch)
          END
     END
  END
  ReturnValue = PARENT.TakeFieldEvent()
  CASE FIELD()
  OF ?sciControl:Region
    CASE EVENT()
    
    END
  OF ?CurrentSearch
    CASE EVENT()
    OF EVENT:TabChanging
      DO UpdateSearchQueueRecord
    END
  OF ?ResultList
    CASE EVENT()
    OF EVENT:ColumnResize
      SearchQueue.szListBoxFormat = ?ResultList{PROP:Format}
    OF EVENT:AlertKey
      CASE KEYCODE()
        OF F5Key
           redoSearch = TRUE
           POST(EVENT:Accepted,?cmdSearch)
        OF CtrlT
           ?CurrentSearch{PROP:ChoiceFEQ} = NewTab
           POST(EVENT:NewSelection,?CurrentSearch)
        OF CtrlF4 OROF CtrlW
           DO HandleCloseTab
        OF EnterKey
           IF RECORDS(SearchQueue.ResultQueue) = 0
              POST(EVENT:Accepted,?cmdSearch)
           ELSE
              ListWithFocus = -1
              Window{Prop:StatusText,2} = 'EDITOR'
              SciControl.GrabFocus()
           END
        OF CtrlC
           SETCLIPBOARD(SearchQueue.ResultQueue.SortName)
        OF MouseLeft2
           ListWithFocus = ?ResultList
           Window{Prop:StatusText,2} = 'RESULTS LIST'
           IF ?ResultList{PROPLIST:MouseDownZone} = LISTZONE:Right
              i = POINTER(SearchQueue.ResultQueue)
              AutoSizer.ResizeColumn(?ResultList,?ResultList{PROPLIST:MouseDownField})
              SearchQueue.szListBoxFormat = ?ResultList{PROP:Format}
              GET(SearchQueue.ResultQueue,i)
              ?ResultList{PROP:Selected} = i
           END
        OF MouseRight
           ListWithFocus = ?ResultList
           Window{Prop:StatusText,2} = 'RESULTS LIST'
           IF ?ResultList{PROPLIST:MouseDownZone} = LISTZONE:Header
              IF POPUP('Results List Formatter')
                 DO FillListFormatQueue
                 szListBoxFormat = ListBoxFormatter(ListFormatQueue)
                 ?ResultList{PROP:Format} = szListBoxFormat
              END
           ELSE
              IF RECORDS(SearchQueue.ResultQueue)
                 ResultListMenu.SetItemEnable('Delete',TRUE)
                 ResultListMenu.SetItemEnable('DeleteInCodeDirect',TRUE)
                 ResultListMenu.SetItemEnable('DeleteInDataDirect',TRUE)
                 ResultListMenu.SetItemEnable('DeleteFileNameDirect',TRUE)
                 ResultListMenu.SetItemEnable('DeleteCommentLinesDirect',TRUE)                   
                 ResultListMenu.SetItemEnable('DeleteBuiltinClwDirect',TRUE)       !rif 20190101                             
                 ResultListMenu.SetItemEnable('Macro',TRUE)
                 ResultListMenu.SetItemEnable('Jump',TRUE)
                 ResultListMenu.SetItemEnable('Create',TRUE)
                 ResultListMenu.SetItemEnable('SendTo',TRUE)
                 ResultListMenu.SetItemEnable('Replace',TRUE)
                 ResultListMenu.SetItemEnable('Save',TRUE)
                 ResultListMenu.SetItemEnable('Print',TRUE)
                 ResultListMenu.SetItemEnable('Copy',TRUE)
                 ResultListMenu.SetItemEnable('CopyPath',TRUE)
                 ResultListMenu.SetItemEnable('ExplorePath',TRUE)
              ELSE
                 ResultListMenu.SetItemEnable('Delete',FALSE)
                 ResultListMenu.SetItemEnable('DeleteInCodeDirect',FALSE)
                 ResultListMenu.SetItemEnable('DeleteInDataDirect',FALSE)
                 ResultListMenu.SetItemEnable('DeleteFileNameDirect',FALSE)
                 ResultListMenu.SetItemEnable('DeleteCommentLinesDirect',FALSE)                  
                 ResultListMenu.SetItemEnable('DeleteBuiltinClwDirect',FALSE)       !rif 20190101                             
                 ResultListMenu.SetItemEnable('Macro',FALSE)
                 ResultListMenu.SetItemEnable('Jump',FALSE)
                 ResultListMenu.SetItemEnable('Create',FALSE)
                 ResultListMenu.SetItemEnable('SendTo',FALSE)
                 ResultListMenu.SetItemEnable('Replace',FALSE)
                 ResultListMenu.SetItemEnable('Save',FALSE)
                 ResultListMenu.SetItemEnable('Print',FALSE)
                 ResultListMenu.SetItemEnable('Copy',FALSE)
                 ResultListMenu.SetItemEnable('CopyPath',FALSE)
                 ResultListMenu.SetItemEnable('ExplorePath',FALSE)
              END
              IF Records(SearchQueue.UndoQueue) = 0
                 ResultListMenu.SetItemEnable('Undo',FALSE)
              ELSE
                 ResultListMenu.SetItemEnable('Undo',TRUE)
              END
              IF ?CurrentSearch{PROP:ChoiceFEQ} = ?Search1
                 ResultListMenu.SetItemEnable('Close',FALSE)
              ELSE
                 ResultListMenu.SetItemEnable('Close',TRUE)
              END
              ResultListMenu.SetItemEnable('HideEdit',CHOOSE(glo:bHideResultsPanel=FALSE,TRUE,FALSE))
      
              GET(SearchQueue.ResultQueue,?ResultList{PROPLIST:MouseDownRow}+0)
              ?ResultList{PROP:Selected} = POINTER(SearchQueue.ResultQueue)
              DISPLAY(?ResultList)
      
              MenuSelection = ResultListMenu.Ask()
              CASE MenuSelection
                OF 'Cancel'
                   POST(EVENT:Accepted,?cmdCancelSearch)
                OF 'Close'
                   DO HandleCloseTab
                OF 'Line'
                   DO DeleteLine
                OF 'Path'
                   DO DeletePath
                OF 'Filename'
                OROF 'DeleteFileNameDirect'  !RIF 2017-12-14 
                   DO DeleteFilename
                OF 'Extension'
                   DO DeleteExtension
                OF 'CommentLines'
                OROF 'DeleteCommentLinesDirect'  !RIF 2017-12-14                  
                   DO DeleteCommentLines
                OF 'LabelLines'
                   DO DeleteLabelLines
                OF 'MatchesinDATA'
                OROF 'DeleteInDataDirect'  !RIF 2017-12-14 
                   DO DeleteDataMatches
                OF 'DeleteBuiltinClwDirect'      !rif 20190101                            
                   DO DeleteBuiltInClwFiles      !rif 20190101 
                OF 'MatchesinCODE'
                OROF 'DeleteInCodeDirect'  !RIF 2017-12-14
                   DO DeleteCodeMatches
                OF 'FindandDelete'
                   DO FindAndDelete
                OF 'Undo'
                   DO HandleUnDo
                OF 'Search'
                   POST(EVENT:Accepted,?cmdSearch)
                OF 'RedoSearch'
                   redoSearch = TRUE
                   POST(EVENT:Accepted,?cmdSearch)
                OF 'Summary'
                   szSelection = winShowMatchSummary(SearchQueue.ResultQueue,SearchQueue.UndoQueue,SearchQueue.bCaseSensitive, SearchQueue.bUseRegularExpressions, SearchQueue.szPattern)
                   IF szSelection <> ''
                      SearchQueue.ResultQueue.SortName = CLIP(szSelection)
                      GET(SearchQueue.ResultQueue,SearchQueue.ResultQueue.SortName)
                      IF NOT ERRORCODE()
                         ?ResultList{PROP:Selected} = POINTER(SearchQueue.ResultQueue)
                         POST(EVENT:NewSelection,?ResultList)
                      END
                   END
                OF 'SendTo'
                   GET(SearchQueue.ResultQueue,CHOICE(?ResultList))
                   szSendToFilename = SearchQueue.ResultQueue.SortName
                   DO CheckEditor
                OF 'Replace'
                   SciControl.ReplaceAsk()
                OF 'Save'
                   POST(EVENT:Accepted,?cmdSave)
                OF 'Copy'
                   SETCLIPBOARD(SearchQueue.ResultQueue.SortName)
                OF 'CopyPath'
                   SETCLIPBOARD(SearchQueue.ResultQueue.Path)
                OF 'CopyFindstr'
                   SETCLIPBOARD(glo:findstrCommandLine)
                OF 'ExplorePath'
                   RUN('explorer.exe /select,"' & SearchQueue.ResultQueue.SortName & '"')
                OF 'PreviousFolder'
                   DO MoveToPreviousFolder
                OF 'NextFolder'
                   DO MoveToNextFolder
                OF 'PreviousFile'
                   DO MoveToPreviousFile
                OF 'NextFile'
                   DO MoveToNextFile
                OF 'PreviousLine' & keyCodeName.ToName(glo:MinusKey)
                   DO MoveToPreviousLine
                OF 'NextLine' & keyCodeName.ToName(glo:PlusKey)
                   DO MoveToNextLine
                OF 'Options'
                   POST(EVENT:Accepted,?cmdUserOptions)
                OF 'Format'
                   DO FillListFormatQueue
                   szListBoxFormat = ListBoxFormatter(ListFormatQueue)
                   ?ResultList{PROP:Format} = szListBoxFormat
                OF 'Layout'
                   POST(EVENT:Accepted,?cmdLayout)
                OF 'CenterBar'
                   DO HandleCenterBar
                OF 'AutoSizeColumns'
                   i = POINTER(SearchQueue.ResultQueue)
                   AutoSizer.ResizeAll(?ResultList)
                   SearchQueue.szListBoxFormat = ?ResultList{PROP:Format}
                   GET(SearchQueue.ResultQueue,i)
                   ?ResultList{PROP:Selected} = i
                OF 'Player'
                   IF MacroPlayerThread = 0
                      MacroPlayerThread = START(MacroPlayer,25000)
                      bRecording = FALSE
                      ResultListMenu.SetItemEnable('Player',FALSE)
                      ResultListMenu.SetText('Record','Record')
                      ResultListMenu.SetItemEnable('Record',FALSE)
                   END
                OF 'Record'
                   IF bRecording = FALSE
                      bRecording = TRUE
                      ResultListMenu.SetText('Record','Stop Recording')
                   ELSE
                      bRecording = FALSE
                      ResultListMenu.SetText('Record','Record')
                   END
                OF 'HideEdit'
                   glo:bHideEditPanel = 1 - glo:bHideEditPanel
                   ResultListMenu.SetIcon('HideEdit',CHOOSE(glo:bHideEditPanel,'Checkbox_on.ico','Checkbox_off.ico'))
                   DO HandleHideEdit
              ELSE
                 IF MenuSelection
                    MESSAGE(MenuSelection)
                 END
              END
           END
        OF DeleteKey
           IF RECORDS(SearchQueue.ResultQueue)
              DO DeleteLine
           END
        OF CtrlE
           IF RECORDS(SearchQueue.ResultQueue)
              FocusPosition = POINTER(SearchQueue.ResultQueue)
              GET(SearchQueue.ResultQueue,CHOICE(?ResultList))
              szSendToFilename = SearchQueue.ResultQueue.SortName
              SETKEYCODE(0)
              DO CheckEditor
           END
        OF CtrlF
           IF ListWithFocus = ?ResultList
              POST(EVENT:Accepted,?cmdSearch)
           END
        OF CtrlZ
           IF RECORDS(SearchQueue.UndoQueue)
              DO HandleUnDo
           END
      END
    END
  OF ?SplitterBar
    CASE EVENT()
    OF EVENT:MouseDown
      ?SplitterBar{PROP:Fill} = COLOR:BLUE
      bTrackMouse = TRUE
    OF EVENT:MouseUp
      SETKEYCODE(0)
      !message(?SplitterBar{PROP:xpos} & ', ' & ?SplitterBar{PROP:ypos} & ', ' & window{PROP:Width} & ', ' & Window{PROP:Height})
      ?SplitterBar{PROP:Fill} = 00B48246h
      DO HandleResize
      bTrackMouse = FALSE
      SELF.Reset(TRUE)
    OF EVENT:MouseIn
      IF glo:SplitterOrientation = SplitterOrientation:Vertical
         SETCURSOR('~SPLITH.CUR')
      ELSE
         SETCURSOR('~SPLITV.CUR')
      END
    OF EVENT:MouseOut
      IF ShowWaitCursor = TRUE
         SETCURSOR(CURSOR:Wait)
      ELSE
         SETCURSOR()
      END
    OF EVENT:MouseMove
      IF KEYCODE() = MouseLeft AND bTrackMouse
         IF glo:SplitterOrientation = SplitterOrientation:Vertical
            X = MouseX()
            XMax = Window{PROP:Width}-MIN_PANELWIDTH
            IF INRANGE(X,MIN_PANELWIDTH,XMax)
               ?SplitterBar{PROP:Fill} = COLOR:BLUE
               ?SplitterBar{PROP:XPos} = X
               DISPLAY(?SplitterBar)
            END
         ELSE
            Y = MouseY()
            YMax = Window{PROP:Height}-(MIN_PANELHEIGHT-8)
            IF INRANGE(Y,MIN_PANELHEIGHT,YMax)
               ?SplitterBar{PROP:Fill} = COLOR:BLUE
               ?SplitterBar{PROP:YPos} = Y
               DISPLAY(?SplitterBar)
            END
         END
         glo:SplitX = ?SplitterBar{PROP:XPos}
         glo:SplitY = ?SplitterBar{PROP:YPos}
      END
    END
  END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeNewSelection PROCEDURE

ReturnValue          BYTE,AUTO

n      LONG
Looped BYTE
  CODE
  LOOP                                                     ! This method receives all NewSelection events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeNewSelection()
    CASE FIELD()
    OF ?CurrentSearch
      IF FoldMarginClicked = TRUE
         FoldMarginClicked = FALSE
      ELSE
         IF ?CurrentSearch{PROP:ChoiceFEQ} = NewTab
            !dbx.debugout('Current Search Choice is New Tab')
            DO UpdateSearchQueueRecord
            DO SetTabFont
            NewTab{PROP:Text} = 'New Search'
            LastTabNumber += 1
            DO AddSearchQueueRecord
            NewTab = CREATE(0,CREATE:tab,?CurrentSearch)
            DO SetNewTabFont
            NewTab{PROP:Text} = NewSearchText
            NewTab{PROP:Hide} = FALSE
            IF ViewerActive
               !SciControl.SetReadOnly(FALSE)
               SciControl.ClearBuffer()
               ViewerActive = FALSE
            END
            bAutoSearch = TRUE
         ELSE
            DO GetSearchQueueRecord
         END
      
         ?ResultList{PROP:Format} = SearchQueue.szListBoxFormat
      
         !hide location column if no data
         IF ResultQueueHasLocations(SearchQueue.ResultQueue)
            LOOP n = 1 TO 6
              IF ?ResultList{PROPLIST:Header,n} = 'Location'
                 IF ?ResultList{PROPLIST:width,n} = 0
                    ?ResultList{PROPLIST:width,n} = CHOOSE(saveColWidth > 0,saveColWidth, 50)
                    saveColWidth = 0
                 END
                 BREAK
              END
            END
         ELSE
            LOOP n = 1 TO 6
              IF ?ResultList{PROPLIST:Header,n} = 'Location'
                 saveColWidth = ?ResultList{PROPLIST:width,n}
                 ?ResultList{PROPLIST:width,n} = 0
                 BREAK
              END
            END
         END
      
         ?ResultList{PROP:From} = SearchQueue.ResultQueue
      
         AutoSizer.Reset(?ResultList,SearchQueue.ResultQueue)
      
         IF glo:AutoSizeResultColumns = TRUE AND bDoAutoSize = TRUE
            AutoSizer.ResizeAll(?ResultList)
            SearchQueue.szListBoxFormat = ?ResultList{PROP:Format}
            bDoAutoSize = FALSE
         END
      
         ?ResultList{PROP:Selected} = SearchQueue.lPointer
         GET(SearchQueue.ResultQueue,SearchQueue.lPointer)
      
         cs.Wait()
         ThreadQueue.tabNumber = ?CurrentSearch{PROP:ChoiceFEQ}
         GET(ThreadQueue,+ThreadQueue.tabNumber)
         IF NOT ERRORCODE()
            !dbx.Debugout('ThreadQueue for tab ' & ThreadQueue.tabNumber & ' found in take new selection')
            ShowWaitCursor = TRUE
            SETCURSOR(CURSOR:Wait)
            DISABLE(?cmdSearch)
            ResultListMenu.SetItemEnable('Search',FALSE)
            ResultListMenu.SetItemEnable('RedoSearch',FALSE)
      
            HIDE(?cmdCloseTab)
            ResultListMenu.SetItemEnable('Close',FALSE)
            UNHIDE(?cmdCancelSearch)
      
            ENABLE(?cmdCancelSearch)
            ResultListMenu.SetItemEnable('Cancel',TRUE)
            SearchQueue.szMatchesFound = ''
         ELSE
            !dbx.Debugout('ThreadQueue for tab ' & ThreadQueue.tabNumber & ' NOT found in take new selection')
            SearchQueue.feqSearchProgress{PROP:Progress} = MAX_PROGRESS
            ShowWaitCursor = FALSE
            SETCURSOR()
            HIDE(SearchQueue.feqSearchProgress)
            IF glo:bHideResultsPanel = FALSE
               UNHIDE(?szMatchesFound)
            END
            ENABLE(?cmdSearch)
            ResultListMenu.SetItemEnable('Search',TRUE)
            ResultListMenu.SetItemEnable('RedoSearch',TRUE)
      
            DISABLE(?cmdCancelSearch)
            ResultListMenu.SetItemEnable('Cancel',FALSE)
            HIDE(?cmdCancelSearch)
      
            IF glo:bHideResultsPanel = FALSE
              UNHIDE(?cmdCloseTab)
            END
            IF ?CurrentSearch{PROP:ChoiceFEQ} = ?Search1
               ResultListMenu.SetItemEnable('Close',FALSE)
            ELSE
               ResultListMenu.SetItemEnable('Close',TRUE)
            END
      
            IF SearchQueue.bSearchPressed
               DO UpdateMatchCount
               IF glo:bDontShowSubdirectoryWarning = FALSE
                  IF RECORDS(SearchQueue.ResultQueue) = 0 AND SearchQueue.bSearchSubdirectories = FALSE
                     CASE GetSearchSubdirectories()
                       OF BUTTON:YES
                          redoSearch = TRUE
                          SearchQueue.bSearchSubdirectories = TRUE
                          POST(EVENT:Accepted,?cmdSearch)
                       OF BUTTON:NO
                          SELECT(?ResultList)
                     END
                     SearchQueue.bSearchPressed = FALSE
                  END
               END
            ELSE
               !SearchQueue.szMatchesFound = ''
            END
         END
         PUT(SearchQueue)
         cs.Release()
      
         DO HandleNewSelection
         DISPLAY()
      END
    OF ?ResultList
       IF bClosingDown = TRUE
          CYCLE
       ELSE
          DO HandleNewSelection
       END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE FIELD()
    OF ?cmdCloseTab
      IF bShowEvalMessage = TRUE
         MESSAGE('Usage of this ' & RegisteredTo & ' will expire on ' & FORMAT(ExpiryDate,@D4),'Kwik Source Search Evaluation Copy',ICON:Exclamation)
         bShowEvalMessage = FALSE
         DO HandleNewSelection
      END
    END
  ReturnValue = PARENT.TakeSelected()
    CASE FIELD()
    OF ?sciControl:Region
      kcr_SetFocus(SciControl.GetHwnd())
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
i                    LONG
j                    LONG
p                    LONG
X                    LONG
thisProcInfo         &PROCESS_INFORMATION
nCode                LONG
buttonPressed        LONG
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE EVENT()
    OF EVENT:CloseDown
      szInstallProgram = ''
      POST(EVENT:CloseWindow)
      if WE::CantCloseNow
        WE::MustClose = 1
        cycle
      else
        self.CancelAction = cancel:cancel
        self.response = requestcancelled
      end
    OF EVENT:CloseWindow
       IF KEYCODE() = EscKey
          SETKEYCODE(0)
          IF FOCUS() = ?ResultList
             POST(EVENT:Accepted,?cmdSearch)
          END
          CYCLE
       ELSE
          IF szInstallProgram <> ''
             !loop check for other running copies before installing
             buttonPressed = BUTTON:RETRY
             LOOP WHILE GetRunningCopyCount() > 1
                buttonPressed = MESSAGE('Please close all other running copies of Kwik Source Search.|Press RETRY to try again or CANCEL to abort the installation process.','Warning Other Copies Running',ICON:Exclamation,BUTTON:RETRY+BUTTON:CANCEL,BUTTON:RETRY)
                IF buttonPressed = BUTTON:CANCEL
                   BREAK
                END
             END
             IF buttonPressed = BUTTON:RETRY
                glo:szInstallProgram = szInstallProgram
                glo:szNull = ''
                glo:bInstallOnExit = TRUE
             END
          END
       END
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:AlertKey
       CASE KEYCODE()
         OF UpKey
            IF ListWithFocus = -1
               SciControl.LineScrollUp()
            ELSE
              FORWARDKEY(ListWithFocus)
            END
         OF DownKey
            IF ListWithFocus = -1
               SciControl.LineScrollDown()
            ELSE
              FORWARDKEY(ListWithFocus)
            END
         OF PgUpKey
            IF ListWithFocus = -1
               SciControl.PageUp()
            ELSE
              FORWARDKEY(ListWithFocus)
            END
         OF PgDnKey
            IF ListWithFocus = -1
               SciControl.PageDown()
            ELSE
              FORWARDKEY(ListWithFocus)
            END
         OF CtrlPgUp
            IF ListWithFocus = -1
               SciControl.DocumentStart()
            ELSE
              FORWARDKEY(ListWithFocus)
            END
         OF CtrlPgDn
            IF ListWithFocus = -1
               SciControl.DocumentEnd()
            ELSE
              FORWARDKEY(ListWithFocus)
            END
         OF CtrlHome
            IF ListWithFocus = -1
               SciControl.DocumentStart()
            ELSE
              FORWARDKEY(ListWithFocus)
            END
         OF CtrlEnd
            IF ListWithFocus = -1
               SciControl.DocumentEnd()
            ELSE
              FORWARDKEY(ListWithFocus)
            END
         OF CtrlShiftHook
            SETCLIPBOARD(glo:findstrCommandLine)
         OF AltDelete
            POST(EVENT:Accepted,?cmdDeleteFile)
         OF CtrlDelete
            POST(EVENT:Accepted,?cmdFindAndDelete)
         OF glo:PlusKey
            POST(EVENT:Accepted,?cmdNextLine)
         OF glo:MinusKey
            POST(EVENT:Accepted,?cmdPreviousLine)
         OF CtrlShiftBar
            DO HandleCenterBar
      !  OF ShiftF11
      !     DO CreateRestorePoint
       END
    OF EVENT:DoResize
       IF glo:bHideEditPanel = TRUE
          SciControl.SetHide(TRUE)
       END
       DO HandleResize
    OF EVENT:GainFocus
      !dbx.debugout('ListWithFocus = ' & ListWithFocus)
      !IF ListWithFocus = -1
      !   SciControl.GrabFocus()
      !ELSE
      !   SELECT(?ResultList)
      !END
    OF EVENT:OpenWindow
        !post(event:visibleondesktop)
    OF EVENT:Timer
       IF ListWithFocus = ?ResultList
          IF EditPaneRefreshPending AND NOT INRANGE(CLOCK(), LastNavTime, LastNavTime + 15)
             Window{PROP:Timer} = 0
             DO RefreshEditPane
          END
       END
    OF EVENT:CreateRestorePoint
       DO CreateRestorePoint
    ELSE
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.Update PROCEDURE

  CODE
  PARENT.Update
    IF RECORDS(SearchQueue.ResultQueue) = 0
       DISABLE(?cmdPreviousFolder,?cmdFindAndDelete)
       DISABLE(?cmdSave,?cmdEdit)
       szTitle = ''
       Window{PROP:StatusText,1} = ''
    ELSE
       ENABLE(?cmdPreviousFolder,?cmdFindAndDelete)
       ENABLE(?cmdSave,?cmdEdit)
    END
    IF RECORDS(SearchQueue.UndoQueue) = 0
       DISABLE(?cmdUndoDelete)
    ELSE
       ENABLE(?cmdUndoDelete)
    END

BookmarkAdd PROCEDURE(LONG lineno)

markerHandle   LONG

  CODE
           IF lineno = -1
                   lineno = GetCurrentLineNumber()
                END
           IF NOT BookmarkPresent(lineno)
                   markerHandle = SciControl.MarkerAdd(lineno, markerBookmark)
                END
BookmarkDelete PROCEDURE(LONG lineno)

  CODE
           IF lineno = -1
                   lineno = GetCurrentLineNumber()
                END
           IF BookmarkPresent(lineno)
                   SciControl.MarkerDelete(lineno, markerBookmark)
                END
BookmarkPresent PROCEDURE(LONG lineno)

state       LONG,AUTO

  CODE
      IF lineno = -1
                   lineno = GetCurrentLineNumber()
                END
           state = SciControl.MarkerGet(lineno)
           RETURN BAND(state,(BSHIFT(1,markerBookmark)))
BookmarkToggle PROCEDURE(LONG lineno)

  CODE
           IF lineno = -1
                   lineno = GetCurrentLineNumber()
                END
           IF BookmarkPresent(lineno)
                   BookmarkDelete(lineno)
           ELSE
                   BookmarkAdd(lineno)
                END
BookmarkNext PROCEDURE(BOOL forwardScan, BOOL select)

lineno         LONG
lineStart      LONG
lineRetry      LONG
anchor         LONG
nextLine       LONG

  CODE
      lineno = GetCurrentLineNumber()
           anchor = SciControl.GetAnchor()
      IF forwardScan
           lineStart = lineno + 1                 !Scan starting from next line
           lineRetry = 0                                            !If not found, try from the beginning
                   nextLine = SciControl.MarkerNext(lineStart, BSHIFT(1,markerBookmark))
           ELSE
                   lineStart = lineno - 1                 !Scan starting from previous line
                   lineRetry = SciControl.GetLineCount()  !If not found, try from the end
                   nextLine = SciControl.MarkerPrevious(lineStart, BSHIFT(1,markerBookmark))
                END
           IF nextLine < 0
              IF forwardScan
                      nextLine = SciControl.MarkerNext(lineRetry, BSHIFT(1,markerBookmark))
                   ELSE
                      nextLine = SciControl.MarkerPrevious(lineRetry, BSHIFT(1,markerBookmark))
         END
      END
           IF nextLine < 0                           !No bookmark
                   MESSAGE('No bookmarks!','Warning',ICON:Exclamation)
           ELSIF nextLine = lineno                   !Only one, and already on it
                   MESSAGE('No more bookmarks!','Warning',ICON:Exclamation)
                ELSE
                        SciControl.EnsureVisibleEnforcePolicy(nextLine)
                   SciControl.GotoLine(nextLine)
                   IF select
                      SciControl.SetAnchor(anchor)
                   END
                END

GetCurrentLineNumber PROCEDURE()

  CODE
      RETURN SciControl.LineFromPosition(SciControl.GetCurrentPos())
FixFileFolding PROCEDURE
i                    LONG
j                    LONG
thisFile             CSTRING(261)
thisLine             LONG
thisText             CSTRING(261)
thisFoldLevel        LONG
bInMap               BOOL(FALSE)
bInClassDeclaration  BOOL(FALSE)
bInProcedure         BOOL(FALSE)
bInOmit              BOOL(FALSE)
bInLoop              BOOL(FALSE)
bInRegion            BOOL(FALSE)
szOmitToken          CSTRING(265)

   CODE
      thisFoldLevel = 1024
      thisFile = SearchQueue.ResultQueue.SortName
      thisLine = 0
      AsciiFilename = thisFile
      OPEN(AsciiFile,ReadOnly+DenyNone)
      IF ~ERRORCODE()
         SET(AsciiFile)
         LOOP
            NEXT(AsciiFile)
            IF ERRORCODE()
               BREAK
            ELSE
               thisLine += 1
               IF bInOmit = TRUE
                  SciControl.SetFoldLevel(thisLine-1,thisFoldLevel)
                  thisText = UPPER(AsciiFile.Buffer)
                  IF INSTRING(szOmitToken,thisText,1)
                     bInOmit = FALSE
                     szOmitToken = ''
                     thisFoldLevel -= 1
                  END
                  CYCLE
               END

               thisText = UPPER(CLIP(LEFT(AsciiFile.Buffer)))
               IF thisText[1] = '!'
                  IF INSTRING('!REGION',thisText,1)
                     SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                     bInRegion = TRUE
                     thisFoldLevel += 1
                     CYCLE
                  ELSIF INSTRING('!ENDREGION',thisText,1)
                     IF bInRegion = TRUE
                        SciControl.SetFoldLevel(thisLine-1,thisFoldLevel)
                        bInRegion = FALSE
                        thisFoldLevel -= 1
                        CYCLE
                     END
                  END
               END

               !remove comment
               IF AsciiFile.Buffer[1] = ' '
                  thisText = ' '
               ELSE
                  thisText = ''
               END
               i = INSTRING('!',AsciiFile.Buffer)
               IF i
                  thisText = thisText & UPPER(CLIP(LEFT(AsciiFile.Buffer[1 : i-1])))
               ELSE
                  thisText = thisText & UPPER(CLIP(LEFT(AsciiFile.Buffer)))
               END


               IF thisText[1 : 7] = ' OMIT('''
                  bInOmit = TRUE
                  !i = oST.FindChar('<27h>',8,,thisText)
                  i = INSTRING('''',thisText,1,8)     
                  ud.Debug('KSS: FixFileFolding(1): i=' & i & ' thisText=' & CLIP(thisText)) ! mr 2018-12-06 In debug mode, intermittently getting GPF when (apparently) I < 8. 
                  UD.Debug('KSS: FixFileFolding(1): UPPER(CLIP(LEFT(AsciiFile.Buffer))=' & CLIP(UPPER(CLIP(LEFT(AsciiFile.Buffer)))))
                  IF i < = 8                            ! mr 2018-12-06 In debug mode, intermittently getting GPF when (apparently) I < 8. 
                  ELSE                                ! mr 2018-12-06 In debug mode, intermittently getting GPF when (apparently) I < 8.  
                     szOmitToken = thisText[8 : i-1]
                  END                                 ! mr 2018-12-06 In debug mode, intermittently getting GPF when (apparently) I < 8.  
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1

               ELSIF thisText[1 : 10] = ' COMPILE('''
                  bInOmit = TRUE
                  i = INSTRING('''',thisText,1,11)
                  ud.Debug('KSS: FixFileFolding(2): i=' & i & ' thisText=' & CLIP(thisText)) ! mr 2018-12-06 In debug mode, intermittently getting GPF when (apparently) I < 8.                   
                  IF i < = 11                           ! mr 2018-12-06 In debug mode, intermittently getting GPF when (apparently) I < 11. 
                  ELSE                                ! mr 2018-12-06 In debug mode, intermittently getting GPF when (apparently) I < 11.                    
                     szOmitToken = thisText[11 : i-1]
                  END                                 ! mr 2018-12-06 In debug mode, intermittently getting GPF when (apparently) I < 11.                    
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1

               !foldable elements that have no labels
               ELSIF thisText[1 : 4] = ' MAP' AND LEN(thisText) = 4
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  bInMap = TRUE
                  thisFoldLevel += 1
               ELSIF thisText[1 : 8] = ' MODULE('
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1
                  bInClassDeclaration = TRUE

               ELSIF thisText = ' RECORD'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1

               !window controls
               ELSIF thisText = ' GROUP' OR thisText[1 : 7] = ' GROUP(' OR thisText[1 : 7] = ' GROUP,'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  IF thisText[LEN(thisText)] = '.' OR thisText[LEN(thisText)-2 : LEN(thisText)] = 'END'
                     !don't bump level
                  ELSE
                     thisFoldLevel += 1
                  END
               ELSIF thisText[1 : 6] = ' MENU('
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1
               ELSIF thisText[1 : 9] = ' MENUBAR,'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1
               ELSIF thisText[1 : 5] = ' OLE,'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1
               ELSIF thisText[1 : 8] = ' OPTION(' OR thisText[1 : 8] = ' OPTION,'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1
               ELSIF thisText[1 : 7] = ' SHEET,'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1
               ELSIF thisText[1 : 5] = ' TAB(' OR thisText[1 : 5] = ' TAB,'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1
               ELSIF thisText[1 : 9] = ' TOOLBAR,'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1

               !execution control
               ELSIF thisText[1 : 7] = ' ACCEPT'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1
               ELSIF thisText[1 : 6] = ' BEGIN'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1
               ELSIF thisText[1 : 5] = ' CASE'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1
               ELSIF thisText[1 : 8] = ' EXECUTE'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1
               ELSIF thisText[1 : 3] = ' IF'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  IF thisText[LEN(thisText)] = '.' OR thisText[LEN(thisText)-3 : LEN(thisText)] = ';END'
                     !don't bump level
                  ELSE
                     thisFoldLevel += 1
                  END
               ELSIF thisText = ' LOOP'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  bInLoop = TRUE
                  thisFoldLevel += 1
               ELSIF thisText[1 : 6] = ' LOOP '
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1

               ELSIF thisText = ' ITEMIZE' OR thisText[1 : 9] = ' ITEMIZE(' OR thisText[1 : 9] = ' ITEMIZE,'
                  SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                  thisFoldLevel += 1

               ELSIF bInLoop = TRUE AND (thisText[1 : 7] = ' UNTIL ' OR thisText[1 : 7] = ' WHILE ')
                  SciControl.SetFoldLevel(thisLine-1,thisFoldLevel)
                  thisFoldLevel -= 1
                  bInLoop = FALSE

               ELSIF thisText = ' END' OR thisText = ' .'
                  SciControl.SetFoldLevel(thisLine-1,thisFoldLevel)
                  thisFoldLevel -= 1
                  IF bInMap = TRUE
                     bInMap = FALSE
                  END
                  IF bInClassDeclaration = TRUE
                     bInClassDeclaration = FALSE
                  END

               !foldable elements that may have labels
               ELSE
                  thisText = CLIP(LEFT(thisText))
                  i = INSTRING(' ',thisText)
                  IF i
                     j = INSTRING('''',thisText)
                     IF j > 0 AND j < i
                        i = 0
                     END
                  END
                  !dbx.debugout('[' & i & '] ' & CLIP(LEFT(thisText[i+1 : LEN(thisText)])))
                  !IF i
                     thisText = CLIP(LEFT(thisText[i+1 : LEN(thisText)]))

                     !data structure
                     IF thisText = 'ITEMIZE' OR thisText[1 : 8] = 'ITEMIZE(' OR thisText[1 : 8] = 'ITEMIZE,'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        thisFoldLevel += 1

                     !entities
                     ELSIF thisText[1 : 6] = 'CLASS(' OR thisText[1 : 6] = 'CLASS,'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        IF thisText[LEN(thisText)] = '.' OR thisText[LEN(thisText)-2 : LEN(thisText)] = 'END'
                           !don't bump level
                        ELSE
                           thisFoldLevel += 1
                           bInClassDeclaration = TRUE
                        END
                     ELSIF thisText[1 : 9] = 'INTERFACE'  OR thisText[1 : 10] = 'INTERFACE(' OR thisText[1 : 10] = 'INTERFACE,'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        IF thisText[LEN(thisText)] = '.' OR thisText[LEN(thisText)-2 : LEN(thisText)] = 'END'
                           !don't bump level
                        ELSE
                           thisFoldLevel += 1
                           bInClassDeclaration = TRUE
                        END
                     ELSIF thisText = 'GROUP' OR thisText[1 : 6] = 'GROUP(' OR thisText[1 : 6] = 'GROUP,'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        IF thisText[LEN(thisText)] = '.' OR thisText[LEN(thisText)-2 : LEN(thisText)] = 'END'
                           !don't bump level
                        ELSE
                           thisFoldLevel += 1
                        END
                     ELSIF thisText = 'QUEUE' OR thisText[1 : 6] = 'QUEUE(' OR thisText[1 : 6] = 'QUEUE,'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        IF thisText[LEN(thisText)] = '.' OR thisText[LEN(thisText)-2 : LEN(thisText)] = 'END'
                           !don't bump level
                        ELSE
                           thisFoldLevel += 1
                        END

                     !views
                     ELSIF thisText = 'VIEW' OR thisText[1 : 5] = 'VIEW(' OR thisText[1 : 5] = 'VIEW,'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        thisFoldLevel += 1
                     ELSIF thisText[1 : 5] = 'JOIN('
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        thisFoldLevel += 1

                     !windows
                     ELSIF thisText[1 : 12] = 'APPLICATION(' OR thisText[1 : 12] = 'APPLICATION,'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        thisFoldLevel += 1
                     ELSIF thisText[1 : 7] = 'WINDOW(' OR thisText[1 : 7] = 'WINDOW,'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        thisFoldLevel += 1

                     !files
                     ELSIF thisText[1 : 5] = 'FILE,'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        thisFoldLevel += 1
                     ELSIF thisText[1 : 6] = 'RECORD'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        thisFoldLevel += 1

                     !reports
                     ELSIF thisText = 'REPORT' OR thisText[1 : 7] = 'REPORT(' OR thisText[1 : 7] = 'REPORT,'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        thisFoldLevel += 1
                     ELSIF thisText[1 : 6] = 'BREAK('
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        thisFoldLevel += 1
                     ELSIF thisText = 'DETAIL' OR thisText[1 : 7] = 'DETAIL,'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        thisFoldLevel += 1
                     ELSIF thisText = 'FORM' OR thisText[1 : 5] = 'FORM,'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        thisFoldLevel += 1
                     ELSIF thisText = 'FOOTER' OR thisText[1 : 7] = 'FOOTER,'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        thisFoldLevel += 1
                     ELSIF thisText = 'HEADER' OR thisText[1 : 7] = 'HEADER,'
                        SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                        thisFoldLevel += 1

                     !procedures, functions and routines
                     ELSIF thisText = 'PROCEDURE' OR thisText[1 : 10] = 'PROCEDURE(' OR thisText[1 : 10] = 'PROCEDURE '  OR  |
                           thisText = 'FUNCTION'  OR thisText[1 : 9]  = 'FUNCTION('  OR thisText[1 : 9]  = 'FUNCTION '   OR  |
                           thisText[1 : 7] = 'ROUTINE'
                        IF bInMap = FALSE AND bInClassDeclaration = FALSE
                           IF bInProcedure
                              SciControl.SetFoldLevel(thisLine-2,thisFoldLevel)
                              !thisFoldLevel -= 1
                              thisFoldLevel = 0400h
                              SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                           ELSE
                              SciControl.SetFoldLevel(thisLine-1,BOR(thisFoldLevel,SC_FOLDLEVELHEADERFLAG))
                              bInProcedure = TRUE
                           END
                           thisFoldLevel += 1
                        ELSE
                           SciControl.SetFoldLevel(thisLine-1,thisFoldLevel)
                        END
                     ELSE
                        SciControl.SetFoldLevel(thisLine-1,thisFoldLevel)
                     END
                  !ELSE
                  !   SciControl.SetFoldLevel(thisLine-1,thisFoldLevel)
                  !END
               END
            END
         END
         CLOSE(AsciiFile)
      ELSE
         !error opening file
      END
      RETURN
ConfirmAutoSave PROCEDURE() !,LONG
ReturnValue                   LONG(0)

MyWindow WINDOW('Confirm AutoSave'),AT(,,350,42),CENTER,GRAY,FONT('Segoe UI',10)
        IMAGE('warning.ico'),AT(5,5,18,16),USE(?imgSave),CENTERED
        PROMPT('Warning: AutoSave is enabled.  Changes to the current file have ' & |
                'been detected and it is about to be saved.  Would you like to s' & |
                'ave the changes to the current file?'),AT(30,5,317,16), |
                USE(?Prompt:Message)
        BUTTON('Yes'),AT(210,26,43,11),USE(?cmdYes),DEFAULT
        BUTTON('No'),AT(258,26,43,11),USE(?cmdNo)
        CHECK(' Enable AutoSave'),AT(30,26,,11),USE(glo:bAutoSave)
    END

   CODE
   OPEN(MyWindow)
   CorrectForOffscreen(MyWindow)
   ACCEPT
     CASE EVENT()
       OF EVENT:CloseDown
          POST(EVENT:CloseWindow,,1)
       OF EVENT:CloseWindow
          glo:bShowAutoSaveWarning = FALSE
          BREAK
       OF EVENT:Accepted
          CASE ACCEPTED()
            OF ?cmdYes
               ReturnValue = BUTTON:YES
               POST(EVENT:CloseWindow)
            OF ?cmdNo
               ReturnValue = BUTTON:NO
               POST(EVENT:CloseWindow)
          END
     END
   END
   CLOSE(MyWindow)

   RETURN ReturnValue
GetSearchSubdirectories PROCEDURE() !,LONG
ReturnValue                   LONG(0)

MyWindow WINDOW('Search Subdirectories'),AT(,,350,42),CENTER,GRAY,FONT('Segoe UI',10)
        IMAGE('help.ico'),AT(5,5,18,16),USE(?imgHelp),CENTERED
        PROMPT('Your search did not return any results and the Search Subdirecto' & |
                'ries option was not checked.<13,10>Would you like to try your s' & |
                'earch again with  the Search Subdirectories option checked?'), |
                AT(30,5,317,16),USE(?Prompt:Message)
        BUTTON('No'),AT(258,26,43,11),USE(?cmdNo),DEFAULT
        BUTTON('Yes'),AT(210,26,43,11),USE(?cmdYes)
        CHECK(' Do not show this message again.'),AT(30,26,,11),USE(glo:bDontShowSubdirectoryWarning)
    END

   CODE
   OPEN(MyWindow)
   CorrectForOffscreen(MyWindow)
   ACCEPT
     CASE EVENT()
       OF EVENT:CloseDown
          POST(EVENT:CloseWindow,,1)
       OF EVENT:CloseWindow
          BREAK
       OF EVENT:Accepted
          CASE ACCEPTED()
            OF ?cmdYes
               ReturnValue = BUTTON:YES
               POST(EVENT:CloseWindow)
            OF ?cmdNo
               ReturnValue = BUTTON:NO
               POST(EVENT:CloseWindow)
          END
     END
   END
   CLOSE(MyWindow)

   RETURN ReturnValue
AutoSizer.Reset   PROCEDURE(SIGNED ListControl, QUEUE ListQueue)
   CODE
      SELF.Kill()
      SELF.Init()
      SELF.AddListBox(ListControl,ListQueue)

SciControl.Colourise PROCEDURE(LONG lStart,LONG lEnd)

!M               LONG
!N               LONG
!P               LONG
!FirstLine       LONG
!LastLine        LONG
!thisLine        LONG
!thisFoldLevel   LONG
!thisClass       LONG
!LastClass       LONG
!SaveClass       LONG
!foldQ           QUEUE,PRE(foldQ)
!foldLine          LONG
!                END
cc              LONG
szProperty      CSTRING('fold')
szPropertyValue CSTRING(2)
  CODE
  cc = SELF.GetProperty(szProperty,szPropertyValue)
  SELF.bFoldMargin = 1 - szPropertyValue
  sciControl.foldMargin()
  PARENT.Colourise(lStart,lEnd)
  !CASE UPPER(SearchQueue.ResultQueue.szExtension)
  !  OF '.CLA' OROF '.CLW' OROF '.TPL' OROF '.TPW'
  IF SELF.GetLexer() = SCLEX_CLWNOCASE
     FixFileFolding()
  END
  Window{PROP:Color} = SELF.StyleGetBack(32)


SciControl.FindWindowTakeOpenWindow PROCEDURE

  CODE
  PARENT.FindWindowTakeOpenWindow
  IF NOT SELF.FindOptionsWindow &= NULL
     CorrectForOffScreen(SELF.FindOptionsWindow)
  END


SciControl.FoldAll PROCEDURE

  CODE
  IF SELF.bFoldMargin = TRUE
  PARENT.FoldAll
  END


SciControl.FoldMargin PROCEDURE

  CODE
  PARENT.FoldMargin
  IF SELF.bFoldMargin
     SELF.Popup.SetItemCheck('FoldMargin',FALSE)
     SELF.Popup.SetIcon('FoldMargin','checkbox_on.ico')
     SELF.Popup.SetItemEnable('ToggleFoldCtrlShiftM',TRUE)
     SELF.Popup.SetItemEnable('ToggleAllFoldsCtrlShiftT',TRUE)
  ELSE
     SELF.Popup.SetIcon('FoldMargin','checkbox_off.ico')
     SELF.Popup.SetItemEnable('ToggleFoldCtrlShiftM',FALSE)
     SELF.Popup.SetItemEnable('ToggleAllFoldsCtrlShiftT',FALSE)
  END


SciControl.GetModify PROCEDURE

ReturnValue          BOOL,AUTO

  CODE
  ReturnValue = PARENT.GetModify()
  SELF.Popup.SetItemEnable('Save',ReturnValue)
  RETURN ReturnValue


SciControl.GrabFocus PROCEDURE

  CODE
  PARENT.GrabFocus
  ListWithFocus = -1
  Window{Prop:StatusText,2} = 'EDITOR'


SciControl.Init PROCEDURE(*WINDOW W,LONG feq,UNSIGNED id,BOOL Themed = 0)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Init(W,feq,id,TRUE)
  OMIT('ReturnValue')
  ReturnValue = PARENT.Init(W,feq,id,Themed)
  SELF.Style = glo:ViewerStyles
  SELF.ResetPopupMenu()
  RETURN ReturnValue


SciControl.Kill PROCEDURE

  CODE
  SELF.ClearAll()
  PARENT.Kill


SciControl.OpenFile PROCEDURE(*CSTRING szFileName)

ReturnValue          BOOL,AUTO

dwResult             ULONG
osvi                 LIKE(OSVERSIONINFO)
  CODE
  ReturnValue = PARENT.OpenFile(szFileName)
  SELF.SetLexerType(SearchQueue.ResultQueue.SortName)
  SELF.SetScrollWidthTracking(TRUE)
  SELF.SetXCaretPolicy(CARET_SLOP,20)
  SELF.SetYCaretPolicy(BOR(CARET_STRICT,BOR(CARET_SLOP,CARET_EVEN)),1)
  SELF.SetVisiblePolicy(VISIBLE_SLOP,0)
  SELF.SetEOLMode(SC_EOL_CRLF)
  !SELF.SetCaretLineBack(COLOR:BLUE)
  !SELF.SetCaretLineVisible(TRUE)
  Window{PROP:COLOR} = SciControl.StyleGetBack(32)
  Window{PROP:StatusText,1} = 'Modified: ' & FORMAT(GetFileDate(szFileName),@D010) & ' ' & FORMAT(GetFileTime(szFileName),@T04)
  RETURN ReturnValue


SciControl.PrintAsk PROCEDURE

  CODE
  IF ListWithFocus <> ?ResultList
     PARENT.OnPrint(TRUE)
  END
  RETURN
  !Don't call PARENT
  PARENT.PrintAsk


SciControl.SearchAsk PROCEDURE(BOOL bShowWindow,tagHTMLHelp HTMLHelp)

  CODE
  PARENT.SearchAsk(bShowWindow,HTMLHelp)
  SELF.SetSel(SELF.GetCurrentPos(),SELF.GetCurrentPos() + LEN(SELF.GetFindWhat()))
  SearchQueue.FindGroup = SELF.GetFindGroup()
  PUT(SearchQueue)


SciControl.SetAlerts PROCEDURE

  CODE
  PARENT.SetAlerts
  !SELF.W{PROP:Alrt,255} = F5Key        !RedoSearch
  SELF.W{PROP:Alrt,255} = CtrlF2       !Toggle Bookmark
  SELF.W{PROP:Alrt,255} = F2Key        !Next Bookmark
  SELF.W{PROP:Alrt,255} = ShiftF2      !Previous Bookmark
  SELF.W{PROP:Alrt,255} = CtrlShiftF2  !Clear All Bookmarks
  SELF.W{PROP:Alrt,255} = CtrlE        !Send To
  SELF.W{PROP:Alrt,255} = CtrlP        !Print
  SELF.W{PROP:Alrt,255} = CtrlR        !Replace
  SELF.W{PROP:Alrt,255} = CtrlS        !Save
  SELF.W{PROP:Alrt,255} = CtrlShiftS   !Save As
  SELF.W{PROP:Alrt,255} = CtrlShiftM   !Toggle Fold
  SELF.W{PROP:Alrt,255} = CtrlShiftT   !Toggle All Folds
  SELF.W{PROP:Alrt,255} = CtrlK        !Copy for Skype
  SELF.W{PROP:Alrt,255} = CtrlL        !Copy for Slack


SciControl.SetClarionLexer PROCEDURE

  CODE
  !  SELF.SetClarionLexer('clarion')
  !  RETURN
  !Don't call PARENT
  PARENT.SetClarionLexer


SciControl.SetColors PROCEDURE(*COLORGROUPTYPE color)

  CODE
  PARENT.SetColors(color)
  SELF.MarkerSetBack(markerBookmark, glo:BookmarkBack)
  SELF.SetSelBack(TRUE,glo:SelectedBack)


SciControl.SetDefaults PROCEDURE

  CODE
  PARENT.SetDefaults
  SELF.SetDefaultStyles()
  SELF.MarkerSetBack(markerBookmark,glo:BookmarkBack)


SciControl.SetLexerType PROCEDURE(STRING szFileType)

i              LONG
n              LONG
p              LONG
String2        CSTRING(256)
defaultLexer   CSTRING(256)
nLexer         LONG
szLexer        CSTRING(33)
  CODE
  LOOP i = 1 TO RECORDS(FileExtensionQueue)
     GET(FileExtensionQueue,i)
     p = 1
     LOOP n = 1 TO LEN(FileExtensionQueue.Extension)
        IF FileExtensionQueue.Extension[n] = '*'
           IF n = 1
              String2 = '^.*'
              p += 3
           ELSIF n = LEN(FileExtensionQueue.Extension)
              String2[p] = '.'
              p += 1
              String2[p] = '*'
              p += 1
              String2[p] = '$'
              p += 1
           ELSE
              String2[p] = '.'
              p += 1
              String2[p] = '*'
              p += 1
           END
        ELSIF FileExtensionQueue.Extension[n] = '.'
              String2[p] = '\'
              p += 1
              String2[p] = '.'
              p += 1
        ELSE
           String2[p] = FileExtensionQueue.Extension[n]
           p += 1
        END
     END
     IF String2[p-1] = '$'
        String2[p] = '<0>'
     ELSE
        String2[p] = '$'
        p += 1
        String2[p] = '<0>'
     END
  
     IF MATCH(UPPER(szFileType),UPPER(String2),Match:Regular)
        SELF.SetDefaultStyles()
        CASE FileExtensionQueue.nLexer
          OF SCLEX_MSSQL OROF SCLEX_MYSQL OROF SCLEX_SQL
             IF FileExtensionQueue.Extension = '*.SQL'
                IF glo:sqlProperties = ''
                   glo:sqlProperties = FileExtensionQueue.FileMode
                ELSE
                   szLexer = GetPropertyFileLexer(glo:sqlProperties)
                   FileExtensionQueue.nLexer = GetLexerNumber(szLexer)
                   FileExtensionQueue.FileMode = glo:sqlProperties
                END
             END
        END
        SELF.SetLexer(FileExtensionQueue.nLexer,FileExtensionQueue.FileMode)
        BREAK
     ELSE
        !cycle
     END
  END
  
  IF i > RECORDS(FileExtensionQueue)
     SELF.SetDefaultStyles()
     defaultLexer = GetPropertyFileLexer(glo:szDefaultPropertyFile)
     CASE defaultLexer
       OF 'clarion'
          nLexer = SCLEX_CLWNOCASE
       OF 'cpp'
          nLexer = SCLEX_CPP
       OF 'html'
          nLexer = SCLEX_HTML
     ELSE !text
         IF NUMERIC(defaultLexer)
            nLexer = defaultLexer
         ELSE
            nLexer = SCLEX_NULL
         END
     END
     SELF.SetLexer(defaultLexer,glo:szDefaultPropertyFile)
     FileExtensionQueue.FileMode = glo:szDefaultPropertyFile
  END
  
  LOOP i = 1 TO RECORDS(FileModeQueue)
     GET(FileModeQueue,i)
     IF FileModeQueue.FileMode = FileExtensionQueue.FileMode
        SELF.Popup.SetIcon(FileModeQueue.FileMode,'check-green.ico')
        !SELF.Popup.SetItemCheck(FileModeQueue.FileMode,TRUE)
     ELSE
        SELF.Popup.SetIcon(FileModeQueue.FileMode,'')
        !SELF.Popup.SetItemCheck(FileModeQueue.FileMode,FALSE)
     END
  END
  OMIT('PARENT.SetLexerType(szFileType)')
  PARENT.SetLexerType(szFileType)


SciControl.SetTextLexer PROCEDURE

szProperty  CSTRING('fold')
szValue     CSTRING('0')
  CODE
  PARENT.SetTextLexer
  SELF.SetDefaultStyles()
  SearchQueue.szPropertyFile = 'text'
  IF LoadLexerProperties(SELF,'text')
     SELF.SetProperty(szProperty, szValue)
  END
  SELF.Colourise(0,-1)


SciControl.TakeContextMenu PROCEDURE

ReturnValue          BYTE,AUTO

AskResult            CSTRING(33)
lSelectionStart      LONG
lSelectionEnd        LONG
cchReplace           LONG
i                    LONG
PropertyFile         CSTRING(256)
szLexer              CSTRING(33)
  CODE
      IF SELF.bInitialised
         !message(SELF.GetStyleAt(SELF.GetCurrentPos()))
         ListWithFocus = -1
         Window{Prop:StatusText,2} = 'EDITOR'
         ReturnValue = Level:Benign
         lSelectionStart = SELF.GetSelectionStart()
         lSelectionEnd   = SELF.GetSelectionEnd()
         IF lSelectionStart = lSelectionEnd
            SELF.Popup.SetItemEnable('CutCtrlX',FALSE)
            SELF.Popup.SetItemEnable('CopyCtrlC',FALSE)
            SELF.Popup.SetItemEnable('CopyCtrlK',FALSE)
            SELF.Popup.SetItemEnable('CopyCtrlL',FALSE)
            SELF.Popup.SetItemEnable('Delete',FALSE)
         ELSE
            SELF.Popup.SetItemEnable('CutCtrlX',TRUE)
            SELF.Popup.SetItemEnable('CopyCtrlC',TRUE)
            SELF.Popup.SetItemEnable('CopyCtrlK',TRUE)
            SELF.Popup.SetItemEnable('CopyCtrlL',TRUE)
            SELF.Popup.SetItemEnable('Delete',TRUE)
         END
         IF CLIPBOARD()
            SELF.Popup.SetItemEnable('PasteCtrlV',TRUE)
         ELSE
            SELF.Popup.SetItemEnable('PasteCtrlV',FALSE)
         END
         IF SELF.GetFindWhat() = ''
            SELF.Popup.SetItemEnable('FindNext',FALSE)
         ELSE
            SELF.Popup.SetItemEnable('FindNext',TRUE)
         END
         SELF.Popup.SetIcon('HotSpotsEnabled',CHOOSE(glo:bHotSpotsEnabled,'Checkbox_on.ico','Checkbox_off.ico'))
         SELF.Popup.SetIcon('AutoSave',CHOOSE(glo:bAutoSave,'Checkbox_on.ico','Checkbox_off.ico'))
         SELF.Popup.SetIcon('HideResults',CHOOSE(glo:bHideResultsPanel,'Checkbox_on.ico','Checkbox_off.ico'))
         SELF.Popup.SetItemEnable('HideResults',CHOOSE(glo:bHideEditPanel=FALSE,TRUE,FALSE))
  
         AskResult = SELF.Popup.Ask()
         CASE AskResult
           OF 'Save'
              SELF.SaveFile(szTitle)
           OF 'SaveAs'
              IF SELF.SaveFileAs(szTitle)
                 szTitle = SELF.szFileName
              END
           OF 'SendTo'
              IF SELF.GetModify() AND szTitle <> ''
                 DO Handle_FileModified
              END
              IF szTitle <> ''
                 szSendToFilename = szTitle
                 POST(EVENT:Accepted,?cmdEdit)
              END
  
           OF 'Print'
              SELF.PrintAsk()
           OF 'CutCtrlX'
              SELF.Cut()
           OF 'CopyCtrlC'
              SELF.Copy()
           OF 'CopyCtrlK'
              SELF.Copy()
              SETCLIPBOARD('{{code}' & CLIP(CLIPBOARD()) & '{{code}<13,10>KSS found this.')
           OF 'CopyCtrlL'
              SELF.Copy()
              SETCLIPBOARD('<60h>' & CLIP(CLIPBOARD()) & '<60h><13,10>KSS found this.')              
           OF 'PasteCtrlV'
              SELF.Paste()
           OF 'Delete'
              SELF.SetTargetStart(SELF.GetSelectionStart())
              SELF.SetTargetEnd(SELF.GetSelectionEnd())
              cchReplace = SELF.ReplaceTarget(0,szNull)
           OF 'ToggleFoldCtrlShiftM'
              SELF.ToggleFold(GetCurrentLineNumber())
           OF 'ToggleAllFoldsCtrlShiftT'
              SELF.FoldAll()
           OF 'FoldMargin'
              SELF.FoldMargin()
           OF 'Find'
              SELF.SetFindGroup(SearchQueue.FindGroup)
              SELF.SearchAsk(TRUE,oHH)
           OF 'FindNext'
              SELF.SetFindGroup(SearchQueue.FindGroup)
              SELF.SearchAsk(FALSE,oHH)
           OF 'Replace'
              SELF.ReplaceAsk()
           OF 'ToggleBookmark'
              BookmarkToggle(-1)
           OF 'PreviousBookmark'
              BookmarkNext(FALSE,FALSE)
           OF 'NextBookmark'
              BookmarkNext(TRUE,FALSE)
           OF 'ClearAllBookmarks'
              SELF.MarkerDeleteAll(markerBookmark)
           OF 'HotSpotsEnabled'
              glo:bHotSpotsEnabled = 1 - glo:bHotSpotsEnabled
              SELF.Popup.SetIcon('HotSpotsEnabled',CHOOSE(glo:bHotSpotsEnabled,'Checkbox_on.ico','Checkbox_off.ico'))
              LoadLexerProperties(SELF,SearchQueue.szPropertyFile)
           OF 'AutoSave'
              glo:bAutoSave = 1 - glo:bAutoSave
              SELF.Popup.SetIcon('AutoSave',CHOOSE(glo:bAutoSave,'Checkbox_on.ico','Checkbox_off.ico'))
           OF 'HideResults'
              glo:bHideResultsPanel = 1 - glo:bHideResultsPanel
              SELF.Popup.SetIcon('HideResults',CHOOSE(glo:bHideResultsPanel,'Checkbox_on.ico','Checkbox_off.ico'))
              DO HandleHideResults
           OF 'GoTo'
              SELF.AskGotoLine()
         ELSE
              IF AskResult
                 LOOP i = 1 TO RECORDS(FileModeQueue)
                    GET(FileModeQueue,i)
                    IF AskResult = FileModeQueue.FileMode
                       IF SELF.Popup.GetItemChecked(AskResult) = FALSE
                          PropertyFile = CLIP(FileModeQueue.FileMode)
                          szLexer = GetPropertyFileLexer(PropertyFile)
                          CASE GetLexerNumber(szLexer)
                            OF SCLEX_MSSQL OROF SCLEX_MYSQL OROF SCLEX_SQL
                               glo:sqlProperties = PropertyFile
                          END
                          SELF.SetLexer(FileModeQueue.nLexer,PropertyFile)
                       END
                       SELF.Popup.SetIcon(CLIP(FileModeQueue.FileMode),'check-green.ico')
                    ELSE
                       SELF.Popup.SetIcon(CLIP(FileModeQueue.FileMode),'')
                    END
                 END
              END
              !IF AskResult
              !   MESSAGE(AskResult)
              !END
         END
         SELECT(?sciControl:Region)
      ELSE
         ReturnValue = Level:Fatal
      END
      SELF.SetSelectionStart(lSelectionStart)
      SELF.SetSelectionEnd(lSelectionEnd)
  
  omit('ReturnValue')
  ReturnValue = PARENT.TakeContextMenu()
  RETURN ReturnValue


SciControl.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

caretPos                LONG
WordEndPosition         LONG,AUTO
WordStartPosition       LONG,AUTO
szHotClickWord          CSTRING(256)
szEncodedHotClickWord   CSTRING(512)
FindGroup               LIKE(FindGrp)
lFoundPosition          LONG
szURL                   CSTRING(256)
  CODE
      CASE EVENT()
        OF EVENT:AlertKey
           CASE KEYCODE()
             OF UpKey
                IF ListWithFocus = -1
                   SELF.GotoLine(GetCurrentLineNumber()-1)
                   SELF.Home()
                END
             OF DownKey
                IF ListWithFocus = -1
                   SELF.GotoLine(GetCurrentLineNumber()+1)
                   SELF.Home()
                END
             !OF F5Key
             !   redoSearch = TRUE
             !   SELECT(?ResultList)
             !   POST(EVENT:Accepted,?cmdSearch)
             OF F3Key OROF CtrlF
                IF ListWithFocus = -1
                   FindGroup = SearchQueue.FindGroup
                   SELF.SetFindGroup(FindGroup)
                   !FindGroup = SELF.GetFindGroup()
  
                   IF FindGroup.What = '' OR KEYCODE() = CtrlF
                      IF SELF.GetSelectionStart() <> SELF.GetSelectionEnd()
                         SELF.GetSelText(FindGroup.What)
                         SELF.SetFindWhat(FindGroup.What)
                      END
                      SELF.SearchAsk(TRUE,oHH)
                   ELSE
                     IF FindGroup.Direction = 'Down'
                        SELF.GoToPos(SELF.GetCurrentPos() + (LEN(FindGroup.What) + 1))
                     ELSE
                        SELF.GoToPos(SELF.GetCurrentPos() - 1)
                     END
                     SELF.SetAnchor(SELF.GetCurrentPos())
                     IF SELF.SearchNext(FindGroup) = INVALID_POSITION
                        IF FindGroup.Direction = 'Down'
                           SELF.SetCurrentPos(0)
                           SELF.SetAnchor(0)
                           SELF.GotoPos(0)
                           lFoundPosition = SELF.SearchNext(FindGroup)
                        ELSE
                           SELF.SetCurrentPos(SELF.GetLength())
                           SELF.SetAnchor(SELF.GetLength())
                           SELF.GotoPos(SELF.GetLength())
                           lFoundPosition = SELF.SearchNext(FindGroup)
                        END
                     END
  
                     SELF.SetSel(SELF.GetCurrentPos(),SELF.GetCurrentPos() + LEN(FindGroup.What))
                   END
                END
           ELSE
              ReturnValue = PARENT.TakeEvent()
           END
      ELSE
  ReturnValue = PARENT.TakeEvent()
     END
     CASE EVENT()
        OF EVENT:AlertKey
           CASE KEYCODE()
             OF CtrlF2       !Toggle Bookmark
                BookmarkToggle(-1)
             OF CtrlShiftF2  !Clear All Bookmarks
                SciControl.MarkerDeleteAll(markerBookmark)
             OF F2Key        !Next Bookmark
                BookmarkNext(TRUE,FALSE)
             OF ShiftF2      !Previous Bookmark
                BookmarkNext(FALSE,FALSE)
             OF CtrlE
                POST(EVENT:Accepted,?cmdEdit)
             OF CtrlS
                SELF.SaveFile(szTitle)
             OF CtrlShiftS
                SELF.SaveFileAs(szTitle)
             OF CtrlR        !Replace
                SELF.ReplaceAsk()
             OF CtrlShiftM   !Toggle Fold
                SELF.ToggleFold(GetCurrentLineNumber())
             OF CtrlShiftT   !Toggle All Folds
                SELF.FoldAll()
             OF CtrlL
                SELF.Copy()
                SETCLIPBOARD('<060h,13,10>' & CLIPBOARD() & '<060h><13,10>KSS found this')
             OF CtrlK
                SELF.Copy()
                SETCLIPBOARD('{{code}<13,10>' & CLIPBOARD() & '<13,10>{{code}<13,10>KSS found this')                
           END
        OF SCEN_CHANGE
           IF glo:SplitterOrientation = SplitterOrientation:Vertical
              IF SELF.GetModify() AND szTitle <> ''
                 ENABLE(?cmdSaveWarn)
              ELSE
                 DISABLE(?cmdSaveWarn)
              END
              IF glo:bHideEditPanel = FALSE
                 UNHIDE(?cmdSaveWarn)
              END
           ELSE
              IF SELF.GetModify() AND szTitle <> ''
                 ENABLE(?cmdSaveWarn:2)
              ELSE
                 DISABLE(?cmdSaveWarn:2)
              END
              IF glo:bHideEditPanel = FALSE
                 UNHIDE(?cmdSaveWarn:2)
              END
           END
        OF SCEN_SETFOCUS
           ListWithFocus = -1
           Window{Prop:StatusText,2} = 'EDITOR'
        OF SCEN_KILLFOCUS
           !do nothing
        OF SCN_UPDATEUI
           IF SELF.Updated = SC_UPDATE_V_SCROLL
              SELF.SetFocus(TRUE)
           END
        OF SCN_MARGINCLICK
           FoldMarginClicked = TRUE
           SELF.ToggleFold(SELF.LineFromPosition(SELF.MarginClickPosition))
        OF SCN_HOTSPOTCLICK        |
        OROF  SCN_HOTSPOTDOUBLECLICK
           WordEndPosition = SELF.WordEndPosition(SELF.HotSpotClickPosition, 0)
           WordStartPosition = SELF.WordStartPosition(SELF.HotSpotClickPosition, 0)
           SELF.SetCurrentPos(WordStartPosition)
           SELF.SetAnchor(WordEndPosition)
           SELF.GetSelText(szHotClickWord)
           SELF.SetSel(SELF.HotSpotClickPosition,SELF.HotSpotClickPosition)
           !SELF.SetSel(WordStartPosition,WordEndPosition)
           URLEncode(szHotClickWord, szEncodedHotClickWord)
           CASE SELF.GetLexer()
             OF SCLEX_CLWNOCASE OROF SCLEX_CLARION
                IF glo:szClarionHelpFile <> ''
                   IF oHH &= NULL
                      oHH &= NEW tagHTMLHelp
                      oHH.Init(glo:szClarionHelpFile)
                   ELSE
                      oHH.SetHelpFile(glo:szClarionHelpFile)
                   END
                   oHH.KeyWordLookup(szHotClickWord)
                   oHH.SetHelpFile( 'kss.chm' )
                   SELECT(?sciControl:Region)
                END
             OF SCLEX_CPP OROF SCLEX_CPPNOCASE
                CASE UPPER(SearchQueue.ResultQueue.szExtension)
                  OF '.CS'
                     szURL = 'http://social.msdn.microsoft.com/search/en-us?query=' & szEncodedHotClickWord & ' (C%23)#)'
                  OF '.JAVA' OROF '.JAD' OROF '.PDE' OROF '.JS' OROF '.ES'
                     szURL = 'http://social.msdn.microsoft.com/search/en-us?query=' & szEncodedHotClickWord & ' (JAVA)'
                ELSE
                     szURL = 'http://social.msdn.microsoft.com/search/en-us?query=' & szEncodedHotClickWord & ' (C++)'
                END
             OF SCLEX_VB OROF SCLEX_VBSCRIPT
                szURL = 'http://social.msdn.microsoft.com/search/en-us?query=' & szEncodedHotClickWord & ' (VB)'
             OF SCLEX_PYTHON
                szURL = 'http://social.msdn.microsoft.com/search/en-us?query=' & szEncodedHotClickWord & ' (PYTHON)'
             OF SCLEX_SQL
                szURL = 'http://search.oracle.com/search/search?num=10&q=' & szEncodedHotClickWord & '+SYNTAX&group=Documentation&sw=t&search_p_main_operator=all&search_p_op=equals'
                !szURL = 'http://social.msdn.microsoft.com/search/en-us?query=' & szEncodedHotClickWord & ' (SQL)'
             OF SCLEX_MSSQL
                szURL = 'http://social.msdn.microsoft.com/search/en-us?query=' & szEncodedHotClickWord & ' (MSSQL)' !& '"'
             OF SCLEX_MYSQL
                szURL = 'http://search.oracle.com/search/search?num=10&q=' & szEncodedHotClickWord & '+SYNTAX&group=Documentation&group=MySQL&sw=t&search_p_main_operator=all&search_p_op=equals'
             ELSE
                szURL = 'http://social.msdn.microsoft.com/search/en-us?query=' & szEncodedHotClickWord !& '"'
           END
           CASE SELF.GetLexer()
             OF SCLEX_CLWNOCASE OROF SCLEX_CLARION
                ! all done
           ELSE
                szNull = ''
                kcr_ShellExecute(SELF.GetHwnd(),0,szURL,0,szNull,1)
           END
     END
  RETURN ReturnValue


SciControl.TakeOpenWindow PROCEDURE

ReturnValue          BYTE,AUTO

i                    LONG
j                    LONG
thisZoom             LONG
  CODE
  ReturnValue = PARENT.TakeOpenWindow()
  IF bControlInitialised
     SELF.UsePopup(FALSE)
     SELF.SetDefaults()
     !SELF.MarkerDefine(markerBookmark,SC_MARK_CIRCLE)
     SELF.SendMessage(SCI_SETSELECTIONMODE, SC_SEL_LINES, 0)
  
     INIMgr.Fetch('Global','Zoom',glo:Zoom)
     IF glo:Zoom < 0
        glo:Zoom += 1
        SELF.SetZoom(glo:Zoom)
        SELF.ZoomOut()
     ELSIF glo:Zoom > 0
        glo:Zoom -= 1
        SELF.SetZoom(glo:Zoom)
        SELF.ZoomIn()
     END
  END
  RETURN ReturnValue


SciControl.ToggleFold PROCEDURE(LONG lLine)

  CODE
  IF SELF.bFoldMargin = TRUE
  PARENT.ToggleFold(lLine)
  END

SciControl.GetHwnd PROCEDURE()

  CODE
      RETURN SELF.hWnd


SciControl.ReplaceAsk PROCEDURE()

! =======================================================================================
! SciControl.ReplaceAsk
! purpose:  Ask User what to look for and replace with.
!           Provides wrap around option if file limit reached
! inputs :
! outputs:  The given line is scrolled to the top of the window
! returns:
! =======================================================================================

! Static variables so that they persist
!======================================
WinInit             BYTE(FALSE),STATIC
WinXPos             SIGNED,AUTO,STATIC
WinYPos             SIGNED,AUTO,STATIC
FindGroup           LIKE(FindGrp),STATIC
ReplacementsString  CSTRING(65),STATIC
bInitFindGroup      BOOL(TRUE),STATIC
szReplaceWith       LIKE(FindGroup.What),STATIC

SearchFlags         LONG

OmitWindow          BYTE(FALSE)
Quit                BYTE(FALSE)
lCurrentPosition    LONG,AUTO
lFoundPosition      LONG,AUTO
lFoundNext          LONG,AUTO
I                   LONG,AUTO
J                   LONG,AUTO
P                   LONG,AUTO
buttonPressed       BOOL(FALSE)
lFirstVisibleLine   LONG
nReplacements       LONG
lSelectionStart     LONG
lSelectionEnd       LONG
cchReplace          LONG
bReplaceAfterFind   BOOL(FALSE)
szCurrentSelection  CSTRING(1024)

ReplaceOptions WINDOW('Replace'),AT(,,285,86),CENTER,GRAY,IMM,SYSTEM,HLP('Replace.htm'), |
            FONT('Segoe UI',10),ALRT(F12Key),DOUBLE
        PROMPT('Find What'),AT(5,5,40),USE(?FindWhat:Prompt)
        ENTRY(@s64),AT(55,5,135,10),USE(FindGroup.What,, ?FindGroup:What),IMM, |
                TIP('Enter the text to Find')
        PROMPT('Replace With'),AT(5,20),USE(?szReplaceWith:Prompt)
        ENTRY(@s64),AT(55,20,135,10),USE(szReplaceWith),IMM,TIP('Enter the Repla' & |
                'cement text')
        CHECK(' &Case Sensitive'),AT(5,32,92,10),USE(FindGroup.MatchCase,, ?MatchCase), |
                TIP('A match only occurs with text that matches the case of the ' & |
                'search string.')
        CHECK(' W&hole Word Only'),AT(5,42,92,10),USE(FindGroup.WholeWord,, ?WholeWord), |
                TIP('A match only occurs if the characters before and after are ' & |
                'not word characters.')
        CHECK(' Word &Start'),AT(5,52,92,10),USE(FindGroup.WordStart,, ?WordStart), |
                TIP('A match only occurs if the character before is not a word c' & |
                'haracter.')
        CHECK(' Regular E&xpression'),AT(5,62,92,10),USE(FindGroup.RegExp,, ?RegExp), |
                TIP('The search string should be interpreted as a regular expression.')
        CHECK(' &POSIX compatible'),AT(100,62,92,10),USE(FindGroup.POSIX,, ?POSIX), |
                DISABLE,TIP('Treat regular expression in a more POSIX compatible' & |
                ' manner<13><10>by interpreting bare ( and ) for tagged sections' & |
                ' rather than \( and \).')
        CHECK(' &Word Wrap'),AT(5,72,92,10),USE(FindGroup.bWordWrap,, ?bWordWrap)
        OPTION('Direction'),AT(100,32,92,25),USE(FindGroup.Direction),BOXED
            RADIO('&Up'),AT(114,42),USE(?Radio1),TIP('Search towards the start o' & |
                    'f the file'),VALUE('Up')
            RADIO('&Down'),AT(150,42),USE(?Radio2),TIP('Search towards the end o' & |
                    'f the file'),VALUE('Down')
        END
        BUTTON('Find &Next'),AT(196,4,84,14),USE(?cmdFindNext),TIP('Find the nex' & |
                't occurrance of the selected text')
        BUTTON('R&eplace'),AT(196,20,84,14),USE(?cmdReplace),DEFAULT
        BUTTON('Replace &All'),AT(196,36,84,14),USE(?cmdReplaceAll)
        BUTTON('Replace in Se&lection'),AT(196,52,84,14),USE(?cmdReplaceInSelection)
        BUTTON('Replace in &Results'),AT(196,68,84,14),USE(?cmdReplaceInResults)
        BUTTON('Cancel'),AT(196,84,84,14),USE(?CancelButton),HIDE,TIP('Cancel Re' & |
                'place mode and close the Replace window')
        STRING(@S32),AT(100,73,92),USE(ReplacementsString)
    END

  CODE
      IF SELF.bInitialised

         FindGroup = SearchQueue.FindGroup
         szReplaceWith = SearchQueue.szReplaceWith

         IF bInitFindGroup
            FindGroup.What = SELF.GetFindWhat()
            FindGroup.Direction = 'Down'
            FindGroup.MatchCase = FALSE
            FindGroup.WholeWord = FALSE
            FindGroup.WordStart = FALSE
            FindGroup.RegExp    = FALSE
            FindGroup.POSIX     = FALSE
            FindGroup.bWordWrap = TRUE
            bInitFindGroup      = FALSE
         END

         nReplacements = 0

         lCurrentPosition = SELF.GetCurrentPos()
         lFoundPosition = lCurrentPosition

         OmitWindow = FALSE

         LOOP
            IF ~OmitWindow
               OPEN(ReplaceOptions)
               IF WinInit
                  SETPOSITION(0,WinXPos,WinYPos)
               ELSE
                  GETPOSITION(0,WinXPos,WinYPos)
                  WinInit=True
               END
               !CorrectForOffScreen(ReplaceOptions)

               ACCEPT
                 CASE KEYCODE()
                 OF EscKey
                    Quit = TRUE
                    BREAK
                 END

                 CASE EVENT()
                 OF EVENT:AlertKey
                    CASE KEYCODE()
                      OF F12Key
                         oHH.ShowTopic('Replace.htm')
                    END

                 OF EVENT:CloseDown
                    POST(EVENT:CloseWindow)

                 OF EVENT:CloseWindow
                    SELF.FindWindowTakeCloseWindow()
!RR                    SELF.SetFindWhat(FindGroup.What)
                    IF buttonPressed = FALSE
                       Quit = TRUE
                    END

                 OF EVENT:OpenWindow
                    SELF.FindWindowTakeOpenWindow()
                    IF FindGroup.What = ''
                       SELF.GetSelText(FindGroup.What)
                       IF FindGroup.What = '' AND SearchQueue.bUseRegularExpressions = FALSE
                          FindGroup.What = SearchQueue.szPattern
                       END
                    END
                    !SELF.SetSelectionStart(SELF.GetCurrentPos())
                    !SELF.SetSelectionEnd(SELF.GetCurrentPos())
                    lFoundNext = INVALID_POSITION
                    DO UpdateWindow
                    SELECT(?FindGroup:What)

                 OF EVENT:Accepted
                    CASE FIELD()
                    OF ?FindGroup:What
                       UPDATE()
                       SELF.SetFindWhat(FindGroup.What)
                    OF ?szReplaceWith
                       DO UpdateWindow
                    OF ?MatchCase
                       DO UpdateWindow
                    OF ?WholeWord
                       DO UpdateWindow
                    OF ?WordStart
                       DO UpdateWindow
                    OF ?RegExp
                       DO UpdateWindow
                    OF ?POSIX
                       DO UpdateWindow
                    OF ?cmdFindNext
                       buttonPressed = ?cmdFindNext
                       POST(EVENT:CloseWindow)
                    OF ?cmdReplace
                       buttonPressed = ?cmdReplace
                       POST(EVENT:CloseWindow)
                    OF ?cmdReplaceAll
                       buttonPressed = ?cmdReplaceAll
                       POST(EVENT:CloseWindow)
                    OF ?cmdReplaceInSelection
                       buttonPressed = ?cmdReplaceInSelection
                       POST(EVENT:CloseWindow)
                    OF ?cmdReplaceInResults
                       buttonPressed = ?cmdReplaceInResults
                       POST(EVENT:CloseWindow)
                    OF ?CancelButton
                       Quit = TRUE
                       buttonPressed = ?CancelButton
                       POST(EVENT:CloseWindow)
                    END

                 OF EVENT:Moved
                    GETPOSITION(0,WinXPos,WinYPos)

                 OF EVENT:NewSelection
                    CASE FIELD()
                      OF ?FindGroup:What OROF ?szReplaceWith
                         DO UpdateWindow
                    END
                 END

               END   !ACCEPT

               CLOSE(ReplaceOptions)
            END ! IF ~OmitWindow

            OmitWindow=FALSE

            IF Quit
               BREAK
            ELSE
               CASE buttonPressed
                 OF ?cmdFindNext OROF ?cmdReplace

                    IF buttonPressed = ?cmdReplace
                       SELF.GetSelText(szCurrentSelection)
                       IF szCurrentSelection = FindGroup.What
                          SELF.ReplaceSel(szReplaceWith)
                          nReplacements = 1
                       ELSE
                          bReplaceAfterFind = TRUE
                       END
                    END

                    IF FindGroup.Direction = 'Down'
                       SELF.GoToPos(lFoundPosition + (LEN(szReplaceWith) + 1))
                    ELSE
                       SELF.GoToPos(lFoundPosition - 1)
                    END
                    SELF.SetAnchor(SELF.GetCurrentPos())


                    lFoundNext = SELF.SearchNext(FindGroup)
                    IF (lFoundNext = INVALID_POSITION)
                       IF FindGroup.bWordWrap = TRUE
                          IF FindGroup.Direction = 'Down'
                             lFoundPosition = 0
                          ELSE
                             lFoundPosition = SELF.GetLength()
                          END
                          SELF.GoToPos(lFoundPosition)
                          SELF.SetSel(lFoundPosition,lFoundPosition + LEN(FindGroup.What))
                          OmitWindow = TRUE
                       ELSE
                          BREAK
                       END
                    ELSIF lFoundPosition = -1
                       SELF.ErrorMgr.Throw('No more matches')
                    ELSE
                       lFoundPosition = lFoundNext
                       SELF.SetSel(lFoundPosition + LEN(FindGroup.What), lFoundPosition)
                       IF bReplaceAfterFind = TRUE
                          SELF.ReplaceSel(szReplaceWith)
                          nReplacements = 1
                          bReplaceAfterFind = FALSE
                       END
                       SELF.GrabFocus()
                    END

                 OF ?cmdReplaceAll
                    nReplacements = 0
                    SELF.SetAnchor(0)
                    LOOP
                       lFoundNext = SELF.SearchNext(FindGroup)
                       IF lFoundNext = -1
                          BREAK
                       ELSIF lFoundNext = INVALID_POSITION
                          BREAK
                       ELSE
                          lFoundPosition = lFoundNext
                          SELF.SetSel(lFoundPosition + LEN(FindGroup.What), lFoundPosition)
                          SELF.TargetFromSelection()
                          cchReplace =SELF.ReplaceTarget(LEN(szReplaceWith),szReplaceWith)
                          nReplacements += 1
                          SELF.SetAnchor(lFoundPosition + (LEN(szReplaceWith) + 1))
                          SELF.SetCurrentPos(SELF.GetAnchor())
                       END
                    END
                    SELF.GotoPos(lCurrentPosition)
                    SELF.SetAnchor(lCurrentPosition)
                    lFoundPosition = lCurrentPosition

                 OF ?cmdReplaceInSelection
                    lSelectionStart = SELF.GetSelectionStart()
                    lSelectionEnd   = SELF.GetSelectionEnd()

                    SearchFlags = 0
                    IF FindGroup.RegExp
                       SearchFlags = BOR(SearchFlags,SCFIND_REGEXP)
                       IF FindGroup.POSIX
                          SearchFlags = BOR(SearchFlags,SCFIND_POSIX)
                       END
                    ELSE
                       IF FindGroup.MatchCase
                          SearchFlags = BOR(SearchFlags,SCFIND_MATCHCASE)
                       END
                       IF FindGroup.WholeWord
                          SearchFlags = BOR(SearchFlags,SCFIND_WHOLEWORD)
                       END
                       IF FindGroup.WordStart
                          SearchFlags = BOR(SearchFlags,SCFIND_WORDSTART)
                       END
                    END
                    SELF.SetSearchFlags(SearchFlags)

                    nReplacements = 0
                    SELF.SetAnchor(SELF.GetSelectionStart())
                    SELF.TargetFromSelection()
                    LOOP
                       lFoundNext = SELF.SearchInTarget(LEN(FindGroup.What),FindGroup.What)
                       IF lFoundNext = -1
                          BREAK
                       ELSIF lFoundNext = INVALID_POSITION
                          BREAK
                       ELSE
                          lFoundPosition = lFoundNext
                          cchReplace = SELF.ReplaceTarget(LEN(szReplaceWith),szReplaceWith)
                          nReplacements += 1
                          SELF.SetAnchor(lFoundPosition + (LEN(szReplaceWith) + 1))
                          SELF.SetCurrentPos(SELF.GetAnchor())
                          SELF.SetTargetStart(SELF.GetCurrentPos())
                          SELF.SetTargetEnd(lSelectionEnd)
                       END
                    END
                    SELF.GotoPos(lCurrentPosition)
                    SELF.SetAnchor(lCurrentPosition)
                    SELF.SetSelectionStart(lSelectionStart)
                    SELF.SetSelectionEnd(lSelectionEnd)
                    lFoundPosition = lCurrentPosition

                 OF ?cmdReplaceInResults
                    !save view position
                    !lFirstVisibleLine = SELF.GetFirstVisibleLine()

                    !if current buffer has been modified, offer to save it first
                    IF SciControl.GetModify() AND szTitle <> ''
                       DO Handle_FileModified
                    END

                    !search and replace in result list lines
                    nReplacements = winReplaceInResults(FindGroup, szReplaceWith, SearchQueue.ResultQueue)

                    !Reload the Buffer
                    SELF.OpenFile(CurrentFilename)

                    SciControl.MarkerDeleteAll(markerBookmark)
                    !set bookmarks
                    p = CHOOSE(POINTER(SearchQueue.ResultQueue) > 0,POINTER(SearchQueue.ResultQueue),1)
                    GET(SearchQueue.ResultQueue,+SearchQueue.ResultQueue.SortName)
                    LOOP WHILE ((NOT ERRORCODE()) AND (SearchQueue.ResultQueue.SortName = CurrentFilename))
                       BookmarkAdd(SearchQueue.ResultQueue.LineNo-1)
                       GET(SearchQueue.ResultQueue,POINTER(SearchQueue.ResultQueue)+1)
                    END
                    GET(SearchQueue.ResultQueue,p)
                    ?ResultList{PROP:Selected} = p

                    POST(EVENT:GOTONEWLINE)
                    !SELF.SetFirstVisibleLine(lFirstVisibleLine)
                    !SELF.GoToLine(SearchQueue.ResultQueue.LineNo-1)

                    !SELF.GotoPos(lCurrentPosition)
                    !SELF.SetAnchor(lCurrentPosition)
                    !lFoundPosition = lCurrentPosition

               END
               buttonPressed = FALSE
               DO UpdateReplacementsString
            END
         END
         SearchQueue.FindGroup = FindGroup
         SearchQueue.szReplaceWith = szReplaceWith
         PUT(SearchQueue)
      END

      RETURN                                    ! Exit Procedure

UpdateReplacementsString   ROUTINE
   ReplacementsString = CLIP(LEFT(FORMAT(nReplacements,@N_13))) & ' Replacements'
   DISPLAY(?ReplacementsString)
   EXIT

UpdateWindow   ROUTINE
   UPDATE()
   ?cmdFindNext{PROP:Disable} = CHOOSE(FindGroup.What = '')
!   ?cmdReplace{PROP:Disable} = CHOOSE(FindGroup.What = '' OR szReplaceWith = '')
!   ?cmdReplaceAll{PROP:Disable} = CHOOSE(FindGroup.What = '' OR szReplaceWith = '')
!   ?cmdReplaceInSelection{PROP:Disable} = CHOOSE(FindGroup.What = '' OR szReplaceWith = '')
!   ?cmdReplaceInResults{PROP:Disable} = CHOOSE(FindGroup.What = '' OR szReplaceWith = '')
   ?cmdReplace{PROP:Disable} = CHOOSE(FindGroup.What = '')
   ?cmdReplaceAll{PROP:Disable} = CHOOSE(FindGroup.What = '')
   ?cmdReplaceInSelection{PROP:Disable} = CHOOSE(FindGroup.What = '')
   ?cmdReplaceInResults{PROP:Disable} = CHOOSE(FindGroup.What = '')

   IF FindGroup.MatchCase = TRUE |
   OR FindGroup.WholeWord = TRUE |
   OR FindGroup.WordStart = TRUE
      FindGroup.RegExp = FALSE
      FindGroup.POSIX  = FALSE
      DISABLE(?RegExp)
      DISABLE(?POSIX)
   ELSE
      ENABLE(?RegExp)
   END
   IF FindGroup.RegExp = TRUE
      DISABLE(?MatchCase,?WordStart)
      ENABLE(?POSIX)
   ELSE
      FindGroup.POSIX = FALSE
      DISABLE(?POSIX)
      ENABLE(?MatchCase,?WordStart)
   END
   DISPLAY(?MatchCase,?POSIX)
   DO UpdateReplacementsString
   EXIT


SciControl.SaveFile PROCEDURE(*CSTRING szFilename)

! =======================================================================================
! CSciViewer.SaveFile
! purpose:  get contents into Scintilla Control buffer and Save to a file
! inputs :  *CSTRING szFilename - Name of file to save
! outputs:  Scintilla Control is loaded with file contents
! returns:  BYTE
!           Level:Benign to indicate success
!           Level:Notify to indicate failure
! =======================================================================================
ReturnValue         BOOL,AUTO

lFileSize           LONG(0)

szExtension         CSTRING(33)
szMsgText           CSTRING(240)
szMsg_NoCanDo       CSTRING('Not possible.')

szFileExtension     CSTRING(33)

szAAFileName        CSTRING(256),STATIC
A_A                 FILE,DRIVER('DOS'),NAME(szAAFileName),PRE(AA),CREATE
Record                RECORD
Bytes                    STRING(65535)
                    END
                  END

  CODE
!SCI_GETTEXT(int length, char *text)
! This returns length-1 characters of text from the start of the document plus one terminating 0 character.
! To collect all the text in a document, use SCI_GETLENGTH to get the number of characters in the document (nLen),
! allocate a character buffer of length nLen+1 bytes, then call SCI_GETTEXT(nLen+1, char *text).
!If the text argument is 0 then the length that should be allocated to store the entire document is returned.
!If you then save the text, you should use SCI_SETSAVEPOINT to mark the text as unmodified.
    IF szFilename <> ''
       lFileSize = SELF.GetTextLength() + 1                   ! returns length with null terminator
       IF ~SELF.szTextBuffer &= NULL                          ! If we already have a buffer allocated
          !ASSERT(0,eqDBG & 'DISPOSE szTextBuffer [' & ADDRESS(SELF.szTextBuffer) & ']')
          DISPOSE(SELF.szTextBuffer)                          !   dispose buffer
          SELF.szTextBuffer &= NULL                           !   clear reference
       END
       SELF.szTextBuffer &= NEW(CSTRING(lFileSize))           ! Create a buffer to hold the file
       !ASSERT(0,eqDBG & 'NEW szTextBuffer [' & ADDRESS(SELF.szTextBuffer) & ']')
?      ASSERT(~SELF.szTextBuffer &= NULL)
       lFileSize = SELF.GetText(lFileSize,SELF.szTextBuffer)  ! Returns length without null terminator

       szAAFileName = szFileName
       SELF.szFileName &= szAAFileName

       LOOP
          CREATE(A_A)
          IF ~ERRORCODE()
             IF ~SELF.szTextBuffer &= NULL
                DO SaveFile
                SELF.SetSavePoint()
                POST(SCEN_CHANGE)
                ReturnValue = TRUE
                BREAK
             ELSE
                SELF.ErrorMgr.ThrowMessage(CSciViewerMsg:BufferAllocationError, lFileSize+1)
                ReturnValue = FALSE
                BREAK
             END
          ELSIF ERRORCODE() = NoAccessErr  !Access Denied
             IF SELF.ErrorMgr.ThrowFile(CSciViewerMsg:AccessDenied, CLIP(szAAFileName)) = Level:Benign
                IF FILEDIALOG('Save as ...',SELF.szFileName,'All Files (*.*)|*.*', FILE:Save + FILE:KeepDir + FILE:LongName)
                   CYCLE
                ELSE
                   SELF.SetSavePoint()
                   ReturnValue = FALSE
                   BREAK
                END
             ELSE
                SELF.SetSavePoint()
                ReturnValue = FALSE
                BREAK
             END
          ELSE
             SELF.ErrorMgr.ThrowFile(CSciViewerMsg:OpenFailed, CLIP(szAAFileName))
             ReturnValue = FALSE
             BREAK
          END
       END
   ELSE
      ReturnValue = FALSE
   END
   IF ReturnValue = TRUE
      Window{PROP:StatusText,1} = 'Modified: ' & FORMAT(GetFileDate(szFileName),@D010) & ' ' & FORMAT(GetFileTime(szFileName),@T04)
   END
   RETURN(ReturnValue)
 ! Exit Procedure

! Procedure Routines
!-------------------------------------------
SaveFile    ROUTINE
!-------------------------------------------
 DATA

lRecSize            LONG(0)                 ! Note new variables to keep track of bytes read
lBytesWritten       LONG(0)                 ! from file and Bytes written to buffer.
lBytes2Write        LONG(0)
lBytePtr            LONG(0)

 CODE                                      ! Enter Routine

    OPEN(A_A,WriteOnly+DenyAll)
    IF ~ERRORCODE()
       lBytesWritten = 0                                         ! We haven't written any bytes yet
       lFileSize  = SIZE(SELF.szTextBuffer) - 1
       lRecSize   = SIZE(AA:Bytes)

       ShowWaitCursor = TRUE
       SETCURSOR(CURSOR:Wait)

       LOOP
         IF (lBytesWritten + lrecSize >= lFileSize)                       ! If we have written up to or past the file size
            lBytes2Write = lFileSize - lBytesWritten                       ! Bytes to write to file is the last "partial" chunk
         ELSE
            lBytes2Write = lRecSize                                     ! Else the Byte to write is the full record
         END

         LOOP lBytePtr = 1 TO lBytes2Write                              ! Loop through the record
            A_A:Bytes[lBytePtr] = SELF.szTextBuffer[(lBytesWritten + lBytePtr)]   ! Storing the bytes
         END
         APPEND(A_A,lBytes2Write)

         lBytesWritten = lBytesWritten + lBytes2Write                        ! Increment the byte written
         IF (lBytesWritten >= lFileSize)                                  ! Break if we are up to the file size
            BREAK
         END
       END
       CLOSE(A_A)

       ShowWaitCursor = FALSE
       SETCURSOR()
    ELSE
       SELF.ErrorMgr.ThrowFile(CSciViewerMsg:OpenFailed, CLIP(szAAFileName))
    END
 EXIT
 !Exit Routine


SciControl.SaveFileAs PROCEDURE(*CSTRING szFilename)

szSaveAsFilename     CSTRING(MAXPATH)

  CODE
      szSaveAsFilename = szFilename
      IF FILEDIALOG('Save As ...',szSaveAsFilename,'*' & SearchQueue.ResultQueue.szExtension & '|*.*',BOR(BOR(FILE:Save,FILE:KeepDir),FILE:LongName))
         RETURN SELF.SaveFile(szSaveAsFilename)
      END


SciControl.SetLexer PROCEDURE(LONG Lexer, *CSTRING PropertyFile)

szProperty           CSTRING(256)
szPropertyValue      CSTRING(256)

  CODE
      IF SELF.GetLexer() <> Lexer
         SELF.SetLexer(Lexer)
         SearchQueue.szPropertyFile = PropertyFile
         IF LoadLexerProperties(SELF,PropertyFile)
            SELF.SetTextLexer()
         ELSE
            SELF.Colourise(0,-1)
         END
      ELSE
         SearchQueue.szPropertyFile = PropertyFile
         IF NOT LoadLexerProperties(SELF,PropertyFile)
            SELF.Colourise(0,-1)
         END
      END


SciControl.SetDefaultStyles PROCEDURE()

  CODE
  !# Global default styles for all languages
  !# Default
  !style.*.32=$(font.base)
      SELF.StyleSetFont(STYLE_DEFAULT,baseFontName)
      SELF.StyleSetSize(STYLE_DEFAULT,baseFontSize)
      SELF.StyleSetBack(STYLE_DEFAULT,kcr_GetSysColor(COLOR_WINDOW))
      SELF.StyleClearAll()
  
  !# Line number
  !style.*.33=back:#C0C0C0,$(font.base)
      SELF.StyleSetFont(STYLE_LINENUMBER,baseFontName)
      SELF.StyleSetSize(STYLE_LINENUMBER,baseFontSize)
      SELF.StyleSetBack(STYLE_LINENUMBER,ColourRGB(192,192,192))
  
  !# Brace highlight
  !style.*.34=fore:#0000FF,bold
      SELF.StyleSetFore(STYLE_BRACELIGHT,ColourRGB(0,0,255))
      SELF.StyleSetBold(STYLE_BRACELIGHT,TRUE)
  
  !# Brace incomplete highlight
  !style.*.35=fore:#FF0000,bold
      SELF.StyleSetFore(STYLE_BRACEBAD,ColourRGB(255,0,0))
      SELF.StyleSetBold(STYLE_BRACEBAD,TRUE)
  
  !# Control characters
  !style.*.36=
  
  !# Indentation guides
  !style.*.37=fore:#C0C0C0,back:#FFFFFF
      SELF.StyleSetFore(STYLE_INDENTGUIDE,ColourRGB(192,192,192))
      SELF.StyleSetBack(STYLE_INDENTGUIDE,ColourRGB(255,255,255))
  
      RETURN


SciControl.ResetPopupMenu PROCEDURE()

  CODE
  SELF.Popup.Kill()
  SELF.Popup.Init(INIMgr)
  SELF.Popup.AddItem('Save<9>Ctrl+S','Save')
  SELF.Popup.SetItemEnable('Save',FALSE)
  SELF.Popup.AddItem('Save As<9>Ctrl+Shift+S','SaveAs')
  SELF.Popup.AddItem('-','Separator0')
  SELF.Popup.AddItem('Auto Save','AutoSave')
  SELF.Popup.AddItem('-','Separator1')
  SELF.Popup.AddItem('Send To<9>Ctrl+E','SendTo')
  SELF.Popup.AddItem('Print<9>Ctrl+P','Print')
  SELF.Popup.AddItem('-','EditMenuHere')
  SELF.Popup.AddItem('Cut<9>Ctrl+X','CutCtrlX')
  SELF.Popup.AddItem('Copy<9>Ctrl+C','CopyCtrlC')
  SELF.Popup.AddItem('Copy for Skype<9>Ctrl+K','CopyCtrlK')
  SELF.Popup.AddItem('Copy for Slack<9>Ctrl+L','CopyCtrlL')
  SELF.Popup.AddItem('Paste<9>Ctrl+V','PasteCtrlV')
  SELF.Popup.AddItem('Delete','Delete')
  SELF.Popup.AddItem('-','FileModeMenuHere')
  SELF.Popup.AddItem('-','FoldMenuHere')
  SELF.Popup.AddItem('-','Separator2')
  SELF.Popup.AddItem('Find<9>Ctrl+F','Find')
  SELF.Popup.AddItem('Find Next<9>F3','FindNext')
  SELF.Popup.AddItem('Replace<9>Ctrl+R','Replace')
  SELF.Popup.AddItem('-','Separator3')
  SELF.Popup.AddItem('Toggle Bookmark<9>Ctrl+F2','ToggleBookmark')
  SELF.Popup.AddItem('Previous Bookmark<9>Shift+F2','PreviousBookmark')
  SELF.Popup.AddItem('Next Bookmark<9>F2','NextBookmark')
  SELF.Popup.AddItem('Clear All Bookmarks<9>Ctrl+Shift+F2','ClearAllBookmarks')
  SELF.Popup.AddItem('Hot Spots Enabled','HotSpotsEnabled')
  SELF.Popup.AddItem('-','Separator4')
  SELF.Popup.AddItem('Go To Line Number<9>Ctrl+G','GoTo')
  SELF.Popup.AddItem('-','Separator5')
  SELF.Popup.AddItem('Hide Results List','HideResults')
  
  FileModeMenu = LoadFileExtensionQueue(FileModeQueue)
  SELF.Popup.AddSubMenu(FileModeMenu,'FileModeMenuHere')
  
  SELF.Popup.AddSubMenu('Folding','{{Toggle Fold<9>Ctrl+Shift+M|Toggle All Folds<9>Ctrl+Shift+T|-|Fold Margin}','FoldMenuHere')
  SELF.Popup.AddSubMenu('Move','{{Top<9>Ctrl+Home|Page Up<9>PgUp|Page Down<9>PgDn|Bottom<9>Ctrl+End}','GoTo')
  
  
  SELF.Popup.DeleteItem('FoldMenuHere')
  SELF.Popup.DeleteItem('FileModeMenuHere')
  
  SELF.Popup.AddItemEvent('TopCtrlHome',EVENT:ScrollTop,SELF.feq)
  SELF.Popup.AddItemEvent('PageUpPgUp',EVENT:PageUp,SELF.feq)
  SELF.Popup.AddItemEvent('PageDownPgDn',EVENT:PageDown,SELF.feq)
  SELF.Popup.AddItemEvent('BottomCtrlEnd',EVENT:ScrollBottom,SELF.feq)
  
  SELF.Popup.SetIcon('Save','FileSave.ico')
  SELF.Popup.SetIcon('SaveAs','SaveAs.ico')
  SELF.Popup.SetIcon('SendTo','SendTo.ico')
  SELF.Popup.SetIcon('Print','Print.ico')
  SELF.Popup.SetIcon('CutCtrlX','Cut.ico')
  SELF.Popup.SetIcon('CopyCtrlC','Copy.ico')
  SELF.Popup.SetIcon('CopyCtrlK','Skype.ico')
  SELF.Popup.SetIcon('CopyCtrlL','SlackIcon.ico')
  SELF.Popup.SetIcon('PasteCtrlV','Paste.ico')
  SELF.Popup.SetIcon('Delete','Delete.ico')
  SELF.Popup.SetIcon('Find','Find.ico')
  SELF.Popup.SetIcon('FindNext','FindNext.ico')
  SELF.Popup.SetIcon('Replace','Replace.ico')
  SELF.Popup.SetIcon('ToggleBookmark','ToggleBookmark.ico')
  SELF.Popup.SetIcon('PreviousBookmark','PreviousBookmark.ico')
  SELF.Popup.SetIcon('NextBookmark','NextBookmark.ico')
  SELF.Popup.SetIcon('ClearAllBookmarks','ClearAllBookmarks.ico')
  SELF.Popup.SetIcon('HotSpotsEnabled',CHOOSE(glo:bHotSpotsEnabled,'Checkbox_on.ico','Checkbox_off.ico'))
  SELF.Popup.SetIcon('AutoSave',CHOOSE(glo:bAutoSave,'Checkbox_on.ico','Checkbox_off.ico'))
  SELF.Popup.SetIcon('HideResults',CHOOSE(glo:bHideResultsPanel,'Checkbox_on.ico','Checkbox_off.ico'))



Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window
  SELF.RemoveControl(?SplitterBar)                         ! Remove ?SplitterBar from the resizer, it will not be moved or sized

!========================================================================================
ResultList::WndProc PROCEDURE(HWND hWnd, UNSIGNED wMsg, UNSIGNED wParam, LONG lParam)
!========================================================================================
WHEEL_DELTA    EQUATE(120)
Distance       SHORT
NewSize        SHORT

    CODE
    CASE wMsg
    OF WM_MOUSEWHEEL
       IF BAND(wParam,MK_CONTROL)
          Distance = BSHIFT(BAND(wParam,0FFFF0000h),-16)
          NewSize = feqResultList{PROP:FontSize} + Distance/WHEEL_DELTA
          IF NewSize < 4
             NewSize = 4
          ELSIF NewSize > 36
             NewSize = 36
          END
          feqResultList{PROP:FontSize} = NewSize
          feqResultList{PROP:LineHeight} = NewSize
          glo:ResultListFontSize = NewSize
          RETURN 0
       END
    END
    RETURN(kcr_CallWindowProc(ResultList::OrigWndProc,hWnd,wMsg,wParam,lParam))
