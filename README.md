# Devuna-KwikSourceSearch #

With Kwik Source Search (KSS) you can perform powerful standard and regular expression searches through one or multiple text files, such as source code, log files, and debugging information.

KSS is capable of finding the exact text you are looking for in any ASCII file or files. Sometimes you have only part of the information that you want to match, or you want to find a wider range of information. In such cases, KSS has the powerful capability to search for patterns of text using regular expressions.

The underlying search engine for KSS is the Findstr command. KSS provides a convenient, user friendly, interface for the many command line options. Files with non-printable characters are always skipped and the regular expression functionality of the Findstr command is used for the main text search.

KSS captures and filters the output of the Findstr command, opens the file associated with the selected result list item, and displays it in a syntax highlighting editor. KSS has built-in styling for Clarion, C++, C#, HTML, Java, and XML source files and can easily be extended, through the use of special property files, to provide styling for other languages.


This version of KSS is slightly different from the last commercial release.

Most notably the Results List 'Print' and the 'CheckForUpdate' features have been removed as they required other 3rd party tools, some of which are no longer available.

**KSSOpen readme begins (content above came from Randy)**

Get installers here: https://github.com/mriffey/Devuna-KwikSourceSearch/releases

**DEPENDENCIES**

Without the list of tools below, you will not be able to build this source. You can remove the references to xFiles and StringTheory. Anyone with a basic level of experience with Clarion will likely be able to do this. 

Required tools / code:

- This repo
- Clarion 10 or later (installer version is based on Clarion 11) 
- Capesoft xFiles
- Capesoft StringTheory
- Capesoft Winevent (as of 1.0.84)


**OPEN SOURCE RELEASE NOTES AND NEWS**

"KSSO" means "Kwik Source Search - Open Edition". 

KSSO does not require registration. 

To purchase KSS, see Devuna.com (purchasing may no longer be available). NOTE: I AM NOT DEVUNA. 

**1.0.97 2019-02-19** More fixes from Randy and a fix from Winevent that caused the window to bounce when maximized. 

**1.0.85 2019-02-16** Applied a couple of fixes from Randy for MATCH and exclusion processing. 

**1.0.84 2019-02-15** KSSO now moves itself to an available monitor so that you don't "lose" it if you were on a 2 monitor system and subsequently disconnected a monitor and went on a trip. It's possible this also resolved an issue with the search window being "stuck" on monitor 2 after manually moving the frame from the now-missing monitor 2 to monitor 1. The addition of WinEvent (a quick, simple way to accomplish the windows location fixes in 1.0.84) also allows KSS to respond properly (and close itself) during a Windows shutdown. 

**1.0.73 2019-01-08** Corrected installer, which was grabbing an older exe. Thanks to Bill Rollins for the heads up.

**1.0.72 2019-01-03** Added a button to the toolbar that deletes generated built-in Clarion files (ie: ```*_BC*.clw```, ```*_SF.clw```, and ```*_R*.clw```) from the result list. These files often clutter search results and it is tedious to delete them one at a time. The right click menu also includes this delete function. 

**1.0.59 2018-12-06** Found a GPF that revealed a bug that causes OMIT and COMPILE folding to be skipped in the Scintilla control. Haven't figured it out yet, as it's quite sensitive to options (like hide comment lines) and the content of the OMIT/COMPILE string - which varies significantly. Bug causes a GPF due to an index going to 0. For now, I've muted the GPF until I can get back to the root cause. 

**1.0.50 2018-11-28** - Rebuild to eliminate a debug mode queue display. KSS is distributed in debug mode so that I get better details if and when we have a GPF. Previous versions were distributed in release mode. 

**1.0.49 2018-11-27** - Added "Copy for Skype" and "Copy for Slack" right click / control key options for syntax window. These options add the necessary text to the front and the back of the copied text so that these two systems display your selection as code rather than as regular text. Also removed a spurious copy to clipboard of the FINDSTR command that was accidentally left in on 1.0.39. 

**1.0.39 2018-10-01** - Adjusted size of Scintilla control. The lower arrow on the scroll bar was clipped and a bit hard to hit accurately with the mouse. 

**1.0.29 (code change, no release)** - Removed registered user text from options window, which had been hard coded to say not registered.

**1.0.28 2018-09-12** - Added option to process files with binary content (ie: remove FINDSTR's /P option) The option has been added to setup and to the search window. Also removed the automatic push of the FINDSTR to the clipboard as that could get annoying, particularly to folks who don't use a clipboard manager. The blank space above the Scintilla window has also been fixed. 

**1.0.16 2018-08-20** - Housekeeping UI and message cleanup, thanks MG. 

**1.0.14 2018-08-19** - Added feature (per Mark Goldberg's request) which allows KSSO to use Clarion's appname.cwproj.filelist.xml as the "list of files to search". It will automatically parse the list and search only the files noted as <Opened_Files>. It will not search the files created (generally binary) and it automatically excludes undesirable file types from the <Opened_Files> list, such as .lib, .ico, etc. **IMPORTANT: The source uses Capesoft xFiles and StringTheory to implement this feature. **

**1.0.14 2018-08-19** - Version numbering added, which is just a build date/time that's automatically generated. Might help you keep track of whether you're up to date or not. Please dont submit bug reports on old versions. Get current, then reproduce the problem. Thank you.

**1.0.14 2018-08-19** - Installer provided, which includes minor mods to the original Devuna SetupBuilder-based installer. Nothing major, mostly changes to support the version numbering noted above. 
