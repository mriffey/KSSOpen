# KSSOpen hand code edition # 
This is a fork of https://github.com/Devuna/Devuna-KwikSourceSearch. As such, it is a derived work of Devuna's work and the associated MIT license stays with it.

This version has been converted from a Clarion .app to a hand code app to make it more palatable to accept pull requests (among other things). 

As a result, I will no longer be maintaining the app version of my fork of KSS ( https://github.com/mriffey/Devuna-KwikSourceSearch/ ).

With Kwik Source Search (KSS) you can perform powerful standard and regular expression searches through one or multiple text files, such as source code, log files, and debugging information.

KSS is capable of finding the exact text you are looking for in any ASCII file or files. Sometimes you have only part of the information that you want to match, or you want to find a wider range of information. In such cases, KSS has the powerful capability to search for patterns of text using regular expressions.

The underlying search engine for KSS is the Findstr command. KSS provides a convenient, user friendly, interface for the many command line options. Files with non-printable characters are always skipped and the regular expression functionality of the Findstr command is used for the main text search.

KSS captures and filters the output of the Findstr command, opens the file associated with the selected result list item, and displays it in a syntax highlighting editor. KSS has built-in styling for Clarion, C++, C#, HTML, Java, and XML source files and can easily be extended, through the use of special property files, to provide styling for other languages.


This version of KSS is slightly different from the last commercial release.

Most notably the Results List 'Print' and the 'CheckForUpdate' features have been removed as they required other 3rd party tools, some of which are no longer available.

**KSSOpen readme begins here (the content above came from Randy/Devuna)**

Get installers here: https://github.com/mriffey/KSSOpen/releases

**DEPENDENCIES**

Without the list of tools below, you will not be able to build this source. Note that the dependency on Capesoft WinEvent was removed - which means the exe will prevent a reboot / shutdown until code is added to do this without WinEvent. 

Required tools / code:

- This repo
- Clarion 10 or later (installer version is based on Clarion 11; builds with Clarion 12)
- Capesoft StringTheory (tested with 3.88)

The dependencies on Capesoft xFiles and UltimateDebug have been removed. The cwproj.FileList.xml search option now reads the file with StringTheory, and debug output goes through a small local class (`libsrc\win\KssDebug.inc`) that writes to `OutputDebugString` (view it with DebugView). The classes from Mark Goldberg (https://github.com/markgoldberg) that KSS uses are included in `libsrc\win`.

**Building with Clarion 12**

`source\Clarion120.red` adds the repo's `libsrc\win`, `lib` and `images` folders to the Clarion 12 redirection, so no paths need editing. (`Clarion100.RED` is the older Clarion 10 file and still points at `c:\projects\KSSOpen`.) Keep the clone in a short folder path: very deep paths can make the Clarion compiler fail with an internal error.

**WARNING** This repo uses Git LFS. See https://git-lfs.github.com/ for details.


**OPEN SOURCE RELEASE NOTES AND NEWS**

"KSSO" means "Kwik Source Search - Open Source Hand Code Edition". 

KSSO does not require registration. 

To purchase KSS, see Devuna.com (purchasing may no longer be available). NOTE: I AM NOT DEVUNA. 

**2022.1.21.24 2022-01-21** Initial hand code edition, with installer and auto-versioning (date.build).

**2.0.0  2022-01-02** Initial hand code edition - note, this is the old versioning scheme. 

