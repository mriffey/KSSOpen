# KSSOpen Hand-code edition # 
This is a fork of https://github.com/Devuna/Devuna-KwikSourceSearch. As such, it is a derived work of Devuna's work and the associated MIT license stays with it.

With Kwik Source Search (KSS) you can perform powerful standard and regular expression searches through one or multiple text files, such as source code, log files, and debugging information.

KSS is capable of finding the exact text you are looking for in any ASCII file or files. Sometimes you have only part of the information that you want to match, or you want to find a wider range of information. In such cases, KSS has the powerful capability to search for patterns of text using regular expressions.

The underlying search engine for KSS is the Findstr command. KSS provides a convenient, user friendly, interface for the many command line options. Files with non-printable characters are always skipped and the regular expression functionality of the Findstr command is used for the main text search.

KSS captures and filters the output of the Findstr command, opens the file associated with the selected result list item, and displays it in a syntax highlighting editor. KSS has built-in styling for Clarion, C++, C#, HTML, Java, and XML source files and can easily be extended, through the use of special property files, to provide styling for other languages.


This version of KSS is slightly different from the last commercial release.

Most notably the Results List 'Print' and the 'CheckForUpdate' features have been removed as they required other 3rd party tools, some of which are no longer available.

**KSSOpen readme begins (content above came from Randy)**

Get installers here: https://github.com/mriffey/KSSOpen/releases

**DEPENDENCIES**

Without the list of tools below, you will not be able to build this source. You can remove the references to xFiles and StringTheory. Anyone with a basic level of experience with Clarion will likely be able to do this. Note that Winevent was removed - which means the exe will prevent a reboot / shutdown until code is added to do this without WinEvent. 

Required tools / code:

- This repo
- Clarion 10 or later (installer version is based on Clarion 11) 
- Capesoft xFiles
- Capesoft StringTheory


**OPEN SOURCE RELEASE NOTES AND NEWS**

"KSSO" means "Kwik Source Search - Open Source Hand Code Edition". 

KSSO does not require registration. 

To purchase KSS, see Devuna.com (purchasing may no longer be available). NOTE: I AM NOT DEVUNA. 

**2.0.0  2022-01-02** Initial hand code edition 

