-- KSSOpen redirection for Clarion 12
-- Output folders and the repo's own search paths are listed here; every other
-- Clarion 12 default comes from the global file through {include}. The include
-- only brings in search paths, which is why [Debug]/[Release] are spelled out.
-- Clarion100.RED is left in place for Clarion 10/11 builds.
-- Keep CRLF line endings: the Clarion redirection parser does not read LF-only files.

[Debug]
*.obj = obj\debug
*.res = obj\debug
*.rsc = obj\debug
*.lib = obj\debug
*.FileList.xml = obj\debug
*.map = map\debug

[Release]
*.obj = obj\release
*.res = obj\release
*.rsc = obj\release
*.lib = obj\release
*.FileList.xml = obj\release
*.map = map\release

[Common]
*.*   = .; .\images; ..\libsrc\win
*.lib = ..\lib

{include %REDDIR%\%REDNAME%}
