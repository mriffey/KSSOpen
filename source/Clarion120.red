-- KSSOpen redirection for Clarion 12.0
-- Derived from the Clarion 12 global CLARION120.RED. The only additions are the
-- repo-relative search paths at the top of [Common] (.\images, ..\libsrc\win, ..\lib).
-- Clarion100.RED is left in place for Clarion 10/11 builds.

[Copy]
-- Directories only used when copying dlls
*.dll = %BIN%;%BIN%\AddIns\BackendBindings\ClarionBinding\Common;%ROOT%\Accessory\bin

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
-- KSSOpen repo paths
*.*   = .; .\images; ..\libsrc\win
*.lib = ..\lib
-- Clarion 12 defaults
*.chm = %BIN%;%ROOT%\Accessory\bin
*.tp? = %ROOT%\template\win
*.trf = %ROOT%\template\win
*.txs = %ROOT%\template\win
*.stt = %ROOT%\template\win
*.*   = .; %ROOT%\libsrc\win; %ROOT%\images; %ROOT%\template\win; %ROOT%\convsrc
*.lib = %ROOT%\lib
*.obj = %ROOT%\lib
*.res = %ROOT%\lib
*.dll = %BIN%
*.tp? = %ROOT%\Accessory\template\win
*.trf = %ROOT%\Accessory\template\win
*.txs = %ROOT%\Accessory\template\win
*.stt = %ROOT%\Accessory\template\win
*.*   = %ROOT%\Accessory\libsrc\win; %ROOT%\Accessory\images; %ROOT%\Accessory\resources; %ROOT%\Accessory\template\win
*.lib = %ROOT%\Accessory\lib
*.obj = %ROOT%\Accessory\lib
*.res = %ROOT%\Accessory\lib
*.dll = %ROOT%\Accessory\bin
