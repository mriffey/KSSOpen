-- Default Redirection for Clarion 10.0

[Copy]
-- Directories only used when copying dlls
*.dll = %BIN%;%BIN%\AddIns\BackendBindings\ClarionBinding\Common;%ROOT%\Accessory\bin;%libpath%\bin\%configuration%

[Debug]
-- Directories only used when building with Debug configuration

*.clw = clw
*.inc = clw
*.shp = clw
*.obj = obj
*.res = obj
*.rsc = obj
*.lib = obj=
*.FileList.xml = .;
*.map = map
*.shp = clw

[Release]
-- Directories only used when building with Release configuration

*.obj = obj
*.clw = clw
*.inc = clw
*.res = obj
*.rsc = obj
*.lib = obj
*.FileList.xml = .;
*.map = map
*.shp = clw

[Common]
*.ico   = .\assets; .\images; %ROOT%\Accessory\images; %ROOT%\Accessory\resources; %ROOT%\Accessory\libsrc\win; %ROOT%\Accessory\template\win
*.gif   = .\assets; .\images; %ROOT%\Accessory\images; %ROOT%\Accessory\resources; %ROOT%\Accessory\libsrc\win; %ROOT%\Accessory\template\win
*.png   = .\assets; .\images; %ROOT%\Accessory\images; %ROOT%\Accessory\resources; %ROOT%\Accessory\libsrc\win; %ROOT%\Accessory\template\win
*.jpg   = .\assets; .\images; %ROOT%\Accessory\images; %ROOT%\Accessory\resources; %ROOT%\Accessory\libsrc\win; %ROOT%\Accessory\template\win
*.wmf   = .\assets; .\images; %ROOT%\Accessory\images; %ROOT%\Accessory\resources; %ROOT%\Accessory\libsrc\win; %ROOT%\Accessory\template\win
*.bmp   = .\assets; .\images; %ROOT%\Accessory\images; %ROOT%\Accessory\resources; %ROOT%\Accessory\libsrc\win; %ROOT%\Accessory\template\win
*.cur   = .\assets; .\images; %ROOT%\Accessory\images; %ROOT%\Accessory\resources; %ROOT%\Accessory\libsrc\win; %ROOT%\Accessory\template\win
*.chm = %BIN%;%ROOT%\Accessory\bin
*.tp? = %ROOT%\template\win
*.trf = %ROOT%\template\win
*.txs = %ROOT%\template\win
*.stt = %ROOT%\template\win
*.*   = .; %ROOT%\libsrc\win; %ROOT%\images; %ROOT%\template\win ;c:\projects\KSSOpen\libsrc\win 
*.lib = %ROOT%\lib; c:\projects\KSSOpen\lib
*.obj = %ROOT%\lib
*.res = %ROOT%\lib
*.hlp = %BIN%;%ROOT%\Accessory\bin
*.dll = %BIN%;%ROOT%\Accessory\bin
*.exe = %BIN%;%ROOT%\Accessory\bin
*.tp? = %ROOT%\Accessory\template\win
*.txs = %ROOT%\Accessory\template\win
*.stt = %ROOT%\Accessory\template\win
*.lib = %ROOT%\Accessory\lib
*.obj = %ROOT%\Accessory\lib
*.res = %ROOT%\Accessory\lib
*.dll = %ROOT%\Accessory\bin
*.*   = %ROOT%\Accessory\images; %ROOT%\Accessory\resources; %ROOT%\Accessory\libsrc\win; %ROOT%\Accessory\template\win
