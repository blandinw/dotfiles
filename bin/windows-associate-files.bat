>emacsclient.tmp where emacsclientw
<emacsclient.tmp set /p emacsclient=
del emacsclient.tmp

>runemacs.tmp where runemacs
<runemacs.tmp set /p runemacs=
del runemacs.tmp

ftype txtfile=%emacsclient% -na "%runemacs%" "%%1"
ftype EmacsLisp=%emacsclient% -na "%runemacs%" "%1"
ftype CodeFile=%emacsclient% -na "%runemacs%" "%%1"
assoc .txt=txtfile
assoc .text=txtfile
assoc .log=txtfile
assoc .org=txtfile
assoc .el=EmacsLisp
assoc .c=CodeFile
assoc .h=CodeFile
assoc .cpp=CodeFile
assoc .hpp=CodeFile
assoc .xaml=CodeFile
assoc .xml=CodeFile
assoc .json=CodeFile
assoc .bat=CodeFile
