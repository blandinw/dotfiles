>whoami.tmp wsl whoami
<whoami.tmp set /p whoami=
del whoami.tmp

setx PATH "%PATH%\\wsl$\Ubuntu\home\%whoami%\dotfiles\bin\windows;%ProgramFiles%\Emacs\x86_64\bin;" /m
