>whoami.tmp wsl whoami
<whoami.tmp set /p whoami=
del whoami.tmp

cd %AppData%
mklink .spacemacs \\wsl$\Ubuntu\home\%whoami%\dotfiles\emacs.d\spacemacs
