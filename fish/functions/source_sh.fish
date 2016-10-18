function simple_translate
  perl -e '
  my @stack;
  my $func = 0;
  my $ignore = 0;

  while(<>) {
    if (/# fish_ignore start/) { $ignore = 1; }
    if ($ignore) { next; }
    s/&&/ ; and /g;
    s/\|\|/ ; or /g;
    s/unset\s/set -e /g;
    if (s/^(\s*)(function )?\s*(\S+)\s*\(\)\s*{/\1function \3/g) {
      push @stack, \'func\';
      $func = $func + 1;
    }
    if (s/(if.*)\s*;\s*then(\s*)/\1\2/g) {
      push @stack, \'if\'
    }
    if (
      s/^(\s*)}/\1end/ ||
      s/^(\s*)fi/\1end/
    ) {
      $x = pop @stack || die(\'stack was empty\');
      if ($x eq \'func\') {
        $func = $func - 1;
      }
    }
    if ($func) { s/\$(\d)/\$argv[\1]/g; }
    if (!$ignore) { print; }
    if (/# fish_ignore end/) { $ignore = 0 }
  }
' < $argv[1] > $argv[2]
end

function test_simple_translate
  set -l filename_in /tmp/source_sh___test___input
  set -l filename_out /tmp/source_sh___test___output
  set -l filename_expected /tmp/source_sh___test___expected

  echo '
  true && ls
  false || ls
  function myfunc () {
    if true; then
      unset FOO
      echo $2
    fi
    source $1
  }
  ' > $filename_in

  echo '
  true  ; and  ls
  false  ; or  ls
  function myfunc
    if true
      set -e FOO
      echo $argv[2]
    end
    source $argv[1]
  end
  ' > $filename_expected

  simple_translate $filename_in $filename_out
  diff -u $filename_expected $filename_out

  if [ $status -ne 0 ]
    set_color red
    echo FAIL
  else
    set_color green
    echo OK
  end
end

function source_sh
  set -l tmp "/tmp"
  set -l filename "$tmp/source_sh_"(echo $argv[1] | tr '/' '_')

  if true or [ ! -e $filename ]
    echo Generating $filename...
    simple_translate $argv[1] $filename
  end

  source $filename
  return 0
end
