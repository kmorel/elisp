#!/bin/sh
#
# This is a really simple shell program that un-archives and "installs"
# some list packages that are required.  It also attempts to set up
# your $HOME/.emacs directory.
#

RUNEMACS=${RUNEMACS:-emacs}

COMPILEDIRS=" \
    bin
"
    # bin/elib-1.0 \
    # bin/speedbar-0.11 \
    # bin/semantic-1.2.1 \
    # bin/jde-2.2.4/lisp

LOADPATH=" \
    features \
    options \
    $COMPILEDIRS
"

cwd=`pwd`
if cygpath -w "$cwd" > /dev/null 2>&1 ; then
    cwd=`cygpath -w "$cwd" | sed 's/\\\\/\\\\\\\\/g'`
fi

echo "Generating elisp-dirs.el..."
rm -f elisp-dirs.el
echo ";;; Computer generated by install.sh.  Do not edit." > elisp-dirs.el
echo "(defvar elispdir \"$cwd\")" >> elisp-dirs.el
echo "(setq load-path" >> elisp-dirs.el
echo "      (append (list" >> elisp-dirs.el
for dir in $LOADPATH ; do
    echo "               \"$cwd/$dir\"" >> elisp-dirs.el
done
echo "               )" >> elisp-dirs.el
echo "              load-path))" >> elisp-dirs.el

# cd bin
# echo "Extracting Emacs Lisp library package..."
# gzip -dc elib.tar.gz | tar xf -
# echo "Extracting Speedbar..."
# gzip -dc speedbar.tar.gz | tar xf -
# echo "Extracting Semantic Bovinator..."
# gzip -dc semantic.tar.gz | tar xf -
# echo "Extracting Java Development Environment..."
# gzip -dc jde.tar.gz | tar xf -
# cd ..

echo "Compiling files..."
for dir in $COMPILEDIRS ; do
    echo "    $dir"
    $RUNEMACS -batch -l elisp-dirs.el -f batch-byte-compile $dir/*.el
done

echo "Setting up .emacs file."

startup_file=$HOME/.emacs

case `uname` in
    Darwin)
	config_file=$cwd/init-mac.el
	;;
    *)
	config_file=$cwd/init.el
	;;
esac

if [ -f $startup_file ] ; then
    if fgrep "$config_file" $startup_file > /dev/null ; then
	:
    else
	mv $startup_file $startup_file.old
	echo "(setq elispdir \"$cwd\")" > $startup_file
	echo "(load \"$config_file\" nil t)" >> $startup_file
	echo >> $startup_file
	cat $startup_file.old >> $startup_file

	echo "#######################################################"
	echo "Added the following to $startup_file:"
	echo
	echo "    (setq elispdir \"$cwd\")"
	echo "    (load \"$config_file\" nil t)"
	echo
	echo "Consider checking the rest of this file to ensure that"
	echo "the rest of the file is necessary or should not be moved"
	echo "the tracked configuration."
	echo
    fi
else
    echo "(setq elispdir \"$cwd\")" > $startup_file
    echo "(load \"$config_file\" nil t)" >> $startup_file

    echo "#######################################################"
    echo "Created $startup_file"
    echo
fi

echo "Done."
