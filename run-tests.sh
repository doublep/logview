#! /bin/sh

# Usage: ./run-tests.sh [ERT-SELECTOR]
#
# You can also set EMACS and ERT_SELECTOR variables in the
# environment.  If ERT_SELECTOR is empty (both on command line and in
# environment), it defaults to t (i.e., everything).

# If `local-environment.el' exists, it is loaded before `logview.el'.
# Can be used e.g. to make package dependencies loadable.  By the time
# `local-environment.el' is loaded, Emacs packaging system is already
# initialized.

set -e

OWN_DIRECTORY=$(dirname $0)

if [ -z "$EMACS" ]; then
    EMACS=emacs
fi

if [ -n "$1" ]; then
    ERT_SELECTOR=$1
fi

if [ -z "$ERT_SELECTOR" ]; then
    ERT_SELECTOR=t
fi

$EMACS --batch                                                                                                               \
       --eval "(message \"Using Emacs %s\" (emacs-version))"                                                                 \
       --eval "(progn (require 'package) (package-initialize))"                                                              \
       --directory "$OWN_DIRECTORY"                                                                                          \
       --eval "(when (locate-file \"local-environment.el\" (list (car load-path))) (load \"local-environment.el\" nil t t))" \
       --load logview.el                                                                                                     \
       --load test/logview.el                                                                                                \
       --eval "(ert-run-tests-batch-and-exit (quote ${ERT_SELECTOR}))"

# Fuck Emacs 24, I'm tired of fighting warnings on it.
$EMACS --batch                                                                                                               \
       --eval "(progn (require 'package) (package-initialize))"                                                              \
       --directory "$OWN_DIRECTORY"                                                                                          \
       --eval "(when (locate-file \"local-environment.el\" (list (car load-path))) (load \"local-environment.el\" nil t t))" \
       --eval "(when (>= emacs-major-version 25) (setq byte-compile-error-on-warn t))"                                       \
       --eval "(batch-byte-compile)" logview.el
