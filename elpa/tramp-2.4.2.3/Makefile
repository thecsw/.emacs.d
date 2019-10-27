# Copyright (C) 2019 Free Software Foundation, Inc.

# Author: Michael Albinus <michael.albinus@gmx.de>
# Keywords: comm, processes

# This file is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

EMACS		= emacs -Q -batch -L .
LISP_FILES	= $(wildcard *.el)

.PHONY: all autoloads check info sync test

.SUFFIXES: .el

all: sync autoloads info

autoloads: $(LISP_FILES)
	$(EMACS) -l autoload						    \
	  --eval "(setq generate-autoload-cookie \";;;###tramp-autoload\")" \
	  --eval "(setq generated-autoload-file				    \
		    (expand-file-name \"tramp-loaddefs.el\"))"		    \
	  --eval "(setq make-backup-files nil)"				    \
	  -f batch-update-autoloads .

info:
	$(MAKE) -C texi

check test: autoloads
	$(MAKE) -C test "TRAMP_TEST_ARGS=$(TRAMP_TEST_ARGS)" all

# This target is for the maintainer only.
sync:
	cp -p ~/src/tramp/lisp/tramp-adb.el tramp-adb.el
	cp -p ~/src/tramp/lisp/tramp-archive.el tramp-archive.el
	cp -p ~/src/tramp/lisp/tramp-cache.el tramp-cache.el
	cp -p ~/src/tramp/lisp/tramp-cmds.el tramp-cmds.el
	cp -p ~/src/tramp/lisp/tramp-compat.el tramp-compat.el
	cp -p ~/src/tramp/lisp/tramp-ftp.el tramp-ftp.el
	cp -p ~/src/tramp/lisp/tramp-gvfs.el tramp-gvfs.el
	cp -p ~/src/tramp/lisp/tramp-integration.el tramp-integration.el
	cp -p ~/src/tramp/lisp/tramp-rclone.el tramp-rclone.el
	cp -p ~/src/tramp/lisp/tramp-sh.el tramp-sh.el
	cp -p ~/src/tramp/lisp/tramp-smb.el tramp-smb.el
	cp -p ~/src/tramp/lisp/tramp-sudoedit.el tramp-sudoedit.el
	cp -p ~/src/tramp/lisp/tramp-uu.el tramp-uu.el
	cp -p ~/src/tramp/lisp/tramp.el tramp.el
	cp -p ~/src/tramp/lisp/trampver.el trampver.el
	$(MAKE) -C texi sync
	$(MAKE) -C test sync
