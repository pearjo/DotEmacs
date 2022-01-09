### Makefile to byte-compile and install my Emacs configuration.

# Copyright (C) 2022 Joe Pearson

# This file is not part of GNU Emacs.

# This file is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this file.  If not, see <https://www.gnu.org/licenses/>.

SHELL = /bin/sh

BYTE_COMPILE_FLAGS = -batch -q -f batch-byte-compile

CURL = curl
EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs
HDIUTIL = hdiutil
UNZIP = unzip

DESTDIR = ~/.emacs.d
FONTSDEST = ~/Library/Fonts

lisp = ./lisp
snippets = ./snippets
submodules = ./submodules

ifeq ($(UNAME), Darwin)
fontname = "SF Mono"
fonturl = https://devimages-cdn.apple.com/design/resources/download/SF-Mono.dmg
else
fontname = "Cascadia Code"
fonturl = https://github.com/microsoft/cascadia-code/releases/download/v2111.01/CascadiaCode-2111.01.zip
endif
fontfile = $(notdir $(fonturl))

find_delete = -delete

all: font bytecompile

.PHONY: bytecompile
bytecompile:
	$(EMACS) $(BYTE_COMPILE_FLAGS) $(shell find $(lisp) -name '*.el')

.PHONY: font
font:
ifeq ($(shell fc-list | grep -i $(fontname)),)
	$(CURL) -L $(fonturl) --create-dirs -o ./font/$(fontfile)
else
	$(info Skip downloading $(fontname) since its already installed.)
endif

.PHONY: install
install: all install-fonts install-lisp install-snippets install-submodules

.PHONY: install-font
install-font: font
ifeq ($(shell fc-list | grep -i $(fontname)),)
ifeq ($(suffix $(fontfile)), .dmg))
	hdiutil mount -mountpoint /Volumes/$(basename $(fontfile)) ./font/$(fontfile)
	installer -pkg /Volumes/$(basename $(fontfile))/*.pkg -target LocalSystem
	hdiutil unmount /Volumes/$(basename $(fontfile))
endif
ifeq ($(suffix $(fontfile)), .zip))
	$(UNZIP) ./font/$(fontfile) -d ./font/$(fontname)
	cp -rf ./font/$(fontname) $(FONTSDEST)/
endif
	fc-cache -vf
else
	$(info Skip $(fontname) since its already installed.)
endif

.PHONY: install-lisp
install-lisp: bytecompile
	mkdir -p $(DESTDIR)/lisp
	cp -f init.el $(DESTDIR)
	cp -f $(lisp)/*.el* $(DESTDIR)/lisp/

.PHONY: install-snippets
install-snippets:
	cp -rf $(snippets) $(DESTDIR)/

.PHONY: install-submodules
install-submodules:
	cp -rf $(submodules) $(DESTDIR)/

clean:
	find $(lisp) -name '*.elc' $(find_delete)
	rm -rf ./font
