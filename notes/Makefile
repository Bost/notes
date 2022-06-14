PAPER=notes
PKG=notes

# plain text in a single file "$(PAPER)txt", with non-ASCII content encoded as UTF-8
text:
	scribble --text $(PAPER).scrbl && rm $(PAPER).txt

all: pkg setup text

pkg:
	raco pkg install --skip-installed

setup:
	raco setup ${PKG}

test:
	raco test -c ${PKG}

compile:
	raco make ${PAPER}.scrbl

clean:
	rm -r compiled

