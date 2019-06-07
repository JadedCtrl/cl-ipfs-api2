===============================================================================
CL-IPFS-API²                                      Binder for galactic transguys
===============================================================================

:cl-ipfs-api² is a pretty simple set of IPFS bindings for Common Lisp, using
the HTTP API. It uses Dexador for HTTP(S) requests and YASON for JSON.


————————————————————————————————————————
USAGE
————————————————————————————————————————
Just use `quicklisp` (pop this in your "~/quicklisp/local-projects/",
and you're good).

Then you can do things like:
	> (ipfs:add #p"~/.bashrc")
	"QmZweanA1JRNio6DKnPN6yECWCrxmWqqG7WWUNtyuji9hZ"
	145
	".bashrc"

	> (ipfs:cat "/ipns/ipfs.io/index.html")
	"<!DOCTYPE html>
	……"

Most commands available are one-to-one with their API/cli counter-parts,
with a few notable exceptions:
	* #'dl	(counter-part to the /get call)

The calls implemented so far:
	* root calls (cat, add, id, ls, resolve, etc)
	* cid calls
	* config calls (config, config/show)
	* version calls (version, version/deps)

Functions return either strings, lists, or hash-tables (parsed JSON)--
depending on context, of course. Read docstrings ♥


————————————————————————————————————————
BORING STUFF
————————————————————————————————————————
License is the GNU GPLv3:
       check COPYING.txt (/ipfs/QmTBpqbvJLZaq3hTMUhxX5hyJaSCeWe6Q5FRctQbsD6EsE)
Author is Jaidyn Ann <jadedctrl@teknik.io>
Sauce is at https://git.eunichx.us/cl-ipfs-api2.git
