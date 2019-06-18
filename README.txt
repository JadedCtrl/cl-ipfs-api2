===============================================================================
CL-IPFS-API²                                   For space-orientied lisp weenies
===============================================================================

:cl-ipfs-api² is a pretty simple set of IPFS bindings for Common Lisp, using
the HTTP API for (almost) everything, except for pubsub (which uses the locally
installed go-ipfs program).
It uses Dexador, YASON, and UIOP.


————————————————————————————————————————
USAGE
————————————————————————————————————————
Just use `quicklisp` (pop this in your "~/quicklisp/local-projects/",
and you're good).

Then you can do things like:
	> (ipfs:add #p"~/.bashrc")
	"QmZweanA1JRNio6DKnPN6yECWCrxmWqqG7WWUNtyuji9hZ"

	> (ipfs:cat "/ipns/ipfs.io/index.html")
	"<!DOCTYPE html>
	……"

Most commands available are one-to-one with their API/cli counter-parts,
with a few notable exceptions:
	* #'dl	(counter-part to the /get call. the name is different, so
	         as to not conflict with #'common-lisp:get)

The calls implemented so far:
	* root (cat, add, id, ls, resolve, etc)
	* bitswap, block, bootstrap
	* cid, config (config, config/show)
	* dag, dht, diag
	* file, files, filestore
	* key, name, object
	* p2p, pin, pubsub
	* version (version, version/deps)

Some calls were skipped over, but wouldn't be hard to add:
	* object/put, object/set-data object/patch/append-data

Functions return either strings, lists, or associative lists, depending on
context. All errors return two values— nil and an error message (string).
Make sure to read docstrings ☆


————————————————————————————————————————
PUBSUB USAGE
————————————————————————————————————————
Pubsub usage here is such an abberation that it warrants its own section.
Since there isn't a (functional) HTTP API for pubsub yet, we're using the
actual go-ipfs program from your computer.

If you don't have go-ipfs locally installed, it won't work.
If you are using Windows, or anything but *nix, it probably won't work.
If you haven't enabled pubsub (--enable-pubsub-experiment argument to daemon),
it won't work.

Well… here we go.

You can sub to a topic with, ofc, #'pubsub-sub, which will return a
UIOP-originated process-info stream— while the `ipfs pubsub sub` command runs
in the background.

This stream can't be directly #'read-char or #'listen with, which is exactly
what you wanna do— instead, running #'uiop/launch-program:process-info-output
on it is necessary to expose a usable stream.

To make all that easier, there's a little abstraction I added which obfuscates
UIOP use and is adequate shorthand:
	* #'pubsub-sub-read-char
	* #'pubsub-sub-listen
	* #'pubsub-sub-process
	* #'pubsub-sub-close

All of those operate on the original UIOP-originated process-info stream, and
work exactly like you'd expect.
The only weird, non-obvious one is probably #'pubsub-sub-process, which applies
#'uiop/launch-program:process-info-output— just in case you need the raw,
usable stream.

Anyway, with this, you can get a continuous read on what's going on with the
topic you're subbed to. To publish to a topic, run #'pubsub-pub with the topic
and data as arguments. Pretty simple.

Both #'pubsub-sub and #'pubsub-pub, being the only functions that run a shell
command, include an :env argument. If you supply a string as the :env argument,
that string will prefix the "ipfs" command— basically only useful for changing
something with the "env" command (like $IPFS_PATH).

Also, if you change the ipfs:*ipfs-root* variable (to the correct value of
$IPFS_PATH), the :env arguments (unless otherwise specified) will default to
"env IPFS_PATH=" + ipfs:*ipfs-root* + " > /dev/null;"


————————————————————————————————————————
BORING STUFF
————————————————————————————————————————
License is the GNU GPLv3:
       check COPYING.txt (/ipfs/QmTBpqbvJLZaq3hTMUhxX5hyJaSCeWe6Q5FRctQbsD6EsE)
Author is Jaidyn Ann <jadedctrl@teknik.io>
Sauce is at https://git.eunichx.us/cl-ipfs-api2.git
