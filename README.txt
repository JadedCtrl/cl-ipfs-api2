===============================================================================
CL-IPFS-API²                                                                   
===============================================================================

:cl-ipfs-api² is a pretty simple set of IPFS bindings for Common Lisp, using
the HTTP API for (almost) everything, except for pubsub (which uses the locally
installed go-ipfs program).
It uses Drakma, YASON, and UIOP.


————————————————————————————————————————
USAGE
————————————————————————————————————————
:cl-ipfs-api² is available on Quicklisp, so just run:
        > (ql:quickload :cl-ipfs-api2)

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
	* refs, repo, stats
	* swarm, urlstore
	* version (version, version/deps)

Some calls were skipped over, but wouldn't be hard to add:
	* object/put, object/set-data object/patch/append-data
	* tar calls were deliberately ignored (useless)

Functions return either strings, lists, or associative lists, depending on
context. All errors return two values— nil and an error message (string).

Make sure to read docstrings for specific information, and keeping the API
reference handy is a good idea (/ipns/docs.ipfs.io/reference/api/http/).

————————————————————
USEFUL VARIABLES
————————————————————
There are three exported variables:
	ipfs:*api-host*    →   "http://127.0.0.1:5001"
	ipfs:*api-root*    →   "/api/v0/"
	ipfs:*ipfs-root*   →   NIL

*api-host* is the protocol, host, and port of the API server— unless you're
using a custom port or remote server, this probably won't need to change.
*api-root* is the URL root of all API calls on the server— only changes under
very strange circumstances.
*ipfs-root* is the “root” of the local IPFS daemon. This is only used with
the pubsub commands, since they actually invoke the local `ipfs` program.
You only need to change this variable if your $IPFS_PATH is irregular, like
"/var/ipfs/" or something weird like that.

————————————————————
PUBSUB USAGE
————————————————————
Pubsub usage here is such an abberation that it warrants its own section.
Since there isn't a (functional) HTTP API for pubsub yet, we're using the
actual go-ipfs program from your computer.

If you don't have go-ipfs locally installed, it won't work.
If you are using Windows, or anything but *nix, it probably won't work.
If you haven't enabled pubsub (--enable-pubsub-experiment argument to daemon),
it won't work.

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
License is the GNU LGPLv3:
       check COPYING.txt (/ipfs/QmR8Rnk5QdXgrXRqmgMLmG5PuHZEjujfa3rfVhPV99TLY7)
Author is Jaidyn Ann <jadedctrl@posteo.at>
Sauce is at https://hak.xwx.moe/jadedctrl/cl-ipfs-api2
