(in-package :cl-ipfs-api2)

(defparameter *api-host* "http://127.0.0.1:5001")
(defparameter *api-root* "/api/v0/")

;; —————————————————————————————————————
;; BASE

;; STRING LIST [:LIST :BOOLEAN :SYMBOL] → STRING | HASHTABLE | (NIL STRING)
(defun ipfs-call (call arguments &key (parameters nil) (want-stream nil)
			   (method :GET))
  "Make an IPFS HTTP API call. Quite commonly used.
   Some calls return strings/raw data, and others return JSON.
   When strings/arbitrary data are recieved, they're returned verbatim.
   But, when JSON is returned, it is parsed into a hashtable.
   If the JSON is 'error JSON', I.E., it signals that an error has been
   recieved, two values are returned: NIL and the string-error-message."
  (let ((result
	  (drakma:http-request
	    (make-call-url *api-host* *api-root* call arguments)
	    :method method
	    :parameters parameters
	    :want-stream want-stream)))

    (cond (want-stream result)
	  ((stringp result) result)
	  ((vectorp result)
	   (let ((json (yason:parse (flexi-streams:octets-to-string result))))
	     (cond ((equal "error" (ignore-errors (gethash "Type" json)))
		    (values nil (gethash "Message" json)))
		   ('T json)))))))


;; STRING STRING LIST → STRING
(defun make-call-url (host root call arguments)
  "Create the URL of an API call, as per the given arguments.
  Symbols are assumed to be something like 'T (so boolean), nil likewise.
  Arguments should look like this:
    (('recursive' nil)('name' 'xabbu'))"
  (let ((call-url (string+ host root call))
	(first-arg 'T))
    (mapcar (lambda (arg-pair)
	      (when arg-pair
		(setq call-url
		      (string+
			call-url
			(if first-arg "?" "&")
			(first arg-pair) "="
			(cond ((not (second arg-pair))
			       "false")
			      ((symbolp (second arg-pair))
			       "true")
			      ('T (second arg-pair)))))
		(setq first-arg nil)))
	    arguments)
    call-url))


;; FORM FORM → FORM
(defmacro bind-api-result (call form)
  "Wrap around an #'ipfs-call form; if #'call returns an error, then return NIL
  and the error message-- (NIL STRING)-- otherwise, execute #'form.
  Binds the result of the API call to… you guessed it, the variable 'result'.
  The error message is assigned to 'message', if such a thing exists."
  `(multiple-value-bind (result message)
     ,call
     (if message
       (values nil message)
       ,form)))

;; FORM FORM → FORM
(defmacro bind-api-alist (call)
  "Basically #'bind-api-result, but it assumes the final form is a hash-table,
   and maps it to an associative list."
  `(bind-api-result ,call  (ignore-errors (re-hash-table-alist result))))



;; —————————————————————————————————————
;; ROOT CALLS

;; PATHNAME → (HASH-STRING SIZE-NUMBER) || (NIL STRING)
(defun add (pathname &key (pin 't) (only-hash nil))
  "Add a file to IPFS, return it's hash.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-add"
  (bind-api-result
    (ipfs-call "add" `(("pin" ,pin) ("only-hash" ,only-hash))
	       :method :post :parameters `(("file" . ,pathname)))
    result))

;; STRING :NUMBER :NUMBER → STRING || (NIL STRING)
(defun cat (ipfs-path &key (offset nil) (length nil))
  "Return a string of the data at the given IPFS path.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-cat"
  (bind-api-result
    (ipfs-call "cat"
	       `(("arg" ,ipfs-path)
		 ,(if offset `("offset" ,offset))
		 ,(if length `("length" ,length))))
    result))

;; STRING [:BOOLEAN :BOOLEAN] → ALIST || (NIL STRING)
(defun ls (ipfs-path &key (resolve-type 't) (size 't))
  "Returns all sub-objects (IPFS hashes) under a given IPFS/IPNS directory
  path. Returns as an associative list.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-ls"
  (bind-api-result
    (ipfs-call "ls" `(("arg" ,ipfs-path)
		      ("resolve-type" ,resolve-type) ("size" ,size)))
    (cdr (caadar (re-hash-table-alist result)))))

;; STRING PATHNAME → NIL
(defun dl (ipfs-path out-file)
  "Write an IPFS file directly to a file on the local file-system.
  Non-recursive, in the case of directories… for now.
  (Thanks to this thread ♥: https://stackoverflow.com/a/12607423)
  Is a general replacement for the 'get' API call, but actually just uses
  the 'cat' call, due to some issues with using 'get'.
  Will not actually return NIL when an error is reached (like other functions)
  with an error-message, it'lll just write the error JSON to the file.
  Whoops."
  (with-open-file (out-stream out-file :direction :output
			      :element-type '(unsigned-byte 8)
			      :if-exists :overwrite :if-does-not-exist :create)
    (let ((in-stream
	    (ipfs-call "cat" `(("arg" ,ipfs-path)) :want-stream 'T)))

      (awhile (read-byte in-stream nil nil)
	      (write-byte it out-stream))
      (close in-stream))))

;; ——————————————————

;; [STRING] → ALIST
(defun id (&optional peer-id)
  "Return info on a node by ID. Returns as an associative list, the public key,
  agent version, etc. If no node ID is specified, then your own is assumed.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-id"
  (bind-api-alist
    (ipfs-call "id" `(,(if peer-id (list "arg" peer-id))))))

;; ——————————————————

;; STRING → (STRING || (NIL STRING)
(defun dns (domain &key (recursive 't))
  "Resolve a domain into a path (usually /ipfs/).
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dns"
  (bind-api-result
    (ipfs-call "dns" `(("arg" ,domain) ("recursive" ,recursive)))
    (gethash "Path" result)))

;; STRING [:BOOLEAN :NUMBER :NUMBER] → STRING || (NIL STRING)
(defun resolve (ipfs-path &key (recursive 't) (dht-record-count nil)
			  (dht-timeout 30))
  "Resolve a given name to an IPFS path."
  (bind-api-result
    (ipfs-call "resolve" `(("arg" ,ipfs-path) ("recursive" ,recursive)
                           ,(if dht-record-count
			      (list "dht-record-count" dht-record-count))
			   ("dht-timeout" ,(string+ dht-timeout "s"))))
    (gethash "Path" result)))
					      
;; ——————————————————

;; NIL → NIL
(defun shutdown ()
  "Shut down the connected IPFS node.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-shutdown"
  (ipfs-call "shutdown" '()))



;; —————————————————————————————————————
;; BITSWAP CALLS

;; STRING → ALIST || (NIL STRING)
(defun bitswap-ledger (peer-id)
  "Show the current ledger for a peer.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bitswap-ledger"
  (bind-api-alist
    (ipfs-call "bitswap/ledger" `(("arg" ,peer-id)))))

;; NIL → NIL
(defun bitswap-reprovide ()
  "Trigger the reprovider.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bitswap-reprovide"
  (ipfs-call "bitswap/reprovide" '()))

;; NIL → ALIST || (NIL STRING)
(defun bitswap-stat ()
  "Show diagnostic info on the bitswap agent.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bitswap-stat"
  (bind-api-alist
    (ipfs-call "bitswap/stat" '())))

;; STRING → ALIST || (NIL STRING)
(defun bitswap-wantlist (&optional peer-id)
  "Show blocks currently on the wantlist.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bitswap-wantlist"
  (bind-api-alist
    (ipfs-call "bitswap/wantlist" `(,(if peer-id (list "peer" peer-id))))))



;; —————————————————————————————————————
;; BLOCK CALLS

;; STRING → STRING || (NIL STRING)
(defun block-get (hash)
  "Get a raw IPFS block.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-block-get"
  (bind-api-result
    (ipfs-call "block/get" `(("arg" ,hash)))
    result))

;; PATHNAME [:STRING :STRING :NUMBER :BOOLEAN] → ALIST || (NIL STRING)
(defun block-put (pathname &key (format nil) (mhtype "sha2-256") (mhlen -1)
		       (pin nil))
  "Store input as an IPFS block.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-block-put"
  (bind-api-alist
    (ipfs-call "block/put" `(,(if format (list "format" format))
			     ("mhtype" ,mhtype)
			     ("mhlen" ,mhlen)
			     ("pin" ,pin))
	       :method :POST :parameters `(("data" . ,pathname)))))

;; STRING → NIL
(defun block-rm (hash &key (force nil))
  "Delete an IPFS block(s).
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-block-rm"
  (bind-api-result
    (ipfs-call "block/rm" `(("arg" ,hash) ,(if force (list "force" force))))
    nil))

;; STRING → ALIST || (NIL STRING)
(defun block-stat (hash)
  "Print info about a raw IPFS block
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-block-stat"
  (bind-api-alist
    (ipfs-call "block/stat" `(("arg" ,hash)))))



;; —————————————————————————————————————
;; BOOTSTRAP CALLS

;; NIL → LIST || (NIL STRING)
(defun bootstrap ()
  "Return a list of bootstrap peers
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap"
  (bind-api-result
    (ipfs-call "bootstrap" '())
    (gethash "Peers" result)))

;; NIL → LIST || (NIL STRING)
(defun bootstrap-list ()
  "Return a list of bootstrap peers
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-list"
  (bootstrap))

;; STRING → LIST || (NIL STRING)
(defun bootstrap-add (peer)
  "Add a peer to the bootstrap list
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-add"
  (bind-api-result
    (ipfs-call "bootstrap/add" `(("arg" ,peer)))
    (gethash "Peers" result)))

;; NIL → LIST || (NIL STRING)
(defun bootstrap-add-default ()
  "Add default peers to the bootstrap list
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-add-default"
  (bind-api-result
    (ipfs-call "bootstrap/add/default" '())
    (gethash "Peers" result)))

;; STRING → LIST || (NIL STRING)
(defun bootstrap-rm (peer)
  "Remove a peer from the bootstrap list
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-rm"
  (bind-api-result
    (ipfs-call "bootstrap/rm" `(("arg" ,peer)))
    (gethash "Peers" result)))

;; NIL → LIST || (NIL STRING)
(defun bootstrap-rm-all (peer)
  "Remove a peer from the bootstrap list
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bootstrap-rm"
  (bind-api-result
    (ipfs-call "bootstrap/rm/all" '())
    (gethash "Peers" result)))



;; —————————————————————————————————————
;; CID CALLS

;; STRING → STRING || (NIL STRING)
(defun cid-base32 (cid)
  "Convert a CID into Base32 CIDv1
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-cid-base32"
  (bind-api-result
    (ipfs-call "cid/base32" `(("arg" ,cid)))
    (if (zerop (length (gethash "ErrorMsg" result)))
      (gethash "Formatted" result)
      (values nil (gethash "ErrorMsg" result)))))

;; NIL → ALIST || (NIL STRING)
(defun cid-bases ()
  "Return a associative list of available bases in plist format; each base's
  name is a assigned a given code-number.
    ((CODE-A . NAME-A) (CODE-B . NAME-B) … (CODE-N . NAME-N))
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-cid-bases"
  (bind-api-result
    (ipfs-call "cid/bases" '())
    (mapcan (lambda (base)
	      `((,(gethash "Code" base) . ,(gethash "Name" base))))
	    result)))



;; —————————————————————————————————————
;; CONFIG CALLS

;; STRING [:STRING :BOOLEAN :BOOLEAN] → STRING || (NIL STRING)
(defun config (key &key (value nil) (bool nil) (json nil))
  "Get/set a config key's value.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-config"
  (bind-api-result
    (ipfs-call "config" `(("arg" ,key) ,(if value (list "value" value))
				       ("bool" ,bool) ("json" ,json)))
    (gethash "Value" result)))

;; NIL → ALIST
(defun config-show ()
  "Return the config file's contents, in alist-format… y'know, with several
  sub-alists.
  Doesn't quite line up with #api-v0-config-show
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-config-show"
  (bind-api-alist
    (ipfs-call "config/show" '())))

;; STRING → STRING || (NIL STRING)
(defun config-get (key)
  "Get a config key's value.
  Doesn't map with any existant API call; it's just a convenience wrapper
  around #'config."
  (config key))

;; STRING → STRING || (NIL STRING)
(defun config-set (key value &key (bool nil) (json nil))
  "Set a config key's value.
  Doesn't map with any existant API call; it's just a convenience wrapper
  around #'config."
  (config key :value value :bool bool :json json))



;; —————————————————————————————————————
;; DAG CALLS

;; STRING → STRING || (NIL STRING)
(defun dag-get (dag-node)
  "Get a dag node from IPFS.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dag-get"
  (bind-api-result
    (ipfs-call "dag/get" `(("arg" ,dag-node)))
    result))

;; STRING [:STRING :STRING :BOOLEAN] → STRING || (NIL STRING
(defun dag-put (dag-node &key (format "cbor") (input-enc "json") (pin 'T))
  "Add a dag node to IPFS. Returns CID string.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dag-put"
  (bind-api-result
    (ipfs-call "dag/put" `(("arg" ,dag-node) ("format" ,format)
                           ("input-enc" ,input-enc) ("pin" ,pin)))
    (gethash "/" (gethash "Cid" result))))

;; STRING → ALIST || (NIL STRING)
(defun dag-resolve (path)
  "Resolve an IPLD block.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dag-resolve"
  (bind-api-alist
    (ipfs-call "dag/resolve" `(("arg" ,path)))))



;; —————————————————————————————————————
;; DHT CALLS

;; STRING → LIST || (NIL STRING)
(defun dht-findpeer (peer-id)
  "Find the multiaddresses associated with a peer ID.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dht-findpeer"
  (bind-api-result
    (ipfs-call "dht/findpeer" `(("arg" ,peer-id)))
    (gethash "Addrs" (car (gethash "Responses"result)))))

;; STRING [:NUMBER] → LIST || (NIL STRING)
(defun dht-findprovs (key &key (provider-quantity 20))
  "Find peers that can provide a specific value, given a key.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dht-findprovs"
  (bind-api-result
    (ipfs-call "dht/findprovs"
	       `(("arg" ,key)("num-providers" ,provider-quantity)))
    (gethash "Addrs" (car (gethash "Responses"result)))))

;; STRING → LIST || (NIL STRING)
(defun dht-get (key)
  "Query the routing system for a key's best value.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dht-get"
  (bind-api-result
    (ipfs-call "dht/get" `(("arg" ,key)))
    (gethash "Addrs" (car (gethash "Responses"result)))))

;; STRING [:BOOLEAN] → NIL
(defun dht-provide (key &key (recursive nil))
  "Announce to the network that you're providing the given values.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dht-provide"
  (bind-api-result
    (ipfs-call "dht/provide" `(("arg" ,key)("recursive" ,recursive)))
    result))

;; STRING STRING → NIL || (NIL STRING)
(defun dht-put (key value)
  "Write a key-value pair to the routing system.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dht-put"
  (bind-api-result (ipfs-call "dht/put" `(("arg" ,key)("arg" ,value))) result))

;; STRING → ALIST || (NIL STRING)
(defun dht-query (peer-id)
  "Find the closest peer IDs to the given one by querying the DHT.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-dht-query"
  (bind-api-result
    (ipfs-call "dht/query" `(("arg" ,key)))
    (re-hash-table-alist (gethash "Responses" result))))



;; —————————————————————————————————————
;; DIAG CALLS

;; NIL → ALIST
(defun diag-cmds ()
  "List commands run on this IPFS node.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-diag-cmds"
  (mapcar #'re-hash-table-alist (ipfs-call "diag/cmds" '())))

;; NIL → NIL || (NIL STRING)
(defun diag-cmds-clear ()
  "Clear inactive requests from the log.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-diag-cmds-clear"
  (bind-api-result (ipfs-call "diag/cmds/clear" '()) result))

;; NUMBER → NIL || (NIL STRING)
(defun diag-cmds-set-time (time)
  "Set how long to keep inactive requests in the log.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-diag-cmds-set-time"
  (bind-api-result (ipfs-call "diag/cmds/set-time" `(("arg" ,time))) result))

;; NIL → STRING || (NIL STRING)
(defun diag-sys ()
  "Print system diagnostic info.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-diag-sys"
  (bind-api-result (ipfs-call "diag/sys" '()) result))



;; —————————————————————————————————————
;; FILE CALLS

(defun file-ls (path)
  "List directory contents for UNIX filesystem objects.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-file-ls"
  (bind-api-result
    (ipfs-call "file/ls" `(("arg" ,path)))
    (assoc "Objects" (re-hash-table-alist result) :test #'equal)))



;; —————————————————————————————————————
;; FILES CALLS

;; STRING [:NUMBER :STRING] → NIL
(defun files-chcid (path &key (cid-version nil) (hash nil))
  "Change the cid version or hash function of the root node of a given path.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-chcid"
  (bind-api-result
    (ipfs-call "files/chcid" `(("arg" ,path)
			       ,(if cid-version `("cid-version" ,cid-version))
			       ,(if hash (list "hash" hash))))
    result))

;; STRING STRING → NIL || (NIL STRING)
(defun files-cp (source destination)
  "Copy files into mfs.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-cp"
  (bind-api-result
    (ipfs-call "files/cp" `(("arg" ,source)("arg" ,destination)))
    result))

;; STRING → STRING
(defun files-flush (&optional (path "/"))
  "Flush a given path's data to disk. Returns CID.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-flush"
  (cadar (bind-api-alist (ipfs-call "files/flush" `(("arg" ,path))))))

;; [STRING] → ALIST || (NIL STRING)
(defun files-ls (&optional (path "/"))
  "List directories in local mutable namespace.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-ls"
  (bind-api-alist (ipfs-call "files/ls" `(("arg" ,path)))))

;; STRING [:BOOLEAN :NUMBER :STRING] → NIL || (NIL STRING)
(defun files-mkdir (path &key (parents nil) (cid-version nil) (hash nil))
  "Make a directory.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-mkdir"
  (bind-api-result
    (ipfs-call "files/mkdir" `(("arg" ,path)
			       ,(if parents (list "parents" parents))
			       ,(if cid-version `("cid-version" ,cid-version))
			       ,(if hash (list "hash" hash))))
    result))

;; STRING STRING → NIL || (NIL STRING)
(defun files-mv (source destination)
  "Move a file.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-mv"
  (bind-api-result
    (ipfs-call "files/mv" `(("arg" ,source)("arg" ,destination)))
    result))

;; STRING [:NUMBER :NUMBER] → STRING || (NIL STRING)
(defun files-read (path &key (offset nil) (max nil))
  "Read a file in given mfs.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-read"
  (bind-api-result
    (ipfs-call "files/read" `(("arg" ,source)
			      ,(if offset (list "offset" offset))
			      ,(if max (list "max" max))))
    result))

;; STRING [:BOOLEAN :BOOLEAN] → NIL || (NIL STRING)
(defun files-rm (path &key (recursive nil) (force nil))
  "Remove a given file.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-rm"
  (bind-api-result
    (ipfs-call "files/read" `(("arg" ,source) ("recursive" recursive)
					      ("force" force)))
    result))

;; STRING → ALIST || (NIL STRING)
(defun files-stat (path)
  "Remove a given file.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-rm"
  (bind-api-alist
    (ipfs-call "files/stat" `(("arg" ,path)))))

;; PATHNAME STRING [:NUMBER :BOOLEAN :BOOLEAN :BOOLEAN :NUMBER :BOOLEAN
;;                  :NUMBER :STRING]
;;                 → NIL || (NIL STRING)
(defun files-write (pathname dest-path
			     &key (offset nil) (create nil) (parents nil)
			     (truncate nil) (count nil) (raw-leaves nil)
			     (cid-version nil) (hash nil))
  "Remove a given file.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-files-rm"
  (bind-api-result
    (ipfs-call "files/write" `(("arg" ,dest-path) ("create" ,create)
			        ("parents" ,parents) ("truncate" ,truncate)
				("raw-leaves" ,raw-leaves)
			       ,(if offset (list "offset" offset))
			       ,(if count (list "count" count))
			       ,(if cid-version `("cid-version" ,cid-version))
			       ,(if hash (list "hash" hash)))
	       :method :post :parameters `(("file" . ,pathname)))
    result))



;; —————————————————————————————————————
;; FILESTORE CALLS

;; NIL → ALIST || (NIL STRING)
(defun filestore-dups ()
  "List blocks that're both in the filestore and standard block storage.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-filestore-dups"
  (bind-api-alist (ipfs-call "filestore/dups")))

;; [STRING] → ALIST || (NIL STRING)
(defun filestore-ls (&optional cid)
  "List objects in filestore.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-filestore-ls"
  (bind-api-alist (ipfs-call "filestore/ls" `(,(if cid (list "arg" cid))))))

;; [STRING] → ALIST || (NIL STRING)
(defun filestore-verify (&optional cid)
  "Verify objects in filestore.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-filestore-verify"
  (bind-api-alist (ipfs-call "filestore/verify" `(,(if cid (list "arg" cid))))))



;; —————————————————————————————————————
;; VERSION CALLS

;; NIL → STRING
(defun version ()
  "Return the current IPFS version.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-version"
  (bind-api-result
    (ipfs-call "version" '())
    (gethash "Version" result)))

;; NIL → ALIST
(defun version-deps ()
  "Return info about dependencies used for build; I.E., Go version, OS, etc.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-version"
  (bind-api-alist
    (ipfs-call "version/deps" '())))




;; —————————————————————————————————————
;; UTIL

;; FORM → BOOLEAN
(defmacro error-p (form)
  "Return whether or not a given form errors out."
  `(multiple-value-bind (return error) (ignore-errors ,form)
     (when error 'T)))

;; VARYING → BOOLEAN
(defun pure-cons-p (item)
  "Return whether or not an item is 'purely' a cons-pair; that is, it isn't of
   a larger list. In these cases, #'consp passes, but #'length errors out."
  (and (consp item)
       (error-p (length item))))

;; FUNCTION FUNCTION LIST/VARYING → LIST
(defun test-apply (test function data)
  "Apply a given function to all items within a list that pass the given test,
  recursively. AKA, if the given function returns another list, the process is
  applied to that list as well. So on and so forth."
  (cond ((pure-cons-p data)
	 (test-apply test function
		     `(,(car data) ,(cdr data))))
	((listp data)
	 (mapcar
	   (lambda (item) (test-apply test function item))
	   data))
	((funcall test data)
	 (test-apply test function
		     (funcall function data)))
	('T data)))

;; STRING-A STRING-B … STRING-N → STRING
(defun string+ (&rest strings)
  "Combine an arbitrary amount of strings into a single string."
  (reduce (lambda (a b) (format nil "~A~A" a b)) strings))

;; HASH-TABLE → ALIST
(defun re-hash-table-alist (hash-table)
  "Turn a hash-table into an associative list, recursively-- if any of the
  hash-table's values are ther hash-tables, they too are turned into alists."
  (test-apply #'hash-table-p
	      #'alexandria:hash-table-alist
	      (alexandria:hash-table-alist hash-table)))
