(in-package :cl-ipfs-api2)

(defparameter *api-host* "http://127.0.0.1:5001")
(defparameter *api-root* "/api/v0/")
(defparameter *ipfs-root* nil) ;; correlates to the env variable $IPFS_PATH,
                               ;; only necessary if yours deviates from the
                               ;; default path. only used for #'pubsub-*


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
	    :url-encoder #'ipfs:url-encode
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
;; KEY CALLS

;; STRING [:STRING :NUMBER] → ALIST || (NIL STRING)
(defun key-gen (name &key (type nil) (size nil))
  "Create a new keypair.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-key-gen"
  (bind-api-alist
    (ipfs-call "key/gen" `(("name" ,name) ,(if type (list "type" type))
					  ,(if size (list "size" size))))))

;; NIL → ALIST || (NIL STRING)
(defun key-list ()
  "List all local keypairs.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-key-list"
  (bind-api-alist (ipfs-call "key/list" '())))

;; STRING STRING [:BOOLEAN] → ALIST || (NIL STRING)
(defun key-rename (old-name new-name &key (force nil))
  "Rename a local keypair.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-key-rename"
  (bind-api-alist
    (ipfs-call "key/rename" `(("arg" ,old-name) ("arg" ,new-name)
						("force" ,force)))))
;; STRING → ALIST || (NIL STRING)
(defun key-remove (name)
  "Remove a keypair, based on name.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-key-remove"
  (bind-api-alist (ipfs-call "key/remove" `(("arg" ,name)))))



;; —————————————————————————————————————
;; LOG CALLS

;; STRING STRING → STRING || (NIL STRING)
(defun log-level (subsystem level)
  "Change the logging level of a subsystem.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-log-level"
  (bind-api-result
    (ipfs-call "log/level" `(("arg" ,subsystem)("arg" ,level)))
    (gethash "Message" result)))

;; NIL → LIST || (NIL STRING)
(defun log-ls ()
  "List the logging subsystems.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-log-ls"
  (bind-api-result (ipfs-call "log/ls" '()) (gethash "Strings" result)))

;; NIL → STRING || (NIL STRING)
(defun log-tail ()
  "Read the event log.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-log-tail"
  (bind-api-result (ipfs-call "log/tail" '()) result))




;; —————————————————————————————————————
;; NAME CALLS

;; STRING [:BOOLEAN :STRING :BOOLEAN :STRING] → ALIST || (NIL STRING)
(defun name-publish (ipfs-path &key (resolve 'T) (lifetime "24h")
			       (allow-offline 'T) (ttl nil))
  "Publish an IPNS name-- associate it with an IPFS path.
   /ipns/docs.ipfs.io/reference/api/http/#api-v0-name-publish"
  (bind-api-alist
    (ipfs-call "name/publish" `(("arg" ,ipfs-path)("resolve" ,resolve)
				("lifetime" ,lifetime)
				("allow-offline" ,allow-offline)
				,(if ttl (list "ttl" ttl))))))

;; STRING → STRING || (NIL STRING)
(defun name-pubsub-cancel (name)
  "Cancel subscription to a name.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-name-pubsub-cancel"
  (bind-api-result (ipfs-call "name/pubsub/cancel" `(("arg" ,name)))
		   (gethash "Canceled" result)))

;; NIL → STRING || (NIL STRING)
(defun name-pubsub-state ()
  "Query the state of IPNS pubsub.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-name-pubsub-state"
  (bind-api-result (ipfs-call "name/pubsub/state" '())
		   (gethash "Enabled" result)))

;; NIL → STRING || (NIL STRING)
(defun name-pubsub-subs ()
  "Show current name subscriptions.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-name-pubsub-subs"
  (bind-api-result (ipfs-call "name/pubsub/subs" '())
		   (gethash "Strings" result)))

;; STRING [:BOOLEAN :BOOLEAN :NUMBER :STRING] → STRING || (NIL STRING)
(defun name-resolve (name &key (recursive 't) (nocache "")
			  (dht-record-count nil) (dht-timeout nil))
  "Resolve a given IPNS name.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-name-resolve"
  (bind-api-result
    (ipfs-call "name/resolve" `(("arg" ,name)("recursive" ,recursive)
				,(when (not (empty-string-p nocache))
				   (list "nocache" nocache))
				,(when dht-record-count
				   (list "dht-record-count" dht-record-count))
				,(when dht-timeout
				   (list "dht-timeout" dht-timeout))))
    (gethash "Path" result)))




;; —————————————————————————————————————
;; OBJECT CALLS

;; STRING → STRING || (NIL STRING)
(defun object-data (key)
  "Output the raw data of an IPFS object.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-data"
  (bind-api-result (ipfs-call "object/data" `(("arg" ,key))) result))

;; STRING STRING → ALIST || (NIL STRING)
(defun object-diff (object-a object-b)
  "Display the differences between two IPFS objects.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-diff"
  (bind-api-alist
    (ipfs-call "object/diff" `(("arg" ,object-a)("arg" ,object-b)))))

;; STRING [:STRING] → STRING || (NIL STRING)
(defun object-get (key &key (data-encoding "text"))
  "Get and serialize the named DAG node.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-get"
  (bind-api-result
    (ipfs-call "object/get" `(("arg" ,key)("data-encoding" ,data-encoding)))
    result))

;; STRING → ALIST || (NIL STRING)
(defun object-links (key)
  "Output the links pointed to by the specified object.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-links"
  (bind-api-alist (ipfs-call "object/links" `(("arg" ,key)))))

;; [:STRING] → ALIST || (NIL STRING)
(defun object-new (&key (template nil))
  "Create a new object from an IPFS template.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-new"
  (bind-api-alist
    (ipfs-call "object/new"`(,(if template `("template" ,template))))))

;; STRING STRING STRING [:BOOLEAN] → ALIST || (NIL STRING)
(defun object-patch-add-link (hash name object &key (create ""))
  "Add a link to a given object.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-patch-add-link"
  (bind-api-alist
    (ipfs-call "object/patch/add-link"
	       `(("arg" ,hash)("arg" ,name)("arg" ,object)
		 ,(when (not (empty-string-p create)) `("create" ,create))))))

;; STRING STRING → ALIST || (NIL STRING)
(defun object-patch-rm-link (hash name)
  "Remove a link from a given object.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-patch-rm-link"
  (bind-api-alist
    (ipfs-call "object/patch/rm-link" `(("arg" ,hash)("arg" ,name)))))

;; STRING → ALIST || (NIL STRING)
(defun object-stat (key)
  "Get stats for a DAG node.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-object-stat"
  (bind-api-alist (ipfs-call "object/stat" `(("arg" ,key)))))




;; —————————————————————————————————————
;; P2P CALLS

;; [:BOOLEAN :STRING :STRING :STRING :STRING] → NUMBER || (NIL STRING)
(defun p2p-close (&key (all "") (protocol nil) (listen-address nil)
		       (target-address nil))
  "Stop listening for new connections to forward.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-p2p-close"
  (bind-api-result
    (ipfs-call "p2p/close" `(,(when (not (empty-string-p all)) `("all" ,all))
			      ,(when protocol `("protocol" ,protocol))
			      ,(when listen-address
				 `("listen-address" ,listen-address))
			      ,(when target-address
				 `("target-address" ,target-address))))
    result))

;; STRING STRING STRING [:BOOLEAN] → STRING || (NIL STRING)
(defun p2p-forward (protocol listening-endpoint target-endpoint
			     &key (allow-custom-protocol ""))
  "Forward connections to libp2p service.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-p2p-forward"
  (bind-api-result
    (ipfs-call "p2p/forward" `(("arg" ,protocol)("arg" ,listening-endpoint)
			       ("arg" ,target-endpoint)
			       ,(when
				  (not (empty-string-p allow-custom-protocol))
				  `("allow-custom-protocol"
				    ,allow-custom-protocol))))
    result))

;; STRING STRING [:BOOLEAN :BOOLEAN] → STRING || (NIL STRING)
(defun p2p-listen (protocol target-endpoint
			    &key (allow-custom-protocol "") (report-peer-id ""))
  "Create libp2p service.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-p2p-listen"
  (bind-api-result
    (ipfs-call "p2p/listen" `(("arg" ,protocol)("arg" ,target-endpoint)
			       ,(when
				  (not (empty-string-p allow-custom-protocol))
				  `("allow-custom-protocol"
				    ,allow-custom-protocol))
			       ,(when (not (empty-string-p report-peer-id))
				  `("report-peer-id" ,report-peer-id))))
    result))

;; NIL → ALIST || (NIL STRING)
(defun p2p-ls ()
  "List active p2p listeners.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-p2p-ls"
  (bind-api-alist (ipfs-call "p2p/ls" '())))

;; [:STRING :BOOLEAN] → STRING || (NIL STRING)
(defun p2p-stream-close (&key (identifier nil) (all ""))
  "Close an active p2p stream.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-p2p-stream-close"
  (bind-api-result
    (ipfs-call "p2p/stream/close" `(,(when identifier `("arg" ,identifier))
				     ,(when (not (empty-string-p all))
					`("all" ,all))))
    result))

;; NIL → ALIST || (NIL STRING)
(defun p2p-stream-ls ()
  "List active p2p streams.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-p2p-stream-ls"
  (bind-api-alist (ipfs-call "p2p/stream/ls" '())))




;; —————————————————————————————————————
;; PIN CALLS

;; STRING [:BOOLEAN] → ALIST || (NIL STRING)
(defun pin-add (path &key (recursive 'T))
  "Pin an object to local storage.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pin-add"
  (bind-api-alist
    (ipfs-call "pin/add" `(("arg" ,path)("recursive" ,recursive)))))

;; [:STRING :STRING] → ALIST || (NIL STRING)
(defun pin-ls (&key (path nil) (type "all"))
  "List objects pinned to local storage.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pin-ls"
  (bind-api-alist
    (ipfs-call "pin/ls" `(,(when path `("arg" ,path)) ("type" ,type)))))

;; STRING [:BOOLEAN] → ALIAS || (NIL STRING)
(defun pin-rm (path &key (recursive 'T))
  "Remove pinned objects from local storage.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pin-rm"
  (bind-api-alist
    (ipfs-call "pin/rm" `(("arg" ,path)("recursive" ,recursive)))))

;; STRING STRING [:BOOLEAN] → ALIST || (NIL STRING)
(defun pin-update (old-path new-path &key (unpin 'T))
  "Update a recursive pin.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pin-update"
  (bind-api-alist
    (ipfs-call "pin/update"
	       `(("arg" ,old-path)("arg" ,new-path)("unpin" ,unpin)))))

;; NIL → ALIST || (NIL STRING)
(defun pin-verify ()
  "Verify that recursive pins are complete.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pin-verify"
  (bind-api-alist (ipfs-call "pin/verify" '())))




;; —————————————————————————————————————
;; PUBSUB CALLS

;; STRING [:STRING] → PROCESS-INFO-STREAM
(defun pubsub-sub (topic &key (env ""))
  "Subscribe to a given pubsub topic— this function requires go-ipfs to be
  installed on the current machine, and that `ipfs` is in the current $PATH.
  This probably will only work on *nix systems (sorry Windows nerds).
  Returns a uiop/launch-program::process-info socket-- can be used in
  conjunction with the #'pubsub-sub-* functions, or with :uiop/launch-program's
  functions.
  A system-dependent replacement for
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pubsub-sub"
  (when (and *ipfs-root* (empty-string-p env))
    (setq env (string+ "env IPFS_PATH=" *ipfs-root* " > /dev/null;")))

  (uiop:launch-program (string+ env "ipfs pubsub sub " topic) :output :stream))

;; PROCESS-INFO-STREAM → FD-STREAM
(defun pubsub-sub-process (pubsub-socket)
  "Turn a uiop process-info-stream ('pubsub stream') into a fd-stream that
  is #'read-char-able, etc."
  (uiop/launch-program:process-info-output pubsub-socket))

;; PROCESS-INFO-STREAM → CHARACTER
(defun pubsub-sub-read-char (pubsub-socket)
  "Process a 'pubsub stream' (process-info-stream) and #'readchar it."
  (read-char (pubsub-sub-process pubsub-socket)))

;; PROCESS-INFO-STREAM → BOOLEAN
(defun pubsub-sub-listen (pubsub-socket)
  "Process a 'pubsub stream' (process-info-stream) and #'listen it."
  (listen (pubsub-sub-process pubsub-socket)))

;; PROCESS-INFO-STREAM → NIL
(defun pubsub-sub-close (pubsub-socket)
  "Close a 'pubsub stream' (process-info-stream) and related processes."
  (and (uiop/launch-program:terminate-process pubsub-socket :urgent 't)
       (uiop/launch-program:close-streams pubsub-socket)))

;; —————————————————

;; STRING STRING [:STRING] → NIL
(defun pubsub-pub (topic string &key (env ""))
  "Publish a string to a given pubsub topic.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pubsub-pub"
  (when (and *ipfs-root* (empty-string-p env))
    (setq env (string+ "env IPFS_PATH=" *ipfs-root* " > /dev/null;")))

  (uiop:run-program (string+ env "ipfs pubsub pub " topic " \"" string "\""))
  nil)

;; —————————————————

;; NIL → LIST || (NIL STRING)
(defun pubsub-ls ()
  "Return a list of subscribed topics.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pubsub-ls"
  (bind-api-result
    (ipfs-call "pubsub/ls" '())
    (gethash "Strings" result)))

;; [STRING] → LIST || (NIL STRING)
(defun pubsub-peers (&optional topic)
  "Return a list of peers with pubsub enabled.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-pubsub-peers"
  (bind-api-result
    (ipfs-call "pubsub/peers" `(,(if topic (list "arg" topic))))
    (gethash "Strings" result)))




;; —————————————————————————————————————
;; REFS CALLS

;; STRING [:BOOLEAN :BOOLEAN :NUMBER] → ALIST || (NIL STRING)
(defun refs (path &key (unique "") (recursive "") (max-depth -1))
  "List links (references) from an object.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-refs"
  (bind-api-alist
    (ipfs-call "refs" `(("arg" ,path)("max-depth" ,max-depth)
			,(if (not (empty-string-p recursive))
			   `("recursive" ,recursive))))))

;; NIL → ALIST || (NIL STRING)
(defun refs-local ()
  "List all local references.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-refs-local"
  (bind-api-alist (ipfs-call "refs/local" '())))




;; —————————————————————————————————————
;; REPO CALLS

;; NIL → STRING || (NIL STRING)
(defun repo-fsck ()
  "Remove repo lock-files.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-repo-fsck"
  (bind-api-result (ipfs-call "repo/fsck" '()) (gethash "Message" result)))

;; NIL → ALIST || (NIL STRING)
(defun repo-gc ()
  "Perform garbage collection on the repo.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-repo-gc"
  (bind-api-alist (ipfs-call "repo/gc" '())))

;; NIL → ALIST || (NIL STRING)
(defun repo-stat ()
  "Get stats for the current repo.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-repo-stat"
  (bind-api-alist (ipfs-call "repo/stat" '())))

;; NIL → ALIST || (NIL STRING)
(defun repo-verify ()
  "Verify that all repo blocks aren't corrupted.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-repo-verify"
  (bind-api-alist (ipfs-call "repo/verify" '())))

;; NIL → NUMBER || (NIL STRING)
(defun repo-version ()
  "Show the repo version.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-repo-version"
  (bind-api-result (ipfs-call "repo/version" '())
		   (read-from-string (gethash "Version" result))))




;; —————————————————————————————————————
;; STATS CALLS

;; NIL → ALIST || (NIL STRING)
(defun stats-bitswap ()
  "Show diagnostics on bitswap.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-stats-bitswap"
  (bind-api-alist (ipfs-call "stats/bitswap" '())))

;; [:STRING :STRING :STRING] → ALIST || (NIL STRING)
(defun stats-bw (&key (peer nil) (proto nil) (interval nil))
  "Return bandwidth information.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-stats-bw"
  (bind-api-alist
    (ipfs-call "stats/bitswap"
	       `(,(when peer `("peer" ,peer)) ,(when proto `("proto" ,proto))
                 ,(when interval `("interval" ,interval))
		 ,(when interval `("poll" 'T))))))

;; NIL → ALIST || (NIL STRING)
(defun stats-repo ()
  "Show diagnostics on current repo.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-stats-repo"
  (bind-api-alist (ipfs-call "stats/repo" '())))




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

;; STRING → BOOLEAN
(defun empty-string-p (string)
  "Return whether or not a given item is an empty string."
  (and (stringp string) (zerop (length string))))

;; HASH-TABLE → ALIST
(defun re-hash-table-alist (hash-table)
  "Turn a hash-table into an associative list, recursively-- if any of the
  hash-table's values are ther hash-tables, they too are turned into alists."
  (test-apply #'hash-table-p
	      #'alexandria:hash-table-alist
	      (alexandria:hash-table-alist hash-table)))

;; STRING → STRING
(defun url-encode (string &rest ignored)
  "Wrap around drakma's url encoder, with a slight change-- instead of using
  plus-signs for spaces, we want to use %20."
  ignored
  (cl-ppcre:regex-replace-all
    "%2520" (drakma:url-encode
	      (cl-ppcre:regex-replace-all " " string "%20")
	      :utf-8)
    "%20"))
