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
(defmacro bind-api-alist (call form)
  "Basically #'bind-api-result, but it assumes the final form is a hash-table,
   and maps it to an associative list."
  `(bind-api-result ,call  (ignore-errors (re-hash-table-alist ,form))))



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
    (ipfs-call "id" `(,(if peer-id (list "arg" peer-id))))
    result))

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
    (ipfs-call "bitswap/ledger" `(("arg" ,peer-id)))
    result))

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
    (ipfs-call "bitswap/stat" '())
    result))

;; STRING → ALIST || (NIL STRING)
(defun bitswap-wantlist (&optional peer-id)
  "Show blocks currently on the wantlist.
  /ipns/docs.ipfs.io/reference/api/http/#api-v0-bitswap-wantlist"
  (bind-api-alist
    (ipfs-call "bitswap/wantlist" `(,(if peer-id (list "peer" peer-id))))
    result))



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
	       :method :POST :parameters `(("data" . ,pathname)))
    result))

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
    (ipfs-call "block/stat" `(("arg" ,hash)))
    result))



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
    (ipfs-call "config/show" '())
    result))

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
    (ipfs-call "version/deps" '())
    result))




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
