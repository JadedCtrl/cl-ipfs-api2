(in-package :ipfs-gno)

(defparameter *api-host* "http://127.0.0.1:5001")

;; STRING LIST … → (STRING/VARYING RETURN-CODE HEADER-LIST …)
(defun api-call (call arguments &key (method :get) (parameters nil)
		      (want-stream nil))
  (let ((call-url (string+ *api-host* "/api/v0/" call))
	(first-arg T))
    (mapcar (lambda (arg-pair)
		(format t "FIRST: ~A~%" first-arg)
	      (if arg-pair
		(progn (setq call-url
			     (string+ call-url (if first-arg "?" "&")
				      (car arg-pair) "=" (cadr arg-pair)))
		       (setq first-arg nil))))
	    arguments)

    (drakma:http-request call-url :method method :parameters parameters
			 :want-stream want-stream)))


;; STRING :NUMBER :NUMBER → STRING
(defun cat (ipfs-path &key (offset nil) (length nil))
  "Return a string of the data at the given IPFS path."
  (api-call "cat"
	    `(("arg" ,ipfs-path)
	      ,(if offset `("offset" ,offset))
	      ,(if length `("length" ,length)))))


;; STRING PATHNAME --> NIL
(defun dl (ipfs-path out-file)
  "Write an IPFS file directly to a file on the local file-system.
  Non-recursive, in the case of directories.
  (Thanks to this thread ♥) https://stackoverflow.com/a/12607423"
  (with-open-file (out-stream out-file :direction :output
			      :element-type '(unsigned-byte 8)
			      :if-exists :overwrite :if-does-not-exist :create)
    (let ((in-stream
	    (api-call "cat" `(("arg" ,ipfs-path)) :want-stream 'T)))

      (awhile (read-byte in-stream nil nil)
	      (write-byte it out-stream))
      (close in-stream))))


;; PATHNAME → STRING
(defun add (file-path)
  "Add a file to IPFS, return it's hash. Does not work recursively."
  (gethash "Hash"
	   (yason:parse
	     (flexi-streams:octets-to-string
	       (api-call "add" '() :method :post
			 :parameters `(("file" . ,file-path)))))))


;; STRING-A STRING-B … STRING-N → STRING
(defun string+ (&rest strings)
  "Combine an arbitrary amount of strings into a single string."
  (reduce (lambda (a b) (format nil "~A~A" a b)) strings))
