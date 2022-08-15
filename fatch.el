(defvar fatch-cbargs nil
  "A plist of callback args passed from the inital request.")
(defvar fatch-status nil
  "The `url-retrieve` status passed to the callback.")

(defun fatch-args (key)
  (plist-get fatch-cbargs key))

(defun fatch-query-string (ass)
  "Converts an alist to a URL query string."
  (url-build-query-string
   (seq-map (lambda (pair)
	      (list (format "%s" (car pair))
		    (format "%s" (cdr pair))))
	    ass)))

(defun fatch-parse-query (str)
  "Converts a URL query string to an alist."
  (seq-map
   (lambda (lst) (cons (intern (car lst)) (cadr lst)))
   (url-parse-query-string str)))

(defun fatch--encode-headers (ass)
  (let ((results nil))
    (dolist (pair ass)
      (let ((key (car pair))
	    (val (cdr pair)))
	(setq results (cons (cons (format "%s" key)
				  (format "%s" val))
			    results))))
    (nreverse results)))

(defun fatch--callback (status cb cbargs)
  (let ((fatch-cbargs cbargs)
	(fatch-status status))
    (with-current-buffer (current-buffer)
      (funcall cb))))

(defun fatch (url callback &rest args)
  "Calls `url-retrieve` with the given URL. The provided CALLBACK is called
within the context of the buffer returned by the callback of `url-retrieve`.

The remaining ARGS are patterned after `request.el`:
  :method binds to `url-request-method`
  :data binds to `url-request-data`
  :headers to `url-request-extra-headers`
  :params are added to URL before the request is sent

ARGS dynamically bound to CALLBACK and accessible via `-fetch-args`.
ex: (-fetch-args :method) returns the method initially passed."
  (let ((url-request-method (or (plist-get args :method) "GET"))
	(url-request-data (plist-get args :data))
	(url-request-extra-headers (fatch--encode-headers
				    (plist-get args :headers)))
	(params (when (plist-get args :params)
		  (fatch-query-string (plist-get args :params)))))
    (url-retrieve (if params
		      (format "%s?%s" url params)
		    url)
		  #'fatch--callback
		  (list callback `(:url ,url ,@args)))))

(defun fatch-read-json ()
  "Reads the JSON text from the response buffer and parses it into
elisp data."
  (goto-char (point-min))
  (re-search-forward "^$")
  (json-read))

(defun fatch-read-text ()
  "Reads the raw body text from the response buffer."
  (goto-char (point-min))
  (buffer-substring-no-properties
   (re-search-forward "^$")
   (point-max)))

(provide 'fatch)
