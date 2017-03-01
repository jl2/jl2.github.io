Here's a Common Lisp function to generate static HTML and CSS from a GitHub Gist URL.  The results are printed to standard out, and can be copy/pasted into an HTML document.  This is really useful when working in Emacs with a Slime REPL open.

``` common lisp
(defun make-static-gist (gist-url) 
  "Retrieve a Gist from GitHub and format it as static CSS and HTML."
  (let* ((nl-string (format nil "~c" #\newline))
         ;; Make sure the url has ".js" on the end
         (ending (subseq gist-url (- (length gist-url) 3)))
         (js-url (if (string= ending ".js")
                     gist-url
                     (concatenate 'string gist-url ".js")))
         ;; Fetch the embedded gist from GitHub
         (gist-data (drakma:http-request js-url)))
    ;; Read the string data
    (with-input-from-string (strm gist-data)
      (let* (
             ;; Get the CSS link
             (css-html (read-line strm))
             ;; Get the document data
             (doc-part (read-line strm))
             ;; Parse out the CSS URL and fetch it
             ;; 45 is the length of "document.write...",
             ;; 4 is the length of "\");" on the end
             (css-url (subseq css-html 45 (- (length css-html) 4)))
             (style-sheet (drakma:http-request css-url))
             ;; Remove \n and \ from the html
             (escaped-html (subseq doc-part 16 (- (length doc-part) 4)))
             (html-nl (cl-ppcre:regex-replace-all "\\\\n" escaped-html nl-string))
             (raw-html (cl-ppcre:regex-replace-all "\\" html-nl "")))
        ;; Print everything out
        (format t "~a~%" style-sheet)
        (format t "~a~%" raw-html)))))
```
