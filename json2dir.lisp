;;;; json2dir: create the directory tree a JSON document describes, in the current directory.
;;;; Implements RFC J2D-1 (https://github.com/kitsunoff/awesome-json2dir/blob/main/spec/rfc-json2dir.md).
;;;; ANSI Common Lisp + SBCL's bundled sb-posix contrib; the JSON parser is hand-written.

(require :sb-posix)

(defpackage :json2dir
  (:use :common-lisp)
  (:export #:main))

(in-package :json2dir)

(define-condition json2dir-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (c s) (write-string (message c) s))))

(defun fail (fmt &rest args)
  (error 'json2dir-error :message (apply #'format nil fmt args)))

;;; ------------------------------------------------------------------
;;; Input

(defun read-stdin ()
  (let ((in (sb-sys:make-fd-stream 0 :input t :element-type '(unsigned-byte 8) :buffering :full))
        (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (buf (make-array 65536 :element-type '(unsigned-byte 8))))
    (loop for n = (read-sequence buf in)
          while (> n 0)
          do (loop for i below n do (vector-push-extend (aref buf i) out)))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

;; §3: strict UTF-8 (no overlong forms, no surrogates, nothing above U+10FFFF).
(defun check-utf8 (b)
  (let ((i 0) (n (length b)))
    (flet ((cont (k) (and (< k n) (= (logand (aref b k) #xC0) #x80))))
      (loop while (< i n) do
        (let ((c (aref b i)))
          (cond ((< c #x80) (incf i))
                ((and (<= #xC2 c #xDF) (cont (+ i 1))) (incf i 2))
                ((and (<= #xE0 c #xEF) (cont (+ i 1)) (cont (+ i 2))
                      (let ((c1 (aref b (+ i 1))))
                        (cond ((= c #xE0) (>= c1 #xA0))
                              ((= c #xED) (< c1 #xA0))
                              (t t))))
                 (incf i 3))
                ((and (<= #xF0 c #xF4) (cont (+ i 1)) (cont (+ i 2)) (cont (+ i 3))
                      (let ((c1 (aref b (+ i 1))))
                        (cond ((= c #xF0) (>= c1 #x90))
                              ((= c #xF4) (< c1 #x90))
                              (t t))))
                 (incf i 4))
                (t (fail "input is not valid UTF-8"))))))))

;;; ------------------------------------------------------------------
;;; JSON parser over the byte vector.
;;; Values: string -> octet vector, object -> (:obj (name-octets . value) ...),
;;; array -> (:arr value ...). Numbers, true, false and null are rejected outright (§4.5).

(defvar *b*)
(defvar *pos*)

(defun peek () (if (< *pos* (length *b*)) (aref *b* *pos*) nil))

(defun skip-ws ()
  (loop while (member (peek) '(32 9 10 13)) do (incf *pos*)))

(defun expect (c)
  (unless (eql (peek) c)
    (fail "input is not valid JSON: expected '~a' at byte ~d" (code-char c) *pos*))
  (incf *pos*))

(defun parse-value ()
  (skip-ws)
  (let ((c (peek)))
    (case c
      ((nil) (fail "input is not valid JSON: unexpected end of input"))
      (#.(char-code #\{) (parse-object))
      (#.(char-code #\[) (parse-array))
      (#.(char-code #\") (parse-string))
      (t (if (or (eql c (char-code #\-)) (<= 48 c 57)
                 (eql c (char-code #\t)) (eql c (char-code #\f)) (eql c (char-code #\n)))
             (fail "numbers, true, false and null values are not allowed (byte ~d)" *pos*)
             (fail "input is not valid JSON: unexpected byte at ~d" *pos*))))))

(defun parse-object ()
  (expect (char-code #\{))
  (skip-ws)
  (let ((members '()))
    (if (eql (peek) (char-code #\}))
        (incf *pos*)
        (loop
          (skip-ws)
          (unless (eql (peek) (char-code #\"))
            (fail "input is not valid JSON: expected a member name at byte ~d" *pos*))
          (let ((name (parse-string)))
            (skip-ws)
            (expect (char-code #\:))
            (push (cons name (parse-value)) members))
          (skip-ws)
          (cond ((eql (peek) (char-code #\,)) (incf *pos*))
                ((eql (peek) (char-code #\})) (incf *pos*) (return))
                (t (fail "input is not valid JSON: expected ',' or '}' at byte ~d" *pos*)))))
    ;; Duplicate names: the last occurrence wins (§4.2).
    (cons :obj (remove-duplicates (nreverse members) :key #'car :test #'equalp))))

(defun parse-array ()
  (expect (char-code #\[))
  (skip-ws)
  (let ((items '()))
    (if (eql (peek) (char-code #\]))
        (incf *pos*)
        (loop
          (push (parse-value) items)
          (skip-ws)
          (cond ((eql (peek) (char-code #\,)) (incf *pos*))
                ((eql (peek) (char-code #\])) (incf *pos*) (return))
                (t (fail "input is not valid JSON: expected ',' or ']' at byte ~d" *pos*)))))
    (cons :arr (nreverse items))))

(defun hex4 ()
  (let ((v 0))
    (dotimes (k 4 v)
      (let* ((c (peek))
             (d (and c (digit-char-p (code-char c) 16))))
        (unless d (fail "input is not valid JSON: bad \\u escape at byte ~d" *pos*))
        (setf v (+ (* v 16) d))
        (incf *pos*)))))

(defun put-utf8 (cp out)
  (flet ((put (x) (vector-push-extend x out)))
    (cond ((< cp #x80) (put cp))
          ((< cp #x800) (put (logior #xC0 (ash cp -6)))
                        (put (logior #x80 (logand cp #x3F))))
          ((< cp #x10000) (put (logior #xE0 (ash cp -12)))
                          (put (logior #x80 (logand (ash cp -6) #x3F)))
                          (put (logior #x80 (logand cp #x3F))))
          (t (put (logior #xF0 (ash cp -18)))
             (put (logior #x80 (logand (ash cp -12) #x3F)))
             (put (logior #x80 (logand (ash cp -6) #x3F)))
             (put (logior #x80 (logand cp #x3F)))))))

(defun parse-string ()
  (expect (char-code #\"))
  (let ((out (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop
      (let ((c (peek)))
        (cond ((null c) (fail "input is not valid JSON: unterminated string"))
              ((= c 34) (incf *pos*) (return))
              ((< c 32) (fail "input is not valid JSON: control character in string at byte ~d" *pos*))
              ((= c 92)
               (incf *pos*)
               (let ((e (peek)))
                 (incf *pos*)
                 (case (and e (code-char e))
                   (#\" (vector-push-extend 34 out))
                   (#\\ (vector-push-extend 92 out))
                   (#\/ (vector-push-extend 47 out))
                   (#\b (vector-push-extend 8 out))
                   (#\f (vector-push-extend 12 out))
                   (#\n (vector-push-extend 10 out))
                   (#\r (vector-push-extend 13 out))
                   (#\t (vector-push-extend 9 out))
                   (#\u
                    (let ((cp (hex4)))
                      (cond ((<= #xD800 cp #xDBFF)
                             (unless (and (eql (peek) 92) (< (1+ *pos*) (length *b*))
                                          (eql (aref *b* (1+ *pos*)) (char-code #\u)))
                               (fail "string contains an unpaired surrogate"))
                             (incf *pos* 2)
                             (let ((lo (hex4)))
                               (unless (<= #xDC00 lo #xDFFF)
                                 (fail "string contains an unpaired surrogate"))
                               (setf cp (+ #x10000 (ash (- cp #xD800) 10) (- lo #xDC00)))))
                            ((<= #xDC00 cp #xDFFF)
                             (fail "string contains an unpaired surrogate")))
                      (put-utf8 cp out)))
                   (t (fail "input is not valid JSON: bad escape at byte ~d" (1- *pos*))))))
              (t (vector-push-extend c out) (incf *pos*)))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun parse (bytes)
  (check-utf8 bytes)
  (let ((*b* bytes)
        (*pos* (if (and (>= (length bytes) 3) (= (aref bytes 0) #xEF)
                        (= (aref bytes 1) #xBB) (= (aref bytes 2) #xBF))
                   3 0)))
    (let ((v (parse-value)))
      (skip-ws)
      (when (< *pos* (length *b*))
        (fail "input is not valid JSON: trailing data at byte ~d" *pos*))
      v)))

;;; ------------------------------------------------------------------
;;; Validation (§4), done for the whole document before writing (§6).

(defun utf8-string (octets) (sb-ext:octets-to-string octets :external-format :utf-8))

(defun octet-vector-p (v) (typep v '(simple-array (unsigned-byte 8) (*))))

(defun check-name (name where)
  (when (or (zerop (length name))
            (equalp name #(46))
            (equalp name #(46 46))
            (find 47 name)
            (find 0 name))
    (fail "~a: invalid name ~s" where (utf8-string name))))

(defun validate (v where)
  (cond ((octet-vector-p v) t)
        ((eq (car v) :arr)
         (let ((items (cdr v)))
           (unless (and (= (length items) 2) (every #'octet-vector-p items))
             (fail "~a: an array must be [\"link\", target] or [\"script\", content]" where))
           (let ((kind (utf8-string (first items))))
             (unless (member kind '("link" "script") :test #'string=)
               (fail "~a: unknown array kind ~s" where kind))
             (when (and (string= kind "link") (find 0 (second items)))
               (fail "~a: a link target cannot contain NUL" where)))))
        (t
         (dolist (m (cdr v))
           (let ((child (if (string= where ".")
                            (utf8-string (car m))
                            (concatenate 'string where "/" (utf8-string (car m))))))
             (check-name (car m) child)
             (validate (cdr m) child))))))

;;; ------------------------------------------------------------------
;;; File system

(defun lstat-or-nil (path)
  (handler-case (sb-posix:lstat path)
    (sb-posix:syscall-error (e)
      (if (= (sb-posix:syscall-errno e) sb-posix:enoent) nil (error e)))))

(defun dir-p (st) (and st (sb-posix:s-isdir (sb-posix:stat-mode st))))

;; §5.2–5.4: remove an existing non-directory (a symlink itself, never its target);
;; a directory in the way of a non-object is an error.
(defun clear (path st)
  (when st
    (when (dir-p st) (fail "~a: a directory is in the way" path))
    (sb-posix:unlink path)))

(defun write-file (path content executable)
  ;; O_EXCL | O_NOFOLLOW: never writes through an entry that appeared after clear().
  (let* ((fd (sb-posix:open path (logior sb-posix:o-wronly sb-posix:o-creat
                                         sb-posix:o-excl sb-posix:o-nofollow)
                            #o666))
         (s (sb-sys:make-fd-stream fd :output t :element-type '(unsigned-byte 8)
                                      :buffering :full)))
    (unwind-protect
         (progn
           (write-sequence content s)
           (finish-output s)
           (when executable
             (let ((mode (sb-posix:stat-mode (sb-posix:fstat fd))))
               (sb-posix:fchmod fd (logior (logand mode #o7777) #o111)))))
      (close s))))

(defun octets< (a b)
  (let ((n (min (length a) (length b))))
    (dotimes (i n (< (length a) (length b)))
      (cond ((< (aref a i) (aref b i)) (return t))
            ((> (aref a i) (aref b i)) (return nil))))))

(defun apply-tree (dir members)
  (dolist (m (sort (copy-list members) #'octets< :key #'car))
    (let* ((path (concatenate 'string dir "/" (utf8-string (car m))))
           (v (cdr m))
           (st (lstat-or-nil path)))
      (cond ((octet-vector-p v)
             (clear path st)
             (write-file path v nil))
            ((eq (car v) :arr)
             (clear path st)
             (let ((kind (utf8-string (second v))))
               (if (string= kind "link")
                   (sb-posix:symlink (utf8-string (third v)) path)
                   (write-file path (third v) t))))
            (t
             (unless (dir-p st)
               (when st (sb-posix:unlink path))
               (sb-posix:mkdir path #o777))
             (apply-tree path (cdr v)))))))

;;; ------------------------------------------------------------------

(defun main ()
  (setf sb-ext:*default-c-string-external-format* :utf-8)
  (let ((code
          (if (rest sb-ext:*posix-argv*)
              (progn (format *error-output* "usage: json2dir < document.json~%") 2)
              (handler-case
                  (let ((doc (parse (read-stdin))))
                    (unless (and (consp doc) (eq (car doc) :obj))
                      (fail "the root of the document must be an object"))
                    (validate doc ".")
                    (apply-tree "." (cdr doc))
                    0)
                (serious-condition (e)
                  (format *error-output* "json2dir: ~a~%" e)
                  1)))))
    (finish-output *error-output*)
    (sb-ext:exit :code code :abort t)))
