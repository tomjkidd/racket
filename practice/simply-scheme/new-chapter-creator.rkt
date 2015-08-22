#lang racket
(require racket/file)

;; http://download.racket-lang.org/docs/5.1/html/reference/strings.html
;; http://docs.racket-lang.org/reference/Manipulating_Paths.html
;; http://docs.racket-lang.org/reference/Filesystem.html
;; http://stackoverflow.com/questions/20556746/how-do-i-do-anything-with-multiple-return-values-in-racket

;; TODO: make this a little easier to use, refactor.
(define (create-files dst)
  (letrec ((base-path (path->string (current-directory)))
           (new-dir (build-path (string-append
                                 base-path
                                 dst)))
           (exercise-filename "exercises.rkt")
           (notes-filename "notes.md")
           (exercise-dst-path (build-path new-dir exercise-filename))
           (notes-dst-path (build-path new-dir notes-filename))
           (exercise-src-path (build-path (build-path base-path "tmp") exercise-filename))
           (notes-src-path (build-path (build-path base-path "tmp") notes-filename)))
    (displayln base-path)
    (displayln new-dir)
    (displayln exercise-src-path)
    (displayln exercise-dst-path)
    (displayln notes-src-path)
    (displayln notes-dst-path)
    (if (not (directory-exists? new-dir))
        (make-directory new-dir)
        void)
    (if (file-exists? exercise-dst-path)
        (delete-directory/files	exercise-dst-path)
        void)
    (if (file-exists? notes-dst-path)
        (delete-directory/files notes-dst-path)
        void)
    (copy-directory/files exercise-src-path exercise-dst-path)
    (copy-directory/files notes-src-path notes-dst-path)))

(create-files "Test")