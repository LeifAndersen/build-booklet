#lang racket/base

(require racket/gui/base
         racket/class
         racket/path
         racket/file
         file/zip
         images/icons/symbol
         pict)

(define (convert-to-cbz in-folder size scale-format out-file)
  (define out-folder (make-temporary-file "cbzizer~a" 'directory))
  (define smaller-images
    (for/list ([in-path (directory-list in-folder)]
               #:when (let ()
                        (define the-path (path-get-extension in-path))
                        (and the-path
                             (or
                              (string-ci=? (bytes->string/locale the-path) ".jpg")
                              (string-ci=? (bytes->string/locale the-path) ".png")))))
      (define the-extension (path-get-extension in-path))
      (define out-path (build-path out-folder (file-name-from-path in-path)))
      (let* ([a (bitmap (build-path in-folder in-path))]
             [a (case scale-format
                  [("%") (scale a (/ size 100))]
                  [("px") (scale-to-fit a size (* size 10) 'preserve)]
                  [else (error "Invalid format")])]
             [a (pict->bitmap a)])
        (send a save-file out-path
              (cond [(string-ci=? (bytes->string/locale the-extension) ".jpg") 'jpeg]
                    [(string-ci=? (bytes->string/locale the-extension) ".png") 'png])
              100)
        out-path)))
  (with-output-to-file out-file
    #:exists 'replace
    (λ ()
      (parameterize ([current-directory out-folder])
        (zip->output smaller-images)))))

(define cbz-maker%
  (class frame%
    (super-new [label "Make CBZ File"])
    
    (define image-folder #f)
    
    (define row1 (new horizontal-panel% [parent this]
                      [alignment '(center center)]))
    
    (define load-button
      (new button% [parent row1]
           [label "Load Images"]
           [callback (λ (b e)
                       (set! image-folder (get-directory))
                       (if image-folder
                           (send file-label set-label
                                 (format "Current Location: ~a" image-folder))
                           (send file-label set-label
                                 "No Loaded File")))]))
    (define file-label
      (new message% [parent row1]
           [label "No Folder"]
           [auto-resize #t]))
    
    (define row2 (new horizontal-panel% [parent this]))
    (define scale-amnt 100)
    (define scale-field
      (new text-field% [parent row2]
           [label "Width"]
           [init-value "100"]
           [callback (λ (f e)
                       (set! scale-amnt (string->number (send f get-value)))
                       (if scale-amnt
                           (send f set-field-background #f)
                           (send f set-field-background (make-object color% "red"))))]))
    (define scale-form
      (new radio-box% [parent row2]
           [label ""]
           [choices (list "%"
                          "px")]))

    (define row3 (new horizontal-panel% [parent this]
                      [alignment '(right center)]))
    (define status-label
      (new message% [parent row3]
           [stretchable-width #t]
           [label (pict->bitmap (blank 200 32))]))
    (define (set-status! status)
      (send status-label set-label
            (case status
              [(ok)
               (pict->bitmap (hc-append 10 (text "Success") (bitmap (check-icon))))]
              [(no-input)
               (pict->bitmap (hc-append 10 (text "No Input Folder") (bitmap (x-icon))))]
              [(invalid-scale)
               (pict->bitmap (hc-append 10 (text "Invalid Scale") (bitmap (x-icon))))]
              [(error)
               (pict->bitmap (hc-append 10 (text "ERROR!") (bitmap (x-icon))))]
              [else (pict->bitmap (blank))])))
    (new button% [parent row3]
         [label "Convert"]
         [callback (λ (b e)
                     (cond [(not image-folder)
                            (set-status! 'no-input)]
                           [(not scale-amnt)
                            (set-status! 'invalid-scale)]
                           [else
                            (define out-file (put-file "CBZ File" #f #f #f "cbz"))
                            (when out-file
                              (with-handlers ([exn:fail? (λ (e)
                                                           (set-status! 'error)
                                                           (raise e))])
                                (convert-to-cbz image-folder
                                                scale-amnt
                                                (send scale-form get-item-label
                                                      (send scale-form get-selection))
                                                out-file)
                                (set-status! 'ok)))]))])))


(send (new cbz-maker%) show #t)
