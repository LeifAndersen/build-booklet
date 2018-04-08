#lang racket/base

#|
   Copyright 2018 Leif Andersen

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
|#

(require racket/gui/base
         racket/class
         racket/draw
         racket/list
         racket/string
         racket/path
         pict
         racket/pretty
         mrlib/path-dialog)

(define (collect-images path)
  (for/list ([i (in-list (directory-list path #:build? #t))]
             #:when (let ()
                      (define the-path (path-get-extension i))
                      (and the-path
                           (string-ci=? (bytes->string/locale the-path) ".png"))))
    i))

(define (images->pdf-order images)
  images)

(define (images->booklet-order forward-images)
  (define backward (reverse forward-images))
  (define steps (/ (length forward-images) 4))
  (let loop ([steps steps]
             [forward forward-images]
             [backward backward])
    (if (0 . >= . steps)
        '()
        (list* (first backward)
               (first forward)
               (second forward)
               (second backward)
               (loop (sub1 steps)
                     (rest (rest forward))
                     (rest (rest backward)))))))

(define (image-list->pdf images [output #f])

  (define-values (width height)
    (cond
      [(and (list? images)
            (not (null? images)))
       (define f (bitmap (first images)))
       (values (pict-width f) (pict-height f))]
      [else (values #f #f)]))
  
  (define default-ps (new ps-setup%))
  (send default-ps set-level-2 #t)
  (send default-ps set-margin 0 0)
  (send default-ps set-orientation 'portrait)
  (send default-ps set-scaling 1 1)
  (send default-ps set-translation 0 0)

  (parameterize ([current-ps-setup default-ps])
    (define p
      (new pdf-dc%
           [interactive #f]
           [as-eps #f]
           [width width]
           [height height]
           [output output]))
    (send p start-doc "Generating")
    (for ([i (in-list images)])
      (send p start-page)
      (define b (make-object bitmap% i))
      (send p draw-bitmap b 0 0)
      (send p end-page))
    (send p end-doc)))

(define (do-render folder order)
  (image-list->pdf (order (collect-images folder))))

(define pdf-orderer%
  (class frame%
    (super-new [label "Generate PDFs"])

    (define image-folder #f)
    
    (define row1 (new horizontal-panel% [parent this]
                      [alignment '(center center)]))
    (define load-button
      (new button% [parent row1]
           [label "Load Images"]
           [callback (λ (b e)
                       (define new-folder
                         (get-directory))
                       (set! image-folder new-folder)
                       (unless (modulo (length (collect-images new-folder)) 4)
                         (set! image-folder #f)
                         (message-box "Invalid Folder"
                                      "Images not a multiple of four."))
                       (if image-folder
                           (send file-label set-label
                                 (format "Current Location: ~a" image-folder))
                           (send file-label set-label
                                 "No Loaded File")))]))
    (define file-label
      (new message% [parent row1]
           [label "No Loaded File"]
           [auto-resize #t]))
    (define row2 (new horizontal-panel% [parent this]))
    (define screen-button
      (new button% [parent row2]
           [label "Generate Screen PDF"]
           [callback (λ (b e)
                       (do-render image-folder images->pdf-order))]))
    (define booklet-button
      (new button% [parent row2]
           [label "Generate Booklet PDF"]
           [callback (λ (b e)
                       (do-render image-folder images->booklet-order))]))))

;(image-list->pdf (images->booklet-order (collect-images "Pencils")))

(send (new pdf-orderer%) show #t)
