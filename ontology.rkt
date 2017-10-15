#lang racket

(require racket/trace)

(define ontology '(Entity #f
                          #:requires identifier-factory
                          #:requires name-factory
                          (Document #f
                                    (Article #f
                                             #:field (title . simple-title)
                                             (JournalArticle #t #:alpha 1)
                                             (NewspaperArticle #t #:alpha 1)
                                             (MagazineArticle #t #:alpha 1)
                                             )
                                    (Book #t)
                                    (Chapter #t)
                                    (Collection #f)
                                    (Legal #f
                                           (CourtDecision #f
                                                          (USCourt #f
                                                                   (FederalCourt #f
                                                                                 (USSupremeCourt #t))
                                                                   (StateCourt #f))
                                                          (FrenchCourt #f
                                                                       ("CourDeCassation" #t)
                                                                       (TribunalCorrectionnel #t)))
                                           (PublicRecord #f
                                                         (BirthCertificate #t)
                                                         (DeathCertificate #t)
                                                         (MarriageCertificate #t)
                                                         )
                                           (Patent #f))
                                    (Report #f)
                                    (Unpublished #f))
                          (Person #f
                                  (NaturalPerson #t)
                                  (LegalPerson #t)
                                  )
                          (Journal #f
                                   (ScientificJournal #t)
                                   (Magazine #t)
                                   (Newspaper #t))))

(define (collect-arguments remaining [collected '()])
  (if (and (not (empty? remaining)) (keyword? (car remaining)))
      (collect-arguments (cddr remaining)
                         (cons `(,(car remaining) . ,(cadr remaining)) collected))
      (values (reverse collected) remaining)))

(define (transform ontology)
  (let*-values ([(name) (first ontology)]
                [(concrete) (second ontology)]
                [(args rest) (collect-arguments (cddr ontology))])
    (string-append
     (make-node name concrete args)
     (make-edges name (map car rest))
     (if (not (empty? rest))
         (apply string-append
                (map transform rest)) ""))))


(define (make-node name concrete args)
  (format
   (if concrete
       "\t~s [shape=box,style=rounded,label=~a]\n"
       "\t~s [shape=box,style=filled,fillcolor=lightgray,label=~a]\n")
   name
   (make-node-label name args)))

(define (make-edges from to)
  (apply string-append
         (map (lambda (t) (format "\t~s -> ~s\n" from t)) to)))

(define (make-node-label name args)
  (string-append
   (format "<<TABLE BORDER=\"0\"><TR><TD COLSPAN=\"2\"><B>~s</B></TD></TR>" name)
   (apply string-append
          (map (lambda (a) (format "<TR><TD ALIGN=\"right\">~s</TD><TD ALIGN=\"left\">~s</TD></TR>" (car a) (cdr a))) args))
   "</TABLE>>"))

(display (format "digraph N {\n ~a}\n"
                 (transform ontology)))
