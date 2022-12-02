;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 2017S-F-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require spd/tags)

(@problem 1)
;; Write partial data definitions to represent the information below. You must
;; write the define-struct (if necessary), type comments, and interpretation for
;; each data definition. Do not write examples or templates.
;;
;; Imagine you are building a system to track your chain of pet kennels (a place
;; for people to leave their pets while on holidays). Each pet kennel has a 
;; branch ID (e.g. "AG87") and is a certain square footage in size and holds 
;; cats and dogs. Each dog has a name, a species, and is either vicious or not.
;; Each cat has a name and a species.












































(@problem 2)
;; Note that this problem is broken into four parts. Read all four parts before
;; you start.
;;
;; You are provided with a data definition for a Contact, the helper function 
;; match? and the signature, purpose and stub for the function get-emails.
;; The get-emails function consumes a list of contacts and a last
;; name, and produces a list of the email addresses of only those contacts with
;; the specified last name.
;; You will complete the function design in 4 different ways in Parts A-D.
;;
;; Remember that for all of your function designs (Parts A - D), you will need
;; to include an origin of template comment and the function definition. If you
;; use accumulators in your function definition, you will need to include the
;; types and invariants for each accumulator.


;; =================
;; Data Definitions

(@htdd Contact)
(define-struct contact (fn ln email))
;; Contact is (make-contact String String String)
;; interp. a contact with first name (fn),
;;         last name (ln) and email address (email)
(define C-AB (make-contact "Adel" "Beller" "adeleb@yahoo.com"))
(define C-BZ (make-contact "Bryn" "Zain" "bryz@hotmail.com"))
(define C-BZ2 (make-contact "Bryne" "Zain" "bryze@hotmail.com"))

#;
(define (fn-for-contact c)
  (... (contact-fn c)
       (contact-ln c)
       (contact-email c)))



;; =================
;; Functions

(@htdf match?)
(@signature Contact String -> Boolean)
;; produce true if c has last name lnm
(check-expect (match? C-AB "Beller") true)
(check-expect (match? C-AB "Zain") false)

(@template Contact)

(define (match? c lnm)
  (string=? (contact-ln c) lnm))


(@htdf get-emails)
(@signature (listof Contact) String -> (listof String))
;; produce a list of the email addresses of contacts from loc with lnm
(check-expect (get-emails empty "Zain") empty)
(check-expect (get-emails (list C-BZ C-AB C-BZ2) "Zain")
              (list "bryz@hotmail.com" "bryze@hotmail.com"))

(define (get-emails loc lnm) empty)









;; Problem 2 (PART A)
;;
;; Write a non-tail recursive function.
;; Do not use function composition or built-in abstract functions.


























;; Problem 2 (PART B)
;;
;; Write a new version of the function definition using only built-in abstract
;; list functions.

























;; Problem 2 (PART C)
;;
;; Write a tail recursive function.
;; Do not use function composition or built-in abstract functions.























































;; Problem 2 (PART D)
;;
;; Write a new version of the function definition using for-each.























































(@problem 3)
;; We have provided partial data definitions to represent airports, with the 
;; template left out. You must construct the template in your function design,
;; but you do not need to include a blank template in the data definitions. 
;;
;; The example data for the Airport data definition is based on the following
;; information (use the link below to view):
;;
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/17sfairprt.png
;;
;; Design a tail recursive function that takes an Airport and a country name,
;; and produces a list of airport codes of all reachable airports in that
;; country.
;;
;; For example, all the United Kingdom airports that are reachable from YYZ,
;; are "LPL" and "LHR".


;; =================
;; Data Definitions

(@htdd Airport)
(define-struct airport (code country dests))
;; Airport is (make-airport String String (listof Airport))
;; interp. code of airport, country name that airport is in,
;;         dests is the list of airports you can fly directly to

(define AIRWAYS
  (shared [(-YVR- (make-airport "YVR" "Canada" (list -SFO- -YYZ-)))
           (-SFO- (make-airport "SFO" "United States" (list -SHA- -SYD-)))
           (-SHA- (make-airport "SHA" "China" (list -SYD-)))
           (-SYD- (make-airport "SYD" "Australia" (list -SHA-)))
           (-YYZ- (make-airport "YYZ" "Canada" (list -YVR- -LHR-)))
           (-LHR- (make-airport "LHR" "United Kingdom"
                                (list -YYZ- -SYD- -LPL-)))
           (-LPL- (make-airport "LPL" "United Kingdom" (list -LHR-)))
           (-GLA- (make-airport "GLA" "United Kingdom" (list -LPL- -LHR-)))]
    (list -YVR- -SFO- -SHA- -SYD- -YYZ- -LHR- -LPL- -GLA-)))

(define YVR (list-ref AIRWAYS 0))
(define SFO (list-ref AIRWAYS 1))
(define SHA (list-ref AIRWAYS 2))
(define SYD (list-ref AIRWAYS 3))
(define YYZ (list-ref AIRWAYS 4))
(define LHR (list-ref AIRWAYS 5))
(define LPL (list-ref AIRWAYS 6))
(define GLA (list-ref AIRWAYS 7))



;; =================
;; Functions




























































































































(@problem 4)
;; Design a function that consumes a natural, and produces the sum of the 
;; squares from 0 to n inclusive.  The signature, stub and check-expects are
;; provided for you.
;;
;;
;; You must use either built-in abstract functions or the for-each template to 
;; write this function. You MUST NOT use the Natural template resembling the
;; list template in your function design.

(@htdf sum-squares)
(@signature Natural -> Natural)
;; produce the sum of the squares from 0 to n inclusive
(check-expect (sum-squares 0) 0)
(check-expect (sum-squares 3) (+ 0 (* 1 1) (* 2 2) (* 3 3)))

(define (sum-squares n) 0)








































(@problem 5)
;; Recall the data definition for Paper in Problem Set 10 that represents the 
;; following information (use link to view):
;;
;; https://s3.amazonaws.com/edx-course-spdx-kiczales/HTC/problems/17sfpaper.png
;;
;; Design an abstract fold function for Paper.
;; You must include the following two check-expects:
;; - one that produces a copy of the given paper
;; - one that counts the number of citations in all the papers referenced, and 
;;   their papers referenced etc., including itself
;; 
;; We have provided you with a copy of the template on the next page.
;; Make your changes directly on this template.


;; =================
;; Data Definitions

(@htdd Paper)
(define-struct paper (ttl cite refs))
;; Paper is (make-paper String Natural (listof Paper))
;; interp. a paper with title (ttl),
;;         the number of times the paper has been cited (cite),
;;         the other papers it is referencing

(define ELECTRO (make-paper "Electrocatalytic and homogeneous approaches to
                             conversion of CO2 to liquid fuels"
                            345 empty))

(define POISON (make-paper "Poisoning and activation of the gold cathode during
                            electroreduction of CO2"
                           55 empty))

(define PWR-PLANET (make-paper "Powering the planet: Chemical challenges in
                                solar energy utilization"
                               4091 empty))
(define PHOTOCHEM (make-paper "Photochemical and photoelectrochemical reduction
                               of CO2"
                              89
                              (list ELECTRO POISON)))
(define SOLAR-FUELS (make-paper "Making solar fuels by artificial
                                 photosynthesis"
                                192 empty))

(define SOLAR (make-paper "Solar photothermochemical alkane reverse combustion"
                          115
                          (list PWR-PLANET PHOTOCHEM SOLAR-FUELS)))

#;
(define (fn-for-paper p)
  (local [(define (fn-for-paper p)
            (... (paper-ttl p)
                 (paper-cite p)
                 (fn-for-lop (paper-refs p))))
          
          (define (fn-for-lop lop)
            (cond [(empty? lop) (...)]
                  [else
                   (... (fn-for-paper (first lop))
                        (fn-for-lop (rest lop)))]))]
    (fn-for-paper p)))



;; =================
;; Functions








































(define (fn-for-paper        p)
  
  (local [(define (fn-for-paper p)
            
            (... (paper-ttl p)
                 
                 (paper-cite p)
                 
                 (fn-for-lop (paper-refs p))))
          
          (define (fn-for-lop lop)
            
            (cond [(empty? lop) (...)]
                  
                  [else
                   
                   (... (fn-for-paper (first lop))
                        
                        (fn-for-lop (rest lop)))]))]
    
    (fn-for-paper p)))


(@problem 6)
;; Setup -- Recall the following data definitions from Midterm 2:


;; =================
;; Data Definitions

(@htdd Flow)
;; Flow is one of:
;; - false
;; - (listof River)
;; interp. a river flow, false if river is dried up or
;;         a list of Rivers that flow into it


(@htdd River)
(define-struct river (nm fl lost))
;; River is (make-river String Flow (listof String))
;; interp. a River with name (nm), flow (fl) and
;;         a list of at least one state it goes through (lost)
(define NO-FLOW false)
(define R-GR (make-river "Grand" NO-FLOW
                         (list "South Dakota")))
(define R-LM (make-river "Little Missouri" NO-FLOW
                         (list "Wyoming" "Montana" "South Dakota" "North Dakota"
                               "Missouri")))
(define R-CHY (make-river "Cheyenne" empty
                          (list "Wyoming" "South Dakota")))
(define R-IL (make-river "Illinois" NO-FLOW
                         (list "Indiana" "Illinois")))
(define R-OHIO (make-river "Ohio" empty
                           (list "Pennsylvania" "Ohio" "West Virginia" "Indiana"
                                 "Kentucky" "Illinois")))
(define R-RED (make-river "Red River" empty
                          (list "Texas" "Oklahoma" "Arkansas" "Louisiana")))
(define R-MSR (make-river "Missouri" (list R-CHY R-LM R-GR)
                          (list "Montana" "South Dakota" "North Dakota"
                                "Nebraska" "Iowa" "Kansas" "Missouri"
                                "Illinois")))
(define R-MSSP (make-river "Mississippi" (list R-MSR R-RED R-OHIO R-IL)
                           (list "Minnesota" "Iowa" "Missouri" "Wisconsin"
                                 "Illinois" "Arkansas" "Tennessee" "Louisiana"
                                 "Mississippi")))
#;
(define (fn-for-river r)
  (local[(define (fn-for-flow f)
           (cond [(false? f) (...)]
                 [else
                  (... (fn-for-lor f))]))
         
         (define (fn-for-river r)
           (... (river-nm r)
                (fn-for-flow (river-fl r))
                (fn-for-lost (river-lost r))))
         
         (define (fn-for-lor lor)
           (cond [(empty? lor) (...)]
                 [else
                  (... (fn-for-river (first lor))
                       (fn-for-lor (rest lor)))]))
         
         (define (fn-for-lost lost)
           (cond [(empty? lost) (...)]
                 [else
                  (... (first lost)
                       (fn-for-lost (rest lost)))]))]
    (fn-for-river r)))



;; =================
;; Functions

;; Refactor the function definitions to use built-in abstract functions wherever
;; possible. Cross out the function bodies of the functions that you are 
;; refactoring and neatly write the new function bodies beside the original 
;; (crossed out) ones. You cannot use the length function. Marks will be 
;; deducted for refactoring where not possible.


(@htdf count-rivers)
(@signature River -> Natural)
;; produce a count of the rivers flowing into r including r
(check-expect (count-rivers R-GR) 1)
(check-expect (count-rivers R-OHIO) 1)
(check-expect (count-rivers R-MSSP) 8)

(@template Flow River (listof River) encapsulated)

(define (count-rivers r)
  (local[(define (fn-for-flow f)
           (cond [(false? f) 1]
                 [else
                  (add1 (fn-for-lor f))]))
         
         (define (fn-for-river r)
           (fn-for-flow (river-fl r)))
         
         (define (fn-for-lor lor)
           (cond [(empty? lor) 0]
                 [else
                  (+ (fn-for-river (first lor))
                     (fn-for-lor (rest lor)))]))]
    (fn-for-river r)))


(@htdf flow-through)
(@signature River String -> (listof String))
;; produce a list of the rivers that flow through st-nm
;; ASSUME: a river flows through at least one state
(check-expect (flow-through R-GR "North Dakota") empty)
(check-expect (flow-through R-GR "South Dakota") (list "Grand"))
(check-expect (flow-through R-OHIO "Pennsylvania") (list "Ohio"))
(check-expect (flow-through R-MSSP "Missouri")
              (list "Mississippi" "Missouri" "Little Missouri"))

(@template Flow River (listof River) (listof String) encapsulated)

(define (flow-through r st-nm)
  (local[(define (fn-for-flow f)
           (cond [(false? f) empty]
                 [else
                  (fn-for-lor f)]))
         
         (define (fn-for-river r)
           (if (fn-for-lost (river-lost r))
               (cons (river-nm r) (fn-for-flow (river-fl r)))
               (fn-for-flow (river-fl r))))
         
         (define (fn-for-lor lor)
           (cond [(empty? lor) empty]
                 [else
                  (append (fn-for-river (first lor))
                          (fn-for-lor (rest lor)))]))
         
         (define (fn-for-lost lost)
           (cond [(empty? lost) false]
                 [else
                  (or (string=? st-nm (first lost))
                      (fn-for-lost (rest lost)))]))]
    (fn-for-river r)))
