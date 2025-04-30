;; code-nest-reviews
;; 
;; This contract implements a decentralized code review system for the Code Nest platform.
;; It allows developers to submit code for review and reviewers to provide structured feedback
;; across multiple dimensions. The contract maintains permanent, immutable records of all
;; review activity, enabling developers to build verifiable portfolios of their work and
;; review contributions.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-RATING (err u101))
(define-constant ERR-REVIEW-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-REVIEWED (err u103))
(define-constant ERR-SUBMISSION-NOT-FOUND (err u104))
(define-constant ERR-RATING-OUT-OF-RANGE (err u105))
(define-constant ERR-SELF-REVIEW (err u106))
(define-constant ERR-DUPLICATE-SUBMISSION (err u107))

;; Data definitions

;; Submission map: tracks code submissions awaiting review
;; Maps submission ID to submission details
(define-map submissions
  { submission-id: uint }
  {
    developer: principal,
    repo-url: (string-utf8 256),
    commit-hash: (string-utf8 64),
    title: (string-utf8 100),
    description: (string-utf8 1000),
    timestamp: uint,
    reviewed: bool
  }
)

;; Review map: stores review details
;; Maps review ID to review information
(define-map reviews
  { review-id: uint }
  {
    submission-id: uint,
    reviewer: principal,
    code-quality-rating: uint,
    documentation-rating: uint,
    efficiency-rating: uint,
    maintainability-rating: uint,
    feedback: (string-utf8 2000),
    timestamp: uint
  }
)

;; User review stats: tracks review activity per user
(define-map user-review-stats
  { user: principal }
  {
    reviews-submitted: uint,
    reviews-received: uint,
    average-code-quality: uint,
    average-documentation: uint,
    average-efficiency: uint,
    average-maintainability: uint,
    total-ratings-received: uint
  }
)

;; Submission to reviews mapping: helps track all reviews for a submission
(define-map submission-reviews
  { submission-id: uint }
  { review-ids: (list 100 uint) }
)

;; User to submissions mapping: tracks all submissions by a user
(define-map user-submissions
  { user: principal }
  { submission-ids: (list 100 uint) }
)

;; User to reviews mapping: tracks all reviews by a user
(define-map user-reviews
  { user: principal }
  { review-ids: (list 100 uint) }
)

;; Counters for IDs
(define-data-var submission-id-counter uint u0)
(define-data-var review-id-counter uint u0)

;; Private functions

;; Initialize or get user stats if they don't exist
(define-private (get-or-init-user-stats (user principal))
  (match (map-get? user-review-stats { user: user })
    stats stats
    {
      reviews-submitted: u0,
      reviews-received: u0,
      average-code-quality: u0,
      average-documentation: u0,
      average-efficiency: u0,
      average-maintainability: u0,
      total-ratings-received: u0
    }
  )
)

;; Update average ratings for a user
(define-private (update-user-averages (user principal) (code-quality uint) (documentation uint) 
               (efficiency uint) (maintainability uint))
  (let
    (
      (user-stats (get-or-init-user-stats user))
      (total-received (+ (get total-ratings-received user-stats) u1))
      (new-code-quality (/ (+ (* (get average-code-quality user-stats) 
                              (get total-ratings-received user-stats)) 
                           code-quality) 
                         total-received))
      (new-documentation (/ (+ (* (get average-documentation user-stats) 
                              (get total-ratings-received user-stats)) 
                           documentation) 
                         total-received))
      (new-efficiency (/ (+ (* (get average-efficiency user-stats) 
                              (get total-ratings-received user-stats)) 
                           efficiency) 
                         total-received))
      (new-maintainability (/ (+ (* (get average-maintainability user-stats) 
                              (get total-ratings-received user-stats)) 
                           maintainability) 
                         total-received))
    )
    (map-set user-review-stats
      { user: user }
      {
        reviews-submitted: (get reviews-submitted user-stats),
        reviews-received: (+ (get reviews-received user-stats) u1),
        average-code-quality: new-code-quality,
        average-documentation: new-documentation,
        average-efficiency: new-efficiency,
        average-maintainability: new-maintainability,
        total-ratings-received: total-received
      }
    )
  )
)

;; Add review ID to submission's review list
(define-private (add-review-to-submission (submission-id uint) (review-id uint))
  (let
    (
      (current-reviews (default-to { review-ids: (list) } 
                        (map-get? submission-reviews { submission-id: submission-id })))
    )
    (map-set submission-reviews
      { submission-id: submission-id }
      { review-ids: (append (get review-ids current-reviews) review-id) }
    )
  )
)

;; Add submission ID to user's submission list
(define-private (add-submission-to-user-list (user principal) (submission-id uint))
  (let
    (
      (current-submissions (default-to { submission-ids: (list) } 
                           (map-get? user-submissions { user: user })))
    )
    (map-set user-submissions
      { user: user }
      { submission-ids: (append (get submission-ids current-submissions) submission-id) }
    )
  )
)

;; Add review ID to user's review list
(define-private (add-review-to-user-list (user principal) (review-id uint))
  (let
    (
      (current-reviews (default-to { review-ids: (list) } 
                       (map-get? user-reviews { user: user })))
    )
    (map-set user-reviews
      { user: user }
      { review-ids: (append (get review-ids current-reviews) review-id) }
    )
  )
)

;; Update reviewer stats after submitting review
(define-private (increment-reviewer-stats (reviewer principal))
  (let
    (
      (reviewer-stats (get-or-init-user-stats reviewer))
    )
    (map-set user-review-stats
      { user: reviewer }
      (merge reviewer-stats { reviews-submitted: (+ (get reviews-submitted reviewer-stats) u1) })
    )
  )
)

;; Validate rating is within range (1-10)
(define-private (validate-rating (rating uint))
  (and (>= rating u1) (<= rating u10))
)

;; Public functions

;; Submit code for review
(define-public (submit-code (repo-url (string-utf8 256)) (commit-hash (string-utf8 64)) 
                           (title (string-utf8 100)) (description (string-utf8 1000)))
  (let
    (
      (current-id (var-get submission-id-counter))
      (next-id (+ current-id u1))
      (developer tx-sender)
    )
    ;; Create new submission record
    (map-set submissions
      { submission-id: current-id }
      {
        developer: developer,
        repo-url: repo-url,
        commit-hash: commit-hash,
        title: title,
        description: description,
        timestamp: block-height,
        reviewed: false
      }
    )
    
    ;; Update user's submission list
    (add-submission-to-user-list developer current-id)
    
    ;; Increment submission counter
    (var-set submission-id-counter next-id)
    
    ;; Return success with submission ID
    (ok current-id)
  )
)

;; Submit a review for a code submission
(define-public (submit-review (submission-id uint) (code-quality-rating uint) 
                             (documentation-rating uint) (efficiency-rating uint) 
                             (maintainability-rating uint) (feedback (string-utf8 2000)))
  (let
    (
      (reviewer tx-sender)
      (submission (map-get? submissions { submission-id: submission-id }))
    )
    ;; Check if submission exists
    (asserts! (is-some submission) ERR-SUBMISSION-NOT-FOUND)
    
    (let
      (
        (submission-data (unwrap! submission ERR-SUBMISSION-NOT-FOUND))
        (developer (get developer submission-data))
      )
      ;; Prevent self-review
      (asserts! (not (is-eq reviewer developer)) ERR-SELF-REVIEW)
      
      ;; Validate ratings
      (asserts! (validate-rating code-quality-rating) ERR-RATING-OUT-OF-RANGE)
      (asserts! (validate-rating documentation-rating) ERR-RATING-OUT-OF-RANGE)
      (asserts! (validate-rating efficiency-rating) ERR-RATING-OUT-OF-RANGE)
      (asserts! (validate-rating maintainability-rating) ERR-RATING-OUT-OF-RANGE)
      
      (let
        (
          (current-id (var-get review-id-counter))
          (next-id (+ current-id u1))
        )
        ;; Create review record
        (map-set reviews
          { review-id: current-id }
          {
            submission-id: submission-id,
            reviewer: reviewer,
            code-quality-rating: code-quality-rating,
            documentation-rating: documentation-rating,
            efficiency-rating: efficiency-rating,
            maintainability-rating: maintainability-rating,
            feedback: feedback,
            timestamp: block-height
          }
        )
        
        ;; Mark submission as reviewed
        (map-set submissions
          { submission-id: submission-id }
          (merge submission-data { reviewed: true })
        )
        
        ;; Update review tracking
        (add-review-to-submission submission-id current-id)
        (add-review-to-user-list reviewer current-id)
        
        ;; Update user stats
        (update-user-averages 
          developer 
          code-quality-rating 
          documentation-rating 
          efficiency-rating 
          maintainability-rating
        )
        (increment-reviewer-stats reviewer)
        
        ;; Increment review counter
        (var-set review-id-counter next-id)
        
        ;; Return success with review ID
        (ok current-id)
      )
    )
  )
)

;; Read-only functions

;; Get submission details
(define-read-only (get-submission (submission-id uint))
  (map-get? submissions { submission-id: submission-id })
)

;; Get review details
(define-read-only (get-review (review-id uint))
  (map-get? reviews { review-id: review-id })
)

;; Get all reviews for a submission
(define-read-only (get-submission-reviews (submission-id uint))
  (map-get? submission-reviews { submission-id: submission-id })
)

;; Get all submissions by a user
(define-read-only (get-user-submissions (user principal))
  (map-get? user-submissions { user: user })
)

;; Get all reviews by a user
(define-read-only (get-user-reviews (user principal))
  (map-get? user-reviews { user: user })
)

;; Get user review statistics
(define-read-only (get-user-stats (user principal))
  (map-get? user-review-stats { user: user })
)

;; Get total number of submissions
(define-read-only (get-total-submissions)
  (var-get submission-id-counter)
)

;; Get total number of reviews
(define-read-only (get-total-reviews)
  (var-get review-id-counter)
)