;; ==========================================================================
;; code-nest-reputation
;; ==========================================================================
;; A reputation tracking system for the Code Nest platform
;; 
;; This contract builds a comprehensive reputation system for developers based 
;; on their platform activities. It calculates reputation scores from completed
;; session feedback, code review ratings, and platform participation.
;; The system tracks expertise by language, framework, and domain to create
;; specialized reputation profiles that are tamper-proof and portable.
;; ==========================================================================

;; ==========================================================================
;; Error codes
;; ==========================================================================
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-USER-ALREADY-EXISTS (err u101))
(define-constant ERR-USER-NOT-FOUND (err u102))
(define-constant ERR-INVALID-CATEGORY (err u103))
(define-constant ERR-INVALID-RATING (err u104))
(define-constant ERR-INVALID-PARAMETERS (err u105))
(define-constant ERR-SKILL-NOT-FOUND (err u106))
(define-constant ERR-SESSION-NOT-FOUND (err u107))
(define-constant ERR-REVIEW-NOT-FOUND (err u108))
(define-constant ERR-ALREADY-RATED (err u109))

;; ==========================================================================
;; Constants
;; ==========================================================================
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-RATING u10)
(define-constant MIN-RATING u1)
(define-constant CATEGORY-LANGUAGE u1)
(define-constant CATEGORY-FRAMEWORK u2)
(define-constant CATEGORY-DOMAIN u3)

;; ==========================================================================
;; Data maps and variables
;; ==========================================================================

;; Track basic user profile and overall reputation score
(define-map user-profiles
  { user: principal }
  {
    overall-score: uint,
    sessions-completed: uint,
    reviews-completed: uint,
    endorsements-received: uint,
    creation-height: uint
  }
)

;; Track specific skills and associated reputation in each category
(define-map user-skills
  { user: principal, skill-name: (string-ascii 64), category: uint }
  {
    rating: uint,
    endorsements: uint,
    last-updated: uint
  }
)

;; Tracks feedback from coding sessions
(define-map session-ratings
  { session-id: uint, rater: principal }
  {
    rated-user: principal,
    rating: uint,
    feedback: (string-utf8 500),
    skills: (list 10 (string-ascii 64)),
    timestamp: uint
  }
)

;; Tracks feedback from code reviews
(define-map review-ratings
  { review-id: uint, rater: principal }
  {
    rated-user: principal,
    rating: uint,
    feedback: (string-utf8 500),
    skills: (list 10 (string-ascii 64)),
    timestamp: uint
  }
)

;; System-wide statistics
(define-data-var total-users uint u0)
(define-data-var total-ratings uint u0)

;; ==========================================================================
;; Private functions
;; ==========================================================================

;; Check if a user exists in the system
(define-private (user-exists (user principal))
  (is-some (map-get? user-profiles { user: user }))
)

;; Check if a category is valid
(define-private (is-valid-category (category uint))
  (or
    (is-eq category CATEGORY-LANGUAGE)
    (is-eq category CATEGORY-FRAMEWORK)
    (is-eq category CATEGORY-DOMAIN)
  )
)

;; Validates a rating is within acceptable range
(define-private (is-valid-rating (rating uint))
  (and (>= rating MIN-RATING) (<= rating MAX-RATING))
)

;; Initialize a new user profile with default values
(define-private (init-user (user principal))
  (map-set user-profiles
    { user: user }
    {
      overall-score: u0,
      sessions-completed: u0,
      reviews-completed: u0,
      endorsements-received: u0,
      creation-height: block-height
    }
  )
)

;; Update a user's overall score based on new activity
(define-private (update-overall-score (user principal))
  (let (
    (profile (unwrap! (map-get? user-profiles { user: user }) ERR-USER-NOT-FOUND))
    (sessions (get sessions-completed profile))
    (reviews (get reviews-completed profile))
    (endorsements (get endorsements-received profile))
    
    ;; Calculate new score - formula can be adjusted as needed
    (new-score (+ (* sessions u2) (* reviews u3) (* endorsements u1)))
  )
    (map-set user-profiles
      { user: user }
      (merge profile { overall-score: new-score })
    )
    (ok new-score)
  )
)

;; ==========================================================================
;; Read-only functions
;; ==========================================================================

;; Get a user's full profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

;; Get a user's specific skill rating
(define-read-only (get-skill-rating (user principal) (skill-name (string-ascii 64)) (category uint))
  (map-get? user-skills { user: user, skill-name: skill-name, category: category })
)

;; Get all skills for a user (only returns in Clarity 2.0+)
;; For Clarity 1.0, this would need to be replaced with specific skills queries
(define-read-only (get-user-skills-by-category (user principal) (category uint))
  (ok category) ;; Placeholder - in Clarity 2.0 would use map functionality to return all skills
)

;; Get session rating details
(define-read-only (get-session-rating (session-id uint) (rater principal))
  (map-get? session-ratings { session-id: session-id, rater: rater })
)

;; Get review rating details
(define-read-only (get-review-rating (review-id uint) (rater principal))
  (map-get? review-ratings { review-id: review-id, rater: rater })
)

;; Get system stats
(define-read-only (get-system-stats)
  {
    total-users: (var-get total-users),
    total-ratings: (var-get total-ratings)
  }
)

;; ==========================================================================
;; Public functions
;; ==========================================================================

;; Register a new user in the system
(define-public (register-user)
  (let ((user tx-sender))
    (asserts! (not (user-exists user)) ERR-USER-ALREADY-EXISTS)
    
    (init-user user)
    (var-set total-users (+ (var-get total-users) u1))
    
    (ok true)
  )
)

;; Add or update a skill for the current user
(define-public (add-skill (skill-name (string-ascii 64)) (category uint) (initial-rating uint))
  (let ((user tx-sender))
    ;; Validate inputs
    (asserts! (user-exists user) ERR-USER-NOT-FOUND)
    (asserts! (is-valid-category category) ERR-INVALID-CATEGORY)
    (asserts! (is-valid-rating initial-rating) ERR-INVALID-RATING)
    
    ;; Add or update the skill
    (map-set user-skills
      { user: user, skill-name: skill-name, category: category }
      {
        rating: initial-rating,
        endorsements: u0,
        last-updated: block-height
      }
    )
    
    (ok true)
  )
)

;; Submit a rating for a completed coding session
(define-public (rate-session (session-id uint) (rated-user principal) (rating uint) (feedback (string-utf8 500)) (skills (list 10 (string-ascii 64))))
  (let ((rater tx-sender))
    ;; Validate inputs
    (asserts! (user-exists rater) ERR-USER-NOT-FOUND)
    (asserts! (user-exists rated-user) ERR-USER-NOT-FOUND)
    (asserts! (is-valid-rating rating) ERR-INVALID-RATING)
    (asserts! (not (is-some (map-get? session-ratings { session-id: session-id, rater: rater }))) ERR-ALREADY-RATED)
    
    ;; Store the session rating
    (map-set session-ratings
      { session-id: session-id, rater: rater }
      {
        rated-user: rated-user,
        rating: rating,
        feedback: feedback,
        skills: skills,
        timestamp: block-height
      }
    )
    
    ;; Update rated user's profile
    (let ((profile (unwrap! (map-get? user-profiles { user: rated-user }) ERR-USER-NOT-FOUND)))
      (map-set user-profiles
        { user: rated-user }
        (merge profile { sessions-completed: (+ (get sessions-completed profile) u1) })
      )
    )
    
    ;; Update skill ratings for each skill mentioned
    (map update-skill-from-rating rated-user skills rating)
    
    ;; Increment total ratings counter
    (var-set total-ratings (+ (var-get total-ratings) u1))
    
    ;; Update overall score
    (try! (update-overall-score rated-user))
    
    (ok true)
  )
)

;; Helper to update a skill based on session/review rating
(define-private (update-skill-from-rating (user principal) (skill-name (string-ascii 64)) (new-rating uint))
  ;; For simplicity, we'll assume these are language skills - a real implementation would track categories
  (let (
    (skill-key { user: user, skill-name: skill-name, category: CATEGORY-LANGUAGE })
    (existing-skill (map-get? user-skills skill-key))
  )
    (if (is-some existing-skill)
      (let (
        (unwrapped-skill (unwrap! existing-skill ERR-SKILL-NOT-FOUND))
        (current-rating (get rating unwrapped-skill))
        ;; Calculate new rating with weighted average - 70% old rating, 30% new rating
        (updated-rating (+ (* current-rating u7) (* new-rating u3)))
        (final-rating (/ updated-rating u10))
      )
        (map-set user-skills
          skill-key
          (merge unwrapped-skill { 
            rating: final-rating,
            last-updated: block-height
          })
        )
      )
      ;; If skill doesn't exist, create it
      (map-set user-skills
        skill-key
        {
          rating: new-rating,
          endorsements: u0,
          last-updated: block-height
        }
      )
    )
    true
  )
)

;; Submit a rating for a completed code review
(define-public (rate-review (review-id uint) (rated-user principal) (rating uint) (feedback (string-utf8 500)) (skills (list 10 (string-ascii 64))))
  (let ((rater tx-sender))
    ;; Validate inputs
    (asserts! (user-exists rater) ERR-USER-NOT-FOUND)
    (asserts! (user-exists rated-user) ERR-USER-NOT-FOUND)
    (asserts! (is-valid-rating rating) ERR-INVALID-RATING)
    (asserts! (not (is-some (map-get? review-ratings { review-id: review-id, rater: rater }))) ERR-ALREADY-RATED)
    
    ;; Store the review rating
    (map-set review-ratings
      { review-id: review-id, rater: rater }
      {
        rated-user: rated-user,
        rating: rating,
        feedback: feedback,
        skills: skills,
        timestamp: block-height
      }
    )
    
    ;; Update rated user's profile
    (let ((profile (unwrap! (map-get? user-profiles { user: rated-user }) ERR-USER-NOT-FOUND)))
      (map-set user-profiles
        { user: rated-user }
        (merge profile { reviews-completed: (+ (get reviews-completed profile) u1) })
      )
    )
    
    ;; Update skill ratings for each skill mentioned
    (map update-skill-from-rating rated-user skills rating)
    
    ;; Increment total ratings counter
    (var-set total-ratings (+ (var-get total-ratings) u1))
    
    ;; Update overall score
    (try! (update-overall-score rated-user))
    
    (ok true)
  )
)

;; Endorse a user for a specific skill
(define-public (endorse-skill (endorsed-user principal) (skill-name (string-ascii 64)) (category uint))
  (let ((endorser tx-sender))
    ;; Validate inputs
    (asserts! (user-exists endorser) ERR-USER-NOT-FOUND)
    (asserts! (user-exists endorsed-user) ERR-USER-NOT-FOUND)
    (asserts! (is-valid-category category) ERR-INVALID-CATEGORY)
    (asserts! (not (is-eq endorser endorsed-user)) ERR-INVALID-PARAMETERS)
    
    ;; Verify the skill exists
    (let (
      (skill-key { user: endorsed-user, skill-name: skill-name, category: category })
      (existing-skill (unwrap! (map-get? user-skills skill-key) ERR-SKILL-NOT-FOUND))
    )
      ;; Update skill with new endorsement
      (map-set user-skills
        skill-key
        (merge existing-skill { 
          endorsements: (+ (get endorsements existing-skill) u1),
          last-updated: block-height
        })
      )
      
      ;; Update user's overall endorsement count
      (let ((profile (unwrap! (map-get? user-profiles { user: endorsed-user }) ERR-USER-NOT-FOUND)))
        (map-set user-profiles
          { user: endorsed-user }
          (merge profile { endorsements-received: (+ (get endorsements-received profile) u1) })
        )
      )
      
      ;; Update overall score
      (try! (update-overall-score endorsed-user))
      
      (ok true)
    )
  )
)

;; Administrative function to reset a user's data (only contract owner can call)
(define-public (admin-reset-user (user principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (user-exists user) ERR-USER-NOT-FOUND)
    
    ;; Reset user's profile
    (map-set user-profiles
      { user: user }
      {
        overall-score: u0,
        sessions-completed: u0,
        reviews-completed: u0,
        endorsements-received: u0,
        creation-height: block-height
      }
    )
    
    (ok true)
  )
)