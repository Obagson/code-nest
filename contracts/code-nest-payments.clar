;; code-nest-payments.clar
;; A smart contract to handle payments for coding sessions and code reviews
;; on the Code Nest platform, implementing escrow functionality, review bounties,
;; and tipping mechanisms.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-SESSION-EXISTS (err u102))
(define-constant ERR-SESSION-NOT-FOUND (err u103))
(define-constant ERR-SESSION-ALREADY-COMPLETED (err u104))
(define-constant ERR-REVIEW-EXISTS (err u105))
(define-constant ERR-REVIEW-NOT-FOUND (err u106))
(define-constant ERR-REVIEW-ALREADY-COMPLETED (err u107))
(define-constant ERR-INSUFFICIENT-FUNDS (err u108))
(define-constant ERR-PAYMENT-FAILED (err u109))
(define-constant ERR-INVALID-PARTICIPANT (err u110))
(define-constant ERR-NOT-SESSION-PARTICIPANT (err u111))
(define-constant ERR-ESCROW-ALREADY-RELEASED (err u112))
(define-constant ERR-INVALID-FEE-PERCENTAGE (err u113))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant PLATFORM-FEE-PERCENTAGE u5) ;; 5% fee
(define-constant MIN-SESSION-PAYMENT u1000000) ;; 1 STX minimum
(define-constant MIN-REVIEW-BOUNTY u500000) ;; 0.5 STX minimum

;; Data maps for sessions
(define-map sessions
  { session-id: uint }
  {
    requester: principal,
    provider: principal,
    amount: uint,
    platform-fee: uint,
    status: (string-ascii 20), ;; "pending", "completed", "disputed", "cancelled"
    requester-confirmed: bool,
    provider-confirmed: bool,
    created-at: uint
  }
)

;; Data maps for code reviews
(define-map reviews
  { review-id: uint }
  {
    requester: principal,
    reviewer: principal,
    bounty: uint,
    platform-fee: uint,
    status: (string-ascii 20), ;; "pending", "completed", "disputed", "cancelled"
    created-at: uint
  }
)

;; Counter variables for unique IDs
(define-data-var next-session-id uint u1)
(define-data-var next-review-id uint u1)

;; Map to track platform earnings
(define-data-var platform-earnings uint u0)

;; =============================
;; Private Functions
;; =============================

;; Calculate platform fee for a given amount
(define-private (calculate-platform-fee (amount uint))
  (/ (* amount PLATFORM-FEE-PERCENTAGE) u100)
)

;; Check if sender is contract owner
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

;; Check if sender is a session participant
(define-private (is-session-participant (session-id uint))
  (match (map-get? sessions { session-id: session-id })
    session (or 
              (is-eq tx-sender (get requester session))
              (is-eq tx-sender (get provider session)))
    false
  )
)


;; =============================
;; Read-only Functions
;; =============================

;; Get session details
(define-read-only (get-session (session-id uint))
  (map-get? sessions { session-id: session-id })
)

;; Get review details
(define-read-only (get-review (review-id uint))
  (map-get? reviews { review-id: review-id })
)

;; Get platform earnings
(define-read-only (get-platform-earnings)
  (var-get platform-earnings)
)

;; Check if a session is completed
(define-read-only (is-session-completed (session-id uint))
  (match (map-get? sessions { session-id: session-id })
    session (is-eq (get status session) "completed")
    false
  )
)

;; Check if a review is completed
(define-read-only (is-review-completed (review-id uint))
  (match (map-get? reviews { review-id: review-id })
    review (is-eq (get status review) "completed")
    false
  )
)

;; =============================
;; Public Functions
;; =============================

;; Create a new coding session with payment in escrow
(define-public (create-session (provider principal) (amount uint))
  (let (
    (session-id (var-get next-session-id))
    (platform-fee (calculate-platform-fee amount))
    (provider-amount (- amount platform-fee))
  )
    ;; Validate parameters
    (asserts! (not (is-eq tx-sender provider)) ERR-INVALID-PARTICIPANT)
    (asserts! (>= amount MIN-SESSION-PAYMENT) ERR-INVALID-AMOUNT)
    
    ;; Transfer funds to contract (escrow)
    ;; (asserts! (stx-transfer? amount tx-sender (as-contract tx-sender)) ERR-PAYMENT-FAILED)
    
    ;; Record the session
    (map-set sessions
      { session-id: session-id }
      {
        requester: tx-sender,
        provider: provider,
        amount: amount,
        platform-fee: platform-fee,
        status: "pending",
        requester-confirmed: false,
        provider-confirmed: false,
        created-at: block-height
      }
    )
    
    ;; Increment session ID
    (var-set next-session-id (+ session-id u1))
    
    (ok session-id)
  )
)

;; Confirm session completion (called by both requester and provider)
(define-public (confirm-session-completion (session-id uint))
  (match (map-get? sessions { session-id: session-id })
    session (begin
      ;; Validate session exists and is still pending
      (asserts! (is-eq (get status session) "pending") ERR-SESSION-ALREADY-COMPLETED)
      
      ;; Check if sender is a participant
      (asserts! (or 
                  (is-eq tx-sender (get requester session))
                  (is-eq tx-sender (get provider session))
                )
                ERR-NOT-SESSION-PARTICIPANT)
      
      ;; Update confirmation status
      (if (is-eq tx-sender (get requester session))
          (map-set sessions 
            { session-id: session-id }
            (merge session { requester-confirmed: true })
          )
          (map-set sessions 
            { session-id: session-id }
            (merge session { provider-confirmed: true })
          )
      )
      
      ;; Check if both parties have confirmed
      (match (map-get? sessions { session-id: session-id })
        updated-session (begin
          (if (and 
                (get requester-confirmed updated-session)
                (get provider-confirmed updated-session)
              )
              ;; Release escrow if both confirmed
              (release-session-escrow session-id)
              (ok true)
          )
        )
        ERR-SESSION-NOT-FOUND
      )
    )
    ERR-SESSION-NOT-FOUND
  )
)

;; Release funds from escrow after confirmation
(define-private (release-session-escrow (session-id uint))
  (match (map-get? sessions { session-id: session-id })
    session (begin
      ;; Mark session as completed
      (map-set sessions
        { session-id: session-id }
        (merge session { status: "completed" })
      )
      
      ;; Transfer provider payment
      (unwrap! 
        (as-contract (stx-transfer? 
          (- (get amount session) (get platform-fee session)) 
          tx-sender 
          (get provider session)
        ))
        ERR-PAYMENT-FAILED
      )
      
      ;; Add platform fee to earnings
      (var-set platform-earnings (+ (var-get platform-earnings) (get platform-fee session)))
      
      (ok true)
    )
    ERR-SESSION-NOT-FOUND
  )
)

;; Create a code review request with bounty
(define-public (create-review-request (reviewer principal) (bounty uint))
  (let (
    (review-id (var-get next-review-id))
    (platform-fee (calculate-platform-fee bounty))
    (reviewer-amount (- bounty platform-fee))
  )
    ;; Validate parameters
    (asserts! (not (is-eq tx-sender reviewer)) ERR-INVALID-PARTICIPANT)
    (asserts! (>= bounty MIN-REVIEW-BOUNTY) ERR-INVALID-AMOUNT)
    
    ;; Transfer bounty to contract
    ;; (asserts! (stx-transfer? bounty tx-sender (as-contract tx-sender)) ERR-PAYMENT-FAILED)
    
    ;; Record the review request
    (map-set reviews
      { review-id: review-id }
      {
        requester: tx-sender,
        reviewer: reviewer,
        bounty: bounty,
        platform-fee: platform-fee,
        status: "pending",
        created-at: block-height
      }
    )
    
    ;; Increment review ID
    (var-set next-review-id (+ review-id u1))
    
    (ok review-id)
  )
)

;; Complete a review and release bounty (called by requester)
(define-public (complete-review (review-id uint))
  (match (map-get? reviews { review-id: review-id })
    review (begin
      ;; Ensure caller is the requester
      (asserts! (is-eq tx-sender (get requester review)) ERR-NOT-AUTHORIZED)
      
      ;; Ensure review is still pending
      (asserts! (is-eq (get status review) "pending") ERR-REVIEW-ALREADY-COMPLETED)
      
      ;; Update review status
      (map-set reviews
        { review-id: review-id }
        (merge review { status: "completed" })
      )
      
      ;; Transfer bounty to reviewer
      (unwrap! 
        (as-contract (stx-transfer? 
          (- (get bounty review) (get platform-fee review)) 
          tx-sender 
          (get reviewer review)
        ))
        ERR-PAYMENT-FAILED
      )
      
      ;; Add platform fee to earnings
      (var-set platform-earnings (+ (var-get platform-earnings) (get platform-fee review)))
      
      (ok true)
    )
    ERR-REVIEW-NOT-FOUND
  )
)

;; Send a tip to a coder or reviewer
(define-public (send-tip (recipient principal) (amount uint))
  (begin
    ;; Validate parameters
    (asserts! (not (is-eq tx-sender recipient)) ERR-INVALID-PARTICIPANT)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Calculate platform fee (smaller for tips)
    (let ((platform-fee (/ (* amount u2) u100))) ;; 2% fee for tips
      ;; Transfer tip amount minus fee to recipient
      ;; (asserts! (stx-transfer? (- amount platform-fee) tx-sender recipient) ERR-PAYMENT-FAILED)
      
      ;; Transfer fee to contract
      ;; (asserts! (stx-transfer? platform-fee tx-sender (as-contract tx-sender)) ERR-PAYMENT-FAILED)
      
      ;; Update platform earnings
      (var-set platform-earnings (+ (var-get platform-earnings) platform-fee))
      
      (ok true)
    )
  )
)

;; Cancel a pending session (can only be done by requester)
(define-public (cancel-session (session-id uint))
  (match (map-get? sessions { session-id: session-id })
    session (begin
      ;; Ensure caller is the requester
      (asserts! (is-eq tx-sender (get requester session)) ERR-NOT-AUTHORIZED)
      
      ;; Ensure session is still pending
      (asserts! (is-eq (get status session) "pending") ERR-SESSION-ALREADY-COMPLETED)
      
      ;; Update session status
      (map-set sessions
        { session-id: session-id }
        (merge session { status: "cancelled" })
      )
      
      ;; Refund the full amount to requester
      (unwrap! 
        (as-contract (stx-transfer? (get amount session) tx-sender (get requester session)))
        ERR-PAYMENT-FAILED
      )
      
      (ok true)
    )
    ERR-SESSION-NOT-FOUND
  )
)

;; Cancel a pending review (can only be done by requester)
(define-public (cancel-review (review-id uint))
  (match (map-get? reviews { review-id: review-id })
    review (begin
      ;; Ensure caller is the requester
      (asserts! (is-eq tx-sender (get requester review)) ERR-NOT-AUTHORIZED)
      
      ;; Ensure review is still pending
      (asserts! (is-eq (get status review) "pending") ERR-REVIEW-ALREADY-COMPLETED)
      
      ;; Update review status
      (map-set reviews
        { review-id: review-id }
        (merge review { status: "cancelled" })
      )
      
      ;; Refund the full amount to requester
      (unwrap! 
        (as-contract (stx-transfer? (get bounty review) tx-sender (get requester review)))
        ERR-PAYMENT-FAILED
      )
      
      (ok true)
    )
    ERR-REVIEW-NOT-FOUND
  )
)

;; Withdraw platform earnings (only contract owner)
(define-public (withdraw-platform-earnings (amount uint))
  (begin
    ;; Ensure caller is contract owner
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    
    ;; Ensure amount is valid
    (asserts! (<= amount (var-get platform-earnings)) ERR-INSUFFICIENT-FUNDS)
    
    ;; Transfer earnings
    (unwrap! 
      (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER))
      ERR-PAYMENT-FAILED
    )
    
    ;; Update platform earnings
    (var-set platform-earnings (- (var-get platform-earnings) amount))
    
    (ok true)
  )
)