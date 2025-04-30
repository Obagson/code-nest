;; code-nest-sessions
;; 
;; A smart contract that manages collaborative coding sessions between developers on the Stacks blockchain.
;; Enables developers to create, join, and complete coding sessions with transparent payment handling
;; and built-in mechanisms for dispute resolution.

;; =======================================
;; Constants & Error Codes
;; =======================================

;; Error codes
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-SESSION-NOT-FOUND (err u101))
(define-constant ERR-INVALID-STATUS (err u102))
(define-constant ERR-ALREADY-JOINED (err u103))
(define-constant ERR-INVALID-SESSION-PARAMS (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))
(define-constant ERR-INVALID-DURATION (err u106))
(define-constant ERR-PAYMENT-FAILED (err u107))
(define-constant ERR-SELF-JOIN (err u108))
(define-constant ERR-SESSION-EXPIRED (err u109))
(define-constant ERR-DISPUTE-EXISTS (err u110))
(define-constant ERR-INVALID-RATING (err u111))

;; Status codes
(define-constant STATUS-PROPOSED u1)
(define-constant STATUS-IN-PROGRESS u2)
(define-constant STATUS-COMPLETED u3)
(define-constant STATUS-CANCELLED u4)
(define-constant STATUS-DISPUTED u5)

;; =======================================
;; Data Maps & Variables
;; =======================================

;; Track the total number of sessions created, used for generating session IDs
(define-data-var session-counter uint u0)

;; Session data structure
(define-map sessions uint 
  {
    creator: principal,                ;; The developer who created the session
    partner: (optional principal),     ;; The developer who joined the session
    title: (string-ascii 100),         ;; Title of the session
    description: (string-utf8 500),    ;; Detailed description of the session
    hourly-rate: uint,                 ;; Hourly rate in microSTX
    estimated-duration: uint,          ;; Estimated duration in minutes
    focus-areas: (string-ascii 200),   ;; Comma-separated list of focus areas/tags
    status: uint,                      ;; Current status of the session
    created-at: uint,                  ;; Block height when created
    started-at: (optional uint),       ;; Block height when started
    completed-at: (optional uint),     ;; Block height when completed
    funds-locked: uint,                ;; Amount of STX locked for payment
    creator-confirmed: bool,           ;; Whether creator confirmed completion
    partner-confirmed: bool            ;; Whether partner confirmed completion
  }
)

;; Tracks disputes for sessions
(define-map session-disputes uint 
  {
    reason: (string-utf8 500),         ;; Reason for the dispute
    initiated-by: principal,           ;; Who initiated the dispute
    resolved: bool                     ;; Whether the dispute is resolved
  }
)

;; Developer profiles tracking their participation
(define-map developer-profiles principal
  {
    sessions-created: uint,            ;; Number of sessions created
    sessions-participated: uint,       ;; Number of sessions joined as partner
    total-earnings: uint,              ;; Total earned in microSTX
    average-rating: uint,              ;; Average rating (0-100)
    ratings-count: uint                ;; Number of ratings received
  }
)

;; Map of session ratings (score out of 100)
(define-map session-ratings (tuple (session-id uint) (rater principal))
  {
    rating: uint,                      ;; Rating score (0-100)
    feedback: (string-utf8 300)        ;; Optional feedback comments
  }
)

;; =======================================
;; Private Functions
;; =======================================

;; Get the next session ID and increment the counter
(define-private (get-next-session-id)
  (let ((current-id (var-get session-counter)))
    (var-set session-counter (+ current-id u1))
    current-id))

;; Check if caller is a participant in the session
(define-private (is-session-participant (session-id uint))
  (match (map-get? sessions session-id)
    session (or
              (is-eq tx-sender (get creator session))
              (match (get partner session)
                partner (is-eq tx-sender partner)
                false))
    false))

;; Initialize or update developer profile
(define-private (init-or-update-profile (developer principal))
  (match (map-get? developer-profiles developer)
    existing-profile true
    (map-set developer-profiles developer {
      sessions-created: u0,
      sessions-participated: u0,
      total-earnings: u0,
      average-rating: u0,
      ratings-count: u0
    })))

;; Update sessions created count for a developer
(define-private (increment-sessions-created (developer principal))
  (match (map-get? developer-profiles developer)
    profile (map-set developer-profiles developer
              (merge profile {
                sessions-created: (+ (get sessions-created profile) u1)
              }))
    (init-or-update-profile developer)))

;; Update sessions participated count for a developer
(define-private (increment-sessions-participated (developer principal))
  (match (map-get? developer-profiles developer)
    profile (map-set developer-profiles developer
              (merge profile {
                sessions-participated: (+ (get sessions-participated profile) u1)
              }))
    (init-or-update-profile developer)))

;; Update developer earnings
(define-private (update-developer-earnings (developer principal) (amount uint))
  (match (map-get? developer-profiles developer)
    profile (map-set developer-profiles developer
              (merge profile {
                total-earnings: (+ (get total-earnings profile) amount)
              }))
    (init-or-update-profile developer)))

;; Calculate payment amount for completed session
(define-private (calculate-payment (session-id uint))
  (match (map-get? sessions session-id)
    session (get funds-locked session)
    u0))

;; =======================================
;; Read-Only Functions
;; =======================================

;; Get session details by ID
(define-read-only (get-session (session-id uint))
  (match (map-get? sessions session-id)
    session (ok session)
    ERR-SESSION-NOT-FOUND))

;; Get dispute details for a session
(define-read-only (get-session-dispute (session-id uint))
  (match (map-get? session-disputes session-id)
    dispute (ok dispute)
    (err u0)))

;; Get developer profile
(define-read-only (get-developer-profile (developer principal))
  (match (map-get? developer-profiles developer)
    profile (ok profile)
    (ok {
      sessions-created: u0,
      sessions-participated: u0,
      total-earnings: u0,
      average-rating: u0,
      ratings-count: u0
    })))

;; Get rating for a session by a specific rater
(define-read-only (get-session-rating (session-id uint) (rater principal))
  (match (map-get? session-ratings {session-id: session-id, rater: rater})
    rating (ok rating)
    (err u0)))

;; Check if a session can be joined
(define-read-only (can-join-session (session-id uint))
  (match (map-get? sessions session-id)
    session (if (and 
                  (is-eq (get status session) STATUS-PROPOSED)
                  (is-none (get partner session)))
              (ok true)
              (err (get status session)))
    ERR-SESSION-NOT-FOUND))

;; =======================================
;; Public Functions
;; =======================================

;; Create a new coding session proposal
(define-public (create-session 
                (title (string-ascii 100))
                (description (string-utf8 500))
                (hourly-rate uint)
                (estimated-duration uint)
                (focus-areas (string-ascii 200)))
  (let
    ((session-id (get-next-session-id))
     (total-cost (* hourly-rate (/ estimated-duration u60))))
    
    ;; Validate inputs
    (asserts! (> hourly-rate u0) ERR-INVALID-SESSION-PARAMS)
    (asserts! (and (> estimated-duration u0) (<= estimated-duration u480)) ERR-INVALID-DURATION)
    (asserts! (stx-transfer? total-cost tx-sender (as-contract tx-sender)) ERR-INSUFFICIENT-FUNDS)
    
    ;; Create the session
    (map-set sessions session-id {
      creator: tx-sender,
      partner: none,
      title: title,
      description: description,
      hourly-rate: hourly-rate,
      estimated-duration: estimated-duration,
      focus-areas: focus-areas,
      status: STATUS-PROPOSED,
      created-at: block-height,
      started-at: none,
      completed-at: none,
      funds-locked: total-cost,
      creator-confirmed: false,
      partner-confirmed: false
    })
    
    ;; Update developer profile
    (increment-sessions-created tx-sender)
    
    (ok session-id)))

;; Join an existing session as a partner
(define-public (join-session (session-id uint))
  (match (map-get? sessions session-id)
    session (begin
      ;; Validations
      (asserts! (is-eq (get status session) STATUS-PROPOSED) ERR-INVALID-STATUS)
      (asserts! (is-none (get partner session)) ERR-ALREADY-JOINED)
      (asserts! (not (is-eq tx-sender (get creator session))) ERR-SELF-JOIN)
      
      ;; Update session
      (map-set sessions session-id 
        (merge session {
          partner: (some tx-sender),
          status: STATUS-IN-PROGRESS,
          started-at: (some block-height)
        }))
      
      ;; Update developer profile
      (increment-sessions-participated tx-sender)
      
      (ok true))
    ERR-SESSION-NOT-FOUND))

;; Confirm session completion (called by both creator and partner)
(define-public (confirm-completion (session-id uint))
  (match (map-get? sessions session-id)
    session (begin
      ;; Validations
      (asserts! (is-session-participant session-id) ERR-UNAUTHORIZED)
      (asserts! (is-eq (get status session) STATUS-IN-PROGRESS) ERR-INVALID-STATUS)
      
      (let 
        ((updated-session (if (is-eq tx-sender (get creator session))
                            (merge session {creator-confirmed: true})
                            (merge session {partner-confirmed: true}))))
        
        ;; Update the session
        (map-set sessions session-id updated-session)
        
        ;; If both confirmed, finalize the session
        (if (and (get creator-confirmed updated-session) 
                (get partner-confirmed updated-session))
          (finalize-session session-id)
          (ok true))))
    ERR-SESSION-NOT-FOUND))

;; Private function to finalize a session after both parties confirmed
(define-private (finalize-session (session-id uint))
  (match (map-get? sessions session-id)
    session (begin
      (let 
        ((payment-amount (calculate-payment session-id))
         (partner-principal (unwrap! (get partner session) ERR-SESSION-NOT-FOUND)))
        
        ;; Transfer payment to partner
        (asserts! (as-contract (stx-transfer? payment-amount tx-sender partner-principal)) ERR-PAYMENT-FAILED)
        
        ;; Update session status
        (map-set sessions session-id 
          (merge session {
            status: STATUS-COMPLETED,
            completed-at: (some block-height)
          }))
        
        ;; Update developer earnings
        (update-developer-earnings partner-principal payment-amount)
        
        (ok true)))
    ERR-SESSION-NOT-FOUND))

;; Rate a participant after session completion
(define-public (rate-participant (session-id uint) (rated-user principal) (rating uint) (feedback (string-utf8 300)))
  (match (map-get? sessions session-id)
    session (begin
      ;; Validations
      (asserts! (is-session-participant session-id) ERR-UNAUTHORIZED)
      (asserts! (is-eq (get status session) STATUS-COMPLETED) ERR-INVALID-STATUS)
      (asserts! (and (>= rating u0) (<= rating u100)) ERR-INVALID-RATING)
      (asserts! (or 
                  (and (is-eq tx-sender (get creator session)) 
                       (is-some (get partner session))
                       (is-eq rated-user (unwrap-panic (get partner session))))
                  (and (is-some (get partner session))
                       (is-eq tx-sender (unwrap-panic (get partner session)))
                       (is-eq rated-user (get creator session))))
                ERR-UNAUTHORIZED)
      
      ;; Record the rating
      (map-set session-ratings {session-id: session-id, rater: tx-sender}
        {rating: rating, feedback: feedback})
      
      ;; Update the developer's average rating
      (match (map-get? developer-profiles rated-user)
        profile (let
                  ((current-total (* (get average-rating profile) (get ratings-count profile)))
                   (new-count (+ (get ratings-count profile) u1))
                   (new-average (/ (+ current-total rating) new-count)))
                  (map-set developer-profiles rated-user
                    (merge profile {
                      average-rating: new-average,
                      ratings-count: new-count
                    }))
                  (ok true))
        (err u0)))
    ERR-SESSION-NOT-FOUND))

;; Cancel a session - only the creator can cancel if not yet joined
(define-public (cancel-session (session-id uint))
  (match (map-get? sessions session-id)
    session (begin
      ;; Validations
      (asserts! (is-eq tx-sender (get creator session)) ERR-UNAUTHORIZED)
      (asserts! (is-eq (get status session) STATUS-PROPOSED) ERR-INVALID-STATUS)
      (asserts! (is-none (get partner session)) ERR-ALREADY-JOINED)
      
      ;; Refund locked funds to creator
      (asserts! (as-contract (stx-transfer? (get funds-locked session) tx-sender 
                                            (get creator session))) 
                ERR-PAYMENT-FAILED)
      
      ;; Update session status
      (map-set sessions session-id 
        (merge session {
          status: STATUS-CANCELLED
        }))
      
      (ok true)))
    ERR-SESSION-NOT-FOUND))

;; Initiate a dispute for a session
(define-public (initiate-dispute (session-id uint) (reason (string-utf8 500)))
  (match (map-get? sessions session-id)
    session (begin
      ;; Validations
      (asserts! (is-session-participant session-id) ERR-UNAUTHORIZED)
      (asserts! (or (is-eq (get status session) STATUS-IN-PROGRESS)
                    (is-eq (get status session) STATUS-COMPLETED)) 
                ERR-INVALID-STATUS)
      (asserts! (map-insert session-disputes session-id 
                            {reason: reason, 
                             initiated-by: tx-sender, 
                             resolved: false}) 
                ERR-DISPUTE-EXISTS)
      
      ;; Update session status
      (map-set sessions session-id 
        (merge session {
          status: STATUS-DISPUTED
        }))
      
      (ok true)))
    ERR-SESSION-NOT-FOUND))

;; Admin function to resolve a dispute (would require contract owner validation in production)
;; This is a placeholder for governance mechanisms
(define-public (resolve-dispute (session-id uint) (creator-percentage uint) (partner-percentage uint))
  (match (map-get? sessions session-id)
    session (begin
      ;; For demonstration only - in production, would need proper authorization checks
      ;; and governance mechanisms
      
      ;; Validations
      (asserts! (is-eq (get status session) STATUS-DISPUTED) ERR-INVALID-STATUS)
      (asserts! (is-eq (+ creator-percentage partner-percentage) u100) ERR-INVALID-SESSION-PARAMS)
      
      (let
        ((total-payment (get funds-locked session))
         (creator-payment (/ (* total-payment creator-percentage) u100))
         (partner-payment (/ (* total-payment partner-percentage) u100))
         (partner-principal (unwrap! (get partner session) ERR-SESSION-NOT-FOUND)))
        
        ;; Transfer payments according to resolution
        (and 
          (> creator-payment u0)
          (as-contract (stx-transfer? creator-payment tx-sender (get creator session))))
        
        (and
          (> partner-payment u0)
          (as-contract (stx-transfer? partner-payment tx-sender partner-principal)))
        
        ;; Update session and dispute status
        (map-set sessions session-id 
          (merge session {
            status: STATUS-COMPLETED,
            completed-at: (some block-height)
          }))
        
        (map-set session-disputes session-id
          (merge (unwrap-panic (map-get? session-disputes session-id))
                 {resolved: true}))
        
        ;; Update earnings
        (update-developer-earnings partner-principal partner-payment)
        
        (ok true))))
    ERR-SESSION-NOT-FOUND))