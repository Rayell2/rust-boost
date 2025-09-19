;; rust-boost-payments.clar
;; A smart contract to handle payments for Rust development tasks
;; and collaboration, implementing escrow functionality, review bounties,
;; and tipping mechanisms.

;; Error codes
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-TASK-EXISTS (err u102))
(define-constant ERR-TASK-NOT-FOUND (err u103))
(define-constant ERR-TASK-ALREADY-COMPLETED (err u104))
(define-constant ERR-REVIEW-EXISTS (err u105))
(define-constant ERR-REVIEW-NOT-FOUND (err u106))
(define-constant ERR-REVIEW-ALREADY-COMPLETED (err u107))
(define-constant ERR-INSUFFICIENT-FUNDS (err u108))
(define-constant ERR-PAYMENT-FAILED (err u109))
(define-constant ERR-INVALID-PARTICIPANT (err u110))
(define-constant ERR-NOT-TASK-PARTICIPANT (err u111))
(define-constant ERR-ESCROW-ALREADY-RELEASED (err u112))
(define-constant ERR-INVALID-FEE-PERCENTAGE (err u113))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant PLATFORM-FEE-PERCENTAGE u5) ;; 5% fee
(define-constant MIN-TASK-PAYMENT u1000000) ;; 1 STX minimum
(define-constant MIN-REVIEW-BOUNTY u500000) ;; 0.5 STX minimum

;; Data maps for Rust development tasks
(define-map rust-tasks
  { task-id: uint }
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
(define-map rust-reviews
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
(define-data-var next-task-id uint u1)
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

;; Check if sender is a task participant
(define-private (is-task-participant (task-id uint))
  (match (map-get? rust-tasks { task-id: task-id })
    task (or 
           (is-eq tx-sender (get requester task))
           (is-eq tx-sender (get provider task)))
    false
  )
)

;; =============================
;; Read-only Functions
;; =============================

;; Get task details
(define-read-only (get-task (task-id uint))
  (map-get? rust-tasks { task-id: task-id })
)

;; Get review details
(define-read-only (get-review (review-id uint))
  (map-get? rust-reviews { review-id: review-id })
)

;; Get platform earnings
(define-read-only (get-platform-earnings)
  (var-get platform-earnings)
)

;; Check if a task is completed
(define-read-only (is-task-completed (task-id uint))
  (match (map-get? rust-tasks { task-id: task-id })
    task (is-eq (get status task) "completed")
    false
  )
)

;; Check if a review is completed
(define-read-only (is-review-completed (review-id uint))
  (match (map-get? rust-reviews { review-id: review-id })
    review (is-eq (get status review) "completed")
    false
  )
)

;; =============================
;; Public Functions
;; =============================

;; Create a new Rust development task with payment in escrow
(define-public (create-task (provider principal) (amount uint))
  (let (
    (task-id (var-get next-task-id))
    (platform-fee (calculate-platform-fee amount))
    (provider-amount (- amount platform-fee))
  )
    ;; Validate parameters
    (asserts! (not (is-eq tx-sender provider)) ERR-INVALID-PARTICIPANT)
    (asserts! (>= amount MIN-TASK-PAYMENT) ERR-INVALID-AMOUNT)
    
    ;; Record the task
    (map-set rust-tasks
      { task-id: task-id }
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
    
    ;; Increment task ID
    (var-set next-task-id (+ task-id u1))
    
    (ok task-id)
  )
)

;; Confirm task completion (called by both requester and provider)
(define-public (confirm-task-completion (task-id uint))
  (match (map-get? rust-tasks { task-id: task-id })
    task (begin
      ;; Validate task exists and is still pending
      (asserts! (is-eq (get status task) "pending") ERR-TASK-ALREADY-COMPLETED)
      
      ;; Check if sender is a participant
      (asserts! (or 
                  (is-eq tx-sender (get requester task))
                  (is-eq tx-sender (get provider task))
                )
                ERR-NOT-TASK-PARTICIPANT)
      
      ;; Update confirmation status
      (if (is-eq tx-sender (get requester task))
          (map-set rust-tasks 
            { task-id: task-id }
            (merge task { requester-confirmed: true })
          )
          (map-set rust-tasks 
            { task-id: task-id }
            (merge task { provider-confirmed: true })
          )
      )
      
      ;; Check if both parties have confirmed
      (match (map-get? rust-tasks { task-id: task-id })
        updated-task (begin
          (if (and 
                (get requester-confirmed updated-task)
                (get provider-confirmed updated-task)
              )
              ;; Release escrow if both confirmed
              (release-task-escrow task-id)
              (ok true)
          )
        )
        ERR-TASK-NOT-FOUND
      )
    )
    ERR-TASK-NOT-FOUND
  )
)

;; Release funds from escrow after confirmation
(define-private (release-task-escrow (task-id uint))
  (match (map-get? rust-tasks { task-id: task-id })
    task (begin
      ;; Mark task as completed
      (map-set rust-tasks
        { task-id: task-id }
        (merge task { status: "completed" })
      )
      
      ;; Transfer provider payment
      (unwrap! 
        (as-contract (stx-transfer? 
          (- (get amount task) (get platform-fee task)) 
          tx-sender 
          (get provider task)
        ))
        ERR-PAYMENT-FAILED
      )
      
      ;; Add platform fee to earnings
      (var-set platform-earnings (+ (var-get platform-earnings) (get platform-fee task)))
      
      (ok true)
    )
    ERR-TASK-NOT-FOUND
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
    
    ;; Record the review request
    (map-set rust-reviews
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
  (match (map-get? rust-reviews { review-id: review-id })
    review (begin
      ;; Ensure caller is the requester
      (asserts! (is-eq tx-sender (get requester review)) ERR-UNAUTHORIZED)
      
      ;; Ensure review is still pending
      (asserts! (is-eq (get status review) "pending") ERR-REVIEW-ALREADY-COMPLETED)
      
      ;; Update review status
      (map-set rust-reviews
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

;; Send a tip to a Rust developer or reviewer
(define-public (send-tip (recipient principal) (amount uint))
  (begin
    ;; Validate parameters
    (asserts! (not (is-eq tx-sender recipient)) ERR-INVALID-PARTICIPANT)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Calculate platform fee (smaller for tips)
    (let ((platform-fee (/ (* amount u2) u100))) ;; 2% fee for tips
      ;; Update platform earnings
      (var-set platform-earnings (+ (var-get platform-earnings) platform-fee))
      
      (ok true)
    )
  )
)

;; Cancel a pending task (can only be done by requester)
(define-public (cancel-task (task-id uint))
  (match (map-get? rust-tasks { task-id: task-id })
    task (begin
      ;; Ensure caller is the requester
      (asserts! (is-eq tx-sender (get requester task)) ERR-UNAUTHORIZED)
      
      ;; Ensure task is still pending
      (asserts! (is-eq (get status task) "pending") ERR-TASK-ALREADY-COMPLETED)
      
      ;; Update task status
      (map-set rust-tasks
        { task-id: task-id }
        (merge task { status: "cancelled" })
      )
      
      ;; Refund the full amount to requester
      (unwrap! 
        (as-contract (stx-transfer? (get amount task) tx-sender (get requester task)))
        ERR-PAYMENT-FAILED
      )
      
      (ok true)
    )
    ERR-TASK-NOT-FOUND
  )
)

;; Cancel a pending review (can only be done by requester)
(define-public (cancel-review (review-id uint))
  (match (map-get? rust-reviews { review-id: review-id })
    review (begin
      ;; Ensure caller is the requester
      (asserts! (is-eq tx-sender (get requester review)) ERR-UNAUTHORIZED)
      
      ;; Ensure review is still pending
      (asserts! (is-eq (get status review) "pending") ERR-REVIEW-ALREADY-COMPLETED)
      
      ;; Update review status
      (map-set rust-reviews
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
    (asserts! (is-contract-owner) ERR-UNAUTHORIZED)
    
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