;; Multi-Send with Recurring Payments Smart Contract
;; Allows scheduling recurring batch payments with automated distributions

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INVALID-INTERVAL (err u102))
(define-constant ERR-PAYMENT-NOT-FOUND (err u103))
(define-constant ERR-PAYMENT-NOT-DUE (err u104))
(define-constant ERR-INSUFFICIENT-BALANCE (err u105))
(define-constant ERR-INVALID-RECIPIENTS (err u106))
(define-constant ERR-PAYMENT-INACTIVE (err u107))
(define-constant ERR-MAX-RECIPIENTS-EXCEEDED (err u108))

;; Constants
(define-constant MAX-RECIPIENTS u50)
(define-constant MIN-INTERVAL u144) ;; Minimum 1 day in blocks (144 blocks = ~24 hours)

;; Data variables
(define-data-var payment-id-nonce uint u0)
(define-data-var contract-owner principal tx-sender)

;; Recipient structure for batch payments
(define-map recipient-info
    { payment-id: uint, recipient-index: uint }
    { 
        recipient: principal,
        amount: uint
    }
)

;; Recurring payment schedule
(define-map recurring-payments
    uint ;; payment-id
    {
        owner: principal,
        total-amount: uint,
        interval-blocks: uint,
        recipient-count: uint,
        last-execution: uint,
        next-execution: uint,
        executions-remaining: uint,
        is-active: bool,
        created-at: uint
    }
)

;; Payment execution history
(define-map payment-executions
    { payment-id: uint, execution-count: uint }
    {
        executed-at: uint,
        total-sent: uint,
        successful-transfers: uint,
        failed-transfers: uint
    }
)

;; User's payment IDs for easy lookup
(define-map user-payments
    principal
    (list 100 uint)
)

;; Read-only functions

;; Get recurring payment details
(define-read-only (get-recurring-payment (payment-id uint))
    (map-get? recurring-payments payment-id)
)

;; Get recipient info for a payment
(define-read-only (get-recipient (payment-id uint) (recipient-index uint))
    (map-get? recipient-info { payment-id: payment-id, recipient-index: recipient-index })
)

;; Get recipient count for a payment
(define-read-only (get-recipient-count (payment-id uint))
    (match (get-recurring-payment payment-id)
        payment-data (some (get recipient-count payment-data))
        none
    )
)

;; Check if payment is due for execution
(define-read-only (is-payment-due (payment-id uint))
    (match (get-recurring-payment payment-id)
        payment-data
        (and 
            (get is-active payment-data)
            (>= block-height (get next-execution payment-data))
            (> (get executions-remaining payment-data) u0)
        )
        false
    )
)

;; Get user's payment IDs
(define-read-only (get-user-payments (user principal))
    (default-to (list) (map-get? user-payments user))
)

;; Get payment execution history
(define-read-only (get-execution-history (payment-id uint) (execution-count uint))
    (map-get? payment-executions { payment-id: payment-id, execution-count: execution-count })
)

;; Private functions

;; Helper function for summing amounts
(define-private (sum-amounts (amount uint) (acc uint))
    (+ amount acc)
)

;; Helper function to get amount from recipient tuple
(define-private (get-amount (recipient { recipient: principal, amount: uint }))
    (get amount recipient)
)

;; Helper function for fold that stores each recipient with auto-incrementing index
(define-private (store-recipient-with-index 
    (recipient { recipient: principal, amount: uint })
    (acc { payment-id: uint, current-index: uint })
)
    (begin
        (map-set recipient-info 
            { payment-id: (get payment-id acc), recipient-index: (get current-index acc) }
            {
                recipient: (get recipient recipient),
                amount: (get amount recipient)
            }
        )
        { 
            payment-id: (get payment-id acc), 
            current-index: (+ (get current-index acc) u1) 
        }
    )
)

;; Store recipients using fold with index tracking
(define-private (store-recipients 
    (payment-id uint) 
    (recipients (list 50 { recipient: principal, amount: uint })) 
    (start-index uint)
)
    (fold store-recipient-with-index recipients { payment-id: payment-id, current-index: u0 })
)

;; Process transfers using a simpler iterative approach
(define-private (process-transfers 
    (payment-id uint) 
    (owner principal) 
    (total-recipients uint) 
    (current-index uint) 
    (total-sent uint) 
    (successful uint) 
    (failed uint)
)
    (let 
        (
            (all-indices (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39 u40 u41 u42 u43 u44 u45 u46 u47 u48 u49))
            (final-result (fold process-single-transfer 
                all-indices
                { payment-id: payment-id, owner: owner, total-sent: u0, successful: u0, failed: u0, max-recipients: total-recipients }
            ))
        )
        (ok {
            total-sent: (get total-sent final-result),
            successful-transfers: (get successful final-result),
            failed-transfers: (get failed final-result)
        })
    )
)

;; Process a single transfer and update stats (updated to check bounds)
(define-private (process-single-transfer 
    (index uint)
    (acc { payment-id: uint, owner: principal, total-sent: uint, successful: uint, failed: uint, max-recipients: uint })
)
    (if (< index (get max-recipients acc))
        (match (get-recipient (get payment-id acc) index)
            recipient-data
            (let 
                (
                    (transfer-result (stx-transfer? 
                        (get amount recipient-data) 
                        (get owner acc) 
                        (get recipient recipient-data)
                    ))
                )
                (match transfer-result
                    success {
                        payment-id: (get payment-id acc),
                        owner: (get owner acc),
                        total-sent: (+ (get total-sent acc) (get amount recipient-data)),
                        successful: (+ (get successful acc) u1),
                        failed: (get failed acc),
                        max-recipients: (get max-recipients acc)
                    }
                    error {
                        payment-id: (get payment-id acc),
                        owner: (get owner acc),
                        total-sent: (get total-sent acc),
                        successful: (get successful acc),
                        failed: (+ (get failed acc) u1),
                        max-recipients: (get max-recipients acc)
                    }
                )
            )
            {
                payment-id: (get payment-id acc),
                owner: (get owner acc),
                total-sent: (get total-sent acc),
                successful: (get successful acc),
                failed: (+ (get failed acc) u1),
                max-recipients: (get max-recipients acc)
            }
        )
        acc ;; Skip processing if index exceeds max recipients
    )
)

;; Execute batch transfer to all recipients
(define-private (execute-batch-transfer (payment-id uint) (payment-data (tuple (owner principal) (total-amount uint) (interval-blocks uint) (recipient-count uint) (last-execution uint) (next-execution uint) (executions-remaining uint) (is-active bool) (created-at uint))))
    (let 
        (
            (owner (get owner payment-data))
            (recipient-count (get recipient-count payment-data))
        )
        (unwrap-panic (process-transfers payment-id owner recipient-count u0 u0 u0 u0))
    )
)


(define-public (create-recurring-payment 
    (recipients (list 50 { recipient: principal, amount: uint }))
    (interval-blocks uint)
    (max-executions uint)
)
    (let 
        (
            (payment-id (+ (var-get payment-id-nonce) u1))
            (recipient-count (len recipients))
            (total-amount (fold sum-amounts (map get-amount recipients) u0))
            (current-block block-height)
        )
        ;; Validation checks
        (asserts! (> recipient-count u0) ERR-INVALID-RECIPIENTS)
        (asserts! (<= recipient-count MAX-RECIPIENTS) ERR-MAX-RECIPIENTS-EXCEEDED)
        (asserts! (>= interval-blocks MIN-INTERVAL) ERR-INVALID-INTERVAL)
        (asserts! (> total-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> max-executions u0) ERR-INVALID-AMOUNT)
        
        ;; Store recipient information
        (store-recipients payment-id recipients u0)
        
        ;; Create recurring payment record
        (map-set recurring-payments payment-id
            {
                owner: tx-sender,
                total-amount: total-amount,
                interval-blocks: interval-blocks,
                recipient-count: recipient-count,
                last-execution: u0,
                next-execution: (+ current-block interval-blocks),
                executions-remaining: max-executions,
                is-active: true,
                created-at: current-block
            }
        )
        
        ;; Update user's payment list with bounds checking
        (let ((current-payments (get-user-payments tx-sender)))
            (if (< (len current-payments) u100)
                (match (as-max-len? (append current-payments payment-id) u100)
                    updated-list (map-set user-payments tx-sender updated-list)
                    false ;; Failed to create bounded list
                )
                false ;; List is already at maximum capacity
            )
        )
        
        ;; Update payment ID nonce
        (var-set payment-id-nonce payment-id)
        
        (ok payment-id)
    )
)

;; Execute a recurring payment
(define-public (execute-recurring-payment (payment-id uint))
    (let 
        (
            (payment-data (unwrap! (get-recurring-payment payment-id) ERR-PAYMENT-NOT-FOUND))
            (sender-balance (stx-get-balance (get owner payment-data)))
        )
        ;; Validation checks
        (asserts! (get is-active payment-data) ERR-PAYMENT-INACTIVE)
        (asserts! (>= block-height (get next-execution payment-data)) ERR-PAYMENT-NOT-DUE)
        (asserts! (> (get executions-remaining payment-data) u0) ERR-PAYMENT-NOT-DUE)
        (asserts! (>= sender-balance (get total-amount payment-data)) ERR-INSUFFICIENT-BALANCE)
        
        ;; Execute the batch payment
        (let 
            (
                (execution-result (execute-batch-transfer payment-id payment-data))
                (executions-done (- (get executions-remaining payment-data) u1))
                (execution-count (- (get executions-remaining payment-data)))
            )
            
            ;; Update payment schedule
            (map-set recurring-payments payment-id
                (merge payment-data
                    {
                        last-execution: block-height,
                        next-execution: (+ block-height (get interval-blocks payment-data)),
                        executions-remaining: executions-done,
                        is-active: (> executions-done u0)
                    }
                )
            )
            
            ;; Record execution history
            (map-set payment-executions 
                { payment-id: payment-id, execution-count: execution-count }
                {
                    executed-at: block-height,
                    total-sent: (get total-sent execution-result),
                    successful-transfers: (get successful-transfers execution-result),
                    failed-transfers: (get failed-transfers execution-result)
                }
            )
            
            (ok execution-result)
        )
    )
)

;; Cancel a recurring payment
(define-public (cancel-recurring-payment (payment-id uint))
    (let 
        (
            (payment-data (unwrap! (get-recurring-payment payment-id) ERR-PAYMENT-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get owner payment-data)) ERR-NOT-AUTHORIZED)
        
        (map-set recurring-payments payment-id
            (merge payment-data { is-active: false })
        )
        
        (ok true)
    )
)

;; Update recurring payment interval (only owner)
(define-public (update-payment-interval (payment-id uint) (new-interval-blocks uint))
    (let 
        (
            (payment-data (unwrap! (get-recurring-payment payment-id) ERR-PAYMENT-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get owner payment-data)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active payment-data) ERR-PAYMENT-INACTIVE)
        (asserts! (>= new-interval-blocks MIN-INTERVAL) ERR-INVALID-INTERVAL)
        
        (map-set recurring-payments payment-id
            (merge payment-data 
                {
                    interval-blocks: new-interval-blocks,
                    next-execution: (+ (get last-execution payment-data) new-interval-blocks)
                }
            )
        )
        
        (ok true)
    )
)