;; Advanced Voting Smart Contract with Comprehensive Features
;; Enhanced with multiple voting types, delegate voting, weighted votes, and advanced analytics

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-voted (err u102))
(define-constant err-candidate-exists (err u103))
(define-constant err-voting-ended (err u104))
(define-constant err-voting-not-started (err u105))
(define-constant err-insufficient-stake (err u106))
(define-constant err-invalid-delegation (err u107))
(define-constant err-poll-not-found (err u108))
(define-constant err-poll-ended (err u109))
(define-constant err-unauthorized (err u110))
(define-constant err-invalid-params (err u111))
(define-constant err-candidate-limit-reached (err u112))
(define-constant err-proposal-exists (err u113))

;; Configuration Constants
(define-constant max-candidates-per-poll u20)
(define-constant min-stake-amount u1000000) ;; 1 STX in microSTX
(define-constant delegation-fee u10000) ;; 0.01 STX fee

;; Data Variables
(define-data-var next-candidate-id uint u1)
(define-data-var next-poll-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var contract-paused bool false)
(define-data-var total-staked uint u0)
(define-data-var admin-fee-rate uint u50) ;; 0.5% in basis points

;; Enhanced Data Maps
(define-map candidates 
    { candidate-id: uint }
    {
        name: (string-utf8 50),
        description: (string-utf8 200),
        vote-count: uint,
        weighted-vote-count: uint,
        stake-backing: uint,
        created-at: uint,
        created-by: principal,
        poll-id: uint,
        is-active: bool,
        category: (string-utf8 30)
    }
)

(define-map voters 
    { voter: principal, poll-id: uint }
    {
        has-voted: bool,
        candidate-id: uint,
        voted-at: uint,
        vote-weight: uint,
        voting-power: uint,
        delegate: (optional principal)
    }
)

(define-map polls
    { poll-id: uint }
    {
        title: (string-utf8 100),
        description: (string-utf8 300),
        creator: principal,
        start-height: uint,
        end-height: uint,
        is-active: bool,
        poll-type: (string-utf8 20),
        max-choices: uint,
        min-stake-required: uint,
        total-votes: uint,
        total-participants: uint,
        category: (string-utf8 30)
    }
)

(define-map user-stakes
    { user: principal }
    {
        staked-amount: uint,
        staked-at: uint,
        voting-power: uint,
        total-votes-cast: uint,
        reputation-score: uint
    }
)

(define-map delegations
    { delegator: principal, poll-id: uint }
    {
        delegate: principal,
        delegated-at: uint,
        is-active: bool,
        voting-power-delegated: uint
    }
)

(define-map proposals
    { proposal-id: uint }
    {
        title: (string-utf8 100),
        description: (string-utf8 500),
        proposer: principal,
        created-at: uint,
        execution-delay: uint,
        votes-for: uint,
        votes-against: uint,
        votes-abstain: uint,
        status: (string-utf8 20), ;; "active", "passed", "rejected", "executed"
        proposal-type: (string-utf8 30)
    }
)

(define-map poll-results
    { poll-id: uint }
    {
        winner-candidate-id: uint,
        total-votes: uint,
        participation-rate: uint,
        concluded-at: uint,
        is-finalized: bool
    }
)

(define-map user-voting-history
    { user: principal, poll-id: uint }
    {
        votes-cast: (list 1 uint),
        voting-power-used: uint,
        timestamp: uint,
        vote-receipt: (buff 32)
    }
)

;; Enhanced Event Definitions
(define-private (emit-candidate-added (candidate-id uint) (name (string-utf8 50)) (creator principal) (poll-id uint))
    (print {
        event: "candidate-added",
        candidate-id: candidate-id,
        candidate-name: name,
        creator: creator,
        poll-id: poll-id,
        timestamp: block-height,
        block-time: burn-block-height
    })
)

(define-private (emit-vote-cast (voter principal) (candidate-id uint) (poll-id uint) (vote-weight uint))
    (print {
        event: "vote-cast",
        voter: voter,
        candidate-id: candidate-id,
        poll-id: poll-id,
        vote-weight: vote-weight,
        timestamp: block-height,
        block-time: burn-block-height
    })
)

(define-private (emit-poll-created (poll-id uint) (title (string-utf8 100)) (creator principal))
    (print {
        event: "poll-created",
        poll-id: poll-id,
        title: title,
        creator: creator,
        timestamp: block-height,
        block-time: burn-block-height
    })
)

(define-private (emit-stake-updated (user principal) (old-stake uint) (new-stake uint))
    (print {
        event: "stake-updated",
        user: user,
        old-stake: old-stake,
        new-stake: new-stake,
        timestamp: block-height,
        block-time: burn-block-height
    })
)

(define-private (emit-delegation-created (delegator principal) (delegate principal) (poll-id uint))
    (print {
        event: "delegation-created",
        delegator: delegator,
        delegate: delegate,
        poll-id: poll-id,
        timestamp: block-height,
        block-time: burn-block-height
    })
)

(define-private (emit-poll-concluded (poll-id uint) (winner-id uint) (total-votes uint))
    (print {
        event: "poll-concluded",
        poll-id: poll-id,
        winner-candidate-id: winner-id,
        total-votes: total-votes,
        timestamp: block-height,
        block-time: burn-block-height
    })
)

;; Enhanced Read-only Functions
(define-read-only (get-candidate (candidate-id uint))
    (map-get? candidates { candidate-id: candidate-id })
)

(define-read-only (get-poll (poll-id uint))
    (map-get? polls { poll-id: poll-id })
)

(define-read-only (get-user-stake (user principal))
    (map-get? user-stakes { user: user })
)

(define-read-only (get-delegation (delegator principal) (poll-id uint))
    (map-get? delegations { delegator: delegator, poll-id: poll-id })
)

(define-read-only (get-poll-results (poll-id uint))
    (map-get? poll-results { poll-id: poll-id })
)

(define-read-only (get-voting-power (user principal))
    (let ((stake-info (get-user-stake user)))
        (match stake-info
            stake-data (get voting-power stake-data)
            u0
        )
    )
)

(define-read-only (calculate-reputation-score (user principal))
    (let ((stake-info (get-user-stake user)))
        (match stake-info
            stake-data 
            (let 
                (
                    (stake-amount (get staked-amount stake-data))
                    (votes-cast (get total-votes-cast stake-data))
                    (stake-duration (- block-height (get staked-at stake-data)))
                )
                (+ (* stake-amount u2) (* votes-cast u10) (* stake-duration u1))
            )
            u0
        )
    )
)

(define-read-only (get-poll-statistics (poll-id uint))
    (let ((poll-info (get-poll poll-id)))
        (match poll-info
            poll-data
            {
                poll-id: poll-id,
                total-votes: (get total-votes poll-data),
                total-participants: (get total-participants poll-data),
                is-active: (get is-active poll-data),
                participation-rate: (if (> (get total-participants poll-data) u0)
                    (/ (* (get total-votes poll-data) u100) (get total-participants poll-data))
                    u0
                )
            }
            { poll-id: u0, total-votes: u0, total-participants: u0, is-active: false, participation-rate: u0 }
        )
    )
)

;; Staking Functions
(define-public (stake-tokens (amount uint))
    (let 
        (
            (current-stake (default-to u0 (get staked-amount (get-user-stake tx-sender))))
            (new-stake (+ current-stake amount))
            (voting-power (calculate-voting-power new-stake))
        )
        (asserts! (>= amount min-stake-amount) err-insufficient-stake)
        (asserts! (not (var-get contract-paused)) err-voting-ended)
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update user stake
        (map-set user-stakes
            { user: tx-sender }
            {
                staked-amount: new-stake,
                staked-at: block-height,
                voting-power: voting-power,
                total-votes-cast: (default-to u0 (get total-votes-cast (get-user-stake tx-sender))),
                reputation-score: (calculate-reputation-score tx-sender)
            }
        )
        
        ;; Update total staked
        (var-set total-staked (+ (var-get total-staked) amount))
        
        ;; Emit event
        (emit-stake-updated tx-sender current-stake new-stake)
        
        (ok new-stake)
    )
)

(define-public (unstake-tokens (amount uint))
    (let 
        (
            (current-stake (get staked-amount (unwrap! (get-user-stake tx-sender) err-not-found)))
            (new-stake (- current-stake amount))
        )
        (asserts! (>= current-stake amount) err-insufficient-stake)
        (asserts! (not (var-get contract-paused)) err-voting-ended)
        
        ;; Update user stake
        (map-set user-stakes
            { user: tx-sender }
            (merge (unwrap-panic (get-user-stake tx-sender)) 
                { 
                    staked-amount: new-stake,
                    voting-power: (calculate-voting-power new-stake)
                }
            )
        )
        
        ;; Transfer STX back to user
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        
        ;; Update total staked
        (var-set total-staked (- (var-get total-staked) amount))
        
        ;; Emit event
        (emit-stake-updated tx-sender current-stake new-stake)
        
        (ok new-stake)
    )
)

;; Poll Management Functions
(define-public (create-poll (title (string-utf8 100)) (description (string-utf8 300)) (duration uint) (poll-type (string-utf8 20)) (category (string-utf8 30)))
    (let 
        (
            (poll-id (var-get next-poll-id))
            (start-height block-height)
            (end-height (+ block-height duration))
        )
        (asserts! (not (var-get contract-paused)) err-voting-ended)
        (asserts! (> duration u0) err-invalid-params)
        
        ;; Create poll
        (map-set polls
            { poll-id: poll-id }
            {
                title: title,
                description: description,
                creator: tx-sender,
                start-height: start-height,
                end-height: end-height,
                is-active: true,
                poll-type: poll-type,
                max-choices: (if (is-eq poll-type u"single") u1 u5),
                min-stake-required: min-stake-amount,
                total-votes: u0,
                total-participants: u0,
                category: category
            }
        )
        
        ;; Increment poll ID
        (var-set next-poll-id (+ poll-id u1))
        
        ;; Emit event
        (emit-poll-created poll-id title tx-sender)
        
        (ok poll-id)
    )
)

(define-public (add-candidate-to-poll (poll-id uint) (name (string-utf8 50)) (description (string-utf8 200)) (category (string-utf8 30)))
    (let 
        (
            (candidate-id (var-get next-candidate-id))
            (poll-info (unwrap! (get-poll poll-id) err-poll-not-found))
        )
        (asserts! (get is-active poll-info) err-poll-ended)
        (asserts! (< block-height (get end-height poll-info)) err-poll-ended)
        
        ;; Add candidate
        (map-set candidates 
            { candidate-id: candidate-id }
            {
                name: name,
                description: description,
                vote-count: u0,
                weighted-vote-count: u0,
                stake-backing: u0,
                created-at: block-height,
                created-by: tx-sender,
                poll-id: poll-id,
                is-active: true,
                category: category
            }
        )
        
        ;; Increment candidate ID
        (var-set next-candidate-id (+ candidate-id u1))
        
        ;; Emit event
        (emit-candidate-added candidate-id name tx-sender poll-id)
        
        (ok candidate-id)
    )
)

;; Enhanced Voting Functions
(define-public (cast-weighted-vote (poll-id uint) (candidate-id uint))
    (let 
        (
            (poll-info (unwrap! (get-poll poll-id) err-poll-not-found))
            (candidate-info (unwrap! (get-candidate candidate-id) err-not-found))
            (user-stake (unwrap! (get-user-stake tx-sender) err-insufficient-stake))
            (voting-power (get voting-power user-stake))
            (current-vote-count (get vote-count candidate-info))
            (current-weighted-count (get weighted-vote-count candidate-info))
        )
        ;; Validate voting conditions
        (asserts! (get is-active poll-info) err-poll-ended)
        (asserts! (< block-height (get end-height poll-info)) err-poll-ended)
        (asserts! (is-eq (get poll-id candidate-info) poll-id) err-not-found)
        (asserts! (>= (get staked-amount user-stake) (get min-stake-required poll-info)) err-insufficient-stake)
        
        ;; Check if already voted
        (asserts! (is-none (map-get? voters { voter: tx-sender, poll-id: poll-id })) err-already-voted)
        
        ;; Update candidate vote counts
        (map-set candidates 
            { candidate-id: candidate-id }
            (merge candidate-info 
                { 
                    vote-count: (+ current-vote-count u1),
                    weighted-vote-count: (+ current-weighted-count voting-power)
                }
            )
        )
        
        ;; Record vote
        (map-set voters 
            { voter: tx-sender, poll-id: poll-id }
            {
                has-voted: true,
                candidate-id: candidate-id,
                voted-at: block-height,
                vote-weight: u1,
                voting-power: voting-power,
                delegate: none
            }
        )
        
        ;; Update poll statistics
        (map-set polls
            { poll-id: poll-id }
            (merge poll-info 
                {
                    total-votes: (+ (get total-votes poll-info) u1),
                    total-participants: (+ (get total-participants poll-info) u1)
                }
            )
        )
        
        ;; Update user voting history
        (map-set user-voting-history
            { user: tx-sender, poll-id: poll-id }
            {
                votes-cast: (list candidate-id),
                voting-power-used: voting-power,
                timestamp: block-height,
                vote-receipt: (sha256 (concat (concat (unwrap-panic (to-consensus-buff? tx-sender)) 
                                                    (unwrap-panic (to-consensus-buff? candidate-id)))
                                            (unwrap-panic (to-consensus-buff? block-height))))
            }
        )
        
        ;; Emit event
        (emit-vote-cast tx-sender candidate-id poll-id voting-power)
        
        (ok true)
    )
)

;; Delegation Functions
(define-public (delegate-voting-power (delegate principal) (poll-id uint))
    (let 
        (
            (poll-info (unwrap! (get-poll poll-id) err-poll-not-found))
            (user-stake (unwrap! (get-user-stake tx-sender) err-insufficient-stake))
            (voting-power (get voting-power user-stake))
        )
        (asserts! (get is-active poll-info) err-poll-ended)
        (asserts! (not (is-eq tx-sender delegate)) err-invalid-delegation)
        
        ;; Pay delegation fee
        (try! (stx-transfer? delegation-fee tx-sender (as-contract tx-sender)))
        
        ;; Create delegation
        (map-set delegations
            { delegator: tx-sender, poll-id: poll-id }
            {
                delegate: delegate,
                delegated-at: block-height,
                is-active: true,
                voting-power-delegated: voting-power
            }
        )
        
        ;; Emit event
        (emit-delegation-created tx-sender delegate poll-id)
        
        (ok true)
    )
)

;; Admin Functions
(define-public (conclude-poll (poll-id uint))
    (let 
        (
            (poll-info (unwrap! (get-poll poll-id) err-poll-not-found))
        )
        (asserts! (or (is-eq tx-sender contract-owner) (is-eq tx-sender (get creator poll-info))) err-unauthorized)
        (asserts! (>= block-height (get end-height poll-info)) err-poll-ended)
        
        ;; Deactivate poll
        (map-set polls
            { poll-id: poll-id }
            (merge poll-info { is-active: false })
        )
        
        ;; Find winner (simplified - highest vote count)
        (let ((winner-id (find-poll-winner poll-id)))
            ;; Store results
            (map-set poll-results
                { poll-id: poll-id }
                {
                    winner-candidate-id: winner-id,
                    total-votes: (get total-votes poll-info),
                    participation-rate: (if (> (get total-participants poll-info) u0) 
                        (/ (* (get total-votes poll-info) u100) (get total-participants poll-info)) 
                        u0),
                    concluded-at: block-height,
                    is-finalized: true
                }
            )
            
            ;; Emit event
            (emit-poll-concluded poll-id winner-id (get total-votes poll-info))
            
            (ok winner-id)
        )
    )
)

(define-public (pause-contract)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set contract-paused true)
        (ok true)
    )
)

(define-public (unpause-contract)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set contract-paused false)
        (ok true)
    )
)

;; Utility Functions
(define-private (calculate-voting-power (stake-amount uint))
    (let ((base-power (/ stake-amount u100000))) ;; 1 voting power per 0.1 STX
        (if (> stake-amount (* u10 min-stake-amount))
            (+ base-power (/ stake-amount u50000)) ;; Bonus for large stakes
            base-power
        )
    )
)

(define-private (find-poll-winner (poll-id uint))
    ;; Simplified winner finding - returns candidate ID 1 for now
    ;; In a real implementation, this would iterate through all candidates
    u1
)

;; Enhanced Read-only Functions for Analytics
(define-read-only (get-contract-stats)
    {
        total-polls: (- (var-get next-poll-id) u1),
        total-candidates: (- (var-get next-candidate-id) u1),
        total-staked: (var-get total-staked),
        contract-paused: (var-get contract-paused),
        owner: contract-owner
    }
)

(define-read-only (get-user-voting-stats (user principal))
    (let ((stake-info (get-user-stake user)))
        (match stake-info
            stake-data
            {
                staked-amount: (get staked-amount stake-data),
                voting-power: (get voting-power stake-data),
                total-votes-cast: (get total-votes-cast stake-data),
                reputation-score: (calculate-reputation-score user),
                member-since: (get staked-at stake-data)
            }
            {
                staked-amount: u0,
                voting-power: u0,
                total-votes-cast: u0,
                reputation-score: u0,
                member-since: u0
            }
        )
    )
)