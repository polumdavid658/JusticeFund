(define-constant contract-owner tx-sender)
(define-constant min-donation u100000)
(define-constant min-campaign-duration u1440)
(define-constant max-campaign-duration u43200)

(define-data-var total-funds-raised uint u0)
(define-data-var campaign-counter uint u0)

(define-map campaigns
    { campaign-id: uint }
    {
        creator: principal,
        title: (string-ascii 50),
        description: (string-ascii 500),
        target-amount: uint,
        end-block: uint,
        status: (string-ascii 10),
        funds-raised: uint
    }
)

(define-map campaign-donations
    { campaign-id: uint, donor: principal }
    { amount: uint }
)

(define-map donor-total-contributions
    { donor: principal }
    { total-amount: uint }
)

(define-public (create-campaign (title (string-ascii 50)) (description (string-ascii 500)) (target-amount uint) (duration uint))
    (let
        (
            (campaign-id (+ (var-get campaign-counter) u1))
            (end-block (+ stacks-block-height duration))
        )
        (asserts! (>= duration min-campaign-duration) (err u1))
        (asserts! (<= duration max-campaign-duration) (err u2))
        (asserts! (>= target-amount min-donation) (err u3))
        
        (map-set campaigns
            { campaign-id: campaign-id }
            {
                creator: tx-sender,
                title: title,
                description: description,
                target-amount: target-amount,
                end-block: end-block,
                status: "active",
                funds-raised: u0
            }
        )
        (var-set campaign-counter campaign-id)
        (ok campaign-id)
    )
)

(define-public (donate (campaign-id uint))
    (let
        (
            (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err u4)))
            (current-donation (default-to { amount: u0 } (map-get? campaign-donations { campaign-id: campaign-id, donor: tx-sender })))
            (donor-contributions (default-to { total-amount: u0 } (map-get? donor-total-contributions { donor: tx-sender })))
            (amount (stx-get-balance tx-sender))
        )
        (asserts! (>= amount min-donation) (err u5))
        (asserts! (is-eq (get status campaign) "active") (err u6))
        (asserts! (<= stacks-block-height (get end-block campaign)) (err u7))
        
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        (map-set campaign-donations
            { campaign-id: campaign-id, donor: tx-sender }
            { amount: (+ (get amount current-donation) amount) }
        )
        
        (map-set donor-total-contributions
            { donor: tx-sender }
            { total-amount: (+ (get total-amount donor-contributions) amount) }
        )
        
        (map-set campaigns
            { campaign-id: campaign-id }
            (merge campaign { funds-raised: (+ (get funds-raised campaign) amount) })
        )
        
        (var-set total-funds-raised (+ (var-get total-funds-raised) amount))
        (let
            (
                (rep-result (update-user-reputation tx-sender "donate" amount))
                (badge-result (check-donation-badges tx-sender amount))
            )
            (ok true)
        )
    )
)
(define-public (finalize-campaign (campaign-id uint))
    (let
        (
            (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err u8)))
        )
        (asserts! (is-eq (get status campaign) "active") (err u9))
        (asserts! (>= stacks-block-height (get end-block campaign)) (err u10))
        
        (if (>= (get funds-raised campaign) (get target-amount campaign))
            (begin
                (try! (as-contract (stx-transfer? (get funds-raised campaign) tx-sender (get creator campaign))))
                (let ((rep-result (update-user-reputation (get creator campaign) "success" u0))) true)
            )
            (begin
                (try! (refund-campaign campaign-id))
                (let ((rep-result (update-user-reputation (get creator campaign) "fail" u0))) true)
            )
        )
        
        (map-set campaigns
            { campaign-id: campaign-id }
            (merge campaign { status: "completed" })
        )
        (ok true)
    )
)

(define-private (refund-campaign (campaign-id uint))
    (let
        (
            (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err u11)))
        )
        (map-set campaigns
            { campaign-id: campaign-id }
            (merge campaign { status: "refunded" })
        )
        (ok true)
    )
)

(define-read-only (get-campaign-details (campaign-id uint))
    (ok (map-get? campaigns { campaign-id: campaign-id }))
)

(define-read-only (get-donation-amount (campaign-id uint) (donor principal))
    (ok (map-get? campaign-donations { campaign-id: campaign-id, donor: donor }))
)

(define-read-only (get-donor-total-contribution (donor principal))
    (ok (map-get? donor-total-contributions { donor: donor }))
)

(define-read-only (get-total-funds-raised)
    (ok (var-get total-funds-raised))
)

(define-data-var reputation-decay-rate uint u5)
(define-data-var min-reputation-score uint u100)
(define-data-var max-reputation-score uint u1000)

(define-map user-reputation
    { user: principal }
    {
        score: uint,
        successful-campaigns: uint,
        failed-campaigns: uint,
        total-donations: uint,
        last-activity-block: uint,
        creator-rating: uint,
        donor-rating: uint
    }
)

(define-map campaign-ratings
    { campaign-id: uint, rater: principal }
    {
        rating: uint,
        review: (string-ascii 200),
        rating-block: uint
    }
)

(define-map reputation-badges
    { user: principal, badge-type: (string-ascii 20) }
    {
        earned-block: uint,
        badge-value: uint
    }
)

(define-private (calculate-reputation-score (user principal))
    (let
        (
            (user-rep (default-to 
                { 
                    score: u500, 
                    successful-campaigns: u0, 
                    failed-campaigns: u0, 
                    total-donations: u0, 
                    last-activity-block: u0, 
                    creator-rating: u500, 
                    donor-rating: u500 
                } 
                (map-get? user-reputation { user: user })
            ))
            (block-diff (- stacks-block-height (get last-activity-block user-rep)))
            (decay-amount (/ (* block-diff (var-get reputation-decay-rate)) u100))
            (success-rate (if (> (+ (get successful-campaigns user-rep) (get failed-campaigns user-rep)) u0)
                (/ (* (get successful-campaigns user-rep) u100) 
                   (+ (get successful-campaigns user-rep) (get failed-campaigns user-rep)))
                u50))
            (donation-bonus-raw (/ (get total-donations user-rep) u1000000))
            (donation-bonus (if (> donation-bonus-raw u200) u200 donation-bonus-raw))
            (rating-average (/ (+ (get creator-rating user-rep) (get donor-rating user-rep)) u2))
            (base-score (+ (+ rating-average donation-bonus) (* success-rate u2)))
            (decayed-score (if (> base-score decay-amount) (- base-score decay-amount) u0))
            (capped-score (if (> decayed-score (var-get max-reputation-score)) (var-get max-reputation-score) decayed-score))
        )
        (if (< capped-score (var-get min-reputation-score)) (var-get min-reputation-score) capped-score)
    )
)

(define-private (update-user-reputation (user principal) (action (string-ascii 20)) (value uint))
    (let
        (
            (current-rep (default-to 
                { 
                    score: u500, 
                    successful-campaigns: u0, 
                    failed-campaigns: u0, 
                    total-donations: u0, 
                    last-activity-block: stacks-block-height, 
                    creator-rating: u500, 
                    donor-rating: u500 
                } 
                (map-get? user-reputation { user: user })
            ))
            (new-rep (if (is-eq action "donate")
                (merge current-rep 
                    { 
                        total-donations: (+ (get total-donations current-rep) value),
                        last-activity-block: stacks-block-height
                    })
                (if (is-eq action "success")
                    (merge current-rep 
                        { 
                            successful-campaigns: (+ (get successful-campaigns current-rep) u1),
                            last-activity-block: stacks-block-height
                        })
                    (if (is-eq action "fail")
                        (merge current-rep 
                            { 
                                failed-campaigns: (+ (get failed-campaigns current-rep) u1),
                                last-activity-block: stacks-block-height
                            })
                        current-rep))))
            (updated-score (calculate-reputation-score user))
        )
        (map-set user-reputation 
            { user: user }
            (merge new-rep { score: updated-score })
        )
        (ok true)
    )
)

(define-public (rate-campaign (campaign-id uint) (rating uint) (review (string-ascii 200)))
    (let
        (
            (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err u20)))
            (existing-rating (map-get? campaign-ratings { campaign-id: campaign-id, rater: tx-sender }))
        )
        (asserts! (and (>= rating u1) (<= rating u5)) (err u21))
        (asserts! (is-eq (get status campaign) "completed") (err u22))
        (asserts! (is-none existing-rating) (err u23))
        (asserts! (not (is-eq tx-sender (get creator campaign))) (err u24))
        
        (map-set campaign-ratings
            { campaign-id: campaign-id, rater: tx-sender }
            {
                rating: rating,
                review: review,
                rating-block: stacks-block-height
            }
        )
        
        (let ((rating-result (update-creator-rating (get creator campaign) campaign-id)))
            (ok true)
        )
    )
)

(define-private (update-creator-rating (creator principal) (campaign-id uint))
    (let
        (
            (current-rep (default-to 
                { 
                    score: u500, 
                    successful-campaigns: u0, 
                    failed-campaigns: u0, 
                    total-donations: u0, 
                    last-activity-block: stacks-block-height, 
                    creator-rating: u500, 
                    donor-rating: u500 
                } 
                (map-get? user-reputation { user: creator })
            ))
            (total-rating (fold calculate-average-rating (list campaign-id) u0))
            (new-creator-rating (if (> total-rating u0) (* total-rating u100) u500))
        )
        (map-set user-reputation
            { user: creator }
            (merge current-rep { creator-rating: new-creator-rating })
        )
        (ok true)
    )
)

(define-private (calculate-average-rating (campaign-id uint) (acc uint))
    (+ acc u5)
)

(define-public (award-badge (user principal) (badge-type (string-ascii 20)) (badge-value uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u25))
        (asserts! (is-none (map-get? reputation-badges { user: user, badge-type: badge-type })) (err u26))
        
        (map-set reputation-badges
            { user: user, badge-type: badge-type }
            {
                earned-block: stacks-block-height,
                badge-value: badge-value
            }
        )
        (ok true)
    )
)

(define-private (check-donation-badges (donor principal) (amount uint))
    (let
        (
            (donor-rep (default-to 
                { 
                    score: u500, 
                    successful-campaigns: u0, 
                    failed-campaigns: u0, 
                    total-donations: u0, 
                    last-activity-block: stacks-block-height, 
                    creator-rating: u500, 
                    donor-rating: u500 
                } 
                (map-get? user-reputation { user: donor })
            ))
            (total-donations (get total-donations donor-rep))
        )
        (if (and (>= total-donations u10000000) (is-none (map-get? reputation-badges { user: donor, badge-type: "generous-donor" })))
            (map-set reputation-badges
                { user: donor, badge-type: "generous-donor" }
                { earned-block: stacks-block-height, badge-value: u100 })
            true)
        (if (and (>= total-donations u1000000) (is-none (map-get? reputation-badges { user: donor, badge-type: "supporter" })))
            (map-set reputation-badges
                { user: donor, badge-type: "supporter" }
                { earned-block: stacks-block-height, badge-value: u50 })
            true)
        (ok true)
    )
)

(define-read-only (get-user-reputation (user principal))
    (let
        (
            (user-rep (map-get? user-reputation { user: user }))
        )
        (if (is-some user-rep)
            (ok (some (merge (unwrap-panic user-rep) { score: (calculate-reputation-score user) })))
            (ok none))
    )
)

(define-read-only (get-campaign-rating (campaign-id uint) (rater principal))
    (ok (map-get? campaign-ratings { campaign-id: campaign-id, rater: rater }))
)

(define-read-only (get-user-badge (user principal) (badge-type (string-ascii 20)))
    (ok (map-get? reputation-badges { user: user, badge-type: badge-type }))
)

(define-read-only (get-reputation-settings)
    (ok {
        decay-rate: (var-get reputation-decay-rate),
        min-score: (var-get min-reputation-score),
        max-score: (var-get max-reputation-score)
    })
)

(define-public (update-reputation-settings (decay-rate uint) (min-score uint) (max-score uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u27))
        (asserts! (<= min-score max-score) (err u28))
        (asserts! (<= decay-rate u10) (err u29))
        
        (var-set reputation-decay-rate decay-rate)
        (var-set min-reputation-score min-score)
        (var-set max-reputation-score max-score)
        (ok true)
    )
)