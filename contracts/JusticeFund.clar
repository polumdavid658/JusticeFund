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

(define-data-var reward-tier-counter uint u0)
(define-data-var max-rewards-per-campaign uint u10)

(define-map campaign-reward-tiers
    { campaign-id: uint, tier-id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 300),
        minimum-donation: uint,
        total-quantity: uint,
        claimed-quantity: uint,
        reward-type: (string-ascii 20),
        delivery-info: (string-ascii 200),
        estimated-delivery: uint
    }
)

(define-map donor-reward-claims
    { campaign-id: uint, tier-id: uint, donor: principal }
    {
        claim-block: uint,
        fulfillment-status: (string-ascii 20),
        fulfillment-date: uint,
        tracking-info: (string-ascii 100)
    }
)

(define-map campaign-reward-settings
    { campaign-id: uint }
    {
        rewards-enabled: bool,
        total-tiers: uint,
        auto-fulfill-digital: bool
    }
)

(define-public (create-reward-tier 
    (campaign-id uint) 
    (title (string-ascii 100)) 
    (description (string-ascii 300)) 
    (minimum-donation uint) 
    (total-quantity uint) 
    (reward-type (string-ascii 20)) 
    (delivery-info (string-ascii 200)) 
    (estimated-delivery uint))
    (let
        (
            (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err u30)))
            (campaign-settings (default-to 
                { rewards-enabled: false, total-tiers: u0, auto-fulfill-digital: false } 
                (map-get? campaign-reward-settings { campaign-id: campaign-id })))
            (tier-id (+ (get total-tiers campaign-settings) u1))
        )
        (asserts! (is-eq tx-sender (get creator campaign)) (err u31))
        (asserts! (is-eq (get status campaign) "active") (err u32))
        (asserts! (< (get total-tiers campaign-settings) (var-get max-rewards-per-campaign)) (err u33))
        (asserts! (>= minimum-donation min-donation) (err u34))
        (asserts! (> total-quantity u0) (err u35))
        (asserts! (> estimated-delivery stacks-block-height) (err u36))
        
        (map-set campaign-reward-tiers
            { campaign-id: campaign-id, tier-id: tier-id }
            {
                title: title,
                description: description,
                minimum-donation: minimum-donation,
                total-quantity: total-quantity,
                claimed-quantity: u0,
                reward-type: reward-type,
                delivery-info: delivery-info,
                estimated-delivery: estimated-delivery
            }
        )
        
        (map-set campaign-reward-settings
            { campaign-id: campaign-id }
            (merge campaign-settings 
                { 
                    rewards-enabled: true, 
                    total-tiers: tier-id 
                })
        )
        
        (ok tier-id)
    )
)

(define-public (claim-reward (campaign-id uint) (tier-id uint))
    (let
        (
            (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err u37)))
            (reward-tier (unwrap! (map-get? campaign-reward-tiers { campaign-id: campaign-id, tier-id: tier-id }) (err u38)))
            (donor-donation (unwrap! (map-get? campaign-donations { campaign-id: campaign-id, donor: tx-sender }) (err u39)))
            (existing-claim (map-get? donor-reward-claims { campaign-id: campaign-id, tier-id: tier-id, donor: tx-sender }))
        )
        (asserts! (is-none existing-claim) (err u40))
        (asserts! (>= (get amount donor-donation) (get minimum-donation reward-tier)) (err u41))
        (asserts! (< (get claimed-quantity reward-tier) (get total-quantity reward-tier)) (err u42))
        
        (map-set donor-reward-claims
            { campaign-id: campaign-id, tier-id: tier-id, donor: tx-sender }
            {
                claim-block: stacks-block-height,
                fulfillment-status: (if (is-eq (get reward-type reward-tier) "digital") "fulfilled" "pending"),
                fulfillment-date: (if (is-eq (get reward-type reward-tier) "digital") stacks-block-height u0),
                tracking-info: ""
            }
        )
        
        (map-set campaign-reward-tiers
            { campaign-id: campaign-id, tier-id: tier-id }
            (merge reward-tier { claimed-quantity: (+ (get claimed-quantity reward-tier) u1) })
        )
        
        (ok true)
    )
)

(define-public (update-reward-fulfillment 
    (campaign-id uint) 
    (tier-id uint) 
    (donor principal) 
    (fulfillment-status (string-ascii 20)) 
    (tracking-info (string-ascii 100)))
    (let
        (
            (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err u43)))
            (reward-claim (unwrap! (map-get? donor-reward-claims { campaign-id: campaign-id, tier-id: tier-id, donor: donor }) (err u44)))
        )
        (asserts! (is-eq tx-sender (get creator campaign)) (err u45))
        
        (map-set donor-reward-claims
            { campaign-id: campaign-id, tier-id: tier-id, donor: donor }
            (merge reward-claim 
                {
                    fulfillment-status: fulfillment-status,
                    fulfillment-date: (if (is-eq fulfillment-status "fulfilled") stacks-block-height (get fulfillment-date reward-claim)),
                    tracking-info: tracking-info
                })
        )
        
        (ok true)
    )
)

(define-public (bulk-fulfill-digital-rewards (campaign-id uint) (tier-id uint))
    (let
        (
            (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err u46)))
            (reward-tier (unwrap! (map-get? campaign-reward-tiers { campaign-id: campaign-id, tier-id: tier-id }) (err u47)))
        )
        (asserts! (is-eq tx-sender (get creator campaign)) (err u48))
        (asserts! (is-eq (get reward-type reward-tier) "digital") (err u49))
        
        (ok true)
    )
)

(define-public (update-reward-tier-quantity (campaign-id uint) (tier-id uint) (new-quantity uint))
    (let
        (
            (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err u50)))
            (reward-tier (unwrap! (map-get? campaign-reward-tiers { campaign-id: campaign-id, tier-id: tier-id }) (err u51)))
        )
        (asserts! (is-eq tx-sender (get creator campaign)) (err u52))
        (asserts! (is-eq (get status campaign) "active") (err u53))
        (asserts! (>= new-quantity (get claimed-quantity reward-tier)) (err u54))
        
        (map-set campaign-reward-tiers
            { campaign-id: campaign-id, tier-id: tier-id }
            (merge reward-tier { total-quantity: new-quantity })
        )
        
        (ok true)
    )
)

(define-private (check-reward-eligibility (campaign-id uint) (donor principal))
    (let
        (
            (donor-donation (map-get? campaign-donations { campaign-id: campaign-id, donor: donor }))
            (campaign-settings (map-get? campaign-reward-settings { campaign-id: campaign-id }))
        )
        (and 
            (is-some donor-donation)
            (is-some campaign-settings)
            (get rewards-enabled (unwrap-panic campaign-settings))
        )
    )
)

(define-private (get-eligible-reward-tiers (campaign-id uint) (donation-amount uint))
    (let
        (
            (campaign-settings (map-get? campaign-reward-settings { campaign-id: campaign-id }))
        )
        (if (is-some campaign-settings)
            (get total-tiers (unwrap-panic campaign-settings))
            u0)
    )
)

(define-read-only (get-campaign-reward-tier (campaign-id uint) (tier-id uint))
    (ok (map-get? campaign-reward-tiers { campaign-id: campaign-id, tier-id: tier-id }))
)

(define-read-only (get-donor-reward-claim (campaign-id uint) (tier-id uint) (donor principal))
    (ok (map-get? donor-reward-claims { campaign-id: campaign-id, tier-id: tier-id, donor: donor }))
)

(define-read-only (get-campaign-reward-settings (campaign-id uint))
    (ok (map-get? campaign-reward-settings { campaign-id: campaign-id }))
)

(define-read-only (get-reward-tier-availability (campaign-id uint) (tier-id uint))
    (let
        (
            (reward-tier (map-get? campaign-reward-tiers { campaign-id: campaign-id, tier-id: tier-id }))
        )
        (if (is-some reward-tier)
            (let
                (
                    (tier-data (unwrap-panic reward-tier))
                )
                (ok (some {
                    available: (- (get total-quantity tier-data) (get claimed-quantity tier-data)),
                    total: (get total-quantity tier-data),
                    claimed: (get claimed-quantity tier-data)
                }))
            )
            (ok none)
        )
    )
)

(define-read-only (get-donor-eligible-rewards (campaign-id uint) (donor principal))
    (let
        (
            (donor-donation (map-get? campaign-donations { campaign-id: campaign-id, donor: donor }))
            (campaign-settings (map-get? campaign-reward-settings { campaign-id: campaign-id }))
        )
        (if (and (is-some donor-donation) (is-some campaign-settings))
            (ok (some {
                donation-amount: (get amount (unwrap-panic donor-donation)),
                rewards-enabled: (get rewards-enabled (unwrap-panic campaign-settings)),
                total-tiers: (get total-tiers (unwrap-panic campaign-settings))
            }))
            (ok none)
        )
    )
)

(define-public (toggle-campaign-rewards (campaign-id uint) (enabled bool))
    (let
        (
            (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err u55)))
            (campaign-settings (default-to 
                { rewards-enabled: false, total-tiers: u0, auto-fulfill-digital: false } 
                (map-get? campaign-reward-settings { campaign-id: campaign-id })))
        )
        (asserts! (is-eq tx-sender (get creator campaign)) (err u56))
        (asserts! (is-eq (get status campaign) "active") (err u57))
        
        (map-set campaign-reward-settings
            { campaign-id: campaign-id }
            (merge campaign-settings { rewards-enabled: enabled })
        )
        
        (ok true)
    )
)


