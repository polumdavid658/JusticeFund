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
        (ok true)
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
            (try! (as-contract (stx-transfer? (get funds-raised campaign) tx-sender (get creator campaign))))
            (try! (refund-campaign campaign-id))
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