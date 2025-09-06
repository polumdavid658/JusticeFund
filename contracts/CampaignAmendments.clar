;; Campaign Amendment System for JusticeFund
;; Enables campaign creators to update campaigns for evolving legal circumstances

(define-constant contract-owner tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_CAMPAIGN_NOT_FOUND (err u201))
(define-constant ERR_CAMPAIGN_NOT_ACTIVE (err u202))
(define-constant ERR_INVALID_AMENDMENT (err u203))
(define-constant ERR_AMENDMENT_LIMIT_REACHED (err u204))
(define-constant ERR_INVALID_TARGET_CHANGE (err u205))
(define-constant ERR_INVALID_DEADLINE_EXTENSION (err u206))

(define-data-var amendment-counter uint u0)
(define-data-var max-amendments-per-campaign uint u10)
(define-data-var max-target-increase-percent uint u150)
(define-data-var max-deadline-extension-blocks uint u14400)

(define-map campaign-amendments
  uint
  {
    campaign-id: uint,
    amendment-type: (string-ascii 20),
    title: (string-ascii 100),
    description: (string-ascii 500),
    creator: principal,
    created-block: uint,
    previous-value: uint,
    new-value: uint,
    status: (string-ascii 15),
    legal-justification: (string-ascii 300)
  }
)

(define-map campaign-amendment-count uint uint)

(define-map campaign-target-history
  {campaign-id: uint, version: uint}
  {
    target-amount: uint,
    changed-block: uint,
    amendment-id: uint
  }
)

(define-map campaign-deadline-history
  {campaign-id: uint, version: uint}
  {
    end-block: uint,
    changed-block: uint,
    amendment-id: uint
  }
)

(define-map campaign-updates
  {campaign-id: uint, update-id: uint}
  {
    update-title: (string-ascii 80),
    update-content: (string-ascii 400),
    update-type: (string-ascii 15),
    posted-block: uint,
    legal-milestone: bool
  }
)

(define-map campaign-update-count uint uint)

(define-public (post-campaign-update (campaign-id uint) (update-title (string-ascii 80)) (update-content (string-ascii 400)) (update-type (string-ascii 15)) (legal-milestone bool))
  (let
    (
      (campaign (unwrap! (contract-call? .JusticeFund get-campaign-details campaign-id) ERR_CAMPAIGN_NOT_FOUND))
      (update-id (+ (default-to u0 (map-get? campaign-update-count campaign-id)) u1))
    )
    (asserts! (is-some campaign) ERR_CAMPAIGN_NOT_FOUND)
    (asserts! (is-eq tx-sender (get creator (unwrap-panic campaign))) ERR_UNAUTHORIZED)
    (asserts! (> (len update-title) u0) ERR_INVALID_AMENDMENT)
    (asserts! (> (len update-content) u0) ERR_INVALID_AMENDMENT)
    
    (map-set campaign-updates
      {campaign-id: campaign-id, update-id: update-id}
      {
        update-title: update-title,
        update-content: update-content,
        update-type: update-type,
        posted-block: stacks-block-height,
        legal-milestone: legal-milestone
      }
    )
    
    (map-set campaign-update-count campaign-id update-id)
    (ok update-id)
  )
)

(define-public (amend-campaign-target (campaign-id uint) (new-target uint) (justification (string-ascii 300)))
  (let
    (
      (campaign (unwrap! (contract-call? .JusticeFund get-campaign-details campaign-id) ERR_CAMPAIGN_NOT_FOUND))
      (amendment-id (+ (var-get amendment-counter) u1))
      (amendment-count (default-to u0 (map-get? campaign-amendment-count campaign-id)))
      (version (+ (get-target-version campaign-id) u1))
    )
    (asserts! (is-some campaign) ERR_CAMPAIGN_NOT_FOUND)
    (let
      (
        (campaign-data (unwrap-panic campaign))
        (current-target (get target-amount campaign-data))
        (max-allowed-target (/ (* current-target (var-get max-target-increase-percent)) u100))
      )
      (asserts! (is-eq tx-sender (get creator campaign-data)) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get status campaign-data) "active") ERR_CAMPAIGN_NOT_ACTIVE)
      (asserts! (< amendment-count (var-get max-amendments-per-campaign)) ERR_AMENDMENT_LIMIT_REACHED)
      (asserts! (<= new-target max-allowed-target) ERR_INVALID_TARGET_CHANGE)
      (asserts! (> new-target current-target) ERR_INVALID_TARGET_CHANGE)
      (asserts! (> (len justification) u0) ERR_INVALID_AMENDMENT)
      
      (map-set campaign-amendments amendment-id
        {
          campaign-id: campaign-id,
          amendment-type: "target-increase",
          title: "Target Amount Increase",
          description: justification,
          creator: tx-sender,
          created-block: stacks-block-height,
          previous-value: current-target,
          new-value: new-target,
          status: "approved",
          legal-justification: justification
        }
      )
      
      (map-set campaign-target-history
        {campaign-id: campaign-id, version: version}
        {
          target-amount: new-target,
          changed-block: stacks-block-height,
          amendment-id: amendment-id
        }
      )
      
      (map-set campaign-amendment-count campaign-id (+ amendment-count u1))
      (var-set amendment-counter amendment-id)
      (ok amendment-id)
    )
  )
)

(define-public (extend-campaign-deadline (campaign-id uint) (additional-blocks uint) (legal-reason (string-ascii 300)))
  (let
    (
      (campaign (unwrap! (contract-call? .JusticeFund get-campaign-details campaign-id) ERR_CAMPAIGN_NOT_FOUND))
      (amendment-id (+ (var-get amendment-counter) u1))
      (amendment-count (default-to u0 (map-get? campaign-amendment-count campaign-id)))
      (version (+ (get-deadline-version campaign-id) u1))
    )
    (asserts! (is-some campaign) ERR_CAMPAIGN_NOT_FOUND)
    (let
      (
        (campaign-data (unwrap-panic campaign))
        (current-deadline (get end-block campaign-data))
        (new-deadline (+ current-deadline additional-blocks))
      )
      (asserts! (is-eq tx-sender (get creator campaign-data)) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get status campaign-data) "active") ERR_CAMPAIGN_NOT_ACTIVE)
      (asserts! (< amendment-count (var-get max-amendments-per-campaign)) ERR_AMENDMENT_LIMIT_REACHED)
      (asserts! (<= additional-blocks (var-get max-deadline-extension-blocks)) ERR_INVALID_DEADLINE_EXTENSION)
      (asserts! (> additional-blocks u0) ERR_INVALID_DEADLINE_EXTENSION)
      (asserts! (> (len legal-reason) u0) ERR_INVALID_AMENDMENT)
      
      (map-set campaign-amendments amendment-id
        {
          campaign-id: campaign-id,
          amendment-type: "deadline-extend",
          title: "Deadline Extension",
          description: legal-reason,
          creator: tx-sender,
          created-block: stacks-block-height,
          previous-value: current-deadline,
          new-value: new-deadline,
          status: "approved",
          legal-justification: legal-reason
        }
      )
      
      (map-set campaign-deadline-history
        {campaign-id: campaign-id, version: version}
        {
          end-block: new-deadline,
          changed-block: stacks-block-height,
          amendment-id: amendment-id
        }
      )
      
      (map-set campaign-amendment-count campaign-id (+ amendment-count u1))
      (var-set amendment-counter amendment-id)
      (ok amendment-id)
    )
  )
)

(define-public (set-amendment-limits (max-amendments uint) (max-target-percent uint) (max-extension-blocks uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) ERR_UNAUTHORIZED)
    (asserts! (<= max-amendments u20) ERR_INVALID_AMENDMENT)
    (asserts! (and (>= max-target-percent u100) (<= max-target-percent u300)) ERR_INVALID_TARGET_CHANGE)
    (asserts! (<= max-extension-blocks u43200) ERR_INVALID_DEADLINE_EXTENSION)
    
    (var-set max-amendments-per-campaign max-amendments)
    (var-set max-target-increase-percent max-target-percent)
    (var-set max-deadline-extension-blocks max-extension-blocks)
    (ok true)
  )
)

(define-private (get-target-version (campaign-id uint))
  (get count (fold count-target-versions (list u1 u2 u3 u4 u5) {campaign-id: campaign-id, count: u0}))
)

(define-private (count-target-versions (version uint) (acc {campaign-id: uint, count: uint}))
  (let
    (
      (target-entry (map-get? campaign-target-history {campaign-id: (get campaign-id acc), version: version}))
    )
    (if (is-some target-entry)
      {campaign-id: (get campaign-id acc), count: version}
      acc
    )
  )
)

(define-private (get-deadline-version (campaign-id uint))
  (get count (fold count-deadline-versions (list u1 u2 u3 u4 u5) {campaign-id: campaign-id, count: u0}))
)

(define-private (count-deadline-versions (version uint) (acc {campaign-id: uint, count: uint}))
  (let
    (
      (deadline-entry (map-get? campaign-deadline-history {campaign-id: (get campaign-id acc), version: version}))
    )
    (if (is-some deadline-entry)
      {campaign-id: (get campaign-id acc), count: version}
      acc
    )
  )
)

(define-read-only (get-campaign-amendment (amendment-id uint))
  (map-get? campaign-amendments amendment-id)
)

(define-read-only (get-campaign-updates (campaign-id uint))
  (let
    (
      (update-count (default-to u0 (map-get? campaign-update-count campaign-id)))
    )
    (ok {
      total-updates: update-count,
      latest-update: (if (> update-count u0) (map-get? campaign-updates {campaign-id: campaign-id, update-id: update-count}) none)
    })
  )
)

(define-read-only (get-campaign-amendment-history (campaign-id uint))
  (let
    (
      (amendment-count (default-to u0 (map-get? campaign-amendment-count campaign-id)))
      (target-version (get-target-version campaign-id))
      (deadline-version (get-deadline-version campaign-id))
    )
    (ok {
      total-amendments: amendment-count,
      target-changes: target-version,
      deadline-extensions: deadline-version
    })
  )
)

(define-read-only (get-campaign-target-history (campaign-id uint) (version uint))
  (map-get? campaign-target-history {campaign-id: campaign-id, version: version})
)

(define-read-only (get-campaign-deadline-history (campaign-id uint) (version uint))
  (map-get? campaign-deadline-history {campaign-id: campaign-id, version: version})
)

(define-read-only (get-specific-update (campaign-id uint) (update-id uint))
  (map-get? campaign-updates {campaign-id: campaign-id, update-id: update-id})
)

(define-read-only (get-amendment-limits)
  (ok {
    max-amendments: (var-get max-amendments-per-campaign),
    max-target-percent: (var-get max-target-increase-percent),
    max-extension-blocks: (var-get max-deadline-extension-blocks)
  })
)
