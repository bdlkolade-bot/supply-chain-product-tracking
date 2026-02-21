;; title: supply_chain_product_tracking
;; version: 1.0.0
;; summary: Smart contract for Supply Chain Product Tracking

;; Error codes
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-INVALID-STATUS (err u402))
(define-constant ERR-PRODUCT-EXISTS (err u403))
(define-constant ERR-INVALID-INPUT (err u405))

;; Data structures
(define-map products
  { product-id: (string-ascii 64) }
  {
    name: (string-ascii 128),
    manufacturer: principal,
    origin-location: (string-ascii 128),
    creation-block: uint,
    status: (string-ascii 32),
    current-holder: principal,
    batch-number: (string-ascii 64)
  }
)

(define-map product-events
  { product-id: (string-ascii 64), event-index: uint }
  {
    event-type: (string-ascii 32),
    location: (string-ascii 128),
    timestamp: uint,
    handler: principal,
    notes: (string-ascii 256)
  }
)

(define-map product-event-count
  { product-id: (string-ascii 64) }
  { count: uint }
)

(define-map authorized-handlers
  { handler: principal, product-id: (string-ascii 64) }
  { authorized: bool }
)

(define-data-var total-products uint u0)

;; Register a new product in the supply chain
(define-public (register-product (product-id (string-ascii 64)) (name (string-ascii 128)) (origin-location (string-ascii 128)) (batch-number (string-ascii 64)))
  (let (
    (product-exists (map-get? products { product-id: product-id }))
  )
    (if (is-some product-exists)
      ERR-PRODUCT-EXISTS
      (begin
        (map-set products
          { product-id: product-id }
          {
            name: name,
            manufacturer: tx-sender,
            origin-location: origin-location,
            creation-block: burn-block-height,
            status: "manufactured",
            current-holder: tx-sender,
            batch-number: batch-number
          }
        )
        (map-set product-event-count
          { product-id: product-id }
          { count: u0 }
        )
        (var-set total-products (+ (var-get total-products) u1))
        (ok true)
      )
    )
  )
)

;; Record a product movement/event in the supply chain
(define-public (record-event (product-id (string-ascii 64)) (event-type (string-ascii 32)) (location (string-ascii 128)) (notes (string-ascii 256)))
  (let (
    (product (map-get? products { product-id: product-id }))
    (event-count-record (map-get? product-event-count { product-id: product-id }))
    (is-authorized (map-get? authorized-handlers { handler: tx-sender, product-id: product-id }))
  )
    (if (is-none product)
      ERR-NOT-FOUND
      (if (and (not (is-eq tx-sender (get manufacturer (unwrap! product ERR-NOT-FOUND)))) (not (is-eq tx-sender (get current-holder (unwrap! product ERR-NOT-FOUND)))) (is-none is-authorized))
        ERR-UNAUTHORIZED
        (begin
          (let (
            (current-count (default-to u0 (get count event-count-record)))
            (new-count (+ current-count u1))
          )
            (map-set product-events
              { product-id: product-id, event-index: current-count }
              {
                event-type: event-type,
                location: location,
                timestamp: burn-block-height,
                handler: tx-sender,
                notes: notes
              }
            )
            (map-set product-event-count
              { product-id: product-id }
              { count: new-count }
            )
            (ok true)
          )
        )
      )
    )
  )
)

;; Transfer product to a new holder
(define-public (transfer-product (product-id (string-ascii 64)) (new-holder principal) (transfer-location (string-ascii 128)))
  (let (
    (product (unwrap! (map-get? products { product-id: product-id }) ERR-NOT-FOUND))
  )
    (if (not (is-eq tx-sender (get current-holder product)))
      ERR-UNAUTHORIZED
      (begin
        (map-set products
          { product-id: product-id }
          (merge product { current-holder: new-holder, status: "in-transit" })
        )
        (record-event product-id "transfer" transfer-location "Product transferred to new holder")
      )
    )
  )
)

;; Update product status
(define-public (update-status (product-id (string-ascii 64)) (new-status (string-ascii 32)))
  (let (
    (product (unwrap! (map-get? products { product-id: product-id }) ERR-NOT-FOUND))
  )
    (if (not (or (is-eq tx-sender (get manufacturer product)) (is-eq tx-sender (get current-holder product))))
      ERR-UNAUTHORIZED
      (begin
        (map-set products
          { product-id: product-id }
          (merge product { status: new-status })
        )
        (record-event product-id "status-update" "system" (concat "Status updated to: " new-status))
      )
    )
  )
)

;; Authorize a handler to manage a specific product
(define-public (authorize-handler (handler principal) (product-id (string-ascii 64)))
  (let (
    (product (unwrap! (map-get? products { product-id: product-id }) ERR-NOT-FOUND))
  )
    (if (not (is-eq tx-sender (get manufacturer product)))
      ERR-UNAUTHORIZED
      (begin
        (map-set authorized-handlers
          { handler: handler, product-id: product-id }
          { authorized: true }
        )
        (ok true)
      )
    )
  )
)

;; Query product details
(define-read-only (get-product (product-id (string-ascii 64)))
  (map-get? products { product-id: product-id })
)

;; Query product event history
(define-read-only (get-product-event (product-id (string-ascii 64)) (event-index uint))
  (map-get? product-events { product-id: product-id, event-index: event-index })
)

;; Query product event count
(define-read-only (get-product-event-count (product-id (string-ascii 64)))
  (map-get? product-event-count { product-id: product-id })
)

;; Query total products registered
(define-read-only (get-total-products)
  (ok (var-get total-products))
)

;; Check if handler is authorized for a product
(define-read-only (is-handler-authorized (handler principal) (product-id (string-ascii 64)))
  (default-to false (get authorized (map-get? authorized-handlers { handler: handler, product-id: product-id })))
)
