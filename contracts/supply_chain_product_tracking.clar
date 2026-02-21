;; title: supply_chain_product_tracking
;; version: 1.0.0
;; summary: Smart contract for Supply Chain Product Tracking

;; Error codes
(define-constant ERR-NOT-FOUND (err u1))
(define-constant ERR-UNAUTHORIZED (err u2))
(define-constant ERR-INVALID-STATUS (err u3))
(define-constant ERR-ALREADY-EXISTS (err u4))
(define-constant ERR-INVALID-INPUT (err u5))

;; Data structures
(define-map products
  { product-id: (string-ascii 64) }
  {
    name: (string-ascii 128),
    manufacturer: principal,
    origin: (string-ascii 128),
    created-at: uint,
    status: (string-ascii 32),
    current-holder: principal
  }
)

(define-map product-locations
  { product-id: (string-ascii 64), location-index: uint }
  {
    location: (string-ascii 128),
    timestamp: uint,
    holder: principal,
    notes: (string-ascii 256)
  }
)

(define-map product-location-count
  { product-id: (string-ascii 64) }
  { count: uint }
)

(define-map authorized-handlers
  { product-id: (string-ascii 64), handler: principal }
  { authorized: bool }
)

(define-data-var contract-owner principal tx-sender)
(define-data-var total-products uint u0)

;; Register a new product in the supply chain
(define-public (register-product (product-id (string-ascii 64)) (name (string-ascii 128)) (origin (string-ascii 128)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (asserts! (is-none (map-get? products { product-id: product-id })) ERR-ALREADY-EXISTS)
    (map-set products
      { product-id: product-id }
      {
        name: name,
        manufacturer: tx-sender,
        origin: origin,
        created-at: burn-block-height,
        status: "registered",
        current-holder: tx-sender
      }
    )
    (map-set product-location-count { product-id: product-id } { count: u0 })
    (var-set total-products (+ (var-get total-products) u1))
    (ok product-id)
  )
)

;; Record a location/movement of a product
(define-public (record-location (product-id (string-ascii 64)) (location (string-ascii 128)) (notes (string-ascii 256)))
  (begin
    (asserts! (is-some (map-get? products { product-id: product-id })) ERR-NOT-FOUND)
    (let ((current-count (default-to { count: u0 } (map-get? product-location-count { product-id: product-id }))))
      (map-set product-locations
        { product-id: product-id, location-index: (get count current-count) }
        {
          location: location,
          timestamp: burn-block-height,
          holder: tx-sender,
          notes: notes
        }
      )
      (map-set product-location-count { product-id: product-id } { count: (+ (get count current-count) u1) })
      (map-set products
        { product-id: product-id }
        (merge (unwrap! (map-get? products { product-id: product-id }) ERR-NOT-FOUND)
          { current-holder: tx-sender }
        )
      )
      (ok (get count current-count))
    )
  )
)

;; Update product status
(define-public (update-product-status (product-id (string-ascii 64)) (new-status (string-ascii 32)))
  (begin
    (asserts! (is-some (map-get? products { product-id: product-id })) ERR-NOT-FOUND)
    (let ((product (unwrap! (map-get? products { product-id: product-id }) ERR-NOT-FOUND)))
      (asserts! (is-eq tx-sender (get manufacturer product)) ERR-UNAUTHORIZED)
      (map-set products
        { product-id: product-id }
        (merge product { status: new-status })
      )
      (ok new-status)
    )
  )
)

;; Authorize a handler to manage a product
(define-public (authorize-handler (product-id (string-ascii 64)) (handler principal))
  (begin
    (asserts! (is-some (map-get? products { product-id: product-id })) ERR-NOT-FOUND)
    (let ((product (unwrap! (map-get? products { product-id: product-id }) ERR-NOT-FOUND)))
      (asserts! (is-eq tx-sender (get manufacturer product)) ERR-UNAUTHORIZED)
      (map-set authorized-handlers
        { product-id: product-id, handler: handler }
        { authorized: true }
      )
      (ok true)
    )
  )
)

;; Revoke handler authorization
(define-public (revoke-handler (product-id (string-ascii 64)) (handler principal))
  (begin
    (asserts! (is-some (map-get? products { product-id: product-id })) ERR-NOT-FOUND)
    (let ((product (unwrap! (map-get? products { product-id: product-id }) ERR-NOT-FOUND)))
      (asserts! (is-eq tx-sender (get manufacturer product)) ERR-UNAUTHORIZED)
      (map-delete authorized-handlers { product-id: product-id, handler: handler })
      (ok true)
    )
  )
)

;; Transfer product custody
(define-public (transfer-custody (product-id (string-ascii 64)) (new-holder principal))
  (begin
    (asserts! (is-some (map-get? products { product-id: product-id })) ERR-NOT-FOUND)
    (let ((product (unwrap! (map-get? products { product-id: product-id }) ERR-NOT-FOUND)))
      (asserts! (or (is-eq tx-sender (get current-holder product)) (is-eq tx-sender (get manufacturer product))) ERR-UNAUTHORIZED)
      (map-set products
        { product-id: product-id }
        (merge product { current-holder: new-holder })
      )
      (ok new-holder)
    )
  )
)

;; Read-only: Get product details
(define-read-only (get-product (product-id (string-ascii 64)))
  (map-get? products { product-id: product-id })
)

;; Read-only: Get product location history
(define-read-only (get-location-history (product-id (string-ascii 64)) (index uint))
  (map-get? product-locations { product-id: product-id, location-index: index })
)

;; Read-only: Get location count for a product
(define-read-only (get-location-count (product-id (string-ascii 64)))
  (map-get? product-location-count { product-id: product-id })
)

;; Read-only: Check if handler is authorized
(define-read-only (is-handler-authorized (product-id (string-ascii 64)) (handler principal))
  (default-to false (get authorized (map-get? authorized-handlers { product-id: product-id, handler: handler })))
)

;; Read-only: Get total products registered
(define-read-only (get-total-products)
  (var-get total-products)
)
