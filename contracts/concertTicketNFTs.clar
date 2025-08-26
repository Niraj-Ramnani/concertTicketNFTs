;; ConcertTicket NFT Contract
;; Anti-scalping concert tickets with transferable ownership and artist royalties

;; Define the NFT
(define-non-fungible-token concert-ticket uint)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-token-exists (err u102))
(define-constant err-token-not-found (err u103))
(define-constant err-invalid-price (err u104))
(define-constant err-scalping-detected (err u105))
(define-constant err-event-not-found (err u106))

;; Data variables
(define-data-var last-token-id uint u0)

;; Concert event details
(define-map events uint {
  artist: (string-ascii 50),
  venue: (string-ascii 100),
  date: uint,
  base-price: uint,
  royalty-percentage: uint
})

;; Ticket details
(define-map tickets uint {
  event-id: uint,
  original-price: uint,
  current-owner: principal,
  is-resale: bool,
  resale-count: uint
})

;; Track original purchasers to prevent scalping
(define-map original-buyers uint principal)

;; Function 1: Mint Concert Ticket
;; Mints a new concert ticket NFT for a specific event
(define-public (mint-ticket (event-id uint) (recipient principal) (price uint))
  (let (
    (token-id (+ (var-get last-token-id) u1))
    (event-data (unwrap! (map-get? events event-id) err-event-not-found))
  )
    (begin
      ;; Only contract owner (venue/platform) can mint tickets
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (asserts! (> price u0) err-invalid-price)
      
      ;; Mint the NFT
      (try! (nft-mint? concert-ticket token-id recipient))
      
      ;; Store ticket details
      (map-set tickets token-id {
        event-id: event-id,
        original-price: price,
        current-owner: recipient,
        is-resale: false,
        resale-count: u0
      })
      
      ;; Track original buyer
      (map-set original-buyers token-id recipient)
      
      ;; Update token counter
      (var-set last-token-id token-id)
      
      (ok token-id)
    )
  )
)

;; Function 2: Transfer Ticket with Anti-Scalping & Royalties
;; Transfers ticket with price validation and artist royalty distribution
(define-public (transfer-ticket (token-id uint) (sender principal) (recipient principal) (sale-price uint))
  (let (
    (ticket-data (unwrap! (map-get? tickets token-id) err-token-not-found))
    (event-data (unwrap! (map-get? events (get event-id ticket-data)) err-event-not-found))
    (original-buyer (unwrap! (map-get? original-buyers token-id) err-token-not-found))
    (max-resale-price (* (get original-price ticket-data) u150)) ;; 150% of original price max
    (royalty-amount (/ (* sale-price (get royalty-percentage event-data)) u100))
    (seller-amount (- sale-price royalty-amount))
  )
    (begin
      ;; Verify sender owns the ticket
      (asserts! (is-eq (some sender) (nft-get-owner? concert-ticket token-id)) err-not-token-owner)
      (asserts! (> sale-price u0) err-invalid-price)
      
      ;; Anti-scalping: Prevent price inflation beyond 150% of original price
      (asserts! (<= sale-price max-resale-price) err-scalping-detected)
      
      ;; Transfer payment: royalties to contract owner (representing artist)
      (if (> royalty-amount u0)
        (try! (stx-transfer? royalty-amount recipient contract-owner))
        true
      )
      
      ;; Transfer remaining amount to seller
      (if (> seller-amount u0)
        (try! (stx-transfer? seller-amount recipient sender))
        true
      )
      
      ;; Transfer the NFT
      (try! (nft-transfer? concert-ticket token-id sender recipient))
      
      ;; Update ticket details
      (map-set tickets token-id (merge ticket-data {
        current-owner: recipient,
        is-resale: true,
        resale-count: (+ (get resale-count ticket-data) u1)
      }))
      
      (ok true)
    )
  )
)

;; Read-only functions
(define-read-only (get-ticket-info (token-id uint))
  (map-get? tickets token-id))

(define-read-only (get-event-info (event-id uint))
  (map-get? events event-id))

(define-read-only (get-last-token-id)
  (ok (var-get last-token-id)))

(define-read-only (get-ticket-owner (token-id uint))
  (ok (nft-get-owner? concert-ticket token-id)))

;; Helper function to create events (only owner)
(define-public (create-event (event-id uint) (artist (string-ascii 50)) (venue (string-ascii 100)) (date uint) (base-price uint) (royalty-percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set events event-id {
      artist: artist,
      venue: venue,
      date: date,
      base-price: base-price,
      royalty-percentage: royalty-percentage
    })
    (ok true)
  )
)