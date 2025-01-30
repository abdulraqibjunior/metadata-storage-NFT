;; NFT Metadata Storage - A robust smart contract for storing and managing NFT metadata

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-uri (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-token-id (err u104))
(define-constant err-invalid-string-length (err u105))
(define-constant err-invalid-attributes (err u106))

;; Data Variables
(define-map metadata-store
    { token-id: uint }
    {
        name: (string-utf8 256),
        description: (string-utf8 1024),
        image-uri: (string-utf8 256),
        attributes: (list 20 {trait: (string-utf8 64), value: (string-utf8 64)}),
        created-at: uint,
        updated-at: uint
    }
)

(define-map token-uris
    { token-id: uint }
    { uri: (string-utf8 256) }
)

;; Private Functions
(define-private (is-contract-owner)
    (is-eq tx-sender contract-owner)
)

(define-private (validate-token-id (token-id uint))
    (if (> token-id u0)
        (ok true)
        err-invalid-token-id
    )
)

(define-private (validate-uri (uri (string-utf8 256)))
    (if (and (> (len uri) u0) (<= (len uri) u256))
        (ok true)
        err-invalid-uri
    )
)

(define-private (validate-string-256 (str (string-utf8 256)))
    (if (and (> (len str) u0) (<= (len str) u256))
        (ok true)
        err-invalid-string-length
    )
)

(define-private (validate-string-1024 (str (string-utf8 1024)))
    (if (and (> (len str) u0) (<= (len str) u1024))
        (ok true)
        err-invalid-string-length
    )
)

(define-private (validate-attributes (attributes (list 20 {trait: (string-utf8 64), value: (string-utf8 64)})))
    (if (<= (len attributes) u20)
        (ok true)
        err-invalid-attributes
    )
)

;; Public Functions
(define-public (set-metadata
        (token-id uint)
        (name (string-utf8 256))
        (description (string-utf8 1024))
        (image-uri (string-utf8 256))
        (attributes (list 20 {trait: (string-utf8 64), value: (string-utf8 64)}))
    )
    (begin
        (asserts! (is-contract-owner) err-owner-only)
        (asserts! (is-ok (validate-token-id token-id)) err-invalid-token-id)
        (asserts! (is-ok (validate-uri image-uri)) err-invalid-uri)
        (asserts! (is-ok (validate-string-256 name)) err-invalid-string-length)
        (asserts! (is-ok (validate-string-1024 description)) err-invalid-string-length)
        (asserts! (is-ok (validate-attributes attributes)) err-invalid-attributes)
        (asserts! (is-none (map-get? metadata-store { token-id: token-id })) err-already-exists)
        
        (map-set metadata-store
            { token-id: token-id }
            {
                name: name,
                description: description,
                image-uri: image-uri,
                attributes: attributes,
                created-at: block-height,
                updated-at: block-height
            }
        )
        (ok true)
    )
)

(define-public (update-metadata
        (token-id uint)
        (name (string-utf8 256))
        (description (string-utf8 1024))
        (image-uri (string-utf8 256))
        (attributes (list 20 {trait: (string-utf8 64), value: (string-utf8 64)}))
    )
    (begin
        (asserts! (is-contract-owner) err-owner-only)
        (asserts! (is-ok (validate-token-id token-id)) err-invalid-token-id)
        (asserts! (is-ok (validate-uri image-uri)) err-invalid-uri)
        (asserts! (is-ok (validate-string-256 name)) err-invalid-string-length)
        (asserts! (is-ok (validate-string-1024 description)) err-invalid-string-length)
        (asserts! (is-ok (validate-attributes attributes)) err-invalid-attributes)
        (asserts! (is-some (map-get? metadata-store { token-id: token-id })) err-not-found)
        
        (map-set metadata-store
            { token-id: token-id }
            {
                name: name,
                description: description,
                image-uri: image-uri,
                attributes: attributes,
                created-at: (get created-at (unwrap-panic (map-get? metadata-store { token-id: token-id }))),
                updated-at: block-height
            }
        )
        (ok true)
    )
)

(define-read-only (get-metadata (token-id uint))
    (begin
        (asserts! (is-ok (validate-token-id token-id)) err-invalid-token-id)
        (ok (map-get? metadata-store { token-id: token-id }))
    )
)

(define-public (set-token-uri (token-id uint) (uri (string-utf8 256)))
    (begin
        (asserts! (is-contract-owner) err-owner-only)
        (asserts! (is-ok (validate-token-id token-id)) err-invalid-token-id)
        (asserts! (is-ok (validate-uri uri)) err-invalid-uri)
        
        (map-set token-uris
            { token-id: token-id }
            { uri: uri }
        )
        (ok true)
    )
)

(define-read-only (get-token-uri (token-id uint))
    (begin
        (asserts! (is-ok (validate-token-id token-id)) err-invalid-token-id)
        (ok (map-get? token-uris { token-id: token-id }))
    )
)

(define-public (bulk-set-metadata
    (metadata-list (list 50 {
        token-id: uint,
        name: (string-utf8 256),
        description: (string-utf8 1024),
        image-uri: (string-utf8 256),
        attributes: (list 20 {trait: (string-utf8 64), value: (string-utf8 64)})
    }))
)
    (begin
        (asserts! (is-contract-owner) err-owner-only)
        (ok (fold process-metadata metadata-list (ok true)))
    )
)

(define-private (process-metadata 
    (metadata {
        token-id: uint,
        name: (string-utf8 256),
        description: (string-utf8 1024),
        image-uri: (string-utf8 256),
        attributes: (list 20 {trait: (string-utf8 64), value: (string-utf8 64)})
    }) 
    (previous-result (response bool uint))
)
    (match previous-result
        result (set-metadata 
            (get token-id metadata)
            (get name metadata)
            (get description metadata)
            (get image-uri metadata)
            (get attributes metadata)
        )
        error previous-result
    )
)