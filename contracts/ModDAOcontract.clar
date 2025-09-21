;; title: ModDAO - Decentralized Content Moderation Network
;; version: 1.0.0
;; summary: A crowdsourced content moderation system with stake-weighted decisions and appeals
;; description: This contract enables decentralized content moderation where moderators stake tokens
;;              on violation classifications, with transparent appeals and reputation tracking

;; traits
(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri (uint) (response (optional (string-utf8 256)) uint))
  )
)

;; token definitions
(define-fungible-token mod-token u1000000000)

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-TOKEN-OWNER (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-CASE-NOT-FOUND (err u104))
(define-constant ERR-ALREADY-VOTED (err u105))
(define-constant ERR-CASE-CLOSED (err u106))
(define-constant ERR-INVALID-PLATFORM (err u107))
(define-constant ERR-APPEAL-WINDOW-CLOSED (err u108))
(define-constant ERR-INSUFFICIENT-STAKE (err u109))
(define-constant ERR-ALREADY-APPEALED (err u110))

(define-constant VIOLATION-NONE u0)
(define-constant VIOLATION-HARASSMENT u1)
(define-constant VIOLATION-SPAM u2)
(define-constant VIOLATION-HATE-SPEECH u3)
(define-constant VIOLATION-MISINFORMATION u4)
(define-constant VIOLATION-ADULT-CONTENT u5)
(define-constant VIOLATION-VIOLENCE u6)

(define-constant STATUS-PENDING u0)
(define-constant STATUS-RESOLVED u1)
(define-constant STATUS-APPEALED u2)
(define-constant STATUS-FINAL u3)

(define-constant MIN-STAKE u1000)
(define-constant APPEAL-WINDOW u144) ;; blocks (~1 day)
(define-constant APPEAL-MULTIPLIER u2)

;; data vars
(define-data-var case-id-nonce uint u0)
(define-data-var platform-id-nonce uint u0)

;; data maps
(define-map platforms
  { platform-id: uint }
  {
    name: (string-ascii 50),
    owner: principal,
    active: bool,
    total-cases: uint,
    reputation-score: uint
  }
)

(define-map moderation-cases
  { case-id: uint }
  {
    platform-id: uint,
    content-hash: (buff 32),
    submitter: principal,
    total-stake: uint,
    violation-votes: (list 7 uint), ;; votes for each violation type (0-6)
    violation-stakes: (list 7 uint), ;; total stake for each violation type
    status: uint,
    created-at: uint,
    resolved-at: uint,
    final-decision: uint,
    cultural-region: (string-ascii 10)
  }
)

(define-map moderator-votes
  { case-id: uint, moderator: principal }
  {
    violation-type: uint,
    stake-amount: uint,
    timestamp: uint,
    cultural-weight: uint
  }
)

(define-map moderator-reputation
  { moderator: principal }
  {
    total-cases: uint,
    correct-votes: uint,
    total-stake: uint,
    earned-rewards: uint,
    cultural-regions: (list 10 (string-ascii 10))
  }
)

(define-map appeals
  { case-id: uint }
  {
    appellant: principal,
    appeal-stake: uint,
    appeal-reason: (string-utf8 500),
    appeal-timestamp: uint,
    appeal-resolved: bool
  }
)

(define-map platform-moderators
  { platform-id: uint, moderator: principal }
  {
    authorized: bool,
    cases-moderated: uint,
    accuracy-score: uint
  }
)

;; public functions
(define-public (register-platform (name (string-ascii 50)))
  (let ((platform-id (+ (var-get platform-id-nonce) u1)))
    (var-set platform-id-nonce platform-id)
    (map-set platforms
      { platform-id: platform-id }
      {
        name: name,
        owner: tx-sender,
        active: true,
        total-cases: u0,
        reputation-score: u1000
      }
    )
    (ok platform-id)
  )
)

(define-public (submit-moderation-case 
  (platform-id uint) 
  (content-hash (buff 32)) 
  (cultural-region (string-ascii 10)))
  (let ((case-id (+ (var-get case-id-nonce) u1))
        (platform-info (unwrap! (map-get? platforms { platform-id: platform-id }) ERR-INVALID-PLATFORM)))
    
    (asserts! (is-eq (get owner platform-info) tx-sender) ERR-NOT-TOKEN-OWNER)
    
    (var-set case-id-nonce case-id)
    (map-set moderation-cases
      { case-id: case-id }
      {
        platform-id: platform-id,
        content-hash: content-hash,
        submitter: tx-sender,
        total-stake: u0,
        violation-votes: (list u0 u0 u0 u0 u0 u0 u0),
        violation-stakes: (list u0 u0 u0 u0 u0 u0 u0),
        status: STATUS-PENDING,
        created-at: u0,
        resolved-at: u0,
        final-decision: VIOLATION-NONE,
        cultural-region: cultural-region
      }
    )
    
    ;; Update platform stats
    (map-set platforms
      { platform-id: platform-id }
      (merge platform-info { total-cases: (+ (get total-cases platform-info) u1) })
    )
    
    (ok case-id)
  )
)

(define-public (vote-on-case 
  (case-id uint) 
  (violation-type uint) 
  (stake-amount uint)
  (cultural-weight uint))
  (let ((case-info (unwrap! (map-get? moderation-cases { case-id: case-id }) ERR-CASE-NOT-FOUND))
        (existing-vote (map-get? moderator-votes { case-id: case-id, moderator: tx-sender })))
    
    (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
    (asserts! (is-eq (get status case-info) STATUS-PENDING) ERR-CASE-CLOSED)
    (asserts! (>= stake-amount MIN-STAKE) ERR-INSUFFICIENT-STAKE)
    (asserts! (<= violation-type VIOLATION-VIOLENCE) ERR-INVALID-AMOUNT)
    (asserts! (>= (ft-get-balance mod-token tx-sender) stake-amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Transfer stake to contract
    (try! (ft-transfer? mod-token stake-amount tx-sender (as-contract tx-sender)))
    
    ;; Record vote
    (map-set moderator-votes
      { case-id: case-id, moderator: tx-sender }
      {
        violation-type: violation-type,
        stake-amount: stake-amount,
        timestamp: u0,
        cultural-weight: cultural-weight
      }
    )
    
    ;; Update case with new vote
    (let ((current-stakes (get violation-stakes case-info))
          (current-votes (get violation-votes case-info))
          (updated-stakes (update-violation-stakes current-stakes violation-type stake-amount))
          (updated-votes (update-violation-votes current-votes violation-type)))
      
      (map-set moderation-cases
        { case-id: case-id }
        (merge case-info {
          total-stake: (+ (get total-stake case-info) stake-amount),
          violation-stakes: updated-stakes,
          violation-votes: updated-votes
        })
      )
    )
    
    ;; Update moderator reputation
    (update-moderator-reputation tx-sender case-id)
    
    (ok true)
  )
)

(define-public (resolve-case (case-id uint))
  (let ((case-info (unwrap! (map-get? moderation-cases { case-id: case-id }) ERR-CASE-NOT-FOUND)))
    
    (asserts! (is-eq (get status case-info) STATUS-PENDING) ERR-CASE-CLOSED)
    
    (let ((final-decision (get-winning-violation (get violation-stakes case-info))))
      (begin
        (map-set moderation-cases
          { case-id: case-id }
          (merge case-info {
            status: STATUS-RESOLVED,
            resolved-at: u0,
            final-decision: final-decision
          })
        )
        
        ;; Distribute rewards to correct voters
        ;; (distribute-rewards case-id final-decision)
        
        (ok final-decision)
      )
    )
  )
)

(define-public (appeal-case 
  (case-id uint) 
  (appeal-reason (string-utf8 500)))
  (let ((case-info (unwrap! (map-get? moderation-cases { case-id: case-id }) ERR-CASE-NOT-FOUND))
        (appeal-stake (* (get total-stake case-info) APPEAL-MULTIPLIER)))
    
    (asserts! (is-eq (get status case-info) STATUS-RESOLVED) ERR-CASE-CLOSED)
    (asserts! (is-eq (get status case-info) STATUS-RESOLVED) ERR-APPEAL-WINDOW-CLOSED)
    (asserts! (is-none (map-get? appeals { case-id: case-id })) ERR-ALREADY-APPEALED)
    (asserts! (>= (ft-get-balance mod-token tx-sender) appeal-stake) ERR-INSUFFICIENT-BALANCE)
    
    ;; Transfer appeal stake
    (try! (ft-transfer? mod-token appeal-stake tx-sender (as-contract tx-sender)))
    
    ;; Record appeal
    (map-set appeals
      { case-id: case-id }
      {
        appellant: tx-sender,
        appeal-stake: appeal-stake,
        appeal-reason: appeal-reason,
        appeal-timestamp: u0,
        appeal-resolved: false
      }
    )
    
    ;; Update case status
    (map-set moderation-cases
      { case-id: case-id }
      (merge case-info { status: STATUS-APPEALED })
    )
    
    (ok true)
  )
)

(define-public (mint-tokens (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (ft-mint? mod-token amount recipient)
  )
)

;; read only functions
(define-read-only (get-case-info (case-id uint))
  (map-get? moderation-cases { case-id: case-id })
)

(define-read-only (get-platform-info (platform-id uint))
  (map-get? platforms { platform-id: platform-id })
)

(define-read-only (get-moderator-vote (case-id uint) (moderator principal))
  (map-get? moderator-votes { case-id: case-id, moderator: moderator })
)

(define-read-only (get-moderator-reputation (moderator principal))
  (map-get? moderator-reputation { moderator: moderator })
)

(define-read-only (get-appeal-info (case-id uint))
  (map-get? appeals { case-id: case-id })
)

(define-read-only (get-token-balance (account principal))
  (ft-get-balance mod-token account)
)

(define-read-only (get-total-supply)
  (ft-get-supply mod-token)
)

(define-read-only (calculate-reputation-score (moderator principal))
  (match (map-get? moderator-reputation { moderator: moderator })
    reputation
    (let ((total-cases (get total-cases reputation))
          (correct-votes (get correct-votes reputation)))
      (if (is-eq total-cases u0)
        u1000
        (/ (* correct-votes u10000) total-cases)
      )
    )
    u1000
  )
)


;; private functions
(define-private (update-violation-stakes (current-stakes (list 7 uint)) (violation-type uint) (stake-amount uint))
  (let ((index violation-type))
    (if (< index u7)
      (list-replace current-stakes index (+ (unwrap-panic (element-at current-stakes index)) stake-amount))
      current-stakes
    )
  )
)

(define-private (update-violation-votes (current-votes (list 7 uint)) (violation-type uint))
  (let ((index violation-type))
    (if (< index u7)
      (list-replace current-votes index (+ (unwrap-panic (element-at current-votes index)) u1))
      current-votes
    )
  )
)

(define-private (get-winning-violation (violation-stakes (list 7 uint)))
  (let ((max-stake (fold max-stake-reducer violation-stakes u0)))
    (find-max-stake-index violation-stakes max-stake)
  )
)

(define-private (max-stake-reducer (stake uint) (current-max uint))
  (if (> stake current-max) stake current-max)
)

(define-private (find-max-stake-index (stakes (list 7 uint)) (target-stake uint))
  (let ((stake-0 (unwrap-panic (element-at stakes u0)))
        (stake-1 (unwrap-panic (element-at stakes u1)))
        (stake-2 (unwrap-panic (element-at stakes u2)))
        (stake-3 (unwrap-panic (element-at stakes u3)))
        (stake-4 (unwrap-panic (element-at stakes u4)))
        (stake-5 (unwrap-panic (element-at stakes u5)))
        (stake-6 (unwrap-panic (element-at stakes u6))))
    
    (if (is-eq stake-0 target-stake) u0
      (if (is-eq stake-1 target-stake) u1
        (if (is-eq stake-2 target-stake) u2
          (if (is-eq stake-3 target-stake) u3
            (if (is-eq stake-4 target-stake) u4
              (if (is-eq stake-5 target-stake) u5
                (if (is-eq stake-6 target-stake) u6
                  u0 ;; fallback to first violation type
                )
              )
            )
          )
        )
      )
    )
  )
)

(define-private (list-replace (lst (list 7 uint)) (index uint) (new-value uint))
  (if (is-eq index u0)
    (list new-value (unwrap-panic (element-at lst u1)) (unwrap-panic (element-at lst u2)) (unwrap-panic (element-at lst u3)) (unwrap-panic (element-at lst u4)) (unwrap-panic (element-at lst u5)) (unwrap-panic (element-at lst u6)))
    (if (is-eq index u1)
      (list (unwrap-panic (element-at lst u0)) new-value (unwrap-panic (element-at lst u2)) (unwrap-panic (element-at lst u3)) (unwrap-panic (element-at lst u4)) (unwrap-panic (element-at lst u5)) (unwrap-panic (element-at lst u6)))
      (if (is-eq index u2)
        (list (unwrap-panic (element-at lst u0)) (unwrap-panic (element-at lst u1)) new-value (unwrap-panic (element-at lst u3)) (unwrap-panic (element-at lst u4)) (unwrap-panic (element-at lst u5)) (unwrap-panic (element-at lst u6)))
        (if (is-eq index u3)
          (list (unwrap-panic (element-at lst u0)) (unwrap-panic (element-at lst u1)) (unwrap-panic (element-at lst u2)) new-value (unwrap-panic (element-at lst u4)) (unwrap-panic (element-at lst u5)) (unwrap-panic (element-at lst u6)))
          (if (is-eq index u4)
            (list (unwrap-panic (element-at lst u0)) (unwrap-panic (element-at lst u1)) (unwrap-panic (element-at lst u2)) (unwrap-panic (element-at lst u3)) new-value (unwrap-panic (element-at lst u5)) (unwrap-panic (element-at lst u6)))
            (if (is-eq index u5)
              (list (unwrap-panic (element-at lst u0)) (unwrap-panic (element-at lst u1)) (unwrap-panic (element-at lst u2)) (unwrap-panic (element-at lst u3)) (unwrap-panic (element-at lst u4)) new-value (unwrap-panic (element-at lst u6)))
              (list (unwrap-panic (element-at lst u0)) (unwrap-panic (element-at lst u1)) (unwrap-panic (element-at lst u2)) (unwrap-panic (element-at lst u3)) (unwrap-panic (element-at lst u4)) (unwrap-panic (element-at lst u5)) new-value)
            )
          )
        )
      )
    )
  )
)

(define-private (update-moderator-reputation (moderator principal) (case-id uint))
  (let ((current-rep (default-to 
                       { total-cases: u0, correct-votes: u0, total-stake: u0, earned-rewards: u0, cultural-regions: (list) }
                       (map-get? moderator-reputation { moderator: moderator }))))
    
    (map-set moderator-reputation
      { moderator: moderator }
      (merge current-rep {
        total-cases: (+ (get total-cases current-rep) u1)
      })
    )
  )
)

(define-private (distribute-rewards (case-id uint) (winning-violation uint))
  (let ((case-info (unwrap-panic (map-get? moderation-cases { case-id: case-id }))))
    ;; Implementation would iterate through voters and reward those who voted correctly
    ;; This is a simplified version - full implementation would require more complex iteration
    (ok true)
  )
)

;; Initialize contract
(begin
  (try! (ft-mint? mod-token u1000000 CONTRACT-OWNER))
  (ok true)
)