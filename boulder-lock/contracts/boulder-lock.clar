;; Boulder Lock - Geological Exploration Blockchain Game
;; A geological exploration game with NFT mining equipment, cave systems, and dual-token economy

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-equipment-broken (err u105))
(define-constant err-invalid-cave (err u106))
(define-constant err-mining-rights-required (err u107))
(define-constant err-invalid-mineral (err u108))

;; Data Variables
(define-data-var rock-token-supply uint u0)
(define-data-var crystal-token-supply uint u0)
(define-data-var next-equipment-id uint u1)
(define-data-var next-cave-id uint u1)
(define-data-var next-mineral-id uint u1)

;; Data Maps

;; ROCK token balances (common transactions)
(define-map rock-balances principal uint)

;; CRYSTAL token balances (rare specimens)
(define-map crystal-balances principal uint)

;; NFT Mining Equipment
;; Equipment degrades based on environmental conditions
(define-map equipment
  uint
  {
    owner: principal,
    equipment-type: (string-ascii 50),
    durability: uint,
    max-durability: uint,
    acid-resistance: uint,
    pressure-resistance: uint,
    temperature-resistance: uint,
    discovery-boost: uint
  }
)

;; Cave Systems
;; Procedurally generated underground cave sectors
(define-map caves
  uint
  {
    owner: principal,
    name: (string-ascii 100),
    depth: uint,
    acidity: uint,
    pressure: uint,
    temperature: uint,
    mineral-richness: uint,
    discovered: bool
  }
)

;; Mining Rights
;; Tracks which player has mining rights in specific caves
(define-map mining-rights
  { cave-id: uint, miner: principal }
  { expires-at: uint, active: bool }
)

;; Mineral Specimens
;; NFTs with metadata that evolves based on discovery conditions
(define-map minerals
  uint
  {
    owner: principal,
    mineral-type: (string-ascii 50),
    rarity: uint,
    discovery-cave: uint,
    discovery-depth: uint,
    discovery-conditions: (string-ascii 200),
    crystal-value: uint
  }
)

;; Player Statistics
(define-map player-stats
  principal
  {
    total-discoveries: uint,
    total-mining-time: uint,
    expertise-level: uint,
    caves-owned: uint
  }
)

;; Read-Only Functions

(define-read-only (get-rock-balance (account principal))
  (default-to u0 (map-get? rock-balances account))
)

(define-read-only (get-crystal-balance (account principal))
  (default-to u0 (map-get? crystal-balances account))
)

(define-read-only (get-equipment (equipment-id uint))
  (map-get? equipment equipment-id)
)

(define-read-only (get-cave (cave-id uint))
  (map-get? caves cave-id)
)

(define-read-only (get-mineral (mineral-id uint))
  (map-get? minerals mineral-id)
)

(define-read-only (get-mining-rights (cave-id uint) (miner principal))
  (map-get? mining-rights { cave-id: cave-id, miner: miner })
)

(define-read-only (get-player-stats (player principal))
  (map-get? player-stats player)
)

(define-read-only (check-equipment-usable (equipment-id uint) (cave-id uint))
  (let
    (
      (equip (unwrap! (get-equipment equipment-id) (err err-not-found)))
      (cave (unwrap! (get-cave cave-id) (err err-not-found)))
    )
    (ok (and
      (>= (get durability equip) u1)
      (>= (get acid-resistance equip) (get acidity cave))
      (>= (get pressure-resistance equip) (get pressure cave))
      (>= (get temperature-resistance equip) (get temperature cave))
    ))
  )
)

;; Public Functions - Token Management

(define-public (mint-rock-tokens (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set rock-balances recipient (+ (get-rock-balance recipient) amount))
    (var-set rock-token-supply (+ (var-get rock-token-supply) amount))
    (ok true)
  )
)

(define-public (mint-crystal-tokens (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set crystal-balances recipient (+ (get-crystal-balance recipient) amount))
    (var-set crystal-token-supply (+ (var-get crystal-token-supply) amount))
    (ok true)
  )
)

(define-public (transfer-rock (amount uint) (sender principal) (recipient principal))
  (let
    (
      (sender-balance (get-rock-balance sender))
    )
    (asserts! (is-eq tx-sender sender) err-unauthorized)
    (asserts! (>= sender-balance amount) err-insufficient-balance)
    (map-set rock-balances sender (- sender-balance amount))
    (map-set rock-balances recipient (+ (get-rock-balance recipient) amount))
    (ok true)
  )
)

(define-public (transfer-crystal (amount uint) (sender principal) (recipient principal))
  (let
    (
      (sender-balance (get-crystal-balance sender))
    )
    (asserts! (is-eq tx-sender sender) err-unauthorized)
    (asserts! (>= sender-balance amount) err-insufficient-balance)
    (map-set crystal-balances sender (- sender-balance amount))
    (map-set crystal-balances recipient (+ (get-crystal-balance recipient) amount))
    (ok true)
  )
)

;; Public Functions - Equipment Management

(define-public (craft-equipment 
  (equipment-type (string-ascii 50))
  (acid-res uint)
  (pressure-res uint)
  (temp-res uint)
  (discovery-boost uint)
  (rock-cost uint)
)
  (let
    (
      (equipment-id (var-get next-equipment-id))
      (max-durability u100)
      (sender-balance (get-rock-balance tx-sender))
    )
    (asserts! (>= sender-balance rock-cost) err-insufficient-balance)
    (map-set rock-balances tx-sender (- sender-balance rock-cost))
    (map-set equipment equipment-id {
      owner: tx-sender,
      equipment-type: equipment-type,
      durability: max-durability,
      max-durability: max-durability,
      acid-resistance: acid-res,
      pressure-resistance: pressure-res,
      temperature-resistance: temp-res,
      discovery-boost: discovery-boost
    })
    (var-set next-equipment-id (+ equipment-id u1))
    (ok equipment-id)
  )
)

(define-public (repair-equipment (equipment-id uint) (rock-cost uint))
  (let
    (
      (equip (unwrap! (get-equipment equipment-id) err-not-found))
      (sender-balance (get-rock-balance tx-sender))
    )
    (asserts! (is-eq (get owner equip) tx-sender) err-unauthorized)
    (asserts! (>= sender-balance rock-cost) err-insufficient-balance)
    (map-set rock-balances tx-sender (- sender-balance rock-cost))
    (map-set equipment equipment-id (merge equip {
      durability: (get max-durability equip)
    }))
    (ok true)
  )
)

;; Public Functions - Cave Management

(define-public (discover-cave
  (name (string-ascii 100))
  (depth uint)
  (acidity uint)
  (pressure uint)
  (temperature uint)
  (mineral-richness uint)
)
  (let
    (
      (cave-id (var-get next-cave-id))
      (current-stats (default-to 
        { total-discoveries: u0, total-mining-time: u0, expertise-level: u0, caves-owned: u0 }
        (get-player-stats tx-sender)))
      (new-caves (+ (get caves-owned current-stats) u1))
    )
    (map-set caves cave-id {
      owner: tx-sender,
      name: name,
      depth: depth,
      acidity: acidity,
      pressure: pressure,
      temperature: temperature,
      mineral-richness: mineral-richness,
      discovered: true
    })
    (map-set player-stats tx-sender (merge current-stats {
      caves-owned: new-caves
    }))
    (var-set next-cave-id (+ cave-id u1))
    (ok cave-id)
  )
)

(define-public (transfer-cave (cave-id uint) (recipient principal) (rock-price uint))
  (let
    (
      (cave (unwrap! (get-cave cave-id) err-not-found))
    )
    (asserts! (is-eq (get owner cave) tx-sender) err-unauthorized)
    (try! (transfer-rock rock-price recipient tx-sender))
    (map-set caves cave-id (merge cave { owner: recipient }))
    (ok true)
  )
)

;; Public Functions - Mining Rights

(define-public (acquire-mining-rights (cave-id uint) (duration uint) (rock-cost uint))
  (let
    (
      (cave (unwrap! (get-cave cave-id) err-not-found))
      (expires-at (+ block-height duration))
      (sender-balance (get-rock-balance tx-sender))
      (owner-address (get owner cave))
    )
    (asserts! (>= sender-balance rock-cost) err-insufficient-balance)
    (map-set rock-balances tx-sender (- sender-balance rock-cost))
    (map-set rock-balances owner-address (+ (get-rock-balance owner-address) rock-cost))
    (map-set mining-rights 
      { cave-id: cave-id, miner: tx-sender }
      { expires-at: expires-at, active: true }
    )
    (ok true)
  )
)

;; Public Functions - Mineral Discovery

(define-public (discover-mineral
  (cave-id uint)
  (equipment-id uint)
  (mineral-type (string-ascii 50))
  (rarity uint)
)
  (let
    (
      (cave (unwrap! (get-cave cave-id) err-not-found))
      (equip (unwrap! (get-equipment equipment-id) err-not-found))
      (rights (unwrap! (get-mining-rights cave-id tx-sender) err-mining-rights-required))
      (mineral-id (var-get next-mineral-id))
      (crystal-reward (calculate-crystal-reward rarity (get mineral-richness cave)))
      (degradation (calculate-degradation equip cave))
      (new-durability (if (> (get durability equip) degradation)
                        (- (get durability equip) degradation)
                        u0))
      (current-stats (default-to 
        { total-discoveries: u0, total-mining-time: u0, expertise-level: u0, caves-owned: u0 }
        (get-player-stats tx-sender)))
      (new-discoveries (+ (get total-discoveries current-stats) u1))
      (new-expertise (/ new-discoveries u10))
    )
    (asserts! (get active rights) err-mining-rights-required)
    (asserts! (>= (get expires-at rights) block-height) err-mining-rights-required)
    (asserts! (is-eq (get owner equip) tx-sender) err-unauthorized)
    (asserts! (>= (get durability equip) u1) err-equipment-broken)
    
    ;; Degrade equipment based on cave conditions
    (map-set equipment equipment-id (merge equip {
      durability: new-durability
    }))
    
    ;; Create mineral NFT
    (map-set minerals mineral-id {
      owner: tx-sender,
      mineral-type: mineral-type,
      rarity: rarity,
      discovery-cave: cave-id,
      discovery-depth: (get depth cave),
      discovery-conditions: "Normal extraction",
      crystal-value: crystal-reward
    })
    
    ;; Reward CRYSTAL tokens
    (map-set crystal-balances tx-sender (+ (get-crystal-balance tx-sender) crystal-reward))
    (var-set crystal-token-supply (+ (var-get crystal-token-supply) crystal-reward))
    
    ;; Update stats
    (map-set player-stats tx-sender (merge current-stats {
      total-discoveries: new-discoveries,
      expertise-level: new-expertise
    }))
    
    (var-set next-mineral-id (+ mineral-id u1))
    (ok mineral-id)
  )
)

(define-public (transfer-mineral (mineral-id uint) (recipient principal) (crystal-price uint))
  (let
    (
      (mineral (unwrap! (get-mineral mineral-id) err-not-found))
      (recipient-balance (get-crystal-balance recipient))
    )
    (asserts! (is-eq (get owner mineral) tx-sender) err-unauthorized)
    (asserts! (>= recipient-balance crystal-price) err-insufficient-balance)
    (map-set crystal-balances recipient (- recipient-balance crystal-price))
    (map-set crystal-balances tx-sender (+ (get-crystal-balance tx-sender) crystal-price))
    (map-set minerals mineral-id (merge mineral { owner: recipient }))
    (ok true)
  )
)

;; Private Functions

(define-private (calculate-degradation (equip (tuple (owner principal) (equipment-type (string-ascii 50)) (durability uint) (max-durability uint) (acid-resistance uint) (pressure-resistance uint) (temperature-resistance uint) (discovery-boost uint))) (cave (tuple (owner principal) (name (string-ascii 100)) (depth uint) (acidity uint) (pressure uint) (temperature uint) (mineral-richness uint) (discovered bool))))
  (let
    (
      (acid-damage (if (< (get acid-resistance equip) (get acidity cave))
                     (- (get acidity cave) (get acid-resistance equip))
                     u0))
      (pressure-damage (if (< (get pressure-resistance equip) (get pressure cave))
                         (- (get pressure cave) (get pressure-resistance equip))
                         u0))
      (temp-damage (if (< (get temperature-resistance equip) (get temperature cave))
                     (- (get temperature cave) (get temperature-resistance equip))
                     u0))
    )
    (+ (+ acid-damage pressure-damage) temp-damage)
  )
)

(define-private (calculate-crystal-reward (rarity uint) (mineral-richness uint))
  (* rarity mineral-richness)
)

;; Initialize contract with initial token supply
(begin
  (map-set rock-balances contract-owner u1000000)
  (var-set rock-token-supply u1000000)
  (map-set crystal-balances contract-owner u100000)
  (var-set crystal-token-supply u100000)
)
